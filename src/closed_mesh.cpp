#include <Rcpp.h>
#include <unordered_map>
#include <vector>
#include <algorithm>
#include <cmath>
#include <string>

using namespace Rcpp;

struct SmoothPoint {
  double x;
  double y;
  double z;
};

struct TubeFrame {
  SmoothPoint tangent;
  SmoothPoint normal;
  SmoothPoint binormal;
};

struct Ring {
  std::vector<SmoothPoint> p;
  std::vector<int> id;
};

struct IndexedMesh {
  std::vector<double> vertices;
  std::vector<int> indices;
  std::vector<int> face_colors;
};

struct GraphNode {
  SmoothPoint p;
  std::vector<int> edges;
};

struct GraphEdge {
  int a;
  int b;

  double ar;
  double ag;
  double ab;

  double br;
  double bg;
  double bb;

  double r0;
  double r1;
};

struct NodeRing {
  Ring ring;

  double r;
  double g;
  double b;

  bool reverse;
};

struct SplitPoint {
  double h;
  SmoothPoint p;

  double radius;

  double cr;
  double cg;
  double cb;
};

struct PointKey {
  long long x;
  long long y;
  long long z;

  bool operator==(const PointKey& other) const {
    return x == other.x && y == other.y && z == other.z;
  }
};

struct PointKeyHash {
  std::size_t operator()(const PointKey& k) const {
    std::size_t h1 = std::hash<long long>()(k.x);
    std::size_t h2 = std::hash<long long>()(k.y);
    std::size_t h3 = std::hash<long long>()(k.z);

    return h1 ^ (h2 << 1) ^ (h3 << 2);
  }
};

// Geometry helpers ------------------------------------------------------------

inline SmoothPoint sm_add(SmoothPoint a, SmoothPoint b) {
  return {a.x + b.x, a.y + b.y, a.z + b.z};
}

inline SmoothPoint sm_sub(SmoothPoint a, SmoothPoint b) {
  return {a.x - b.x, a.y - b.y, a.z - b.z};
}

inline SmoothPoint sm_mul(SmoothPoint a, double s) {
  return {a.x * s, a.y * s, a.z * s};
}

inline double sm_dot(SmoothPoint a, SmoothPoint b) {
  return a.x * b.x + a.y * b.y + a.z * b.z;
}

inline SmoothPoint sm_cross(SmoothPoint a, SmoothPoint b) {
  return {
  a.y * b.z - a.z * b.y,
  a.z * b.x - a.x * b.z,
  a.x * b.y - a.y * b.x
};
}

inline double sm_norm(SmoothPoint a) {
  return std::sqrt(sm_dot(a, a));
}

inline SmoothPoint sm_unit(SmoothPoint a) {
  double n = sm_norm(a);

  if (n < 1e-12) {
    return {0.0, 0.0, 1.0};
  }

  return sm_mul(a, 1.0 / n);
}

inline double sm_distance(SmoothPoint a, SmoothPoint b) {
  return sm_norm(sm_sub(a, b));
}

inline double sm_smoothstep(double x) {
  if (x < 0.0) x = 0.0;
  if (x > 1.0) x = 1.0;

  return x * x * (3.0 - 2.0 * x);
}

inline SmoothPoint sm_lerp_point(
    SmoothPoint a,
    SmoothPoint b,
    double t
) {
  return sm_add(
    sm_mul(a, 1.0 - t),
    sm_mul(b, t)
  );
}

inline double sm_lerp(
    double a,
    double b,
    double t
) {
  return a * (1.0 - t) + b * t;
}

inline PointKey make_point_key(SmoothPoint p) {
  const double scale = 1e8;

  PointKey key;
  key.x = static_cast<long long>(std::llround(p.x * scale));
  key.y = static_cast<long long>(std::llround(p.y * scale));
  key.z = static_cast<long long>(std::llround(p.z * scale));

  return key;
}

inline long long make_edge_key(int a, int b) {
  int lo = std::min(a, b);
  int hi = std::max(a, b);

  return (static_cast<long long>(lo) << 32) ^
    static_cast<unsigned int>(hi);
}

// Color helpers ---------------------------------------------------------------

inline double clamp_rgb(double x) {
  if (x < 0.0) return 0.0;
  if (x > 255.0) return 255.0;
  return x;
}

inline int hex_value(char c) {
  if (c >= '0' && c <= '9') return c - '0';
  if (c >= 'a' && c <= 'f') return 10 + c - 'a';
  if (c >= 'A' && c <= 'F') return 10 + c - 'A';

  return 0;
}

inline void hex_to_rgb(
    std::string hex,
    double& r,
    double& g,
    double& b
) {
  if (hex.size() >= 7 && hex[0] == '#') {
    r = 16 * hex_value(hex[1]) + hex_value(hex[2]);
    g = 16 * hex_value(hex[3]) + hex_value(hex[4]);
    b = 16 * hex_value(hex[5]) + hex_value(hex[6]);
  } else {
    r = 255.0;
    g = 255.0;
    b = 255.0;
  }
}

inline std::string get_color_string(
    CharacterVector color,
    int i
) {
  if (CharacterVector::is_na(color[i])) {
    return "#FFFFFF";
  }

  return as<std::string>(color[i]);
}

// Indexed mesh helpers --------------------------------------------------------

inline int add_indexed_vertex(
    IndexedMesh& mesh,
    SmoothPoint p
) {
  int id = mesh.vertices.size() / 3;

  mesh.vertices.push_back(p.x);
  mesh.vertices.push_back(p.y);
  mesh.vertices.push_back(p.z);

  return id;
}

inline void add_indexed_tri(
    IndexedMesh& mesh,
    int a,
    int b,
    int c,
    double r,
    double g,
    double b_col
) {
  mesh.indices.push_back(a + 1);
  mesh.indices.push_back(b + 1);
  mesh.indices.push_back(c + 1);

  mesh.face_colors.push_back(static_cast<int>(std::round(clamp_rgb(r))));
  mesh.face_colors.push_back(static_cast<int>(std::round(clamp_rgb(g))));
  mesh.face_colors.push_back(static_cast<int>(std::round(clamp_rgb(b_col))));
}

// Smoothing helpers -----------------------------------------------------------

std::vector<SmoothPoint> smooth_points(
    std::vector<SmoothPoint> x,
    int passes
) {
  if (x.size() < 3) {
    return x;
  }

  for (int pass = 0; pass < passes; pass++) {
    std::vector<SmoothPoint> y = x;

    for (int i = 1; i < static_cast<int>(x.size()) - 1; i++) {
      y[i].x = 0.25 * x[i - 1].x + 0.50 * x[i].x + 0.25 * x[i + 1].x;
      y[i].y = 0.25 * x[i - 1].y + 0.50 * x[i].y + 0.25 * x[i + 1].y;
      y[i].z = 0.25 * x[i - 1].z + 0.50 * x[i].z + 0.25 * x[i + 1].z;
    }

    x = y;
  }

  return x;
}

std::vector<double> smooth_values(
    std::vector<double> x,
    int passes
) {
  if (x.size() < 3) {
    return x;
  }

  for (int pass = 0; pass < passes; pass++) {
    std::vector<double> y = x;

    for (int i = 1; i < static_cast<int>(x.size()) - 1; i++) {
      y[i] = 0.25 * x[i - 1] + 0.50 * x[i] + 0.25 * x[i + 1];
    }

    x = y;
  }

  return x;
}

void remove_near_duplicate_samples(
    std::vector<SmoothPoint>& pts,
    std::vector<double>& rad,
    std::vector<double>& red,
    std::vector<double>& green,
    std::vector<double>& blue
) {
  if (pts.empty()) {
    return;
  }

  std::vector<SmoothPoint> out_pts;
  std::vector<double> out_rad;
  std::vector<double> out_red;
  std::vector<double> out_green;
  std::vector<double> out_blue;

  out_pts.reserve(pts.size());
  out_rad.reserve(rad.size());
  out_red.reserve(red.size());
  out_green.reserve(green.size());
  out_blue.reserve(blue.size());

  out_pts.push_back(pts[0]);
  out_rad.push_back(rad[0]);
  out_red.push_back(red[0]);
  out_green.push_back(green[0]);
  out_blue.push_back(blue[0]);

  for (int i = 1; i < static_cast<int>(pts.size()); i++) {
    if (sm_distance(pts[i], out_pts.back()) < 1e-8) {
      out_rad.back() = rad[i];
      out_red.back() = red[i];
      out_green.back() = green[i];
      out_blue.back() = blue[i];
      continue;
    }

    out_pts.push_back(pts[i]);
    out_rad.push_back(rad[i]);
    out_red.push_back(red[i]);
    out_green.push_back(green[i]);
    out_blue.push_back(blue[i]);
  }

  pts = out_pts;
  rad = out_rad;
  red = out_red;
  green = out_green;
  blue = out_blue;
}

void resample_smooth_chain(
    const std::vector<SmoothPoint>& pts,
    const std::vector<double>& rad,
    const std::vector<double>& red,
    const std::vector<double>& green,
    const std::vector<double>& blue,
    std::vector<SmoothPoint>& sm_pts,
    std::vector<double>& sm_rad,
    std::vector<double>& sm_red,
    std::vector<double>& sm_green,
    std::vector<double>& sm_blue
) {
  sm_pts.clear();
  sm_rad.clear();
  sm_red.clear();
  sm_green.clear();
  sm_blue.clear();

  if (pts.size() < 2) {
    return;
  }

  double mean_r = 0.0;

  for (int i = 0; i < static_cast<int>(rad.size()); i++) {
    mean_r += rad[i];
  }

  mean_r /= static_cast<double>(rad.size());

  if (mean_r < 1e-8) {
    mean_r = 1e-8;
  }

  double target_step = mean_r * 1.25;

  if (target_step < 1e-8) {
    target_step = 1e-8;
  }

  int estimated_samples =
    std::max(
      16,
      static_cast<int>(pts.size()) * 4
    );

  sm_pts.reserve(estimated_samples);
  sm_rad.reserve(estimated_samples);
  sm_red.reserve(estimated_samples);
  sm_green.reserve(estimated_samples);
  sm_blue.reserve(estimated_samples);

  for (int s = 0; s < static_cast<int>(pts.size()) - 1; s++) {
    SmoothPoint p1 = pts[s];
    SmoothPoint p2 = pts[s + 1];

    double seg_len = sm_distance(p1, p2);

    int n_seg =
      std::max(
        2,
        static_cast<int>(std::ceil(seg_len / target_step))
      );

    for (int q = 0; q < n_seg; q++) {
      double t =
        static_cast<double>(q) /
          static_cast<double>(n_seg);

      double ts = sm_smoothstep(t);

      SmoothPoint p =
        sm_lerp_point(p1, p2, t);

      double r =
        sm_lerp(rad[s], rad[s + 1], ts);

      double rr =
        sm_lerp(red[s], red[s + 1], ts);

      double gg =
        sm_lerp(green[s], green[s + 1], ts);

      double bb =
        sm_lerp(blue[s], blue[s + 1], ts);

      if (r < 1e-8) {
        r = 1e-8;
      }

      sm_pts.push_back(p);
      sm_rad.push_back(r);
      sm_red.push_back(clamp_rgb(rr));
      sm_green.push_back(clamp_rgb(gg));
      sm_blue.push_back(clamp_rgb(bb));
    }
  }

  sm_pts.push_back(pts.back());
  sm_rad.push_back(rad.back());
  sm_red.push_back(clamp_rgb(red.back()));
  sm_green.push_back(clamp_rgb(green.back()));
  sm_blue.push_back(clamp_rgb(blue.back()));
}

// Frame / ring helpers --------------------------------------------------------

inline TubeFrame make_initial_frame(SmoothPoint tangent) {
  SmoothPoint t = sm_unit(tangent);

  SmoothPoint ref;

  if (std::abs(t.z) < 0.9) {
    ref = {0.0, 0.0, 1.0};
  } else {
    ref = {1.0, 0.0, 0.0};
  }

  SmoothPoint n = sm_unit(sm_cross(ref, t));
  SmoothPoint b = sm_unit(sm_cross(t, n));

  TubeFrame frame;
  frame.tangent = t;
  frame.normal = n;
  frame.binormal = b;

  return frame;
}

inline TubeFrame transport_frame(
    TubeFrame prev,
    SmoothPoint new_tangent
) {
  SmoothPoint t0 = sm_unit(prev.tangent);
  SmoothPoint t1 = sm_unit(new_tangent);

  SmoothPoint axis = sm_cross(t0, t1);
  double axis_norm = sm_norm(axis);

  TubeFrame out;
  out.tangent = t1;

  if (axis_norm < 1e-10) {
    out.normal = prev.normal;
    out.binormal = sm_unit(sm_cross(out.tangent, out.normal));
    out.normal = sm_unit(sm_cross(out.binormal, out.tangent));

    return out;
  }

  axis = sm_mul(axis, 1.0 / axis_norm);

  double cos_a = sm_dot(t0, t1);

  if (cos_a < -1.0) cos_a = -1.0;
  if (cos_a > 1.0) cos_a = 1.0;

  double angle = std::acos(cos_a);

  auto rotate_vec = [&](SmoothPoint v) {
    double c = std::cos(angle);
    double s = std::sin(angle);

    SmoothPoint term1 = sm_mul(v, c);
    SmoothPoint term2 = sm_mul(sm_cross(axis, v), s);
    SmoothPoint term3 = sm_mul(axis, sm_dot(axis, v) * (1.0 - c));

    return sm_add(sm_add(term1, term2), term3);
  };

  out.normal = sm_unit(rotate_vec(prev.normal));
  out.binormal = sm_unit(sm_cross(out.tangent, out.normal));
  out.normal = sm_unit(sm_cross(out.binormal, out.tangent));

  return out;
}

Ring make_ring_frame(
    IndexedMesh& mesh,
    SmoothPoint center,
    TubeFrame frame,
    double radius,
    int facets,
    const std::vector<double>& cos_theta,
    const std::vector<double>& sin_theta
) {
  Ring ring;
  ring.p.resize(facets + 1);
  ring.id.resize(facets + 1);

  for (int j = 0; j < facets; j++) {
    SmoothPoint p =
      sm_add(
        center,
        sm_add(
          sm_mul(frame.normal, cos_theta[j] * radius),
          sm_mul(frame.binormal, sin_theta[j] * radius)
        )
      );

    ring.p[j] = p;
    ring.id[j] = add_indexed_vertex(mesh, p);
  }

  ring.p[facets] = ring.p[0];
  ring.id[facets] = ring.id[0];

  return ring;
}

Ring align_ring(
    const Ring& ring,
    const Ring& target,
    int facets
) {
  double best_score = R_PosInf;
  int best_shift = 0;
  bool best_reverse = false;

  for (int reverse = 0; reverse < 2; reverse++) {
    for (int shift = 0; shift < facets; shift++) {

      double score = 0.0;

      for (int j = 0; j < facets; j++) {

        int idx;

        if (reverse == 0) {
          idx = (j + shift) % facets;
        } else {
          idx = (shift - j + facets) % facets;
        }

        double dx = ring.p[idx].x - target.p[j].x;
        double dy = ring.p[idx].y - target.p[j].y;
        double dz = ring.p[idx].z - target.p[j].z;

        score += dx * dx + dy * dy + dz * dz;
      }

      if (score < best_score) {
        best_score = score;
        best_shift = shift;
        best_reverse = reverse == 1;
      }
    }
  }

  Ring aligned;
  aligned.p.resize(facets + 1);
  aligned.id.resize(facets + 1);

  for (int j = 0; j < facets; j++) {

    int idx;

    if (!best_reverse) {
      idx = (j + best_shift) % facets;
    } else {
      idx = (best_shift - j + facets) % facets;
    }

    aligned.p[j] = ring.p[idx];
    aligned.id[j] = ring.id[idx];
  }

  aligned.p[facets] = aligned.p[0];
  aligned.id[facets] = aligned.id[0];

  return aligned;
}

// Emit helpers ----------------------------------------------------------------

void emit_strip(
    IndexedMesh& mesh,
    const Ring& ring0,
    const Ring& ring1,
    int facets,
    double r0,
    double g0,
    double b0,
    double r1,
    double g1,
    double b1
) {
  double rf = 0.5 * (r0 + r1);
  double gf = 0.5 * (g0 + g1);
  double bf = 0.5 * (b0 + b1);

  for (int j = 0; j < facets; j++) {
    add_indexed_tri(
      mesh,
      ring0.id[j],
              ring1.id[j],
                      ring1.id[j + 1],
                              rf,
                              gf,
                              bf
    );

    add_indexed_tri(
      mesh,
      ring0.id[j],
              ring1.id[j + 1],
                      ring0.id[j + 1],
                              rf,
                              gf,
                              bf
    );
  }
}

void emit_cap(
    IndexedMesh& mesh,
    SmoothPoint center,
    const Ring& ring,
    int facets,
    bool reverse,
    double r,
    double g,
    double b
) {
  int center_id = add_indexed_vertex(
    mesh,
    center
  );

  for (int j = 0; j < facets; j++) {
    if (!reverse) {
      add_indexed_tri(
        mesh,
        center_id,
        ring.id[j + 1],
               ring.id[j],
                      r,
                      g,
                      b
      );
    } else {
      add_indexed_tri(
        mesh,
        center_id,
        ring.id[j],
               ring.id[j + 1],
                      r,
                      g,
                      b
      );
    }
  }
}

// Graph helpers ---------------------------------------------------------------

int add_graph_node(
    std::vector<GraphNode>& nodes,
    std::unordered_map<PointKey, int, PointKeyHash>& node_lookup,
    SmoothPoint p
) {
  PointKey key = make_point_key(p);

  auto it = node_lookup.find(key);

  if (it != node_lookup.end()) {
    return it->second;
  }

  GraphNode node;
  node.p = p;

  int node_id = nodes.size();

  nodes.push_back(node);
  node_lookup[key] = node_id;

  return node_id;
}

void add_graph_edge(
    std::vector<GraphNode>& nodes,
    std::vector<GraphEdge>& edges,
    std::unordered_map<long long, int>& edge_lookup,
    int a,
    int b,
    double r0,
    double ar,
    double ag,
    double ab,
    double r1,
    double br,
    double bg,
    double bb
) {
  if (a == b) {
    return;
  }

  long long key = make_edge_key(a, b);

  if (edge_lookup.find(key) != edge_lookup.end()) {
    return;
  }

  GraphEdge edge;
  edge.a = a;
  edge.b = b;

  edge.r0 = r0;
  edge.r1 = r1;

  edge.ar = ar;
  edge.ag = ag;
  edge.ab = ab;

  edge.br = br;
  edge.bg = bg;
  edge.bb = bb;

  int edge_id = edges.size();

  edges.push_back(edge);
  edge_lookup[key] = edge_id;

  nodes[a].edges.push_back(edge_id);
  nodes[b].edges.push_back(edge_id);
}

void edge_value_at_node(
    const GraphEdge& edge,
    int node_id,
    double& r,
    double& cr,
    double& cg,
    double& cb
) {
  if (edge.a == node_id) {
    r = edge.r0;
    cr = edge.ar;
    cg = edge.ag;
    cb = edge.ab;
  } else {
    r = edge.r1;
    cr = edge.br;
    cg = edge.bg;
    cb = edge.bb;
  }
}

int other_node(
    const GraphEdge& edge,
    int node
) {
  return edge.a == node ? edge.b : edge.a;
}

void node_sample_from_chain(
    const std::vector<GraphNode>& nodes,
    const std::vector<GraphEdge>& edges,
    const std::vector<int>& chain_nodes,
    const std::vector<int>& chain_edges,
    int idx,
    SmoothPoint& p,
    double& r,
    double& cr,
    double& cg,
    double& cb
) {
  int node = chain_nodes[idx];

  p = nodes[node].p;

  if (idx == 0) {
    edge_value_at_node(
      edges[chain_edges.front()],
           node,
           r,
           cr,
           cg,
           cb
    );

    return;
  }

  if (idx == static_cast<int>(chain_nodes.size()) - 1) {
    edge_value_at_node(
      edges[chain_edges.back()],
           node,
           r,
           cr,
           cg,
           cb
    );

    return;
  }

  double r0;
  double cr0;
  double cg0;
  double cb0;

  double r1;
  double cr1;
  double cg1;
  double cb1;

  edge_value_at_node(
    edges[chain_edges[idx - 1]],
         node,
         r0,
         cr0,
         cg0,
         cb0
  );

  edge_value_at_node(
    edges[chain_edges[idx]],
         node,
         r1,
         cr1,
         cg1,
         cb1
  );

  r = 0.5 * (r0 + r1);
  cr = 0.5 * (cr0 + cr1);
  cg = 0.5 * (cg0 + cg1);
  cb = 0.5 * (cb0 + cb1);
}

// Junction helpers ------------------------------------------------------------

void emit_node_junction(
    IndexedMesh& mesh,
    const std::vector<GraphNode>& nodes,
    const std::vector< std::vector<NodeRing> >& node_rings,
    int node_id,
    int facets
) {
  const std::vector<NodeRing>& rings = node_rings[node_id];

  int deg = rings.size();

  if (deg == 0) {
    return;
  }

  if (deg == 1) {
    emit_cap(
      mesh,
      nodes[node_id].p,
      rings[0].ring,
      facets,
      rings[0].reverse,
      rings[0].r,
      rings[0].g,
      rings[0].b
    );

    return;
  }

  for (int q = 0; q < deg; q++) {
    const NodeRing& a = rings[q];
    const NodeRing& b = rings[(q + 1) % deg];

    Ring ring_a = align_ring(
      a.ring,
      b.ring,
      facets
    );

    emit_strip(
      mesh,
      ring_a,
      b.ring,
      facets,
      a.r,
      a.g,
      a.b,
      b.r,
      b.g,
      b.b
    );
  }
}

// Chain meshing ---------------------------------------------------------------

void emit_smooth_chain(
    IndexedMesh& mesh,
    std::vector< std::vector<NodeRing> >& node_rings,
    const std::vector<GraphNode>& nodes,
    const std::vector<GraphEdge>& edges,
    const std::vector<int>& chain_nodes,
    const std::vector<int>& chain_edges,
    int facets,
    const std::vector<double>& cos_theta,
    const std::vector<double>& sin_theta
) {
  if (chain_nodes.size() < 2 || chain_edges.empty()) {
    return;
  }

  std::vector<SmoothPoint> pts;
  std::vector<double> rad;
  std::vector<double> red;
  std::vector<double> green;
  std::vector<double> blue;

  pts.reserve(chain_nodes.size());
  rad.reserve(chain_nodes.size());
  red.reserve(chain_nodes.size());
  green.reserve(chain_nodes.size());
  blue.reserve(chain_nodes.size());

  for (int i = 0; i < static_cast<int>(chain_nodes.size()); i++) {
    SmoothPoint p;
    double r;
    double cr;
    double cg;
    double cb;

    node_sample_from_chain(
      nodes,
      edges,
      chain_nodes,
      chain_edges,
      i,
      p,
      r,
      cr,
      cg,
      cb
    );

    pts.push_back(p);
    rad.push_back(r);
    red.push_back(cr);
    green.push_back(cg);
    blue.push_back(cb);
  }

  remove_near_duplicate_samples(
    pts,
    rad,
    red,
    green,
    blue
  );

  if (pts.size() < 2) {
    return;
  }

  pts = smooth_points(pts, 4);
  rad = smooth_values(rad, 4);
  red = smooth_values(red, 2);
  green = smooth_values(green, 2);
  blue = smooth_values(blue, 2);

  std::vector<SmoothPoint> sm_pts;
  std::vector<double> sm_rad;
  std::vector<double> sm_red;
  std::vector<double> sm_green;
  std::vector<double> sm_blue;

  resample_smooth_chain(
    pts,
    rad,
    red,
    green,
    blue,
    sm_pts,
    sm_rad,
    sm_red,
    sm_green,
    sm_blue
  );

  if (sm_pts.size() < 2) {
    return;
  }

  TubeFrame prev_frame =
    make_initial_frame(
      sm_sub(sm_pts[1], sm_pts[0])
    );

  Ring first_ring =
    make_ring_frame(
      mesh,
      sm_pts[0],
            prev_frame,
            sm_rad[0],
                  facets,
                  cos_theta,
                  sin_theta
    );

  Ring prev_ring = first_ring;
  Ring last_ring = first_ring;

  for (int s = 1; s < static_cast<int>(sm_pts.size()); s++) {
    SmoothPoint tangent;

    if (s == static_cast<int>(sm_pts.size()) - 1) {
      tangent = sm_sub(sm_pts[s], sm_pts[s - 1]);
    } else {
      tangent = sm_sub(sm_pts[s + 1], sm_pts[s - 1]);
    }

    TubeFrame frame =
      transport_frame(
        prev_frame,
        tangent
      );

    Ring ring =
      make_ring_frame(
        mesh,
        sm_pts[s],
              frame,
              sm_rad[s],
                    facets,
                    cos_theta,
                    sin_theta
      );

    emit_strip(
      mesh,
      prev_ring,
      ring,
      facets,
      sm_red[s - 1],
            sm_green[s - 1],
                    sm_blue[s - 1],
                           sm_red[s],
                                 sm_green[s],
                                         sm_blue[s]
    );

    prev_ring = ring;
    last_ring = ring;
    prev_frame = frame;
  }

  int start_node = chain_nodes.front();
  int end_node = chain_nodes.back();

  NodeRing start_ring;
  start_ring.ring = first_ring;
  start_ring.r = sm_red.front();
  start_ring.g = sm_green.front();
  start_ring.b = sm_blue.front();
  start_ring.reverse = false;

  NodeRing end_ring;
  end_ring.ring = last_ring;
  end_ring.r = sm_red.back();
  end_ring.g = sm_green.back();
  end_ring.b = sm_blue.back();
  end_ring.reverse = true;

  node_rings[start_node].push_back(start_ring);
  node_rings[end_node].push_back(end_ring);
}

//' @title Generate Closed Mesh
//'
//' @description Build a closed tube mesh from QSM skeleton samples.
//'
//' @param start cylinder starts
//' @param axis cylinder axes
//' @param length cylinder lengths
//' @param radius cylinder radii
//' @param facets mesh facets around each cross section
//' @param id cylinder id
//' @param parent parent cylinder id
//' @param branch branch id
//' @param color hex color per cylinder
//' @return List with vertices, indices, and face_colors for rgl::tmesh3d()
//'
//' @noRd
//'
// [[Rcpp::export]]
List generate_closed_mesh(
   NumericMatrix start,
   NumericMatrix axis,
   NumericVector length,
   NumericVector radius,
   int facets,
   IntegerVector id,
   IntegerVector parent,
   IntegerVector branch,
   CharacterVector color = CharacterVector()
) {
 int n = start.nrow();

 if (n == 0) {
   return List::create(
     Named("vertices") = NumericMatrix(3, 0),
     Named("indices") = IntegerMatrix(3, 0),
     Named("face_colors") = IntegerMatrix(0, 3)
   );
 }

 if (facets < 3) {
   stop("facets must be >= 3.");
 }

 if (
     id.size() != n ||
       parent.size() != n ||
       branch.size() != n
 ) {
   stop("id, parent, and branch must all have length nrow(start).");
 }

 std::vector<double> cos_theta(facets + 1);
 std::vector<double> sin_theta(facets + 1);

 for (int j = 0; j < facets + 1; j++) {
   double theta =
     2.0 * M_PI *
     static_cast<double>(j) /
       static_cast<double>(facets);

   cos_theta[j] = std::cos(theta);
   sin_theta[j] = std::sin(theta);
 }

 bool use_color = color.size() == n;

 if (color.size() != 0 && color.size() != n) {
   stop("color must be empty or have length nrow(start).");
 }

 std::vector<SmoothPoint> base(n);
 std::vector<SmoothPoint> top(n);
 std::vector<SmoothPoint> axis_unit(n);

 std::vector<double> cr(n, 255.0);
 std::vector<double> cg(n, 255.0);
 std::vector<double> cb(n, 255.0);

 for (int i = 0; i < n; i++) {
   SmoothPoint u = {
     axis(i, 0),
     axis(i, 1),
     axis(i, 2)
   };

   u = sm_unit(u);

   axis_unit[i] = u;

   base[i] = {
     start(i, 0),
     start(i, 1),
     start(i, 2)
   };

   top[i] = sm_add(base[i], sm_mul(u, length[i]));

   if (use_color) {
     hex_to_rgb(get_color_string(color, i), cr[i], cg[i], cb[i]);
   }
 }

 std::unordered_map<int, int> id_to_row;
 id_to_row.reserve(n * 2);

 for (int i = 0; i < n; i++) {
   if (!IntegerVector::is_na(id[i])) {
     id_to_row[id[i]] = i;
   }
 }

 std::vector<int> parent_row(n, -1);
 std::vector<bool> has_parent(n, false);

 for (int i = 0; i < n; i++) {
   if (IntegerVector::is_na(parent[i])) {
     continue;
   }

   auto it = id_to_row.find(parent[i]);

   if (it == id_to_row.end()) {
     continue;
   }

   parent_row[i] = it->second;
   has_parent[i] = true;
 }

 std::vector< std::vector<SplitPoint> > splits(n);

 for (int i = 0; i < n; i++) {
   splits[i].reserve(3);

   SplitPoint s0;
   s0.h = 0.0;
   s0.p = base[i];
   s0.radius = radius[i];
   s0.cr = cr[i];
   s0.cg = cg[i];
   s0.cb = cb[i];

   SplitPoint s1;
   s1.h = length[i];
   s1.p = top[i];
   s1.radius = radius[i];
   s1.cr = cr[i];
   s1.cg = cg[i];
   s1.cb = cb[i];

   splits[i].push_back(s0);
   splits[i].push_back(s1);
 }

 for (int i = 0; i < n; i++) {
   if (!has_parent[i]) {
     continue;
   }

   int p = parent_row[i];

   bool same_branch =
     !IntegerVector::is_na(branch[i]) &&
     !IntegerVector::is_na(branch[p]) &&
     branch[i] == branch[p];

   if (same_branch) {
     continue;
   }

   double h = sm_dot(sm_sub(base[i], base[p]), axis_unit[p]);

   if (h < 0.0) {
     h = 0.0;
   }

   if (h > length[p]) {
     h = length[p];
   }

   SmoothPoint fork_pt =
     sm_add(base[p], sm_mul(axis_unit[p], h));

   SplitPoint sp;
   sp.h = h;
   sp.p = fork_pt;
   sp.radius = radius[p];
   sp.cr = cr[p];
   sp.cg = cg[p];
   sp.cb = cb[p];

   splits[p].push_back(sp);
 }

 for (int i = 0; i < n; i++) {
   std::sort(
     splits[i].begin(),
     splits[i].end(),
     [&](const SplitPoint& a, const SplitPoint& b) {
       return a.h < b.h;
     }
   );

   std::vector<SplitPoint> clean;
   clean.reserve(splits[i].size());

   for (int j = 0; j < static_cast<int>(splits[i].size()); j++) {
     if (clean.empty()) {
       clean.push_back(splits[i][j]);
       continue;
     }

     if (std::abs(splits[i][j].h - clean.back().h) > 1e-8) {
       clean.push_back(splits[i][j]);
     }
   }

   splits[i] = clean;
 }

 std::vector<GraphNode> nodes;
 std::vector<GraphEdge> edges;

 nodes.reserve(n * 3);
 edges.reserve(n * 3);

 std::unordered_map<PointKey, int, PointKeyHash> node_lookup;
 std::unordered_map<long long, int> edge_lookup;

 node_lookup.reserve(n * 3);
 edge_lookup.reserve(n * 3);

 std::vector<int> base_node(n, -1);
 std::vector<int> top_node(n, -1);
 std::vector< std::vector<int> > split_node(n);

 for (int i = 0; i < n; i++) {
   split_node[i].resize(splits[i].size());

   for (int j = 0; j < static_cast<int>(splits[i].size()); j++) {
     split_node[i][j] = add_graph_node(
       nodes,
       node_lookup,
       splits[i][j].p
     );
   }

   base_node[i] = split_node[i].front();
   top_node[i] = split_node[i].back();
 }

 for (int i = 0; i < n; i++) {
   for (int j = 0; j < static_cast<int>(splits[i].size()) - 1; j++) {
     add_graph_edge(
       nodes,
       edges,
       edge_lookup,
       split_node[i][j],
                    split_node[i][j + 1],
                                 splits[i][j].radius,
                                 splits[i][j].cr,
                                 splits[i][j].cg,
                                 splits[i][j].cb,
                                 splits[i][j + 1].radius,
                                 splits[i][j + 1].cr,
                                 splits[i][j + 1].cg,
                                 splits[i][j + 1].cb
     );
   }
 }

 for (int i = 0; i < n; i++) {
   if (!has_parent[i]) {
     continue;
   }

   int p = parent_row[i];

   bool same_branch =
     !IntegerVector::is_na(branch[i]) &&
     !IntegerVector::is_na(branch[p]) &&
     branch[i] == branch[p];

   if (same_branch) {
     add_graph_edge(
       nodes,
       edges,
       edge_lookup,
       top_node[p],
               base_node[i],
                        radius[p],
                              cr[p],
                                cg[p],
                                  cb[p],
                                    radius[i],
                                          cr[i],
                                            cg[i],
                                              cb[i]
     );

   } else {
     double h = sm_dot(sm_sub(base[i], base[p]), axis_unit[p]);

     if (h < 0.0) {
       h = 0.0;
     }

     if (h > length[p]) {
       h = length[p];
     }

     int fork_node = -1;

     for (int j = 0; j < static_cast<int>(splits[p].size()); j++) {
       if (std::abs(splits[p][j].h - h) < 1e-8) {
         fork_node = split_node[p][j];
         break;
       }
     }

     if (fork_node >= 0) {
       add_graph_edge(
         nodes,
         edges,
         edge_lookup,
         fork_node,
         base_node[i],
                  radius[i],
                        cr[i],
                          cg[i],
                            cb[i],
                              radius[i],
                                    cr[i],
                                      cg[i],
                                        cb[i]
       );
     }
   }
 }

 IndexedMesh mesh;
 std::vector< std::vector<NodeRing> > node_rings(nodes.size());

 int estimated_vertices =
   std::max(
     1024,
     static_cast<int>(edges.size()) * facets * 4
   );

 mesh.vertices.reserve(estimated_vertices * 3);
 mesh.indices.reserve(estimated_vertices * 6);
 mesh.face_colors.reserve(estimated_vertices * 6);

 std::vector<bool> edge_used(edges.size(), false);

 for (int node_id = 0; node_id < static_cast<int>(nodes.size()); node_id++) {
   int deg = nodes[node_id].edges.size();

   if (deg == 2) {
     continue;
   }

   for (int q = 0; q < deg; q++) {
     int start_edge = nodes[node_id].edges[q];

     if (edge_used[start_edge]) {
       continue;
     }

     std::vector<int> chain_nodes;
     std::vector<int> chain_edges;

     int cur_node = node_id;
     int cur_edge = start_edge;

     chain_nodes.push_back(cur_node);

     while (true) {
       edge_used[cur_edge] = true;
       chain_edges.push_back(cur_edge);

       int next_node = other_node(
         edges[cur_edge],
              cur_node
       );

       chain_nodes.push_back(next_node);

       if (nodes[next_node].edges.size() != 2) {
         break;
       }

       int e0 = nodes[next_node].edges[0];
       int e1 = nodes[next_node].edges[1];

       int next_edge = e0 == cur_edge ? e1 : e0;

       if (edge_used[next_edge]) {
         break;
       }

       cur_node = next_node;
       cur_edge = next_edge;
     }

     emit_smooth_chain(
       mesh,
       node_rings,
       nodes,
       edges,
       chain_nodes,
       chain_edges,
       facets,
       cos_theta,
       sin_theta
     );
   }
 }

 for (int e = 0; e < static_cast<int>(edges.size()); e++) {
   if (edge_used[e]) {
     continue;
   }

   std::vector<int> chain_nodes;
   std::vector<int> chain_edges;

   int start_edge = e;
   int cur_edge = start_edge;
   int cur_node = edges[e].a;

   chain_nodes.push_back(cur_node);

   while (true) {
     if (edge_used[cur_edge]) {
       break;
     }

     edge_used[cur_edge] = true;
     chain_edges.push_back(cur_edge);

     int next_node = other_node(
       edges[cur_edge],
            cur_node
     );

     chain_nodes.push_back(next_node);

     int next_edge = -1;

     for (int q = 0; q < static_cast<int>(nodes[next_node].edges.size()); q++) {
       int candidate = nodes[next_node].edges[q];

       if (!edge_used[candidate]) {
         next_edge = candidate;
         break;
       }
     }

     if (next_edge < 0 || next_edge == start_edge) {
       break;
     }

     cur_node = next_node;
     cur_edge = next_edge;
   }

   emit_smooth_chain(
     mesh,
     node_rings,
     nodes,
     edges,
     chain_nodes,
     chain_edges,
     facets,
     cos_theta,
     sin_theta
   );
 }

 for (int node_id = 0; node_id < static_cast<int>(nodes.size()); node_id++) {
   emit_node_junction(
     mesh,
     nodes,
     node_rings,
     node_id,
     facets
   );
 }

 int n_vertices = mesh.vertices.size() / 3;
 int n_faces = mesh.indices.size() / 3;

 NumericMatrix out_vertices(3, n_vertices);
 IntegerMatrix out_indices(3, n_faces);
 IntegerMatrix out_face_colors(n_faces, 3);

 for (int i = 0; i < n_vertices; i++) {
   out_vertices(0, i) = mesh.vertices[i * 3];
   out_vertices(1, i) = mesh.vertices[i * 3 + 1];
   out_vertices(2, i) = mesh.vertices[i * 3 + 2];
 }

 for (int i = 0; i < n_faces; i++) {
   out_indices(0, i) = mesh.indices[i * 3];
   out_indices(1, i) = mesh.indices[i * 3 + 1];
   out_indices(2, i) = mesh.indices[i * 3 + 2];

   out_face_colors(i, 0) = mesh.face_colors[i * 3];
   out_face_colors(i, 1) = mesh.face_colors[i * 3 + 1];
   out_face_colors(i, 2) = mesh.face_colors[i * 3 + 2];
 }

 return List::create(
   Named("vertices") = out_vertices,
   Named("indices") = out_indices,
   Named("face_colors") = out_face_colors
 );
}
