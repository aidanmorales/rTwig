#include <Rcpp.h>
#include <unordered_map>
#include <vector>
#include <algorithm>
#include <cmath>
#include <string>

using namespace Rcpp;

struct V3 {
  double x, y, z;
};

struct RGB {
  double r, g, b;
};

struct Cyl {
  V3 base;
  V3 axis;
  V3 top;
  double length;
  double radius;
  int id;
  int parent;
  int branch;
  bool id_na;
  bool parent_na;
  bool branch_na;
  RGB color;
};

struct Knot {
  V3 p;
  double radius;
  RGB color;
};

struct Sample {
  V3 p;
  double radius;
  RGB color;
};

struct Frame {
  V3 t;
  V3 n;
  V3 b;
};

struct Mesh {
  std::vector<double> vertices;
  std::vector<int> indices;
  std::vector<int> colors;
};

// Geometry --------------------------------------------------------------------

inline V3 v_add(V3 a, V3 b) {
  return {a.x + b.x, a.y + b.y, a.z + b.z};
}

inline V3 v_sub(V3 a, V3 b) {
  return {a.x - b.x, a.y - b.y, a.z - b.z};
}

inline V3 v_mul(V3 a, double s) {
  return {a.x * s, a.y * s, a.z * s};
}

inline double v_dot(V3 a, V3 b) {
  return a.x * b.x + a.y * b.y + a.z * b.z;
}

inline V3 v_cross(V3 a, V3 b) {
  return {
  a.y * b.z - a.z * b.y,
  a.z * b.x - a.x * b.z,
  a.x * b.y - a.y * b.x
};
}

inline double v_norm(V3 a) {
  return std::sqrt(v_dot(a, a));
}

inline V3 v_unit(V3 a) {
  double n = v_norm(a);
  if (n < 1e-14) return {0.0, 0.0, 1.0};
  return v_mul(a, 1.0 / n);
}

inline double v_dist(V3 a, V3 b) {
  return v_norm(v_sub(a, b));
}

inline V3 v_lerp(V3 a, V3 b, double t) {
  return v_add(v_mul(a, 1.0 - t), v_mul(b, t));
}

inline double d_lerp(double a, double b, double t) {
  return a * (1.0 - t) + b * t;
}

inline RGB c_lerp(RGB a, RGB b, double t) {
  return {
  d_lerp(a.r, b.r, t),
  d_lerp(a.g, b.g, t),
  d_lerp(a.b, b.b, t)
};
}

inline bool same_branch(const Cyl& a, const Cyl& b) {
  return !a.branch_na && !b.branch_na && a.branch == b.branch;
}

// Color -----------------------------------------------------------------------

inline int hex_digit(char c) {
  if (c >= '0' && c <= '9') return c - '0';
  if (c >= 'a' && c <= 'f') return 10 + c - 'a';
  if (c >= 'A' && c <= 'F') return 10 + c - 'A';
  return 0;
}

inline RGB read_color(CharacterVector color, int i, bool use_color) {
  if (!use_color || CharacterVector::is_na(color[i])) {
    return {255.0, 255.0, 255.0};
  }

  std::string s = as<std::string>(color[i]);

  if (s.size() < 7 || s[0] != '#') {
    return {255.0, 255.0, 255.0};
  }

  return {
    static_cast<double>(16 * hex_digit(s[1]) + hex_digit(s[2])),
    static_cast<double>(16 * hex_digit(s[3]) + hex_digit(s[4])),
    static_cast<double>(16 * hex_digit(s[5]) + hex_digit(s[6]))
  };
}

inline int color_byte(double x) {
  if (x < 0.0) x = 0.0;
  if (x > 255.0) x = 255.0;
  return static_cast<int>(std::lround(x));
}

// Mesh ------------------------------------------------------------------------

inline int add_vertex(Mesh& mesh, V3 p) {
  int id = static_cast<int>(mesh.vertices.size() / 3);
  mesh.vertices.push_back(p.x);
  mesh.vertices.push_back(p.y);
  mesh.vertices.push_back(p.z);
  return id;
}

inline void add_face(Mesh& mesh, int a, int b, int c, RGB color) {
  mesh.indices.push_back(a + 1);
  mesh.indices.push_back(b + 1);
  mesh.indices.push_back(c + 1);

  mesh.colors.push_back(color_byte(color.r));
  mesh.colors.push_back(color_byte(color.g));
  mesh.colors.push_back(color_byte(color.b));
}

// Skeleton --------------------------------------------------------------------

std::vector< std::vector<int> > build_parent_ordered_chains(
    const std::vector<Cyl>& cyl,
    const std::vector<int>& parent_row
) {
  int n = static_cast<int>(cyl.size());

  std::vector< std::vector<int> > same_children(n);
  std::vector<int> same_parent(n, -1);

  for (int i = 0; i < n; ++i) {
    int p = parent_row[i];

    if (p >= 0 && same_branch(cyl[i], cyl[p])) {
      same_parent[i] = p;
      same_children[p].push_back(i);
    }
  }

  // A QSM branch is a chain. If the same branch ID forks internally, using it
  // as one smooth centerline would necessarily create an artificial jump.
  for (int i = 0; i < n; ++i) {
    if (same_children[i].size() > 1) {
      stop("A branch contains multiple same-branch children; branch IDs do not describe simple QSM chains.");
    }
  }

  std::vector<bool> used(n, false);
  std::vector< std::vector<int> > chains;

  // Start only where the parent is absent or belongs to another branch.
  for (int start = 0; start < n; ++start) {
    if (same_parent[start] >= 0 || used[start]) continue;

    std::vector<int> chain;
    int cur = start;

    while (cur >= 0 && !used[cur]) {
      used[cur] = true;
      chain.push_back(cur);

      if (same_children[cur].empty()) {
        cur = -1;
      } else {
        cur = same_children[cur][0];
      }
    }

    if (!chain.empty()) chains.push_back(chain);
  }

  // Correct parent/child data should leave nothing here. This fallback keeps
  // isolated/cyclic bad records from making a long synthetic connection.
  for (int i = 0; i < n; ++i) {
    if (!used[i]) {
      std::vector<int> chain(1, i);
      chains.push_back(chain);
      used[i] = true;
    }
  }

  return chains;
}

std::vector<Knot> build_chain_knots(
    const std::vector<Cyl>& cyl,
    const std::vector<int>& parent_row,
    const std::vector<int>& rows
) {
  std::vector<Knot> k;
  if (rows.empty()) return k;

  int first = rows.front();
  int p = parent_row[first];

  // For a child branch, extend only a short distance INSIDE its exact parent
  // cylinder. There is no nearest-branch search and no long connector.
  if (p >= 0 && !same_branch(cyl[first], cyl[p])) {
    V3 rel = v_sub(cyl[first].base, cyl[p].base);
    double h = v_dot(rel, cyl[p].axis);

    if (h < 0.0) h = 0.0;
    if (h > cyl[p].length) h = cyl[p].length;

    V3 axis_point = v_add(cyl[p].base, v_mul(cyl[p].axis, h));
    V3 radial = v_sub(cyl[first].base, axis_point);
    double radial_distance = v_norm(radial);

    if (radial_distance > 1e-12) {
      V3 radial_dir = v_mul(radial, 1.0 / radial_distance);

      // Put the child centerline far enough inside the parent that the entire
      // child cross-section overlaps the parent surface before emerging.
      double inside_radius =
        std::max(0.0, cyl[p].radius - 1.10 * cyl[first].radius);

      if (radial_distance > inside_radius + 1e-10) {
        V3 inside = v_add(axis_point, v_mul(radial_dir, inside_radius));
        k.push_back({inside, cyl[first].radius, cyl[first].color});
      }
    }
  }

  // Exact branch base.
  k.push_back({cyl[first].base, cyl[first].radius, cyl[first].color});

  // Consecutive cylinders are joined at the midpoint of their two reported
  // endpoints. This removes tiny QSM endpoint mismatch without moving the
  // skeleton away from the cylinders.
  for (int q = 0; q + 1 < static_cast<int>(rows.size()); ++q) {
    int a = rows[q];
    int b = rows[q + 1];

    V3 joint = v_mul(v_add(cyl[a].top, cyl[b].base), 0.5);
    double r = 0.5 * (cyl[a].radius + cyl[b].radius);
    RGB c = c_lerp(cyl[a].color, cyl[b].color, 0.5);

    k.push_back({joint, r, c});
  }

  int last = rows.back();
  k.push_back({cyl[last].top, cyl[last].radius, cyl[last].color});

  // Remove only exact/near-exact duplicate knots. No global smoothing.
  std::vector<Knot> clean;
  clean.reserve(k.size());

  for (int i = 0; i < static_cast<int>(k.size()); ++i) {
    if (clean.empty() || v_dist(clean.back().p, k[i].p) > 1e-10) {
      clean.push_back(k[i]);
    } else {
      clean.back().radius = k[i].radius;
      clean.back().color = k[i].color;
    }
  }

  return clean;
}

// Local corner rounding -------------------------------------------------------

inline Knot knot_lerp(const Knot& a, const Knot& b, double t) {
  return {
  v_lerp(a.p, b.p, t),
  d_lerp(a.radius, b.radius, t),
  c_lerp(a.color, b.color, t)
};
}

inline Sample quadratic_sample(
    const Knot& a,
    const Knot& control,
    const Knot& b,
    double t
) {
  double u = 1.0 - t;

  Sample s;
  s.p = v_add(
    v_add(v_mul(a.p, u * u), v_mul(control.p, 2.0 * u * t)),
    v_mul(b.p, t * t)
  );

  s.radius =
    a.radius * u * u +
    control.radius * 2.0 * u * t +
    b.radius * t * t;

  s.color = {
    a.color.r * u * u + control.color.r * 2.0 * u * t + b.color.r * t * t,
    a.color.g * u * u + control.color.g * 2.0 * u * t + b.color.g * t * t,
    a.color.b * u * u + control.color.b * 2.0 * u * t + b.color.b * t * t
  };

  return s;
}

inline void append_sample_unique(std::vector<Sample>& out, const Sample& s) {
  if (out.empty() || v_dist(out.back().p, s.p) > 1e-10) {
    out.push_back(s);
  } else {
    out.back() = s;
  }
}

std::vector<Sample> make_rounded_samples(
    const std::vector<Knot>& k,
    int corner_steps
) {
  std::vector<Sample> out;
  int n = static_cast<int>(k.size());

  if (n < 2) return out;

  corner_steps = std::max(2, corner_steps);

  append_sample_unique(out, {k[0].p, k[0].radius, k[0].color});

  for (int i = 1; i < n - 1; ++i) {
    V3 in_vec = v_sub(k[i].p, k[i - 1].p);
    V3 out_vec = v_sub(k[i + 1].p, k[i].p);

    double lin = v_norm(in_vec);
    double lout = v_norm(out_vec);

    if (lin < 1e-10 || lout < 1e-10) {
      append_sample_unique(out, {k[i].p, k[i].radius, k[i].color});
      continue;
    }

    // Only round a small local neighborhood of the QSM joint. The curve is a
    // quadratic Bezier contained by the two cylinder segments, so it cannot
    // overshoot or make loops/squiggles.
    double cut = 0.18 * std::min(lin, lout);

    // Keep the rounding local even when cylinders are unusually long.
    double radius_limit = 1.5 * std::max(k[i].radius, 1e-8);
    if (cut > radius_limit) cut = radius_limit;

    if (cut < 1e-10) {
      append_sample_unique(out, {k[i].p, k[i].radius, k[i].color});
      continue;
    }

    double tin = (lin - cut) / lin;
    double tout = cut / lout;

    Knot entry = knot_lerp(k[i - 1], k[i], tin);
    Knot exit = knot_lerp(k[i], k[i + 1], tout);

    append_sample_unique(out, {entry.p, entry.radius, entry.color});

    for (int q = 1; q <= corner_steps; ++q) {
      double t = static_cast<double>(q) / static_cast<double>(corner_steps);
      append_sample_unique(out, quadratic_sample(entry, k[i], exit, t));
    }
  }

  append_sample_unique(out, {k[n - 1].p, k[n - 1].radius, k[n - 1].color});

  return out;
}

// Parallel-transport tube -----------------------------------------------------

Frame initial_frame(V3 tangent) {
  V3 t = v_unit(tangent);
  V3 ref = std::abs(t.z) < 0.9 ? V3{0.0, 0.0, 1.0} : V3{1.0, 0.0, 0.0};
  V3 n = v_unit(v_cross(ref, t));
  V3 b = v_unit(v_cross(t, n));
  return {t, n, b};
}

V3 rotate_rodrigues(V3 v, V3 axis, double angle) {
  double c = std::cos(angle);
  double s = std::sin(angle);

  return v_add(
    v_add(v_mul(v, c), v_mul(v_cross(axis, v), s)),
    v_mul(axis, v_dot(axis, v) * (1.0 - c))
  );
}

Frame transport_frame(Frame prev, V3 tangent) {
  V3 t0 = v_unit(prev.t);
  V3 t1 = v_unit(tangent);
  V3 axis = v_cross(t0, t1);
  double an = v_norm(axis);

  if (an < 1e-12) {
    V3 b = v_unit(v_cross(t1, prev.n));
    V3 n = v_unit(v_cross(b, t1));
    return {t1, n, b};
  }

  axis = v_mul(axis, 1.0 / an);

  double c = v_dot(t0, t1);
  if (c < -1.0) c = -1.0;
  if (c > 1.0) c = 1.0;

  double angle = std::acos(c);

  V3 n = v_unit(rotate_rodrigues(prev.n, axis, angle));
  V3 b = v_unit(v_cross(t1, n));
  n = v_unit(v_cross(b, t1));

  return {t1, n, b};
}

void connect_rings(
    Mesh& mesh,
    const std::vector<int>& a,
    const std::vector<int>& b,
    RGB color
) {
  int facets = static_cast<int>(a.size());

  for (int j = 0; j < facets; ++j) {
    int j1 = (j + 1) % facets;

    add_face(mesh, a[j], b[j], b[j1], color);
    add_face(mesh, a[j], b[j1], a[j1], color);
  }
}

std::vector<int> make_ring(
    Mesh& mesh,
    V3 center,
    Frame frame,
    double radius,
    int facets,
    const std::vector<double>& ct,
    const std::vector<double>& st
) {
  std::vector<int> ring(facets);

  for (int j = 0; j < facets; ++j) {
    V3 off = v_add(
      v_mul(frame.n, ct[j] * radius),
      v_mul(frame.b, st[j] * radius)
    );

    ring[j] = add_vertex(mesh, v_add(center, off));
  }

  return ring;
}

void emit_tube(
    Mesh& mesh,
    const std::vector<Sample>& s,
    int facets,
    const std::vector<double>& ct,
    const std::vector<double>& st
) {
  int ns = static_cast<int>(s.size());
  if (ns < 2) return;

  std::vector<Frame> frames(ns);

  frames[0] = initial_frame(v_sub(s[1].p, s[0].p));

  for (int i = 1; i < ns; ++i) {
    V3 tangent;

    if (i == ns - 1) {
      tangent = v_sub(s[i].p, s[i - 1].p);
    } else {
      tangent = v_sub(s[i + 1].p, s[i - 1].p);
    }

    frames[i] = transport_frame(frames[i - 1], tangent);
  }

  std::vector< std::vector<int> > rings(ns);

  for (int i = 0; i < ns; ++i) {
    rings[i] = make_ring(
      mesh,
      s[i].p,
      frames[i],
            std::max(s[i].radius, 1e-10),
            facets,
            ct,
            st
    );
  }

  for (int i = 0; i < ns - 1; ++i) {
    connect_rings(
      mesh,
      rings[i],
           rings[i + 1],
                c_lerp(s[i].color, s[i + 1].color, 0.5)
    );
  }

  // Closed branch base. Child bases are deliberately placed inside their
  // parent cylinder, so this cap is internal and never creates a visible gap.
  int base_center = add_vertex(mesh, s.front().p);

  for (int j = 0; j < facets; ++j) {
    int j1 = (j + 1) % facets;
    add_face(mesh, base_center, rings[0][j1], rings[0][j], s.front().color);
  }

  // Rounded terminal cap. This changes only the very end of the branch and
  // does not taper the QSM radius along the branch itself.
  double end_radius = std::max(s.back().radius, 1e-10);
  V3 end_tangent = frames.back().t;
  std::vector<int> previous = rings.back();

  int cap_steps = std::max(2, std::min(4, facets / 3));
  const double half_pi = 1.57079632679489661923;

  for (int q = 1; q <= cap_steps; ++q) {
    double angle = half_pi *
      static_cast<double>(q) /
        static_cast<double>(cap_steps + 1);

    V3 center = v_add(
      s.back().p,
      v_mul(end_tangent, end_radius * std::sin(angle))
    );

    double rr = end_radius * std::cos(angle);

    std::vector<int> ring = make_ring(
      mesh,
      center,
      frames.back(),
      rr,
      facets,
      ct,
      st
    );

    connect_rings(mesh, previous, ring, s.back().color);
    previous = ring;
  }

  V3 tip = v_add(s.back().p, v_mul(end_tangent, end_radius));
  int tip_id = add_vertex(mesh, tip);

  for (int j = 0; j < facets; ++j) {
    int j1 = (j + 1) % facets;
    add_face(mesh, previous[j], tip_id, previous[j1], s.back().color);
  }
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

  if (
      axis.nrow() != n ||
        length.size() != n ||
        radius.size() != n ||
        id.size() != n ||
        parent.size() != n ||
        branch.size() != n
  ) {
    stop("QSM inputs have inconsistent dimensions.");
  }

  if (facets < 3) {
    stop("facets must be >= 3.");
  }

  if (color.size() != 0 && color.size() != n) {
    stop("color must be empty or have length nrow(start).");
  }

  if (n == 0) {
    return List::create(
      Named("vertices") = NumericMatrix(3, 0),
      Named("indices") = IntegerMatrix(3, 0),
      Named("face_colors") = IntegerMatrix(0, 3)
    );
  }

  bool use_color = color.size() == n;

  std::vector<Cyl> cyl(n);
  std::unordered_map<int, int> id_to_row;
  id_to_row.reserve(n * 2);

  for (int i = 0; i < n; ++i) {
    V3 a = {axis(i, 0), axis(i, 1), axis(i, 2)};
    a = v_unit(a);

    cyl[i].base = {start(i, 0), start(i, 1), start(i, 2)};
    cyl[i].axis = a;
    cyl[i].length = std::max(static_cast<double>(length[i]), 0.0);
    cyl[i].radius = std::max(static_cast<double>(radius[i]), 1e-10);
    cyl[i].top = v_add(cyl[i].base, v_mul(a, cyl[i].length));

    cyl[i].id_na = IntegerVector::is_na(id[i]);
    cyl[i].parent_na = IntegerVector::is_na(parent[i]);
    cyl[i].branch_na = IntegerVector::is_na(branch[i]);

    cyl[i].id = cyl[i].id_na ? 0 : id[i];
    cyl[i].parent = cyl[i].parent_na ? 0 : parent[i];
    cyl[i].branch = cyl[i].branch_na ? 0 : branch[i];
    cyl[i].color = read_color(color, i, use_color);

    if (!cyl[i].id_na) {
      if (id_to_row.find(cyl[i].id) != id_to_row.end()) {
        stop("Cylinder id values must be unique.");
      }

      id_to_row[cyl[i].id] = i;
    }
  }

  std::vector<int> parent_row(n, -1);

  for (int i = 0; i < n; ++i) {
    if (cyl[i].parent_na) continue;

    auto it = id_to_row.find(cyl[i].parent);
    if (it != id_to_row.end()) {
      parent_row[i] = it->second;
    }
  }

  // Parent/child topology determines the branch order. Input row order is
  // never used to connect cylinders.
  std::vector< std::vector<int> > chains =
    build_parent_ordered_chains(cyl, parent_row);

  std::vector<double> ct(facets);
  std::vector<double> st(facets);
  const double two_pi = 6.28318530717958647693;

  for (int j = 0; j < facets; ++j) {
    double a = two_pi * static_cast<double>(j) / static_cast<double>(facets);
    ct[j] = std::cos(a);
    st[j] = std::sin(a);
  }

  Mesh mesh;

  std::size_t estimate =
    static_cast<std::size_t>(n) *
    static_cast<std::size_t>(facets) * 8u;

  mesh.vertices.reserve(estimate * 3u);
  mesh.indices.reserve(estimate * 6u);
  mesh.colors.reserve(estimate * 6u);

  // A few samples are enough because the only curvature is a short local
  // quadratic round at each QSM joint.
  int corner_steps = std::max(2, std::min(4, facets / 4));

  for (int c = 0; c < static_cast<int>(chains.size()); ++c) {
    std::vector<Knot> knots =
      build_chain_knots(cyl, parent_row, chains[c]);

    std::vector<Sample> samples =
      make_rounded_samples(knots, corner_steps);

    emit_tube(mesh, samples, facets, ct, st);
  }

  int nv = static_cast<int>(mesh.vertices.size() / 3);
  int nf = static_cast<int>(mesh.indices.size() / 3);

  NumericMatrix out_v(3, nv);
  IntegerMatrix out_i(3, nf);
  IntegerMatrix out_c(nf, 3);

  for (int i = 0; i < nv; ++i) {
    out_v(0, i) = mesh.vertices[3 * i];
    out_v(1, i) = mesh.vertices[3 * i + 1];
    out_v(2, i) = mesh.vertices[3 * i + 2];
  }

  for (int i = 0; i < nf; ++i) {
    out_i(0, i) = mesh.indices[3 * i];
    out_i(1, i) = mesh.indices[3 * i + 1];
    out_i(2, i) = mesh.indices[3 * i + 2];

    out_c(i, 0) = mesh.colors[3 * i];
    out_c(i, 1) = mesh.colors[3 * i + 1];
    out_c(i, 2) = mesh.colors[3 * i + 2];
  }

  return List::create(
    Named("vertices") = out_v,
    Named("indices") = out_i,
    Named("face_colors") = out_c
  );
}
