// [[Rcpp::depends(Rcpp)]]
#include <Rcpp.h>
#include <cmath>
#include <algorithm>
using namespace Rcpp;

struct V3 { double x,y,z; };
inline V3  sub(const V3& a, const V3& b){ return {a.x-b.x, a.y-b.y, a.z-b.z}; }
inline double dot3(const V3& a, const V3& b){ return a.x*b.x + a.y*b.y + a.z*b.z; }
inline double nrm (const V3& a){ return std::sqrt(dot3(a,a)); }

inline void centroid_radius(
    const double* px, const double* py, const double* pz,
    int i0,int k, double& cx, double& cy, double& cz, double& r){
  double sx=0, sy=0, sz=0;
  for(int t=0;t<k;++t){ sx+=px[i0+t]; sy+=py[i0+t]; sz+=pz[i0+t]; }
  cx=sx/k; cy=sy/k; cz=sz/k;
  double sum=0;
  for(int t=0;t<k;++t){
    double dx=px[i0+t]-cx, dy=py[i0+t]-cy, dz=pz[i0+t]-cz;
    sum += std::sqrt(dx*dx+dy*dy+dz*dz);
  }
  r = sum/k;
}

inline void centroid_radius_idx(
    const double* px, const double* py, const double* pz,
    const std::vector<int>& idx,
    double& cx, double& cy,double& cz, double& r){
  const int k = (int)idx.size();
  double sx=0, sy=0, sz=0;
  for(int t=0;t<k;++t){ const int i=idx[t]; sx+=px[i]; sy+=py[i]; sz+=pz[i]; }
  cx=sx/k; cy=sy/k; cz=sz/k;
  double sum=0;
  for(int t=0;t<k;++t){
    const int i=idx[t];
    const double dx=px[i]-cx, dy=py[i]-cy, dz=pz[i]-cz;
    sum += std::sqrt(dx*dx+dy*dy+dz*dz);
  }
  r = sum/k;
}

inline bool circumcenter(
    double x1, double y1,
    double x2, double y2,
    double x3, double y3,
    double& cx, double& cy){
  const double d = 2.0*(x1*(y2-y3) + x2*(y3-y1) + x3*(y1-y2));
  if (d == 0.0) return false;
  const double r1 = x1*x1 + y1*y1;
  const double r2 = x2*x2 + y2*y2;
  const double r3 = x3*x3 + y3*y3;
  cx = (r1*(y2-y3) + r2*(y3-y1) + r3*(y1-y2)) / d;
  cy = (r1*(x3-x2) + r2*(x1-x3) + r3*(x2-x1)) / d;
  return true;
}

inline bool is_circle(
    const double* px, const double* py, const double* pz,
    int i0, int k, double tol,
    std::vector<V3>& pbuf,
    std::vector<double>& qx,
    std::vector<double>& qy){
  for(int t=0;t<k;++t) pbuf[t] = { px[i0+t], py[i0+t], pz[i0+t] };

  int i1=-1,i2=-1;
  for(int i=1;i<k && i1<0;++i){
    V3 u = sub(pbuf[i], pbuf[0]);
    for(int j=i+1;j<k;++j){
      V3 v = sub(pbuf[j], pbuf[0]);
      V3 n = { u.y*v.z - u.z*v.y, u.z*v.x - u.x*v.z, u.x*v.y - u.y*v.x };
      if (n.x!=0.0 || n.y!=0.0 || n.z!=0.0){ i1=i; i2=j; break; }
    }
  }
  if (i1<0) return false;

  V3 u = sub(pbuf[i1], pbuf[0]); double un=nrm(u); if (un==0.0) return false; u={u.x/un,u.y/un,u.z/un};
  V3 a = sub(pbuf[i1], pbuf[0]), b = sub(pbuf[i2], pbuf[0]);
  V3 n = { a.y*b.z - a.z*b.y, a.z*b.x - a.x*b.z, a.x*b.y - a.y*b.x };
  double nn=nrm(n); if (nn==0.0) return false; n={n.x/nn,n.y/nn,n.z/nn};
  V3 v = { n.y*u.z - n.z*u.y, n.z*u.x - n.x*u.z, n.x*u.y - n.y*u.x };

  for(int t=0;t<k;++t){
    V3 d = sub(pbuf[t], pbuf[0]);
    qx[t] = d.x*u.x + d.y*u.y + d.z*u.z;
    qy[t] = d.x*v.x + d.y*v.y + d.z*v.z;
  }

  double cx,cy;
  if (!circumcenter(qx[0],qy[0], qx[i1],qy[i1], qx[i2],qy[i2], cx, cy)) return false;

  const double dx0= qx[0]-cx, dy0= qy[0]-cy;
  const double r2 = dx0*dx0 + dy0*dy0;
  const double thr = tol * (1.0 + std::fabs(r2));
  for(int t=0;t<k;++t){
    const double dx=qx[t]-cx, dy=qy[t]-cy;
    const double d2=dx*dx + dy*dy;
    if (std::fabs(d2 - r2) > thr) return false;
  }
  return true;
}

// Split branch base into bottom and top rings
// bottom = {1,2,5,7,9,...,2f-1}, top = {3,4,6,8,10,...,2f}
inline void split_base(
    int i0, int facets,
    std::vector<int>& idxBottom,
    std::vector<int>& idxTop){
  const int m = 2 * facets;
  idxBottom.clear(); idxTop.clear();
  idxBottom.reserve(facets); idxTop.reserve(facets);
  for (int rel = 0; rel < m; ++rel) {
    const int pos = rel + 1;
    const int absIdx = i0 + rel;
    const bool toBottom = (pos == 1) || (pos == 2) || (pos >= 5 && (pos % 2 == 1));
    (toBottom ? idxBottom : idxTop).push_back(absIdx);
  }
}

// The first branch base cylinder consists of 2*facets points in the order
// bottom = {1,2,5,7,9,...,2f-1}, top = {3,4,6,8,10,...,2f}
// Define the base cylinder, then proceed up by facets until the branch end is
// reaches (1 pt instead of facet ring). Continue recursively until all points
// are segmented into connected cylinders and branches.
// [[Rcpp::export]]
DataFrame build_adqsm(
    DataFrame vertices,
    int facets = 10,
    double tol = 1e-5,
    double match_tol = 1e-4) {
  NumericVector X = vertices["x"];
  NumericVector Y = vertices["y"];
  NumericVector Z = vertices["z"];
  const double* px = REAL(X);
  const double* py = REAL(Y);
  const double* pz = REAL(Z);
  const int n = X.size();

  // outputs (reserve a rough upper bound)
  const int rough_max = std::max(1, n / std::max(1, facets));
  std::vector<int>    out_id;     out_id.reserve(rough_max);
  std::vector<int>    out_parent; out_parent.reserve(rough_max);
  std::vector<int>    out_branch; out_branch.reserve(rough_max);
  std::vector<int>    out_base;   out_base.reserve(rough_max);
  std::vector<double> sx,sy,sz, ax,ay,az, ex,ey,ez, L, R;
  sx.reserve(rough_max); sy.reserve(rough_max); sz.reserve(rough_max);
  ax.reserve(rough_max); ay.reserve(rough_max); az.reserve(rough_max);
  ex.reserve(rough_max); ey.reserve(rough_max); ez.reserve(rough_max);
  L.reserve(rough_max);  R.reserve(rough_max);

  // buffers
  std::vector<V3> pbuf(facets);
  std::vector<double> qx(facets), qy(facets);
  std::vector<int> idxBottom, idxTop;

  const int base_pts = 2 * facets;
  int i = 0;
  int branch = 1;
  int next_id = 1;

  auto find_parent_for_start = [&](double Sx, double Sy, double Sz) -> int {
    if (out_id.empty()) return 0;  // first branch/cylinder
    const double tol2 = match_tol * match_tol;
    int best = 0;
    double best_d2 = std::numeric_limits<double>::infinity();
    for (size_t k = 0; k < ex.size(); ++k) {
      const double dx = ex[k] - Sx, dy = ey[k] - Sy, dz = ez[k] - Sz;
      const double d2 = dx*dx + dy*dy + dz*dz;
      if (d2 <= tol2 && d2 < best_d2) {
        best = out_id[k];
        best_d2 = d2;
      }
    }
    return best; // 0 if no close match
  };

  while (i + base_pts <= n) {
    // split base rings by pattern and build the base cylinder
    split_base(i, facets, idxBottom, idxTop);

    // base and top centroids / radii
    double bcx,bcy,bcz, br, tcx,tcy,tcz, tr;
    centroid_radius_idx(px,py,pz, idxBottom, bcx,bcy,bcz, br); // START ring
    centroid_radius_idx(px,py,pz, idxTop,    tcx,tcy,tcz, tr); // END   ring

    // branch base parent cylinder
    // match base START to any previous END within tolerance
    const int parent_id = find_parent_for_start(bcx,bcy,bcz);

    // base cylinder
    // start=center(bottom), end=center(top)
    // radius is from the from start
    {
      const double dx = tcx-bcx, dy = tcy-bcy, dz = tcz-bcz;
      const double len = std::sqrt(dx*dx+dy*dy+dz*dz);
      double ax_=0, ay_=0, az_=0; if (len>0){ ax_=dx/len; ay_=dy/len; az_=dz/len; }

      out_id.push_back(next_id);
      out_parent.push_back(parent_id); // assign parent 0 to first branch
      out_branch.push_back(branch);
      out_base.push_back(1);
      sx.push_back(bcx); sy.push_back(bcy); sz.push_back(bcz);
      ax.push_back(ax_); ay.push_back(ay_); az.push_back(az_);
      ex.push_back(tcx); ey.push_back(tcy); ez.push_back(tcz);
      L.push_back(len);
      R.push_back(br);
    }
    int last_id_in_branch = next_id; // for children within the branch
    ++next_id;

    // continue up current branch
    double prev_cx=tcx, prev_cy=tcy, prev_cz=tcz, prev_r=tr;
    int j = i + base_pts;
    bool branched = false;

    while (j + facets <= n) {
      if (is_circle(px,py,pz, j, facets, tol, pbuf, qx, qy)) {
        // next ring
        double cx,cy,cz, rr;
        centroid_radius(px,py,pz, j, facets, cx,cy,cz, rr);

        const double dx=cx-prev_cx, dy=cy-prev_cy, dz=cz-prev_cz;
        const double len=std::sqrt(dx*dx+dy*dy+dz*dz);
        double ax_=0, ay_=0, az_=0; if (len>0){ ax_=dx/len; ay_=dy/len; az_=dz/len; }

        out_id.push_back(next_id);
        out_parent.push_back(last_id_in_branch); // previous in this branch
        out_branch.push_back(branch);
        out_base.push_back(0);
        sx.push_back(prev_cx); sy.push_back(prev_cy); sz.push_back(prev_cz);
        ax.push_back(ax_); ay.push_back(ay_); az.push_back(az_);
        ex.push_back(cx); ey.push_back(cy); ez.push_back(cz);
        L.push_back(len);
        R.push_back(prev_r);

        last_id_in_branch = next_id;
        ++next_id;

        prev_cx=cx; prev_cy=cy; prev_cz=cz; prev_r=rr;
        j += facets;
      } else {
        // branch end is the first point in a non-circular ring
        const double tx=px[j], ty=py[j], tz=pz[j];
        const double dx=tx-prev_cx, dy=ty-prev_cy, dz=tz-prev_cz;
        const double len=std::sqrt(dx*dx+dy*dy+dz*dz);
        double ax_=0, ay_=0, az_=0; if (len>0){ ax_=dx/len; ay_=dy/len; az_=dz/len; }

        out_id.push_back(next_id);
        out_parent.push_back(last_id_in_branch); // previous in this branch
        out_branch.push_back(branch);
        out_base.push_back(0);
        sx.push_back(prev_cx); sy.push_back(prev_cy); sz.push_back(prev_cz);
        ax.push_back(ax_); ay.push_back(ay_); az.push_back(az_);
        ex.push_back(tx); ey.push_back(ty); ez.push_back(tz);
        L.push_back(len);
        R.push_back(prev_r);

        ++next_id;

        // begin next branch immediately after the branch ends
        i = j + 1;
        ++branch;
        branched = true;
        break;
      }
    }

    if (!branched) {
      // assign any remaining points at end of data to branch tips
      if (j < n) {
        const double tx=px[j], ty=py[j], tz=pz[j];
        const double dx=tx-prev_cx, dy=ty-prev_cy, dz=tz-prev_cz;
        const double len=std::sqrt(dx*dx+dy*dy+dz*dz);
        double ax_=0, ay_=0, az_=0; if (len>0){ ax_=dx/len; ay_=dy/len; az_=dz/len; }

        out_id.push_back(next_id);
        out_parent.push_back(last_id_in_branch); // previous in this branch
        out_branch.push_back(branch);
        out_base.push_back(0);
        sx.push_back(prev_cx); sy.push_back(prev_cy); sz.push_back(prev_cz);
        ax.push_back(ax_); ay.push_back(ay_); az.push_back(az_);
        ex.push_back(tx); ey.push_back(ty); ez.push_back(tz);
        L.push_back(len);
        R.push_back(prev_r);

        ++next_id;

        i = j + 1;
        ++branch;
      } else {
        i = j;
        ++branch;
      }
    }
  }

  return DataFrame::create(
    _["id"]      = IntegerVector(out_id.begin(), out_id.end()),
    _["parent"]  = IntegerVector(out_parent.begin(), out_parent.end()),
    _["branch"]  = IntegerVector(out_branch.begin(), out_branch.end()),
    _["base"]    = IntegerVector(out_base.begin(), out_base.end()),
    _["start_x"] = NumericVector(sx.begin(), sx.end()),
    _["start_y"] = NumericVector(sy.begin(), sy.end()),
    _["start_z"] = NumericVector(sz.begin(), sz.end()),
    _["axis_x"]  = NumericVector(ax.begin(), ax.end()),
    _["axis_y"]  = NumericVector(ay.begin(), ay.end()),
    _["axis_z"]  = NumericVector(az.begin(), az.end()),
    _["end_x"]   = NumericVector(ex.begin(), ex.end()),
    _["end_y"]   = NumericVector(ey.begin(), ey.end()),
    _["end_z"]   = NumericVector(ez.begin(), ez.end()),
    _["length"]  = NumericVector(L.begin(),  L.end()),
    _["radius"]  = NumericVector(R.begin(),  R.end())
  );
}
