// [[Rcpp::depends(Rcpp)]]
#include <Rcpp.h>
#include <unordered_map>
#include <array>
#include <queue>
#include <cmath>
using namespace Rcpp;

static double circ_r2(const NumericMatrix &V, int A, int B, int C, int D) {
  A-=1; B-=1; C-=1; D-=1;
  double x0=V(A,0), y0=V(A,1), z0=V(A,2);
  double x1=V(B,0), y1=V(B,1), z1=V(B,2);
  double x2=V(C,0), y2=V(C,1), z2=V(C,2);
  double x3=V(D,0), y3=V(D,1), z3=V(D,2);

  double a11=x1-x0, a12=y1-y0, a13=z1-z0;
  double a21=x2-x0, a22=y2-y0, a23=z2-z0;
  double a31=x3-x0, a32=y3-y0, a33=z3-z0;

  double b1=0.5*(a11*a11+a12*a12+a13*a13);
  double b2=0.5*(a21*a21+a22*a22+a23*a23);
  double b3=0.5*(a31*a31+a32*a32+a33*a33);

  double Dm = a11*(a22*a33-a23*a32) - a12*(a21*a33-a23*a31) + a13*(a21*a32-a22*a31);
  if (std::fabs(Dm) < 1e-18) return 1e300; // degenerate -> very large radius

  double Dx = b1*(a22*a33-a23*a32) - a12*(b2*a33-a23*b3) + a13*(b2*a32-a22*b3);
  double Dy = a11*(b2*a33-a23*b3) - b1*(a21*a33-a23*a31) + a13*(a21*b3-b2*a31);
  double Dz = a11*(a22*b3-b2*a32) - a12*(a21*b3-b2*a31) + b1*(a21*a32-a22*a31);

  double cx = Dx/Dm + x0, cy = Dy/Dm + y0, cz = Dz/Dm + z0;
  double dx=x0-cx, dy=y0-cy, dz=z0-cz;
  return dx*dx + dy*dy + dz*dz;
}

struct FaceKey {
  int a,b,c;
  FaceKey(int i,int j,int k){
    if(i>j) std::swap(i,j);
    if(j>k) std::swap(j,k);
    if(i>j) std::swap(i,j);
    a=i; b=j; c=k;
  }
  bool operator==(FaceKey const& o) const { return a==o.a && b==o.b && c==o.c; }
};

struct FaceHash {
  size_t operator()(FaceKey const& f) const noexcept {
    return ((size_t)f.a*73856093u)^((size_t)f.b*19349663u)^((size_t)f.c*83492791u);
  }
};

static inline void tet_faces(int a,int b,int c,int d, int Fv[4][3], int Opp[4]){
  Fv[0][0]=b; Fv[0][1]=c; Fv[0][2]=d; Opp[0]=a;
  Fv[1][0]=a; Fv[1][1]=c; Fv[1][2]=d; Opp[1]=b;
  Fv[2][0]=a; Fv[2][1]=b; Fv[2][2]=d; Opp[2]=c;
  Fv[3][0]=a; Fv[3][1]=b; Fv[3][2]=c; Opp[3]=d;
}

// [[Rcpp::export]]
Rcpp::List alpha_shape_delaunay(
    const Rcpp::NumericMatrix &cloud,
    Rcpp::IntegerMatrix dt,
    double alpha) {
  const int nt = dt.nrow();
  const double r2max = alpha*alpha;

  // --- Build face map and tet adjacency (neighbor per face; -1 means hull) ---
  std::unordered_map<FaceKey, std::pair<int,int>, FaceHash> fmap;
  fmap.reserve((size_t)nt*4);
  std::vector<std::array<int,4>> nbr(nt);
  for (int t=0;t<nt;++t) nbr[t] = {-1,-1,-1,-1};

  for (int t=0; t<nt; ++t) {
    int a=dt(t,0), b=dt(t,1), c=dt(t,2), d=dt(t,3);
    int Fv[4][3]; int Opp[4];
    tet_faces(a,b,c,d, Fv, Opp);
    for (int f=0; f<4; ++f) {
      FaceKey key(Fv[f][0], Fv[f][1], Fv[f][2]);
      auto it = fmap.find(key);
      if (it==fmap.end()) {
        fmap.emplace(key, std::make_pair(t,f));
      } else {
        int t2 = it->second.first;
        int f2 = it->second.second;
        nbr[t][f]   = t2;
        nbr[t2][f2] = t;
      }
    }
  }

  // --- Keep tets with circumradius <= alpha ---
  std::vector<char> keep(nt, 0);
  for (int t=0; t<nt; ++t) {
    int a=dt(t,0), b=dt(t,1), c=dt(t,2), d=dt(t,3);
    double r2 = circ_r2(cloud,a,b,c,d);
    if (r2 <= r2max) keep[t]=1;
  }

  // --- Find outside (not-kept) region connected to hull via BFS ---
  std::vector<char> outside(nt, 0);
  std::queue<int> q;
  for (int t=0; t<nt; ++t) if (!keep[t]) {
    bool touchesHull=false;
    for (int f=0; f<4; ++f) if (nbr[t][f] == -1) { touchesHull=true; break; }
    if (touchesHull) { outside[t]=1; q.push(t); }
  }
  while(!q.empty()){
    int u=q.front(); q.pop();
    for (int f=0; f<4; ++f){
      int v = nbr[u][f];
      if (v>=0 && !keep[v] && !outside[v]) { outside[v]=1; q.push(v); }
    }
  }

  // --- Collect boundary faces (kept tet vs hull or outside), orient outward ---
  std::vector<std::array<int,3>> faces;
  faces.reserve(nt*2);
  double area = 0.0, volume = 0.0;

  for (int t=0; t<nt; ++t) if (keep[t]) {
    int a=dt(t,0), b=dt(t,1), c=dt(t,2), d=dt(t,3);
    int Fv[4][3]; int Opp[4];
    tet_faces(a,b,c,d, Fv, Opp);
    for (int f=0; f<4; ++f) {
      int nei = nbr[t][f];
      bool boundary = (nei == -1) || (!keep[nei] && outside[nei]);
      if (!boundary) continue;

      int i=Fv[f][0], j=Fv[f][1], k=Fv[f][2], opp=Opp[f];
      int i0=i-1, j0=j-1, k0=k-1, o0=opp-1;

      double x0=cloud(i0,0), y0=cloud(i0,1), z0=cloud(i0,2);
      double x1=cloud(j0,0), y1=cloud(j0,1), z1=cloud(j0,2);
      double x2=cloud(k0,0), y2=cloud(k0,1), z2=cloud(k0,2);
      double xo=cloud(o0,0), yo=cloud(o0,1), zo=cloud(o0,2);

      // normal = (p1-p0) x (p2-p0)
      double ax=x1-x0, ay=y1-y0, az=z1-z0;
      double bx=x2-x0, by=y2-y0, bz=z2-z0;
      double nx = ay*bz - az*by;
      double ny = az*bx - ax*bz;
      double nz = ax*by - ay*bx;

      // flip so normal points AWAY from opp (outward from kept tet)
      double dot = nx*(xo-x0) + ny*(yo-y0) + nz*(zo-z0);
      if (dot > 0) { std::swap(j,k); std::swap(x1,x2); std::swap(y1,y2); std::swap(z1,z2); }

      faces.push_back({i,j,k});

      // area
      ax=x1-x0; ay=y1-y0; az=z1-z0;
      bx=x2-x0; by=y2-x0? (y2-y0): (y2-y0); bz=z2-z0;
      nx = ay*bz - az*by;
      ny = az*bx - ax*bz;
      nz = ax*by - ay*bx;
      area += 0.5 * std::sqrt(nx*nx + ny*ny + nz*nz);

      // oriented volume contribution (closed surface -> correct total)
      double v6 = x0*(y1*z2 - z1*y2)
        - y0*(x1*z2 - z1*x2)
        + z0*(x1*y2 - y1*x2);
        volume += v6 / 6.0;
    }
  }

  // Pack F
  const int ntri = (int)faces.size();
  IntegerMatrix F(ntri, 3);
  for (int r=0; r<ntri; ++r) {
    F(r,0)=faces[r][0]; F(r,1)=faces[r][1]; F(r,2)=faces[r][2];
  }

  return List::create(
    _["V"] = cloud,
    _["F"] = F,
    _["area"] = area,
    _["volume"] = std::fabs(volume)
  );
}
