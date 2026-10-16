// Fit path GAMs using the compiled COBS, SparseM, and quantreg kernels.
// [[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>
#include <algorithm>
#include <numeric>
#include <vector>
#include <cmath>
using namespace Rcpp;
// Match cobs/src/splines.h, SparseM/src/SparseM.h, and
// quantreg/src/srqfnc.f. Apple ARM64 passes variadic arguments differently
// from fixed arguments, so these routines must not be called through (...).
using BasisFn = void (*)(double*, int*, int*, double*, int*, int*, double*, int*);
using ValueFn = void (*)(double*, double*, int*, int*, double*, int*, int*, double*);
using TransposeFn = void (*)(int*, int*, int*, int*, double*, int*, int*,
                            double*, int*, int*);
using CountMultiplyFn = void (*)(int*, int*, int*, int*, int*, int*, int*,
                                int*, int*, int*);
using MultiplyFn = void (*)(int*, int*, int*, double*, int*, int*, double*,
                           int*, int*, double*, int*, int*, int*, int*, int*);
using AddFn = void (*)(int*, int*, int*, double*, int*, int*, double*, double*,
                      int*, int*, double*, int*, int*, int*, int*, int*);
using MatvecFn = void (*)(int*, double*, double*, double*, int*, int*);
using CholFn = void (*)(
  int* m, int* nnzdmax, double* d, int* jd, int* id,
  int* nnzdsm, double* dsub, int* jdsub, int* nsub, int* nsubmax,
  int* lindx, int* xlindx, int* nsuper, int* nnzlmax, double* lnz,
  int* xlnz, int* invp, int* perm, int* iwmax, int* iwork,
  int* colcnt, int* snode, int* xsuper, int* split, int* tmpmax,
  double* tmpvec, int* cachsz, int* level, int* ierr,
  double* timed, double* tiny, double* large);
using BacksolveFn = void (*)(int*, int*, int*, int*, int*, int*, int*, double*,
                            int*, int*, int*, int*, double*, double*, double*, double*);
using SolveFn = void (*)(
  int* n1, int* m, int* nnza1, double* a1, int* ja1, int* ia1,
  double* ao1, int* jao1, int* iao1, int* n2, int* nnza2,
  double* a2, int* ja2, int* ia2, double* ao2, int* jao2, int* iao2,
  int* nnzdmax, double* d, int* jd, int* id, double* dsub, int* jdsub,
  int* nnzemax, double* e, int* je, int* ie, int* nnzgmax,
  double* g, int* jg, int* ig, int* nnzhmax, double* h, int* jh, int* ih,
  int* nsubmax, int* lindx, int* xlindx, int* nnzlmax, double* lnz,
  int* xlnz, int* iw, int* iwmax, int* iwork, int* xsuper,
  int* tmpmax, double* tmpvec, int* maxn1n2,
  double* ww1, double* wwm, double* wwn1, double* wwn2,
  int* cachsz, int* level, double* x1, double* x2, double* s, double* u,
  double* c1, double* c2, double* sm_tn_lrg, double* y,
  int* ierr, int* maxit, double* timewd);

static BasisFn basis_fn = nullptr;
static ValueFn value_fn = nullptr;
static TransposeFn transpose_fn = nullptr;
static CountMultiplyFn countmul_fn = nullptr;
static MultiplyFn multiply_fn = nullptr;
static AddFn plus_fn = nullptr;
static MatvecFn matvec_fn = nullptr;
static CholFn chol_fn = nullptr;
static BacksolveFn back_fn = nullptr;
static SolveFn solve_fn = nullptr;

template <typename Function>
static Function path_gam_symbol(const List& symbols, const char* name) {
  SEXP pointer = symbols[name];
  if (TYPEOF(pointer) != EXTPTRSXP) stop("Expected NativeSymbol pointer");
  Function fn = reinterpret_cast<Function>(R_ExternalPtrAddr(pointer));
  if (!fn) stop("Missing native kernel: %s", name);
  return fn;
}

// Caller supplies NativeSymbol addresses with withRegistrationInfo=FALSE.
// [[Rcpp::export]]
void initialize_path_gam(List symbols) {
  basis_fn = path_gam_symbol<BasisFn>(symbols, "basis");
  value_fn = path_gam_symbol<ValueFn>(symbols, "value");
  transpose_fn = path_gam_symbol<TransposeFn>(symbols, "transpose");
  countmul_fn = path_gam_symbol<CountMultiplyFn>(symbols, "countmul");
  multiply_fn = path_gam_symbol<MultiplyFn>(symbols, "multiply");
  plus_fn = path_gam_symbol<AddFn>(symbols, "plus");
  matvec_fn = path_gam_symbol<MatvecFn>(symbols, "matvec");
  chol_fn = path_gam_symbol<CholFn>(symbols, "chol");
  back_fn = path_gam_symbol<BacksolveFn>(symbols, "backsolve");
  solve_fn = path_gam_symbol<SolveFn>(symbols, "solver");
}

struct CSR {
  int rows, cols;
  std::vector<double> a;
  std::vector<int> j, p;
  CSR(int nr, int nc): rows(nr), cols(nc), p(nr+1,1) {}
  void finish(int row) { p[row+1]=a.size()+1; }
  void entry(double value,int col) { a.push_back(value); j.push_back(col); }
};

static CSR transpose(CSR& x) {
  CSR out(x.cols,x.rows); out.a.resize(x.a.size()); out.j.resize(x.j.size());
  int job=1,ipos=1;
  transpose_fn(&x.rows,&x.cols,&job,&ipos,x.a.data(),x.j.data(),x.p.data(),
               out.a.data(),out.j.data(),out.p.data());
  return out;
}
static CSR multiply(CSR& x,CSR& y) {
  CSR out(x.rows,y.cols);
  std::vector<int> nrowcounts(x.rows),work(y.cols);
  int nz=0,job=1,ierr=0;
  countmul_fn(&x.rows,&x.cols,&y.cols,x.j.data(),x.p.data(),y.j.data(),y.p.data(),
              nrowcounts.data(),&nz,work.data());
  out.a.resize(nz); out.j.resize(nz);
  std::fill(work.begin(),work.end(),0);
  multiply_fn(&x.rows,&y.cols,&job,x.a.data(),x.j.data(),x.p.data(),y.a.data(),
              y.j.data(),y.p.data(),out.a.data(),out.j.data(),out.p.data(),
              &nz,work.data(),&ierr);
  if (ierr) stop("Native sparse multiplication failed");
  out.a.resize(out.p.back()-1); out.j.resize(out.p.back()-1);
  return out;
}
static CSR add(CSR& x,CSR& y) {
  CSR out(x.rows,x.cols);
  int nz=x.a.size()+y.a.size()+1,job=1,ierr=0;
  double scale=1;
  out.a.resize(nz); out.j.resize(nz); std::vector<int> work(x.cols);
  plus_fn(&x.rows,&x.cols,&job,x.a.data(),x.j.data(),x.p.data(),&scale,y.a.data(),
          y.j.data(),y.p.data(),out.a.data(),out.j.data(),out.p.data(),&nz,
          work.data(),&ierr);
  if (ierr) stop("Native sparse addition failed");
  out.a.resize(out.p.back()-1); out.j.resize(out.p.back()-1);
  return out;
}
static std::vector<double> matvec(CSR& x,std::vector<double>& y) {
  std::vector<double> out(x.rows);
  matvec_fn(&x.rows,y.data(),out.data(),x.a.data(),x.j.data(),x.p.data());
  return out;
}

struct Basis {std::vector<double> values; std::vector<int> offsets;};
static Basis basis(std::vector<double>& knots,std::vector<double> x,int deriv) {
  int n=x.size(),m=knots.size(),ord=2;
  std::vector<double> augmented; augmented.push_back(knots.front());
  augmented.insert(augmented.end(),knots.begin(),knots.end());
  augmented.push_back(knots.back());
  std::vector<int> derivatives(n,deriv);
  Basis out; out.values.resize(2*n); out.offsets.resize(n);
  basis_fn(augmented.data(),&m,&ord,x.data(),derivatives.data(),&n,
           out.values.data(),out.offsets.data());
  return out;
}

static List solve(CSR& A,CSR& C,std::vector<double> y,std::vector<double> r,
                  std::vector<double> rhs) {
  int n1=A.rows,m=A.cols,n2=C.rows;
  for (auto& v:y) v=-v;
  for (auto& v:r) v=-v;
  CSR AT=transpose(A),CT=transpose(C);
  CSR E=multiply(AT,A),G=multiply(CT,C),H=add(E,G);
  int nnzx=A.a.size(),nnzR=C.a.size(),nnzdmax=std::max(nnzx,nnzR),
    nnzemax=E.a.size(),nnzgmax=G.a.size(),nnzhmax=H.a.size(),
    nsubmax=nnzhmax,tmpmax=6*m,nnzlmax=4*nnzdmax,iwmax=7*m+3;

  // Exactly the SparseM chol() initialization used by rq.fit.sfnc().
  int nnzdsm=nnzemax+m+1,nsub=0,nsuper=0,cache=1024,level=8,ierr=0;
  double tiny=1e-30,Large=1e128,time0=0;
  std::vector<double> dsub0(nnzdsm),lnz0(nnzlmax),tmp0(tmpmax);
  std::vector<int> jdsub0(nnzdsm),lindx0(nsubmax),xlindx0(m+1),xlnz0(m+1),
    invp0(m),perm0(m),iwork0(iwmax),colcnt0(m),snode0(m),xsuper0(m+1),split0(m);
  // .Fortran duplicates its input arguments; retain the original E for srqfnc.
  CSR Ecopy=E;
  chol_fn(&m,&nnzemax,Ecopy.a.data(),Ecopy.j.data(),Ecopy.p.data(),&nnzdsm,
    dsub0.data(),jdsub0.data(),&nsub,&nsubmax,lindx0.data(),xlindx0.data(),
    &nsuper,&nnzlmax,lnz0.data(),xlnz0.data(),invp0.data(),perm0.data(),&iwmax,
    iwork0.data(),colcnt0.data(),snode0.data(),xsuper0.data(),split0.data(),
    &tmpmax,tmp0.data(),&cache,&level,&ierr,&time0,&tiny,&Large);
  if (ierr) stop("Native initial Cholesky failed (status %d)",ierr);
  int nnzl=xlnz0.back()-1,nrhs=1;
  std::vector<double> initial_rhs=matvec(AT,y),b(m),backwork(m);
  back_fn(&m,&nsub,&nsuper,&nrhs,lindx0.data(),xlindx0.data(),&nnzl,lnz0.data(),
          xlnz0.data(),invp0.data(),perm0.data(),xsuper0.data(),backwork.data(),
          b.data(),initial_rhs.data(),&time0);
  std::vector<double> fitted=matvec(A,b);
  std::vector<double> wwn1(10*n1),wwn2(7*n2),x1(n1,.5),x2(n2,1),s(n1,.5),u(n1,1);
  double small=1e-8;
  for(int i=0;i<n1;++i) {
    double residual=y[i]-fitted[i];
    double z=residual*(residual>0);
    if (std::abs(residual)<small) z+=small;
    wwn1[i]=z; wwn1[n1+i]=z-residual;
  }
  for(int i=0;i<n2;++i) wwn2[n2+i]=1;
  int maxn1n2=std::max(n1,n2),maxiter=100;
  cache=64; ierr=0;
  std::vector<double> d(nnzdmax),dsub(nnzhmax+1),g(nnzgmax),h(nnzhmax),
    lnz(nnzlmax),tmp(tmpmax),ww1(maxn1n2),wwm(6*m),times(7),scale={small,tiny,Large};
  std::vector<int> jd(nnzdmax),id(m+1),jdsub(nnzhmax+1),jg(nnzgmax),ig(m+1),
    jh(nnzhmax),ih(m+1),lindx(nsubmax),xlindx(m+1),xlnz(m+1),iw(5*m),iwork(iwmax),xsuper(m+1);
  std::copy(rhs.begin(),rhs.end(),wwm.begin());
  solve_fn(&n1,&m,&nnzx,A.a.data(),A.j.data(),A.p.data(),AT.a.data(),AT.j.data(),
    AT.p.data(),&n2,&nnzR,C.a.data(),C.j.data(),C.p.data(),CT.a.data(),CT.j.data(),
    CT.p.data(),&nnzdmax,d.data(),jd.data(),id.data(),dsub.data(),jdsub.data(),
    &nnzemax,E.a.data(),E.j.data(),E.p.data(),&nnzgmax,g.data(),jg.data(),ig.data(),
    &nnzhmax,h.data(),jh.data(),ih.data(),&nsubmax,lindx.data(),xlindx.data(),
    &nnzlmax,lnz.data(),xlnz.data(),iw.data(),&iwmax,iwork.data(),xsuper.data(),
    &tmpmax,tmp.data(),&maxn1n2,ww1.data(),wwm.data(),wwn1.data(),wwn2.data(),
    &cache,&level,x1.data(),x2.data(),s.data(),u.data(),y.data(),r.data(),
    scale.data(),b.data(),&ierr,&maxiter,times.data());
  for (auto& v:b) v=-v;
  return List::create(_["coef"]=b,_["status"]=ierr,_["iterations"]=maxiter);
}

// [[Rcpp::export]]
List path_gam_fit(NumericVector x, NumericVector y, NumericVector z,
                  double twig_radius, double min_gl) {
  if (!solve_fn) stop("Native kernels not initialized");
  int n=x.size();
  if (y.size()!=n) stop("x and y sizes differ");
  if (n <= 3) {
    return List::create(
      _["predictions"] = NumericVector(z.size(), twig_radius),
      _["status"] = 0
    );
  }
  for (int i = 0; i < n; ++i) {
    if (!std::isfinite(x[i]) || !std::isfinite(y[i])) {
      stop("Native path fitter requires finite observations");
    }
  }
  std::vector<int> order(n); std::iota(order.begin(),order.end(),0);
  std::stable_sort(order.begin(),order.end(),[&](int i,int j){return x[i]<x[j];});
  std::vector<double> sorted(n),unique;
  for(int i=0;i<n;++i) sorted[i]=x[order[i]];
  unique=sorted; unique.erase(std::unique(unique.begin(),unique.end()),unique.end());
  int m=std::min(n-1,static_cast<int>(unique.size()));
  if(m<3) stop("Too few distinct growth lengths");
  std::vector<double> knots(m);
  double by=(static_cast<double>(unique.size())-1)/(m-1);
  for(int i=0;i<m;++i) knots[i]=unique[static_cast<int>(1+i*by)-1];
  knots.back()=unique.back();
  double delta=1e-6*(knots.back()-knots.front());
  knots.front()-=delta; knots.back()+=delta;

  Basis obs=basis(knots,sorted,0);
  std::vector<double> derivative_points(knots.begin(),knots.end()-1);
  Basis slope=basis(knots,derivative_points,1),anchor=basis(knots,{min_gl},0);
  CSR A(n+m-2,m),C(m+1,m);
  std::vector<int> reverse(n);
  for(int i=0;i<n;++i) reverse[order[i]]=i;
  for(int i=0;i<n;++i) {
    int k=reverse[i];
    A.entry(obs.values[2*k],obs.offsets[k]+1);
    A.entry(obs.values[2*k+1],obs.offsets[k]+2); A.finish(i);
  }
  for(int i=0;i<m-2;++i) {
    A.entry(-slope.values[2*i]*.01,i+1);
    A.entry((slope.values[2*(i+1)]-slope.values[2*i+1])*.01,i+2);
    A.entry(slope.values[2*(i+1)+1]*.01,i+3); A.finish(n+i);
  }
  for(int i=0;i<m-1;++i) {
    C.entry(slope.values[2*i],slope.offsets[i]+1);
    C.entry(slope.values[2*i+1],slope.offsets[i]+2); C.finish(i);
  }
  C.entry(anchor.values[0],anchor.offsets[0]+1);
  C.entry(anchor.values[1],anchor.offsets[0]+2); C.finish(m-1);
  C.entry(-anchor.values[0],anchor.offsets[0]+1);
  C.entry(-anchor.values[1],anchor.offsets[0]+2); C.finish(m);

  // Match R colSums accumulation precision and original row order.
  std::vector<long double> sums(m,0);
  for(int i=0;i<A.rows;++i) for(int k=A.p[i]-1;k<A.p[i+1]-1;++k)
    sums[A.j[k]-1]+=static_cast<long double>(.5*A.a[k]);
  std::vector<double> rhs(m),yy(A.rows,0),rr(C.rows,0);
  for(int i=0;i<m;++i) rhs[i]=static_cast<double>(sums[i]);
  for(int i=0;i<n;++i) yy[i]=y[i];
  rr[m-1]=twig_radius; rr[m]=-twig_radius;
  List result=solve(A,C,yy,rr,rhs);
  std::vector<double> coef=as<std::vector<double>>(result["coef"]);
  std::vector<int> zo(z.size()); std::iota(zo.begin(),zo.end(),0);
  for (double v : z) {
    if (!std::isfinite(v)) {
      stop("Finite prediction coordinates required");
    }
  }
  std::stable_sort(zo.begin(),zo.end(),[&](int i,int j){return z[i]<z[j];});
  std::vector<double> zz(z.size()),pred(z.size()),augmented;
  for(int i=0;i<z.size();++i) zz[i]=z[zo[i]];
  augmented.push_back(knots.front());
  augmented.insert(augmented.end(),knots.begin(),knots.end());
  augmented.push_back(knots.back());
  int ord=2,nn=z.size(),deriv=0;
  value_fn(augmented.data(),coef.data(),&m,&ord,zz.data(),&nn,&deriv,pred.data());
  NumericVector predictions(nn);
  for(int i=0;i<nn;++i) predictions[zo[i]]=pred[i];
  int status = as<int>(result["status"]);
  return List::create(_["predictions"] = predictions, _["status"] = status);
}
