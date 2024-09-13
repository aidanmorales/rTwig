// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// box_counting
List box_counting(NumericMatrix cloud, double lowercutoff);
RcppExport SEXP _rTwig_box_counting(SEXP cloudSEXP, SEXP lowercutoffSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type cloud(cloudSEXP);
    Rcpp::traits::input_parameter< double >::type lowercutoff(lowercutoffSEXP);
    rcpp_result_gen = Rcpp::wrap(box_counting(cloud, lowercutoff));
    return rcpp_result_gen;
END_RCPP
}
// generate_random_colors
CharacterVector generate_random_colors(int n);
RcppExport SEXP _rTwig_generate_random_colors(SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(generate_random_colors(n));
    return rcpp_result_gen;
END_RCPP
}
// hex_to_float
NumericVector hex_to_float(std::vector<std::string> hex_colors);
RcppExport SEXP _rTwig_hex_to_float(SEXP hex_colorsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<std::string> >::type hex_colors(hex_colorsSEXP);
    rcpp_result_gen = Rcpp::wrap(hex_to_float(hex_colors));
    return rcpp_result_gen;
END_RCPP
}
// convex_hull
IntegerVector convex_hull(NumericMatrix points);
RcppExport SEXP _rTwig_convex_hull(SEXP pointsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type points(pointsSEXP);
    rcpp_result_gen = Rcpp::wrap(convex_hull(points));
    return rcpp_result_gen;
END_RCPP
}
// convex_hull_area
double convex_hull_area(NumericMatrix points);
RcppExport SEXP _rTwig_convex_hull_area(SEXP pointsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type points(pointsSEXP);
    rcpp_result_gen = Rcpp::wrap(convex_hull_area(points));
    return rcpp_result_gen;
END_RCPP
}
// generate_circle_points
NumericMatrix generate_circle_points(NumericVector center, double radius, int facets);
RcppExport SEXP _rTwig_generate_circle_points(SEXP centerSEXP, SEXP radiusSEXP, SEXP facetsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type center(centerSEXP);
    Rcpp::traits::input_parameter< double >::type radius(radiusSEXP);
    Rcpp::traits::input_parameter< int >::type facets(facetsSEXP);
    rcpp_result_gen = Rcpp::wrap(generate_circle_points(center, radius, facets));
    return rcpp_result_gen;
END_RCPP
}
// rotate_circle_points
NumericMatrix rotate_circle_points(NumericMatrix points, NumericVector start, NumericVector axis);
RcppExport SEXP _rTwig_rotate_circle_points(SEXP pointsSEXP, SEXP startSEXP, SEXP axisSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type points(pointsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type start(startSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type axis(axisSEXP);
    rcpp_result_gen = Rcpp::wrap(rotate_circle_points(points, start, axis));
    return rcpp_result_gen;
END_RCPP
}
// generate_mesh
NumericMatrix generate_mesh(NumericMatrix start, NumericMatrix axis, NumericVector length, NumericVector radius, int facets);
RcppExport SEXP _rTwig_generate_mesh(SEXP startSEXP, SEXP axisSEXP, SEXP lengthSEXP, SEXP radiusSEXP, SEXP facetsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type start(startSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type axis(axisSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type length(lengthSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type radius(radiusSEXP);
    Rcpp::traits::input_parameter< int >::type facets(facetsSEXP);
    rcpp_result_gen = Rcpp::wrap(generate_mesh(start, axis, length, radius, facets));
    return rcpp_result_gen;
END_RCPP
}
// generate_cloud
NumericMatrix generate_cloud(NumericMatrix start, NumericMatrix axis, NumericMatrix tips, NumericVector length, NumericVector radius, IntegerVector branch);
RcppExport SEXP _rTwig_generate_cloud(SEXP startSEXP, SEXP axisSEXP, SEXP tipsSEXP, SEXP lengthSEXP, SEXP radiusSEXP, SEXP branchSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type start(startSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type axis(axisSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type tips(tipsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type length(lengthSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type radius(radiusSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type branch(branchSEXP);
    rcpp_result_gen = Rcpp::wrap(generate_cloud(start, axis, tips, length, radius, branch));
    return rcpp_result_gen;
END_RCPP
}
// cross_product
NumericVector cross_product(NumericVector a, NumericVector b);
RcppExport SEXP _rTwig_cross_product(SEXP aSEXP, SEXP bSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type a(aSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type b(bSEXP);
    rcpp_result_gen = Rcpp::wrap(cross_product(a, b));
    return rcpp_result_gen;
END_RCPP
}
// norm
double norm(NumericVector x);
RcppExport SEXP _rTwig_norm(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(norm(x));
    return rcpp_result_gen;
END_RCPP
}
// orthonormal_vectors
List orthonormal_vectors(NumericVector U);
RcppExport SEXP _rTwig_orthonormal_vectors(SEXP USEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type U(USEXP);
    rcpp_result_gen = Rcpp::wrap(orthonormal_vectors(U));
    return rcpp_result_gen;
END_RCPP
}
// rotation_matrix
NumericMatrix rotation_matrix(NumericVector A, double angle);
RcppExport SEXP _rTwig_rotation_matrix(SEXP ASEXP, SEXP angleSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type A(ASEXP);
    Rcpp::traits::input_parameter< double >::type angle(angleSEXP);
    rcpp_result_gen = Rcpp::wrap(rotation_matrix(A, angle));
    return rcpp_result_gen;
END_RCPP
}
// mat_vec_subtraction
NumericMatrix mat_vec_subtraction(NumericMatrix A, NumericVector v);
RcppExport SEXP _rTwig_mat_vec_subtraction(SEXP ASEXP, SEXP vSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type A(ASEXP);
    Rcpp::traits::input_parameter< NumericVector >::type v(vSEXP);
    rcpp_result_gen = Rcpp::wrap(mat_vec_subtraction(A, v));
    return rcpp_result_gen;
END_RCPP
}
// mat_multiplication
NumericMatrix mat_multiplication(NumericMatrix A, NumericMatrix B);
RcppExport SEXP _rTwig_mat_multiplication(SEXP ASEXP, SEXP BSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type A(ASEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type B(BSEXP);
    rcpp_result_gen = Rcpp::wrap(mat_multiplication(A, B));
    return rcpp_result_gen;
END_RCPP
}
// index_order
IntegerVector index_order(NumericVector x);
RcppExport SEXP _rTwig_index_order(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(index_order(x));
    return rcpp_result_gen;
END_RCPP
}
// sort_index
NumericVector sort_index(NumericVector x, IntegerVector indexes);
RcppExport SEXP _rTwig_sort_index(SEXP xSEXP, SEXP indexesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type indexes(indexesSEXP);
    rcpp_result_gen = Rcpp::wrap(sort_index(x, indexes));
    return rcpp_result_gen;
END_RCPP
}
// normalize_view
NumericMatrix normalize_view(NumericVector x, NumericVector y, NumericVector z, NumericVector viewport);
RcppExport SEXP _rTwig_normalize_view(SEXP xSEXP, SEXP ySEXP, SEXP zSEXP, SEXP viewportSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< NumericVector >::type z(zSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type viewport(viewportSEXP);
    rcpp_result_gen = Rcpp::wrap(normalize_view(x, y, z, viewport));
    return rcpp_result_gen;
END_RCPP
}
// solve_and_transpose
NumericMatrix solve_and_transpose(const NumericMatrix& proj, const NumericMatrix& model, const NumericMatrix& normalized);
RcppExport SEXP _rTwig_solve_and_transpose(SEXP projSEXP, SEXP modelSEXP, SEXP normalizedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const NumericMatrix& >::type proj(projSEXP);
    Rcpp::traits::input_parameter< const NumericMatrix& >::type model(modelSEXP);
    Rcpp::traits::input_parameter< const NumericMatrix& >::type normalized(normalizedSEXP);
    rcpp_result_gen = Rcpp::wrap(solve_and_transpose(proj, model, normalized));
    return rcpp_result_gen;
END_RCPP
}
// as_euclidean
NumericMatrix as_euclidean(const NumericMatrix& x);
RcppExport SEXP _rTwig_as_euclidean(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const NumericMatrix& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(as_euclidean(x));
    return rcpp_result_gen;
END_RCPP
}
// rtwig_window2user
NumericMatrix rtwig_window2user(NumericVector x, NumericVector y, NumericVector z, List projection);
RcppExport SEXP _rTwig_rtwig_window2user(SEXP xSEXP, SEXP ySEXP, SEXP zSEXP, SEXP projectionSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< NumericVector >::type z(zSEXP);
    Rcpp::traits::input_parameter< List >::type projection(projectionSEXP);
    rcpp_result_gen = Rcpp::wrap(rtwig_window2user(x, y, z, projection));
    return rcpp_result_gen;
END_RCPP
}
// translation_matrix
NumericMatrix translation_matrix(double x, double y, double z);
RcppExport SEXP _rTwig_translation_matrix(SEXP xSEXP, SEXP ySEXP, SEXP zSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type y(ySEXP);
    Rcpp::traits::input_parameter< double >::type z(zSEXP);
    rcpp_result_gen = Rcpp::wrap(translation_matrix(x, y, z));
    return rcpp_result_gen;
END_RCPP
}
// user_matrix
NumericMatrix user_matrix(NumericVector x, NumericVector y, NumericVector z, List start);
RcppExport SEXP _rTwig_user_matrix(SEXP xSEXP, SEXP ySEXP, SEXP zSEXP, SEXP startSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< NumericVector >::type z(zSEXP);
    Rcpp::traits::input_parameter< List >::type start(startSEXP);
    rcpp_result_gen = Rcpp::wrap(user_matrix(x, y, z, start));
    return rcpp_result_gen;
END_RCPP
}
// connect_cylinders
void connect_cylinders(DataFrame& cylinder);
RcppExport SEXP _rTwig_connect_cylinders(SEXP cylinderSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DataFrame& >::type cylinder(cylinderSEXP);
    connect_cylinders(cylinder);
    return R_NilValue;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_rTwig_box_counting", (DL_FUNC) &_rTwig_box_counting, 2},
    {"_rTwig_generate_random_colors", (DL_FUNC) &_rTwig_generate_random_colors, 1},
    {"_rTwig_hex_to_float", (DL_FUNC) &_rTwig_hex_to_float, 1},
    {"_rTwig_convex_hull", (DL_FUNC) &_rTwig_convex_hull, 1},
    {"_rTwig_convex_hull_area", (DL_FUNC) &_rTwig_convex_hull_area, 1},
    {"_rTwig_generate_circle_points", (DL_FUNC) &_rTwig_generate_circle_points, 3},
    {"_rTwig_rotate_circle_points", (DL_FUNC) &_rTwig_rotate_circle_points, 3},
    {"_rTwig_generate_mesh", (DL_FUNC) &_rTwig_generate_mesh, 5},
    {"_rTwig_generate_cloud", (DL_FUNC) &_rTwig_generate_cloud, 6},
    {"_rTwig_cross_product", (DL_FUNC) &_rTwig_cross_product, 2},
    {"_rTwig_norm", (DL_FUNC) &_rTwig_norm, 1},
    {"_rTwig_orthonormal_vectors", (DL_FUNC) &_rTwig_orthonormal_vectors, 1},
    {"_rTwig_rotation_matrix", (DL_FUNC) &_rTwig_rotation_matrix, 2},
    {"_rTwig_mat_vec_subtraction", (DL_FUNC) &_rTwig_mat_vec_subtraction, 2},
    {"_rTwig_mat_multiplication", (DL_FUNC) &_rTwig_mat_multiplication, 2},
    {"_rTwig_index_order", (DL_FUNC) &_rTwig_index_order, 1},
    {"_rTwig_sort_index", (DL_FUNC) &_rTwig_sort_index, 2},
    {"_rTwig_normalize_view", (DL_FUNC) &_rTwig_normalize_view, 4},
    {"_rTwig_solve_and_transpose", (DL_FUNC) &_rTwig_solve_and_transpose, 3},
    {"_rTwig_as_euclidean", (DL_FUNC) &_rTwig_as_euclidean, 1},
    {"_rTwig_rtwig_window2user", (DL_FUNC) &_rTwig_rtwig_window2user, 4},
    {"_rTwig_translation_matrix", (DL_FUNC) &_rTwig_translation_matrix, 3},
    {"_rTwig_user_matrix", (DL_FUNC) &_rTwig_user_matrix, 4},
    {"_rTwig_connect_cylinders", (DL_FUNC) &_rTwig_connect_cylinders, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_rTwig(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
