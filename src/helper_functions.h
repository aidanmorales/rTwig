// helper_functions.h
#ifndef HELPER_FUNCTIONS_H
#define HELPER_FUNCTIONS_H

#include <Rcpp.h>
using namespace Rcpp;

// Declaration of helper functions
NumericVector cross_product(NumericVector a, NumericVector b);
NumericVector sort_index(NumericVector x, IntegerVector indexes);

IntegerVector index_order(NumericVector x);

NumericMatrix rotation_matrix(NumericVector A, double angle);
NumericMatrix mat_vec_subtraction(NumericMatrix A, NumericVector v);
NumericMatrix mat_multiplication(NumericMatrix A, NumericMatrix B);

List orthonormal_vectors(NumericVector U);

double norm(NumericVector x);

#endif // HELPER_FUNCTIONS_H
