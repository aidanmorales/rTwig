#include <Rcpp.h>
using namespace Rcpp;


//' @title Cross Product
//'
//' @description Calculates the cross product of two vectors
//'
//' @param a first vector
//' @param b second vector
//' @return cross product of a and b
//'
//' @noRd
//'
// [[Rcpp::export]]
NumericVector cross_product(NumericVector a, NumericVector b) {
  NumericVector result(3);
  result[0] = a[1] * b[2] - a[2] * b[1];
  result[1] = a[2] * b[0] - a[0] * b[2];
  result[2] = a[0] * b[1] - a[1] * b[0];
  return result;
}



//' @title Norm
//'
//' @description Calculates the norm of a 1 x 3 vector
//'
//' @param x vector x with size 1 x 3
//' @return norm of vector x
//'
//' @noRd
//'
// [[Rcpp::export]]
double norm(NumericVector x) {
  return std::sqrt(std::pow(x[0], 2) + std::pow(x[1], 2) + std::pow(x[2], 2));
}



//' @title Orthonormal Vectors
//'
//' @description Generates vectors V and W that are unit vectors orthogonal to
//' themselves and to the input vector U
//'
//' @param U input vector
//' @return list containing vectors V and W
//'
//' @noRd
//'
// [[Rcpp::export]]
List orthonormal_vectors(NumericVector U) {
  U = U / norm(U);
  NumericVector V = runif(3);
  V = cross_product(V, U);

  while (norm(V) == 0) {
    V = NumericVector::create(0.5, 0.5, 0.5);
    V = cross_product(V, U);
  }

  V = V / norm(V);
  NumericVector W = cross_product(U, V);
  W = W / norm(W);

  return List::create(Named("V") = V, Named("W") = W);
}



//' @title Rotation matrix
//'
//' @description Returns the rotation matrix for the given axis A and angle
//'
//' @param A vector of size 1 x 3
//' @param angle angle in radians
//' @return rotation matrix
//'
//' @noRd
//'
// [[Rcpp::export]]
NumericMatrix rotation_matrix(NumericVector A, double angle) {
  double norm_A = norm(A);
  A[0] /= norm_A;
  A[1] /= norm_A;
  A[2] /= norm_A;

  NumericMatrix R(3, 3);
  double c = std::cos(angle);
  double s = std::sin(angle);

  R(0, 0) = A[0] * A[0] + (1 - A[0] * A[0]) * c;
  R(0, 1) = A[0] * A[1] * (1 - c) - A[2] * s;
  R(0, 2) = A[0] * A[2] * (1 - c) + A[1] * s;

  R(1, 0) = A[0] * A[1] * (1 - c) + A[2] * s;
  R(1, 1) = A[1] * A[1] + (1 - A[1] * A[1]) * c;
  R(1, 2) = A[1] * A[2] * (1 - c) - A[0] * s;

  R(2, 0) = A[0] * A[2] * (1 - c) - A[1] * s;
  R(2, 1) = A[1] * A[2] * (1 - c) + A[0] * s;
  R(2, 2) = A[2] * A[2] + (1 - A[2] * A[2]) * c;

  return R;
}



//' @title Matrix Vector Subtraction
//'
//' @description Subtract a vector in a matrix
//'
//' @param A matrix of dimensions n x n
//' @param v vector with dimensions 1 x n
//' @return matrix
//'
//' @noRd
//'
// [[Rcpp::export]]
NumericMatrix mat_vec_subtraction(NumericMatrix A, NumericVector v) {
  int n = A.nrow();
  int m = A.ncol();

  for (int i = 0; i < m; i++) {
    for (int j = 0; j < n; j++) {
      A(j, i) -= v[i];
    }
  }

  return A;
}


//' @title Matrix Multiplication
//'
//' @description Subtract a matrix by a matrix
//'
//' @param A matrix of dimensions n x n
//' @param B vector with dimensions n x n
//' @return matrix
//'
//' @noRd
//'
// [[Rcpp::export]]
NumericMatrix mat_multiplication(NumericMatrix A, NumericMatrix B) {
  int nrow_A = A.nrow();
  int ncol_A = A.ncol();
  int ncol_B = B.ncol();

  // Result matrix dimensions
  NumericMatrix C(nrow_A, ncol_B);

  // Perform matrix multiplication
  for (int i = 0; i < nrow_A; i++) {
    for (int j = 0; j < ncol_B; j++) {
      double sum = 0.0;
      for (int k = 0; k < ncol_A; k++) {
        sum += A(i, k) * B(k, j);
      }
      C(i, j) = sum;
    }
  }

  return C;
}



//' @title Index Order
//'
//' @description Get order of sorted vector indexes
//'
//' @param v vector with dimensions 1 x n
//' @return integer vector
//'
//' @noRd
//'
// [[Rcpp::export]]
IntegerVector index_order(NumericVector x) {
  IntegerVector indexes = seq_along(x); // Initialize indexes from 1 to length of x
  std::sort(indexes.begin(), indexes.end(), [&x](int i, int j) {
    return x[i - 1] < x[j - 1]; // Subtract 1 from i and j to adjust to 0-based index
  });
  return indexes;
}



//' @title Sort Index
//'
//' @description Sort a vector by a specified order
//'
//' @param v vector with dimensions 1 x n
//' @param indexes integer vector with desired ordering
//' @return vector
//'
//' @noRd
//'
// [[Rcpp::export]]
NumericVector sort_index(NumericVector x, IntegerVector indexes) {
  NumericVector sorted(x.size());
  for (int i = 0; i < indexes.size(); ++i) {
    sorted[i] = x[indexes[i]];
  }
  return sorted;
}
