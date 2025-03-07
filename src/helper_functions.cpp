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
//' @return NumericVector
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


//' @title Which Rcpp
//'
//' @description Find the indices where a condition is true
//'
//' @param condition logical vector
//' @return integer vector
//'
//' @noRd
//'
// [[Rcpp::export]]
IntegerVector which_rcpp(LogicalVector condition) {
  IntegerVector indices;
  for (int i = 0; i < condition.size(); i++) {
    if (condition[i]) {
      indices.push_back(i);
    }
  }
  return indices;
}

//' @title Calculate Normals
//'
//' @description Calculate normals per cylinder vertex
//'
//' @param vertices NumericMatrix
//' @return NumericMatrix
//'
//' @noRd
//'
// [[Rcpp::export]]
NumericMatrix calculate_normals(NumericMatrix vertices) {
  int n_triangles = vertices.nrow() / 3;
  NumericMatrix normals(vertices.nrow(), 3);

  // Loop through each triangle
  for (int i = 0; i < n_triangles; ++i) {
    // Get the triangle vertics
    double x0 = vertices(i * 3, 0);
    double y0 = vertices(i * 3, 1);
    double z0 = vertices(i * 3, 2);

    double x1 = vertices(i * 3 + 1, 0);
    double y1 = vertices(i * 3 + 1, 1);
    double z1 = vertices(i * 3 + 1, 2);

    double x2 = vertices(i * 3 + 2, 0);
    double y2 = vertices(i * 3 + 2, 1);
    double z2 = vertices(i * 3 + 2, 2);

    // Calculate the two edge vectors
    double e1_x = x1 - x0;
    double e1_y = y1 - y0;
    double e1_z = z1 - z0;

    double e2_x = x2 - x0;
    double e2_y = y2 - y0;
    double e2_z = z2 - z0;

    // Compute the cross product (normal)
    double normal_x = e1_y * e2_z - e1_z * e2_y;
    double normal_y = e1_z * e2_x - e1_x * e2_z;
    double normal_z = e1_x * e2_y - e1_y * e2_x;

    // Normalize the normal vector
    double length = std::sqrt(normal_x * normal_x + normal_y * normal_y + normal_z * normal_z);

    normal_x /= length;
    normal_y /= length;
    normal_z /= length;

    // Store the normals for each triangle vertex
    normals(i * 3, 0) = normal_x;
    normals(i * 3, 1) = normal_y;
    normals(i * 3, 2) = normal_z;

    normals(i * 3 + 1, 0) = normal_x;
    normals(i * 3 + 1, 1) = normal_y;
    normals(i * 3 + 1, 2) = normal_z;

    normals(i * 3 + 2, 0) = normal_x;
    normals(i * 3 + 2, 1) = normal_y;
    normals(i * 3 + 2, 2) = normal_z;
  }

  return normals;
}
