#include <Rcpp.h>
using namespace Rcpp;

//' @title Normalize View
//'
//' @description Normalizes RGL plot view
//'
//' @param x NumericVector x coordinate
//' @param y NumericVector y coordinate
//' @param z NumericVector z coordinate
//' @param viewport NumericVector from RGL
//' @return Numeric Matrix
//'
//' @noRd
//'
// [[Rcpp::export]]
NumericMatrix normalize_view(
    const NumericVector& x,
    const NumericVector& y,
    const NumericVector& z,
    const NumericVector& viewport) {

  //////////////////////////////////////////////////////////////////////////////
  ///////// RECYCLE XYZ ////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////
  int n_x = x.size();
  int n_y = y.size();
  int n_z = z.size();

  int max_len = std::max({n_x, n_y, n_z});

  NumericVector x_out(max_len);
  NumericVector y_out(max_len);
  NumericVector z_out(max_len);

  // Recycle x
  for (int i = 0; i < max_len; ++i) {
    x_out[i] = x[i % n_x];
  }

  // Recycle y
  for (int i = 0; i < max_len; ++i) {
    y_out[i] = y[i % n_y];
  }

  // Recycle z
  for (int i = 0; i < max_len; ++i) {
    z_out[i] = z[i % n_z];
  }

  //////////////////////////////////////////////////////////////////////////////
  ///////// NORMALIZE MATRIX ///////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////
  int n = x_out.size();

  // Initialize output matrix with 4 rows and n columns
  NumericMatrix result(4, n);

  // Extract viewport values for efficiency
  double v1 = viewport[0];
  double v2 = viewport[1];
  double v3 = viewport[2];
  double v4 = viewport[3];

  // Calculate transformed vectors
  for (int i = 0; i < n; ++i) {
    result(0, i) = 2 * (x_out[i] - v1 / v3) - 1;
    result(1, i) = 2 * (y_out[i] - v2 / v4) - 1;
    result(2, i) = 2 * z_out[i] - 1;
    result(3, i) = 1;  // The last row is always 1
  }

  return result;
}

//' @title Solve and Transpose
//'
//' @description Solves and transposes RGL view matrices
//'
//' @param proj NumericMatrix from rgl.projection function
//' @param model NumericMatrix from rgl.projection function
//' @param normalized NumericMatrix from normalize_view function
//' @return Numeric Matrix
//'
//' @noRd
//'
// [[Rcpp::export]]
NumericMatrix solve_and_transpose(
    const NumericMatrix& proj,
    const NumericMatrix& model,
    const NumericMatrix& normalized) {
  int n = proj.nrow();
  int k = proj.ncol();
  int m = model.ncol();

  //////////////////////////////////////////////////////////////////////////////
  ///////// MATRIX MULTIPLICATION //////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////
  NumericMatrix proj_model_product(n, m);

  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < m; ++j) {
      proj_model_product(i, j) = 0;
      for (int l = 0; l < k; ++l) {
        proj_model_product(i, j) += proj(i, l) * model(l, j);
      }
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  ///////// MATRIX INVERSION ///////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////
  NumericMatrix mat_inv(n, n);

  // Create identity matrix
  for (int i = 0; i < n; ++i) {
    mat_inv(i, i) = 1.0;
  }

  // Gaussian elimination
  for (int i = 0; i < n; ++i) {
    double diag = proj_model_product(i, i);
    if (diag == 0) {
      stop("Matrix inversion failed: zero diagonal element.");
    }
    for (int j = 0; j < n; ++j) {
      proj_model_product(i, j) /= diag;
      mat_inv(i, j) /= diag;
    }
    for (int k = 0; k < n; ++k) {
      if (k != i) {
        double factor = proj_model_product(k, i);
        for (int j = 0; j < n; ++j) {
          proj_model_product(k, j) -= factor * proj_model_product(i, j);
          mat_inv(k, j) -= factor * mat_inv(i, j);
        }
      }
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  ///////// CALCULATE RESULT ///////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////
  NumericMatrix result(n, normalized.ncol());

  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < normalized.ncol(); ++j) {
      result(i, j) = 0;
      for (int l = 0; l < n; ++l) {
        result(i, j) += mat_inv(i, l) * normalized(l, j);
      }
    }
  }

  return transpose(result);
}

//' @title As Euclidean
//'
//' @description Rcpp port of RGL's asEuclidean function.
//' Converts n x 4 matrix to n x 3 euclidean matrix.
//'
//' @param x NumericMatrix
//' @return Numeric Matrix
//'
//' @noRd
//'
// [[Rcpp::export]]
NumericMatrix as_euclidean(const NumericMatrix& x) {
  int ncol_x = x.ncol();

  // Case 1: Matrix with 4 columns
  if (ncol_x == 4) {
    NumericMatrix result(x.nrow(), 3);
    for (int i = 0; i < x.nrow(); ++i) {
      double w = x(i, 3);
      if (w != 0) { // Avoid division by zero
        for (int j = 0; j < 3; ++j) {
          result(i, j) = x(i, j) / w;
        }
      } else {
        Rcpp::stop("Division by zero encountered in as_euclidean conversion.");
      }
    }
    return result;
  }

  // Case 2: Matrix with 3 columns
  else if (ncol_x == 3) {
    return x;
  }

  // Case 3: Vector
  else if (x.size() % 4 == 0) {
    NumericMatrix matrix_x(x.size() / 4, 4);
    for (int i = 0; i < x.size(); ++i) {
      matrix_x(i / 4, i % 4) = x[i];
    }
    return as_euclidean(matrix_x);
  } else if (x.size() % 3 == 0) {
    NumericMatrix matrix_x(x.size() / 3, 3);
    for (int i = 0; i < x.size(); ++i) {
      matrix_x(i / 3, i % 3) = x[i];
    }
    return as_euclidean(matrix_x);
  }

  // Handle unexpected cases
  Rcpp::stop("Don't know how to convert x");
}

//' @title RGL Window to User
//'
//' @description Rcpp port of RGL's rgl.window2user function
//'
//' @param x NumericVector x coordinate
//' @param y NumericVector y coordinate
//' @param z NumericVector z coordinate
//' @param projection List from RGL rgl.projection function
//' @return Numeric Matrix
//'
//' @noRd
//'
// [[Rcpp::export]]
NumericMatrix rtwig_window2user(
    const NumericVector& x,
    const NumericVector& y,
    const NumericVector& z,
    const List& projection) {

  // Extract List Elements
  NumericMatrix model = projection[0]; // model
  NumericMatrix proj = projection[1];  // proj
  NumericVector view = projection[2];  // view

  // Normalize Matrix
  NumericMatrix normalized = normalize_view(x, y, z, view);

  // Convert to Euclidean
  NumericMatrix euclidean = as_euclidean(
    solve_and_transpose(proj, model, normalized)
  );

  return euclidean;
}

//' @title Translation Matrix
//'
//' @description Generates translation matrix
//'
//' @param x coordinate
//' @param y coordinate
//' @param z coordinate
//'
//' @noRd
//'
// [[Rcpp::export]]
NumericMatrix translation_matrix(
    const double& x,
    const double& y,
    const double& z) {

  NumericMatrix result = NumericMatrix::diag(4, 1.0);
  result(3, 0) = x;
  result(3, 1) = y;
  result(3, 2) = z;

  return result;
}

//' @title User Matrix
//'
//' @description Generates user matrix to update RGL plot view
//'
//' @param x coordinate
//' @param y coordinate
//' @param z coordinate
//' @param start List of starting values
//'
//' @noRd
//'
// [[Rcpp::export]]
NumericMatrix user_matrix(
    const NumericVector& x,
    const NumericVector& y,
    const NumericVector& z,
    const List& start) {

  // Extract start elements
  NumericMatrix user_matrix = start[0];
  NumericVector viewport = start[1];
  NumericVector scale = start[2];
  List projection = start[3];
  NumericVector pos = start[4];

  // Calculate xlat
  NumericVector x_normalized = x / viewport[2];
  NumericVector y_normalized = 1 - (y / viewport[3]);
  NumericMatrix xlat = rtwig_window2user(
    x_normalized,
    y_normalized,
    z,
    projection
  );

  // Adjust xlat
  for (int i = 0; i < 3; ++i) {
    xlat(0, i) = (xlat(0, i) - pos[i]) * scale[i];
  }

  // Calculate mouse matrix
  NumericMatrix mouse_matrix = translation_matrix(xlat(0, 0), xlat(0, 1), xlat(0, 2));

  // Calculate user matrix
  NumericMatrix result(4, 4);
  NumericMatrix mouse_matrix_t = transpose(mouse_matrix);

  for (int i = 0; i < 4; ++i) {
    for (int j = 0; j < 4; ++j) {
      result(i, j) = 0.0;
      for (int k = 0; k < 4; ++k) {
        result(i, j) += user_matrix(i, k) * mouse_matrix_t(k, j);
      }
    }
  }

  return result;
}
