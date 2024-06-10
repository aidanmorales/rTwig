#include <Rcpp.h>
#include "helper_functions.h"
using namespace Rcpp;

//' @title Generate Cloud
//'
//' @description Generates a synthetic point cloud from QSM cylinder attributes
//'
//' @param start n x 3 matrix containing the x, y, z cylinder start points
//' @param axis n x 3 matrix containing the x, y, z cylinder axis directions
//' @param tips n x 3 matrix containing the x, y, z cylinder end points
//' @param length vector containing the cylinder lengths
//' @param radius vector containing the cylinder radii
//' @param branch vector containing the cylinder branch ids
//' @return n x 3 point cloud matrix
//'
//' @noRd
//'
// [[Rcpp::export]]
NumericMatrix generate_cloud(
    NumericMatrix start,
    NumericMatrix axis,
    NumericMatrix tips,
    NumericVector length,
    NumericVector radius,
    IntegerVector branch) {
  int nc = length.size();
  int t = 0;
  int initial_size = 5 * nc;
  NumericMatrix cloud(initial_size, 3);

  auto resize_cloud = [&](int new_size) {
    NumericMatrix temp(new_size, 3);
    for (int i = 0; i < cloud.nrow(); ++i) {
      for (int j = 0; j < 3; ++j) {
        temp(i, j) = cloud(i, j);
      }
    }
    cloud = temp;
  };

  for (int i = 0; i < nc; i++) {
    List UV = orthonormal_vectors(axis(i, _));
    NumericVector U = radius[i] * as<NumericVector>(UV["V"]);
    if (branch[i] == 1) {
      for (int k = 0; k < 4; k++) {
        NumericVector M = start(i, _) + (k + 1) * length[i] / 4 * axis(i, _);
        NumericMatrix R = rotation_matrix(axis(i, _), M_PI / 12);
        for (int j = 0; j < 12; j++) {
          if (j > 0) {
            NumericVector tmp(3);
            for (int l = 0; l < 3; l++) {
              tmp[l] = R(l, 0) * U[0] + R(l, 1) * U[1] + R(l, 2) * U[2];
            }
            U = tmp;
          }
          if (t >= cloud.nrow()) {
            resize_cloud(cloud.nrow() * 2);  // resize cloud if needed
          }
          for (int l = 0; l < 3; l++) {
            cloud(t, l) = M[l] + U[l];
          }
          t++;
        }
      }
    } else {
      NumericVector M = start(i, _) + length[i] / 2 * axis(i, _);
      NumericMatrix R = rotation_matrix(axis(i, _), M_PI / 4);
      for (int j = 0; j < 4; j++) {
        if (j > 0) {
          NumericVector tmp(3);
          for (int l = 0; l < 3; l++) {
            tmp[l] = R(l, 0) * U[0] + R(l, 1) * U[1] + R(l, 2) * U[2];
          }
          U = tmp;
        }
        if (t >= cloud.nrow()) {
          resize_cloud(cloud.nrow() * 2);  // resize cloud if needed
        }
        for (int l = 0; l < 3; l++) {
          cloud(t, l) = M[l] + U[l];
        }
        t++;
      }
    }
  }

  // Ensure cloud is proper size
  cloud = cloud(Range(0, t - 1), _);

  // Create matrix to store cloud, start and tips
  NumericMatrix combined(cloud.nrow() + start.nrow() + tips.nrow(), cloud.ncol());

  // Add cloud
  int idx = 0;
  for (int i = 0; i < cloud.nrow(); ++i) {
    for (int j = 0; j < cloud.ncol(); ++j) {
      combined(idx, j) = cloud(i, j);
    }
    ++idx;
  }

  // Add start
  for (int i = 0; i < start.nrow(); ++i) {
    for (int j = 0; j < start.ncol(); ++j) {
      combined(idx, j) = start(i, j);
    }
    ++idx;
  }

  // Add tips
  for (int i = 0; i < tips.nrow(); ++i) {
    for (int j = 0; j < tips.ncol(); ++j) {
      combined(idx, j) = tips(i, j);
    }
    ++idx;
  }

  // Return Combined Matrix
  return combined;
}
