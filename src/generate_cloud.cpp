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
    IntegerVector branch,
    bool metrics,
    double spacing) {
  int nc = length.size();
  int t = 0;

  if (metrics == TRUE) {
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

    return combined;
  }

  // Estimate total points
  int tp = 0;
  for (int i = 0; i < nc; i++) {
    int height_pts = static_cast<int>(length[i] / spacing);
    height_pts = height_pts > 0 ? height_pts : 1;
    int circ_pts = static_cast<int>((2 * M_PI * radius[i]) / spacing);
    circ_pts = circ_pts > 0 ? circ_pts : 1;
    tp += (height_pts + 1) * circ_pts;
  }

  NumericMatrix cloud(tp, 4);

  for (int i = 0; i < nc; i++) {
    int height_pts = static_cast<int>(length[i] / spacing);
    height_pts = height_pts > 0 ? height_pts : 1;
    double height_step = length[i] / height_pts;
    int circ_pts = static_cast<int>((2 * M_PI * radius[i]) / spacing);
    circ_pts = circ_pts > 0 ? circ_pts : 1;
    double angle_step = 2 * M_PI / circ_pts;

    for (int h = 0; h <= height_pts; h++) {
      NumericVector M = start(i, _) + h * height_step * axis(i, _);

      for (int j = 0; j < circ_pts; j++) {
        double angle = j * angle_step;

        NumericVector offset = {
          radius[i] * cos(angle),
          radius[i] * sin(angle),
          0
        };

        NumericMatrix R = rotation_matrix(axis(i, _), angle);
        NumericVector rotated_offset(3);
        for (int l = 0; l < 3; l++) {
          rotated_offset[l] = R(l, 0) * offset[0] + R(l, 1) * offset[1] + R(l, 2) * offset[2];
        }

        for (int l = 0; l < 3; l++) {
          cloud(t, l) = M[l] + rotated_offset[l];
        }

        cloud(t, 3) = i + 1;

        t++;
      }
    }
  }

  return cloud;
}
