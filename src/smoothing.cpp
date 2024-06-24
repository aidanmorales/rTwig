#include <Rcpp.h>
using namespace Rcpp;

// Function to calculate Euclidean distance
inline double euclidean_distance(double x1, double y1, double z1, double x2, double y2, double z2) {
  return sqrt(pow(x2 - x1, 2) + pow(y2 - y1, 2) + pow(z2 - z1, 2));
}

//' @title Connect Cylinders
//'
//' @description Connects cylinder endpoints and smooths axes
//'
//' @param branch_position position in the branch
//' @param branch branch id
//' @param start x x start
//' @param start y y start
//' @param start z z start
//' @param axis x x axis
//' @param axis y y axis
//' @param axis z z axis
//' @param end x x end
//' @param end y y end
//' @param end z z end
//' @return n x 3 point cloud matrix
//'
//' @noRd
//'
// [[Rcpp::export]]
void connect_cylinders(DataFrame& cylinder) {
  IntegerVector branch_position = cylinder["PositionInBranch"];
  IntegerVector branch = cylinder["branch"];
  NumericVector start_x = cylinder["start.x"];
  NumericVector start_y = cylinder["start.y"];
  NumericVector start_z = cylinder["start.z"];
  NumericVector end_x = cylinder["end.x"];
  NumericVector end_y = cylinder["end.y"];
  NumericVector end_z = cylinder["end.z"];
  NumericVector axis_x = cylinder["axis.x"];
  NumericVector axis_y = cylinder["axis.y"];
  NumericVector axis_z = cylinder["axis.z"];

  int n = cylinder.nrows();

  // Pre-compute branch indices and maximum positions
  std::vector<std::vector<int>> branch_indices(max(branch) + 1);
  std::vector<int> max_positions(max(branch) + 1, 0);

  for (int i = 0; i < n; ++i) {
    int b = branch[i];
    branch_indices[b].push_back(i);
    if (branch_position[i] > max_positions[b]) {
      max_positions[b] = branch_position[i];
    }
  }

  // Loop through each unique branch
  for (int b = 1; b <= max(branch); ++b) {
    std::vector<int>& indices = branch_indices[b];
    int m = indices.size();

    // Apply operations within each branch
    for (int i = 0; i < m; ++i) {
      int idx = indices[i];

      // Calculate start coordinates if necessary
      if (branch_position[idx] > 1) {
        start_x[idx] = end_x[idx - 1];
        start_y[idx] = end_y[idx - 1];
        start_z[idx] = end_z[idx - 1];
      }

      // Calculate end coordinates if necessary
      if (branch_position[idx] > 1 && branch_position[idx] < max_positions[b]) {
        end_x[idx] = (start_x[idx + 1] + end_x[idx]) / 2;
        end_y[idx] = (start_y[idx + 1] + end_y[idx]) / 2;
        end_z[idx] = (start_z[idx + 1] + end_z[idx]) / 2;
      }

      // Calculate length using Euclidean distance
      double length2 = euclidean_distance(start_x[idx], start_y[idx], start_z[idx], end_x[idx], end_y[idx], end_z[idx]);

      // Update axis vectors
      axis_x[idx] = (end_x[idx] - start_x[idx]) / length2;
      axis_y[idx] = (end_y[idx] - start_y[idx]) / length2;
      axis_z[idx] = (end_z[idx] - start_z[idx]) / length2;
    }
  }
}
