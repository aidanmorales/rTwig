#include <Rcpp.h>
#include <set>
#include <array>
#include <cmath>

using namespace Rcpp;

// Calculates the number of boxes in each size needed to enclose all points.
// This is done by rounding each coordinate in the point cloud to the highest
// multiple of the box size and tallying the number of distinct points.
// The total number of distinct points is the total number of boxes of that
// size needed to enclose all points in the point cloud.

// [[Rcpp::export]]
List box_counting(NumericMatrix cloud, double lowercutoff) {
  int n = cloud.nrow(); // Number of points
  const int m = 3; // Number of dimensions (should be 3)

  // Finds the largest box size edge length that can contain all points
  double x_max = max(cloud(_, 0));
  double y_max = max(cloud(_, 1));
  double z_max = max(cloud(_, 2));
  double x_min = min(cloud(_, 0));
  double y_min = min(cloud(_, 1));
  double z_min = min(cloud(_, 2));

  double x_range = std::round((x_max - x_min) * 100) / 100;
  double y_range = std::round((y_max - y_min) * 100) / 100;
  double z_range = std::round((z_max - z_min) * 100) / 100;
  double rulerlimit = std::max({x_range, y_range, z_range});

  // Halves the largest box size until it reaches the lower cutoff point
  std::vector<double> size;
  size.push_back(rulerlimit);
  while (size.back() > lowercutoff && size.back() / 2 >= lowercutoff) {
    size.push_back(size.back() / 2);
  }

  // Calculate the ratio between all box edge lengths and the initial box
  NumericVector ruler(size.size());
  for (size_t j = 0; j < size.size(); ++j) {
    ruler[j] = size[j] / rulerlimit;
  }

  // Calculates the number of distinct points for each size
  NumericVector voxelnumber(size.size());
  for (size_t j = 0; j < size.size(); ++j) {
    std::set<std::array<double, 3>> unique_points;
    for (int i = 0; i < n; ++i) {
      std::array<double, 3> rounded_point;
      for (int k = 0; k < m; ++k) {
        double coord = cloud(i, k);
        double size_j = size[j];
        rounded_point[k] = std::floor(coord / size_j) * size_j;
      }
      unique_points.insert(rounded_point);
    }
    voxelnumber[j] = unique_points.size();
  }

  // Return a list containing voxelnumber and ruler
  return List::create(_["voxelnumber"] = voxelnumber, _["ruler"] = ruler, _["size"] = size);
}
