#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <map>
#include <vector>
using namespace Rcpp;

//' Connect cylinder endpoints within branches.
//' @param cylinder Updated TreeQSM cylinder data frame.
//' @return A smoothed cylinder data frame.
//' @noRd
// [[Rcpp::export]]
DataFrame connect_cylinders(DataFrame cylinder) {
  DataFrame result = clone(cylinder);
  IntegerVector position = as<IntegerVector>(result["PositionInBranch"]);
  IntegerVector branch = as<IntegerVector>(result["branch"]);
  IntegerVector id = as<IntegerVector>(result["extension"]);
  IntegerVector parent = as<IntegerVector>(result["parent"]);
  NumericVector sx = result["start.x"], sy = result["start.y"], sz = result["start.z"];
  NumericVector ex = result["end.x"], ey = result["end.y"], ez = result["end.z"];
  NumericVector ax = result["axis.x"], ay = result["axis.y"], az = result["axis.z"];
  NumericVector length = result["length"];
  const int n = result.nrows();
  std::map<int, std::vector<int>> branches;
  for (int i = 0; i < n; ++i) {
    if (IntegerVector::is_na(branch[i]) || branch[i] < 1 ||
        IntegerVector::is_na(position[i]) || position[i] < 1) {
      stop("Invalid branch or PositionInBranch at cylinder row %d.", i + 1);
    }
    if (!std::isfinite(sx[i]) || !std::isfinite(sy[i]) || !std::isfinite(sz[i]) ||
        !std::isfinite(ex[i]) || !std::isfinite(ey[i]) || !std::isfinite(ez[i])) {
      stop("Non-finite endpoint geometry at cylinder row %d.", i + 1);
    }
    branches[branch[i]].push_back(i);
  }
  for (auto& entry : branches) {
    checkUserInterrupt();
    auto& rows = entry.second;
    std::sort(rows.begin(), rows.end(), [&](int a, int b) {
      return position[a] < position[b];
    });
    for (size_t j = 1; j < rows.size(); ++j) {
      if (position[rows[j]] == position[rows[j - 1]]) {
        stop("Duplicate PositionInBranch in branch %d.", entry.first);
      }
    }
    for (size_t j = 0; j < rows.size(); ++j) {
      const int i = rows[j];
      const bool continuation = j > 0 && parent[i] == id[rows[j - 1]];
      if (continuation) {
        const int previous = rows[j - 1];
        sx[i] = ex[previous];
        sy[i] = ey[previous];
        sz[i] = ez[previous];
      }
      // Preserve the existing smoothing rule: the branch base endpoint and
      // branch tip stay fixed; interior joints use the original gap midpoint.
      if (continuation && j + 1 < rows.size() && parent[rows[j + 1]] == id[i]) {
        const int next = rows[j + 1];
        ex[i] = ex[i] / 2 + sx[next] / 2;
        ey[i] = ey[i] / 2 + sy[next] / 2;
        ez[i] = ez[i] / 2 + sz[next] / 2;
      }
      const double dx = ex[i] - sx[i], dy = ey[i] - sy[i], dz = ez[i] - sz[i];
      const double distance = std::hypot(std::hypot(dx, dy), dz);
      if (!std::isfinite(distance) || distance <= 0) {
        stop("Smoothing produced zero-length or non-finite geometry at cylinder %d. Input unchanged.", id[i]);
      }
      length[i] = distance;
      ax[i] = dx / distance;
      ay[i] = dy / distance;
      az[i] = dz / distance;
    }
  }
  return result;
}
