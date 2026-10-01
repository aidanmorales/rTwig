#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <limits>
#include <vector>

using namespace Rcpp;

namespace {
IntegerVector read_parents(DataFrame cylinder) {
  NumericVector values = as<NumericVector>(cylinder["parent"]);
  const int n = cylinder.nrows();
  if (n == 0) stop("TreeQSM must contain a trunk cylinder.");
  if (!std::isfinite(values[0]) || values[0] != 0.0) {
    stop("TreeQSM cylinder 1 must be the trunk base with parent 0.");
  }
  IntegerVector parent(n);
  for (int i = 0; i < n; ++i) {
    if (NumericVector::is_na(values[i])) {
      parent[i] = NA_INTEGER;
    } else if (!std::isfinite(values[i]) || values[i] != std::floor(values[i])) {
      stop("TreeQSM parent at cylinder %d must be an integer or NA.", i + 1);
    } else {
      // Out-of-range ids are missing connections, not usable edges.
      parent[i] = values[i] < 0 || values[i] > n ? NA_INTEGER : values[i];
    }
  }
  return parent;
}

std::vector<std::vector<int>> child_links(const IntegerVector& parent) {
  const int n = parent.size();
  std::vector<std::vector<int>> children(n);
  for (int i = 0; i < n; ++i) {
    if (parent[i] >= 1 && parent[i] <= n) {
      children[parent[i] - 1].push_back(i);
    }
  }
  return children;
}

int mark_component(int root, const std::vector<std::vector<int>>& children,
                   std::vector<bool>& reached) {
  int count = 0;
  std::vector<int> pending(1, root);
  while (!pending.empty()) {
    const int current = pending.back();
    pending.pop_back();
    if (reached[current]) continue;
    reached[current] = true;
    ++count;
    pending.insert(pending.end(), children[current].begin(), children[current].end());
  }
  return count;
}
}

// [[Rcpp::export]]
IntegerVector verify_treeqsm(DataFrame cylinder) {
  IntegerVector parent = read_parents(cylinder);
  const int n = cylinder.nrows();
  const auto children = child_links(parent);
  std::vector<bool> reached(n, false);
  const int connected = mark_component(0, children, reached);
  std::vector<int> missing;
  for (int i = 1; i < n; ++i) {
    if (IntegerVector::is_na(parent[i]) || parent[i] < 1 || parent[i] > n) {
      missing.push_back(i + 1);
      mark_component(i, children, reached);
    }
  }
  for (int i = 0; i < n; ++i) {
    if (!reached[i]) {
      stop("TreeQSM contains a parent cycle (cylinder %d cannot reach a component root). Repair aborted.", i + 1);
    }
  }
  IntegerVector roots = wrap(missing);
  roots.attr("disconnected_cylinders") = n - connected;
  return roots;
}

// [[Rcpp::export]]
DataFrame repair_treeqsm(DataFrame cylinder) {
  IntegerVector missing = verify_treeqsm(cylinder);
  DataFrame repaired = clone(cylinder);
  if (missing.size() == 0) return repaired;

  const int n = cylinder.nrows();
  IntegerVector parent = read_parents(cylinder);
  NumericVector old_parent = as<NumericVector>(cylinder["parent"]);
  const auto children = child_links(parent);
  std::vector<bool> connected(n, false);
  mark_component(0, children, connected);
  IntegerVector new_parents(missing.size());
  NumericVector old_parents(missing.size()), distances(missing.size());
  NumericVector sx = cylinder["start.x"], sy = cylinder["start.y"];
  NumericVector sz = cylinder["start.z"], ax = cylinder["axis.x"];
  NumericVector ay = cylinder["axis.y"], az = cylinder["axis.z"];
  NumericVector length = cylinder["length"];
  std::vector<double> dx(n), dy(n), dz(n), squared_length(n);

  for (int i = 0; i < n; ++i) {
    dx[i] = ax[i] * length[i];
    dy[i] = ay[i] * length[i];
    dz[i] = az[i] * length[i];
    squared_length[i] = dx[i]*dx[i] + dy[i]*dy[i] + dz[i]*dz[i];
    if (!std::isfinite(sx[i]) || !std::isfinite(sy[i]) ||
        !std::isfinite(sz[i]) || !std::isfinite(squared_length[i]) ||
        length[i] < 0) {
      stop("Finite cylinder geometry is required to repair TreeQSM connections.");
    }
  }

  for (int k = 0; k < missing.size(); ++k) {
    checkUserInterrupt();
    const int id = missing[k];
    const int root = id - 1;

    int best = -1;
    double best_distance = std::numeric_limits<double>::infinity();
    for (int j = 0; j < n; ++j) {
      if (!connected[j]) continue;
      const double ox = sx[root] - sx[j];
      const double oy = sy[root] - sy[j];
      const double oz = sz[root] - sz[j];
      double t = squared_length[j] > 0.0 ?
        (ox*dx[j] + oy*dy[j] + oz*dz[j]) / squared_length[j] : 0.0;
      t = std::max(0.0, std::min(1.0, t));
      const double gx = ox - t*dx[j];
      const double gy = oy - t*dy[j];
      const double gz = oz - t*dz[j];
      const double distance = gx*gx + gy*gy + gz*gz;
      if (distance < best_distance) {
        best_distance = distance;
        best = j;
      }
    }
    if (best < 0) stop("No eligible parent cylinder found for cylinder %d.", id);
    parent[root] = best + 1;

    old_parents[k] = old_parent[root];
    new_parents[k] = best + 1;
    distances[k] = std::sqrt(best_distance);
    // The entire repaired component is now eligible for later attachments.
    mark_component(root, children, connected);
  }

  repaired["parent"] = parent;
  if (verify_treeqsm(repaired).size() != 0) {
    stop("TreeQSM repair failed to connect every cylinder to the trunk.");
  }

  repaired.attr("treeqsm_repairs") = DataFrame::create(
    _["cylinder"] = missing, _["old_parent"] = old_parents,
    _["new_parent"] = new_parents, _["distance"] = distances
  );
  return repaired;
}
