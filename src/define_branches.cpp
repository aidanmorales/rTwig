#include <Rcpp.h>
#include <unordered_map>
#include <unordered_set>
#include <vector>
#include <algorithm>
using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
IntegerVector define_branches(DataFrame cylinder) {
  IntegerVector id = cylinder["id"];
  IntegerVector parent = cylinder["parent"];
  NumericVector growth_length = cylinder["growth_length"];
  int n = id.size();

  IntegerVector branch(n, NA_INTEGER);

  // id to row
  unordered_map<int, int> id_to_row;
  for (int i = 0; i < n; ++i) {
    id_to_row[id[i]] = i;
  }

  // parent to child
  unordered_map<int, vector<int>> children_map;
  for (int i = 0; i < n; ++i) {
    if (!IntegerVector::is_na(parent[i])) {
      children_map[parent[i]].push_back(i);
    }
  }

  auto follow_path = [&](int start_row, int idx) {
    int current = start_row;
    while (branch[current] == NA_INTEGER) {
      branch[current] = idx;

      auto it = children_map.find(id[current]);
      if (it == children_map.end()) break;

      const vector<int>& children = it->second;
      vector<int> unassigned;
      for (int child : children) {
        if (branch[child] == NA_INTEGER) {
          unassigned.push_back(child);
        }
      }

      if (unassigned.empty()) break;

      int next_row = unassigned[0];
      double max_len = growth_length[next_row];
      for (int row : unassigned) {
        if (growth_length[row] > max_len) {
          next_row = row;
          max_len = growth_length[row];
        }
      }

      current = next_row;
    }
  };

  // Iteratively assign branches
  int branch_counter = 1;
  for (int i = 0; i < n; ++i) {
    if (branch[i] == NA_INTEGER) {
      follow_path(i, branch_counter++);
    }
  }

  return branch;
}

// [[Rcpp::export]]
IntegerVector define_branch_order(DataFrame cylinder) {
  IntegerVector id = cylinder["id"];
  IntegerVector parent = cylinder["parent"];
  IntegerVector branch = cylinder["branch"];
  int n = id.size();

  IntegerVector branch_order(n, NA_INTEGER);

  // Build maps
  std::unordered_map<int, int> id_to_branch;
  std::unordered_map<int, std::vector<int>> branch_to_indices;
  std::unordered_set<int> all_ids(id.begin(), id.end());

  for (int i = 0; i < n; ++i) {
    id_to_branch[id[i]] = branch[i];
    branch_to_indices[branch[i]].push_back(i);
  }

  // Find branch order 0
  std::unordered_set<int> assigned_branches;
  for (int i = 0; i < n; ++i) {
    if (all_ids.find(parent[i]) == all_ids.end()) {
      // Root found — assign order 0 to the whole branch
      int b = branch[i];
      for (int idx : branch_to_indices[b]) {
        branch_order[idx] = 0;
      }
      assigned_branches.insert(b);
    }
  }

  // Assign branch orders
  bool updated = true;
  while (updated) {
    updated = false;
    for (const auto& kv : branch_to_indices) {
      int b = kv.first;
      if (assigned_branches.find(b) != assigned_branches.end()) continue;

      // Get parent id from any row in this branch
      int parent_id = parent[kv.second[0]];

      // Look up parent branch
      auto it = id_to_branch.find(parent_id);
      if (it != id_to_branch.end()) {
        int parent_branch = it->second;
        // Check if parent branch is already assigned
        if (assigned_branches.find(parent_branch) != assigned_branches.end()) {
          int parent_order = branch_order[branch_to_indices[parent_branch][0]];
          for (int idx : kv.second) {
            branch_order[idx] = parent_order + 1;
          }
          assigned_branches.insert(b);
          updated = true;
        }
      }
    }
  }

  return branch_order;
}

// Original R code for reference
// assign_cylinder_indices <- function(df) {
//   n <- nrow(df)
//   df$index <- NA_integer_
//
//   # Create a fast lookup for id to row index
//   id_to_row <- setNames(seq_len(n), df$id)
//
//   # Build children map using row indices
//   children_map <- split(seq_len(n), df$parent)
//
//   # Find forks: parents with more than one child
//   forks <- as.integer(names(children_map)[sapply(children_map, length) > 1])
//   forks <- forks[!is.na(forks) & forks %in% df$id]
//
//   index_counter <- 1
//
//   follow_path <- function(start_row, idx) {
//     current_row <- start_row
//
//     while (is.na(df$index[current_row])) {
//       df$index[current_row] <<- idx
//       children_rows <- children_map[[as.character(df$id[current_row])]]
//
//       if (is.null(children_rows)) break
//
//         # Filter to unassigned children
//         unassigned <- children_rows[is.na(df$index[children_rows])]
//         if (length(unassigned) == 0) break
//
//           # Pick the child with max growth_length
//           next_row <- unassigned[which.max(df$growth_length[unassigned])]
//           current_row <- next_row
//     }
//   }
//
//   # Start from forks
//   for (fork_id in forks) {
//     fork_row <- id_to_row[as.character(fork_id)]
//     if (is.na(df$index[fork_row])) {
//       follow_path(fork_row, index_counter)
//       index_counter <- index_counter + 1
//     }
//
//     # Also handle any unassigned children from this fork
//     children_rows <- children_map[[as.character(fork_id)]]
//     unassigned <- children_rows[is.na(df$index[children_rows])]
//     for (child_row in unassigned) {
//       follow_path(child_row, index_counter)
//       index_counter <- index_counter + 1
//     }
//   }
//
//   return(df)
// }

// # Step 1: Identify the branch whose parent ID does not exist in the 'id' column (i.e., root)
// root_parent_id <- branches$parent[!branches$parent %in% branches$id]
//
// # Step 2: Assign order 0 to the entire branch with the root parent ID (and all its children)
// # Find all branches connected to the root
// branches_to_assign_order_0 <- branches$branch[branches$parent == root_parent_id | branches$id == root_parent_id]
// branches$branch_order[branches$branch %in% branches_to_assign_order_0] <- 0
//
//
// # Iterate to assign orders based on parent-child relationships
// while (any(is.na(branches$branch_order))) {
// # Loop through branches that don't have an order yet
//   for (i in unique(branches$branch[is.na(branches$branch_order)])) {
// # Get the row corresponding to the current branch
//     branch <- branches[branches$branch == i, ]
//
// # Get the parent id of the current branch
//     parent_id <- branch$parent[1]
//
// # Check if the parent branch has an order
//     parent_order <- branches$branch_order[branches$id == parent_id]
//
// # If the parent has an order, assign the current branch order
//     if (!is.na(parent_order)) {
//       branches$branch_order[branches$branch == i] <- parent_order + 1
//     }
//   }
// }
