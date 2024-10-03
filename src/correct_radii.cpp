#include <Rcpp.h>
#include "helper_functions.h"
#include <set>
#include <algorithm>
using namespace Rcpp;

//' @title IQR Filter
//'
//' @description Filter cylinder outliers
//'
//' @param x NumericVector
//' @param method int
//' @return IntegerVector
//'
//' @noRd
//'
// [[Rcpp::export]]
IntegerVector iqr_filter(NumericVector x, int method) {
 int n = x.size();

 // Calculate indices for the 25th and 75th percentiles
 int q1_idx = static_cast<int>(0.25 * (n - 1));
 int q3_idx = static_cast<int>(0.75 * (n - 1));

 // Clone the input vector to avoid modifying the original
 Rcpp::NumericVector x_copy(x.size());
 std::copy(x.begin(), x.end(), x_copy.begin());

 // Find Q1 using nth_element
 std::nth_element(x_copy.begin(), x_copy.begin() + q1_idx, x_copy.end());
 double q1 = x_copy[q1_idx];

 // Interpolate Q1 if needed
 double q1_fraction = 0.25 * (n - 1) - q1_idx;
 if (q1_fraction > 0 && q1_idx + 1 < n) {
   std::nth_element(x_copy.begin(), x_copy.begin() + q1_idx + 1, x_copy.end());
   q1 += q1_fraction * (x_copy[q1_idx + 1] - q1);
 }

 // Find Q3 using nth_element
 std::nth_element(x_copy.begin(), x_copy.begin() + q3_idx, x_copy.end());
 double q3 = x_copy[q3_idx];

 // Interpolate Q3 if needed
 double q3_fraction = 0.75 * (n - 1) - q3_idx;
 if (q3_fraction > 0 && q3_idx + 1 < n) {
   std::nth_element(x_copy.begin(), x_copy.begin() + q3_idx + 1, x_copy.end());
   q3 += q3_fraction * (x_copy[q3_idx + 1] - q3);
 }

 double iqr = q3 - q1;
 double upper = q3 + 1.5 * iqr;
 double lower = q1 - 1.5 * iqr;

 IntegerVector outliers(n);
 for (int i = 0; i < n; ++i) {
   // General or small cylinder pass
   if (method == 1) {
     if ((lower <= x[i]) && (x[i] >= upper)) {
       outliers[i] = 1;
     } else {
       outliers[i] = 0;
     }
   }

   // Large cylinder pass
   if (method == 2) {
     if ((lower <= x[i]) && (x[i] <= upper)) {
       outliers[i] = 0;
     } else {
       outliers[i] = 1;
     }
   }
 }

 return outliers;
}

//' @title Taper Filter
//'
//' @description Taper filter for QSM paths
//'
//' @param radius NumericVector cylinder radius
//' @param taper int taper method
//' @return IntegerVector
//'
//' @noRd
//'
// [[Rcpp::export]]
IntegerVector taper_filter(NumericVector radius, int taper) {
 int n = radius.size();
 IntegerVector index(n);

 if (taper == 1) {
   for (int i = 0; i < n; ++i) {
     int z = 0;
     for (int j = 0; j <= i; ++j) {
       if ((radius[i] - radius[j]) / radius[i] > 1 / pow(i + 1, 0.5)) {
         z++;
       }
     }
     if (z > 0) {
       index[i] = 1;
     } else {
       index[i] = 0;
     }
   }
 } else if (taper == 2) {
   for (int i = 0; i < n; ++i) {
     int z = 0;
     for (int j = 0; j <= i; ++j) {
       if ((radius[i] - radius[j]) / radius[i] > 0) {
         z++;
       }
     }
     if (z > 0) {
       index[i] = 1;
     } else {
       index[i] = 0;
     }
   }
 }

 return index;
}

//' @title Broken Branch Filter
//'
//' @description Find broken branches and prepare the paths for modeling.
//' Broken branches have <= 1 child branch in the 1st order branches.
//'
//' @param path a path DataFrame
//' @param twig_radius the twig radius in meters as a double
//' @param broken_branch boolean to enable or disable the filter
//' @return List
//'
//' @noRd
//'
// [[Rcpp::export]]
List broken_branch_filter(
  DataFrame path,
  double twig_radius,
  bool broken_branch) {
  // Extract and prepare path variables
  NumericVector index = path["index"];
  NumericVector growth_length = path["growth_length"];
  NumericVector radius = path["radius"];
  IntegerVector branch_order = path["branch_order"];
  IntegerVector branch = path["branch"];
  IntegerVector total_children = path["total_children"];

  double min_gl = min(growth_length);
  double min_rad, max_rad_ord;

  // Filter path where index is 0 (good cylinder fits)
  NumericVector x = growth_length[index == 0];
  NumericVector y = radius[index == 0];

  if (broken_branch) {
    int max_order = branch_order[path.nrows() - 1];

    if (max_order == 1) {
       min_rad = y[y.size() - 1];

       // Find indices where radius == min_rad
       IntegerVector radius_indices = which_rcpp(radius == min_rad);

       // Find branch orders for radius_indices
       std::set<int> unique_orders;
       for (int i = 0; i < radius_indices.size(); i++) {
         unique_orders.insert(branch_order[radius_indices[i]]);
       }

       max_rad_ord = *unique_orders.begin();

       // Broken branch radii are 25% less in new orders when no good cylinders exist
       min_rad = (y[y.size() - 1] - (0.25 * y[y.size() - 1])) / max_order;

       int last_branch = branch[path.nrows() - 1];
       IntegerVector first_branch_indices = which_rcpp(branch != 1);
       int first_branch = branch[first_branch_indices[0]];

       int max_children = 0;
       for (int i = 0; i < path.nrows(); i++) {
         if ((branch[i] == last_branch || branch[i] == first_branch) && total_children[i] >= 2) {
           max_children++;
         }
       }

       // Bypasses broken branch filter if branch is alive in 1st order
       if (max_children > 3) {
         // Append new values, no unnecessary copies
         x.push_back(min(growth_length));
         y.push_back(twig_radius);

         min_rad = y[y.size() - 1];
         max_rad_ord = 0;
       }
    } else { // Bypasses broken branch filter for alive branches
      x.push_back(min(growth_length));
      y.push_back(twig_radius);

      min_rad = y[y.size() - 1];
      max_rad_ord = 0;
    }
  } else { // Bypass broken branch filter
    min_rad = twig_radius;
    max_rad_ord = branch_order[path.nrows() - 1];
  }

  return List::create(
    _["x"] = x,
    _["y"] = y,
    _["min_rad"] = min_rad,
    _["max_rad_ord"] = max_rad_ord,
    _["min_gl"] = min_gl
  );
}

//' @title Model Matrix
//'
//' @description Generate constraint the matrix for the cobs GAM
//'
//' @param min_growth_length min growth length as a double
//' @param twig_radius the twig radius in meters as a double
//' @return NumericMatrix
//'
//' @noRd
//'
// [[Rcpp::export]]
NumericMatrix model_matrix(double min_growth_length, double twig_radius) {
// Create a matrix with 1 row and 3 columns
NumericMatrix mat(1, 3);

mat(0, 0) = 0;
mat(0, 1) = min_growth_length;
mat(0, 2) = twig_radius;

return mat;
}

// R CODE FOR REFERENCE
// broken_branch_filter_r <- function(path, twig_radius, broken_branch) {
//   path_temp <- filter(path, .data$index == 0)
//   min_gl <- min(path$growth_length)
//
//   if (broken_branch == TRUE) {
// # Identifies broken branches and removes any real twig tapering
// # Broken branches have <= 1 child branch in the 1st order branches
//     max_order <- path$branch_order[nrow(path)]
//
//     if (max_order == 1) {
//       x <- path_temp$growth_length
//       y <- path_temp$radius
//       min_rad <- y[length(y)]
//       max_rad_ord <- unique(path$branch_order[path$radius == min_rad])
//
// # Broken branch radii are 25% less in new orders when no good cylinders exist
//       min_rad <- (y[length(y)] - (0.25 * y[length(y)])) / max_order
//
//       last_branch <- path$branch[nrow(path)]
//       first_branch <- path$branch[which(path$branch != 1)[1]]
//       max_children <- sum(path$branch %in% c(last_branch, first_branch) & path$total_children >= 2)
//
// # Bypasses broken branch filter if branch is alive in 1st order
//         if (max_children > 3) {
//           x <- path_temp$growth_length
//           x[length(x) + 1] <- min(path$growth_length)
//           y <- path_temp$radius
//           y[length(y) + 1] <- twig_radius
//
//           min_rad <- y[length(y)]
//           max_rad_ord <- 0
//         }
//     } else { # Bypasses broken branch filter for alive branches
//     x <- path_temp$growth_length
//     x[length(x) + 1] <- min(path$growth_length)
//       y <- path_temp$radius
//     y[length(y) + 1] <- twig_radius
//
//     min_rad <- y[length(y)]
//     max_rad_ord <- 0
//     }
//   } else { # Bypass broken branch filter
//     x <- path_temp$growth_length
//     y <- path_temp$radius
//
//     min_rad <- twig_radius
//     max_rad_ord <- path$branch_order[nrow(path)]
//   }
//
//   return(
//     list(
//       x = x,
//       y = y,
//       min_rad = min_rad,
//       max_rad_ord = max_rad_ord,
//       min_gl = min_gl
//     )
//   )
// }

// RCPP IQR CODE
// // [[Rcpp::export]]
// double IQR_rcpp(Rcpp::NumericVector x) {
//   int n = x.size();
//
//   // Calculate indices for the 25th and 75th percentiles
//   int q1_idx = static_cast<int>(0.25 * (n - 1));
//   int q3_idx = static_cast<int>(0.75 * (n - 1));
//
//   // Clone the input vector to avoid modifying the original
//   Rcpp::NumericVector x_copy = Rcpp::clone(x);
//
//   // Find Q1 using nth_element
//   std::nth_element(x_copy.begin(), x_copy.begin() + q1_idx, x_copy.end());
//   double q1 = x_copy[q1_idx];
//
//   // Interpolate Q1 if needed
//   double q1_fraction = 0.25 * (n - 1) - q1_idx;
//   if (q1_fraction > 0 && q1_idx + 1 < n) {
//     std::nth_element(x_copy.begin(), x_copy.begin() + q1_idx + 1, x_copy.end());
//     q1 += q1_fraction * (x_copy[q1_idx + 1] - q1);
//   }
//
//   // Find Q3 using nth_element
//   std::nth_element(x_copy.begin(), x_copy.begin() + q3_idx, x_copy.end());
//   double q3 = x_copy[q3_idx];
//
//   // Interpolate Q3 if needed
//   double q3_fraction = 0.75 * (n - 1) - q3_idx;
//   if (q3_fraction > 0 && q3_idx + 1 < n) {
//     std::nth_element(x_copy.begin(), x_copy.begin() + q3_idx + 1, x_copy.end());
//     q3 += q3_fraction * (x_copy[q3_idx + 1] - q3);
//   }
//
//   // Return the interquartile range
//   return q3 - q1;
// }

