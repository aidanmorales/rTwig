#include <Rcpp.h>
#include <algorithm>
#include <vector>
#include "helper_functions.h"
using namespace Rcpp;

// Structure to represent a point in 2D space
struct Point {
  double x, y;
  int index; // To store the original index of the point
};

// Function to compare two points by x coordinate, and by y coordinate if x is the same
bool compare(Point a, Point b) {
  return (a.x < b.x) || (a.x == b.x && a.y < b.y);
}

// Function to check if three points make a counter-clockwise turn
double cross(const Point &O, const Point &A, const Point &B) {
  return (A.x - O.x) * (B.y - O.y) - (A.y - O.y) * (B.x - O.x);
}

//' @title Convex Hull
//'
//' @description Finds the indexes forming a 2d convex hull of a point cloud
//'
//' @param points a point cloud numeric matrix of x and y values
//' @return integer vector of convex hull point indexes
//'
//' @noRd
//'
// [[Rcpp::export]]
IntegerVector convex_hull(NumericMatrix points) {
  int n = points.nrow();

  // Create an array of points
  std::vector<Point> pts(n);
  for (int i = 0; i < n; ++i) {
    pts[i].x = points(i, 0);
    pts[i].y = points(i, 1);
    pts[i].index = i; // Store original index
  }

  // Sort points lexicographically
  std::sort(pts.begin(), pts.end(), compare);

  // Build the lower hull
  std::vector<Point> lower;
  for (int i = 0; i < n; ++i) {
    while (lower.size() >= 2 && cross(lower[lower.size() - 2], lower.back(), pts[i]) <= 0) {
      lower.pop_back();
    }
    lower.push_back(pts[i]);
  }

  // Build the upper hull
  std::vector<Point> upper;
  for (int i = n - 1; i >= 0; --i) {
    while (upper.size() >= 2 && cross(upper[upper.size() - 2], upper.back(), pts[i]) <= 0) {
      upper.pop_back();
    }
    upper.push_back(pts[i]);
  }

  // Remove the last point of each half because it is repeated at the beginning of the other half
  lower.pop_back();
  upper.pop_back();

  // Concatenate lower and upper hull
  lower.insert(lower.end(), upper.begin(), upper.end());

  // Extract indices of the hull points
  IntegerVector hull_indices(lower.size() + 1); // +1 to close the loop
  for (size_t i = 0; i < lower.size(); ++i) {
    hull_indices[i] = lower[i].index;
  }
  hull_indices[lower.size()] = lower[0].index; // Close the loop

  return hull_indices;
}



//' @title Convex Hull Area
//'
//' @description Finds area of a 2d convex hull with the Shoelace formula
//'
//' @param points a point cloud numeric matrix of x and y values
//' @return a double of the convex hull area in square meters
//'
//' @noRd
//'
// [[Rcpp::export]]
double convex_hull_area(NumericMatrix points) {
  int n = points.nrow();

  // Create an array of points
  std::vector<Point> pts(n);
  for (int i = 0; i < n; ++i) {
    pts[i].x = points(i, 0);
    pts[i].y = points(i, 1);
    pts[i].index = i;
  }

  // Get the convex hull indices
  IntegerVector hull_indices = convex_hull(points);

  // Create a matrix to store convex hull points
  NumericMatrix hull_points(hull_indices.size(), 2);
  for (int i = 0; i < hull_indices.size(); ++i) {
    hull_points(i, 0) = points(hull_indices[i], 0);
    hull_points(i, 1) = points(hull_indices[i], 1);
  }

  // Apply the Shoelace formula to calculate the area
  double area = 0.0;
  for (int i = 0; i < hull_points.nrow() - 1; ++i) {
    area += hull_points(i, 0) * hull_points(i + 1, 1) - hull_points(i, 1) * hull_points(i + 1, 0);
  }
  area += hull_points(hull_points.nrow() - 1, 0) * hull_points(0, 1) - hull_points(hull_points.nrow() - 1, 1) * hull_points(0, 0);
  area = std::abs(area) * 0.5;

  return area;
}
