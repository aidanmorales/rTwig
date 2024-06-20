#include <Rcpp.h>
#include "helper_functions.h"
using namespace Rcpp;

//' @title Generate Circle Points
//'
//' @description Generate circular points from n facets
//'
//' @param center cylinder center
//' @param radius cylinder radius
//' @param facets number of cylinder facets
//' @return Numeric Matrix
//'
//' @noRd
//'
// [[Rcpp::export]]
NumericMatrix generate_circle_points(
    NumericVector center,
    double radius,
    int facets){

  NumericVector theta(facets + 1);

  double start = 0.0;
  double end = 2 * M_PI;
  double step = (end - start) / facets;

  for (int i = 0; i < theta.size(); ++i) {
    theta[i] = start + i * step;
  }

  NumericMatrix points(facets + 1, 3);

  points(_, 0) = radius * cos(theta);
  points(_, 1) = radius * sin(theta);
  points(_, 2) = rep(0, facets + 1);

  NumericMatrix ptcenter(facets + 1, 3);

  for(int i = 0; i < ptcenter.nrow(); i++){
    ptcenter(i, _) = center;
  }

  for(int i = 0; i < points.nrow(); i++){
    for(int j = 0; j < points.ncol(); j++){
      points(i, j) = points(i, j) + ptcenter(i, j);
    }
  }

  return(points);
}

//' @title Rotate Circle Points
//'
//' @description Rotate circle points along an axis
//'
//' @param points circle points
//' @param start cylinder start point
//' @param axis cylinder axis
//' @return Numeric Matrix
//'
//' @noRd
//'
// [[Rcpp::export]]
NumericMatrix rotate_circle_points(
    NumericMatrix points,
    NumericVector start,
    NumericVector axis){

  // Calculate Rotation Matrix
  NumericVector start_z  = {0, 0, 1};
  NumericVector rot_axis = cross_product(start_z, axis);
  double rot_angle = acos(sum(start_z * axis));
  NumericMatrix rot_matrix = transpose(rotation_matrix(rot_axis, rot_angle));

  // Rotate Points & Translate Points
  NumericMatrix result = mat_vec_subtraction(mat_multiplication(points, rot_matrix), -start);

  return result;
}

//' @title Generate Mesh
//'
//' @description Generate mesh vertices to visualize cylinder
//'
//' @param start cylinder starts
//' @param axis cylinder axes
//' @param length cylinder length
//' @param radius cylinder radius
//' @param facets number of cylinder facets
//' @return Numeric Matrix
//'
//' @noRd
//'
// [[Rcpp::export]]
NumericMatrix generate_mesh(
    NumericMatrix start,
    NumericMatrix axis,
    NumericVector length,
    NumericVector radius,
    int facets){

  // Pre-allocate the matrix of all cylinder vertices
  // There are two triangles per facet
  // There are three vertices per triangle
  // There are n cylinders
  // Total vertices = n * facets * 2 * 3
  int n = start.nrow();
  int v_size = facets * 6;
  NumericMatrix vertices(v_size * n, 3);

  for (int i = 0; i < n; i++){

    // Initial cylinder base and top
    NumericVector base_center = {0, 0, 0};
    NumericVector top_center = {0, 0, length[i]};

    // Initial cylinder base points
    NumericMatrix base_points = generate_circle_points(
      base_center,
      radius[i],
      facets
    );

    // Initial cylinder top points
    NumericMatrix top_points = generate_circle_points(
      top_center,
      radius[i],
      facets
    );

    // Rotate base and top points along the cylinder's axis
    base_points = rotate_circle_points(base_points, start(i, _), axis(i, _));
    top_points = rotate_circle_points(top_points, start(i, _), axis(i, _));

    // Pre-allocate the matrix of individual cylinder vertices
    // There are two triangles per facet
    // There are three vertices per triangle
    // Total vertices = facets * 2 * 3
    NumericMatrix cyl_vert(v_size, 3);

    // Create the facets using cyl_vert
    for (int j = 0; j < facets; j++) {
      int index = j * 6;

      cyl_vert(index, _) = base_points(j, _);
      cyl_vert(index + 1, _) = top_points(j, _);
      cyl_vert(index + 2, _) = top_points(j + 1, _);
      cyl_vert(index + 3, _) = base_points(j, _);
      cyl_vert(index + 4, _) = top_points(j + 1, _);
      cyl_vert(index + 5, _) = base_points(j + 1, _);
    }

    // Append individual cylinder vertices to all cylinder vertices
    int index = i * v_size;

    for (int j = 0; j < v_size; j++){
      vertices(index + j, _) = cyl_vert(j, _);
    }
  }

  return(vertices);
}
