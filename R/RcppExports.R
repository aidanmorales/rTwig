# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

box_counting <- function(cloud, lowercutoff) {
    .Call(`_rTwig_box_counting`, cloud, lowercutoff)
}

assign_cloud_ids <- function(cloud_ref, cloud) {
    .Call(`_rTwig_assign_cloud_ids`, cloud_ref, cloud)
}

#' @title Generate random colors
#'
#' @description Generates random hex colors
#'
#' @param n number of colors to generate as an integer
#' @return returns a character vector of hexidecimal colors
#'
#' @noRd
#'
generate_random_colors <- function(n) {
    .Call(`_rTwig_generate_random_colors`, n)
}

#' @title Hexidecimal to floating point
#'
#' @description Convert hex colors to floating points between 0-1
#'
#' @param hex_colors character string of hex colors
#' @return returns a character vector of floating point colors
#'
#' @noRd
#'
hex_to_float <- function(hex_colors) {
    .Call(`_rTwig_hex_to_float`, hex_colors)
}

#' @title Convex Hull
#'
#' @description Finds the indexes forming a 2d convex hull of a point cloud
#'
#' @param points a point cloud numeric matrix of x and y values
#' @return integer vector of convex hull point indexes
#'
#' @noRd
#'
convex_hull <- function(points) {
    .Call(`_rTwig_convex_hull`, points)
}

#' @title Convex Hull Area
#'
#' @description Finds area of a 2d convex hull with the Shoelace formula
#'
#' @param points a point cloud numeric matrix of x and y values
#' @return a double of the convex hull area in square meters
#'
#' @noRd
#'
convex_hull_area <- function(points) {
    .Call(`_rTwig_convex_hull_area`, points)
}

#' @title IQR Filter
#'
#' @description Filter cylinder outliers
#'
#' @param x NumericVector
#' @param method int
#' @return IntegerVector
#'
#' @noRd
#'
iqr_filter <- function(x, method) {
    .Call(`_rTwig_iqr_filter`, x, method)
}

#' @title Taper Filter
#'
#' @description Taper filter for QSM paths
#'
#' @param radius NumericVector cylinder radius
#' @param taper int taper method
#' @return IntegerVector
#'
#' @noRd
#'
taper_filter <- function(radius, taper) {
    .Call(`_rTwig_taper_filter`, radius, taper)
}

#' @title Broken Branch Filter
#'
#' @description Find broken branches and prepare the paths for modeling.
#' Broken branches have <= 1 child branch in the 1st order branches.
#'
#' @param path a path DataFrame
#' @param twig_radius the twig radius in meters as a double
#' @param broken_branch boolean to enable or disable the filter
#' @return List
#'
#' @noRd
#'
broken_branch_filter <- function(path, twig_radius, broken_branch) {
    .Call(`_rTwig_broken_branch_filter`, path, twig_radius, broken_branch)
}

#' @title Model Matrix
#'
#' @description Generate constraint the matrix for the cobs GAM
#'
#' @param min_growth_length min growth length as a double
#' @param twig_radius the twig radius in meters as a double
#' @return NumericMatrix
#'
#' @noRd
#'
model_matrix <- function(min_growth_length, twig_radius) {
    .Call(`_rTwig_model_matrix`, min_growth_length, twig_radius)
}

#' @title Generate Circle Points
#'
#' @description Generate circular points from n facets
#'
#' @param center cylinder center
#' @param radius cylinder radius
#' @param facets number of cylinder facets
#' @return Numeric Matrix
#'
#' @noRd
#'
generate_circle_points <- function(center, radius, facets) {
    .Call(`_rTwig_generate_circle_points`, center, radius, facets)
}

#' @title Rotate Circle Points
#'
#' @description Rotate circle points along an axis
#'
#' @param points circle points
#' @param start cylinder start point
#' @param axis cylinder axis
#' @return Numeric Matrix
#'
#' @noRd
#'
rotate_circle_points <- function(points, start, axis) {
    .Call(`_rTwig_rotate_circle_points`, points, start, axis)
}

#' @title Generate Mesh
#'
#' @description Generate mesh vertices to visualize cylinder
#'
#' @param start cylinder starts
#' @param axis cylinder axes
#' @param length cylinder length
#' @param radius cylinder radius
#' @param facets number of cylinder facets
#' @return Numeric Matrix
#'
#' @noRd
#'
generate_mesh <- function(start, axis, length, radius, facets) {
    .Call(`_rTwig_generate_mesh`, start, axis, length, radius, facets)
}

define_branches <- function(cylinder) {
    .Call(`_rTwig_define_branches`, cylinder)
}

define_branch_order <- function(cylinder) {
    .Call(`_rTwig_define_branch_order`, cylinder)
}

#' @title Generate Cloud
#'
#' @description Generates a synthetic point cloud from QSM cylinder attributes
#'
#' @param start n x 3 matrix containing the x, y, z cylinder start points
#' @param axis n x 3 matrix containing the x, y, z cylinder axis directions
#' @param tips n x 3 matrix containing the x, y, z cylinder end points
#' @param length vector containing the cylinder lengths
#' @param radius vector containing the cylinder radii
#' @param branch vector containing the cylinder branch ids
#' @return n x 3 point cloud matrix
#'
#' @noRd
#'
generate_cloud <- function(start, axis, tips, length, radius, branch, metrics, spacing) {
    .Call(`_rTwig_generate_cloud`, start, axis, tips, length, radius, branch, metrics, spacing)
}

#' @title Cross Product
#'
#' @description Calculates the cross product of two vectors
#'
#' @param a first vector
#' @param b second vector
#' @return cross product of a and b
#'
#' @noRd
#'
cross_product <- function(a, b) {
    .Call(`_rTwig_cross_product`, a, b)
}

#' @title Norm
#'
#' @description Calculates the norm of a 1 x 3 vector
#'
#' @param x vector x with size 1 x 3
#' @return norm of vector x
#'
#' @noRd
#'
norm <- function(x) {
    .Call(`_rTwig_norm`, x)
}

#' @title Orthonormal Vectors
#'
#' @description Generates vectors V and W that are unit vectors orthogonal to
#' themselves and to the input vector U
#'
#' @param U input vector
#' @return list containing vectors V and W
#'
#' @noRd
#'
orthonormal_vectors <- function(U) {
    .Call(`_rTwig_orthonormal_vectors`, U)
}

#' @title Rotation matrix
#'
#' @description Returns the rotation matrix for the given axis A and angle
#'
#' @param A vector of size 1 x 3
#' @param angle angle in radians
#' @return rotation matrix
#'
#' @noRd
#'
rotation_matrix <- function(A, angle) {
    .Call(`_rTwig_rotation_matrix`, A, angle)
}

#' @title Matrix Vector Subtraction
#'
#' @description Subtract a vector in a matrix
#'
#' @param A matrix of dimensions n x n
#' @param v vector with dimensions 1 x n
#' @return matrix
#'
#' @noRd
#'
mat_vec_subtraction <- function(A, v) {
    .Call(`_rTwig_mat_vec_subtraction`, A, v)
}

#' @title Matrix Multiplication
#'
#' @description Subtract a matrix by a matrix
#'
#' @param A matrix of dimensions n x n
#' @param B vector with dimensions n x n
#' @return matrix
#'
#' @noRd
#'
mat_multiplication <- function(A, B) {
    .Call(`_rTwig_mat_multiplication`, A, B)
}

#' @title Index Order
#'
#' @description Get order of sorted vector indexes
#'
#' @param v vector with dimensions 1 x n
#' @return integer vector
#'
#' @noRd
#'
index_order <- function(x) {
    .Call(`_rTwig_index_order`, x)
}

#' @title Sort Index
#'
#' @description Sort a vector by a specified order
#'
#' @param v vector with dimensions 1 x n
#' @param indexes integer vector with desired ordering
#' @return NumericVector
#'
#' @noRd
#'
sort_index <- function(x, indexes) {
    .Call(`_rTwig_sort_index`, x, indexes)
}

#' @title Which Rcpp
#'
#' @description Find the indices where a condition is true
#'
#' @param condition logical vector
#' @return integer vector
#'
#' @noRd
#'
which_rcpp <- function(condition) {
    .Call(`_rTwig_which_rcpp`, condition)
}

#' @title Calculate Normals
#'
#' @description Calculate normals per cylinder vertex
#'
#' @param vertices NumericMatrix
#' @return NumericMatrix
#'
#' @noRd
#'
calculate_normals <- function(vertices) {
    .Call(`_rTwig_calculate_normals`, vertices)
}

#' @title Write PLY
#'
#' @description Export a QSM cylinder mesh to .ply
#'
#' @param vertices NumericMatrix
#' @param colors NumericMatrix
#' @param normals NumericMatrix
#' @param filename string
#' @return ply
#'
#' @noRd
#'
write_ply <- function(vertices, colors, normals, filename) {
    invisible(.Call(`_rTwig_write_ply`, vertices, colors, normals, filename))
}

#' @title Write OBJ
#'
#' @description Export a QSM cylinder mesh to .obj
#'
#' @param vertices NumericMatrix
#' @param normals NumericMatrix
#' @param filename string
#' @return obj
#'
#' @noRd
#'
write_obj <- function(vertices, normals, filename) {
    invisible(.Call(`_rTwig_write_obj`, vertices, normals, filename))
}

#' @title Write STL
#'
#' @description Export a QSM cylinder mesh to .stl
#'
#' @param vertices NumericMatrix
#' @param normals NumericMatrix
#' @param filename string
#' @return stl
#'
#' @noRd
#'
write_stl <- function(vertices, normals, filename) {
    invisible(.Call(`_rTwig_write_stl`, vertices, normals, filename))
}

#' @title Read OBJ
#'
#' @description Import leaf meshes from QSM-FaNNI
#'
#' @param filename string
#' @param format string
#' @return ply
#'
#' @noRd
#'
read_obj <- function(filename, format) {
    .Call(`_rTwig_read_obj`, filename, format)
}

#' @title Normalize View
#'
#' @description Normalizes RGL plot view
#'
#' @param x NumericVector x coordinate
#' @param y NumericVector y coordinate
#' @param z NumericVector z coordinate
#' @param viewport NumericVector from RGL
#' @return Numeric Matrix
#'
#' @noRd
#'
normalize_view <- function(x, y, z, viewport) {
    .Call(`_rTwig_normalize_view`, x, y, z, viewport)
}

#' @title Solve and Transpose
#'
#' @description Solves and transposes RGL view matrices
#'
#' @param proj NumericMatrix from rgl.projection function
#' @param model NumericMatrix from rgl.projection function
#' @param normalized NumericMatrix from normalize_view function
#' @return Numeric Matrix
#'
#' @noRd
#'
solve_and_transpose <- function(proj, model, normalized) {
    .Call(`_rTwig_solve_and_transpose`, proj, model, normalized)
}

#' @title As Euclidean
#'
#' @description Rcpp port of RGL's asEuclidean function.
#' Converts n x 4 matrix to n x 3 euclidean matrix.
#'
#' @param x NumericMatrix
#' @return Numeric Matrix
#'
#' @noRd
#'
as_euclidean <- function(x) {
    .Call(`_rTwig_as_euclidean`, x)
}

#' @title RGL Window to User
#'
#' @description Rcpp port of RGL's rgl.window2user function
#'
#' @param x NumericVector x coordinate
#' @param y NumericVector y coordinate
#' @param z NumericVector z coordinate
#' @param projection List from RGL rgl.projection function
#' @return Numeric Matrix
#'
#' @noRd
#'
rtwig_window2user <- function(x, y, z, projection) {
    .Call(`_rTwig_rtwig_window2user`, x, y, z, projection)
}

#' @title Translation Matrix
#'
#' @description Generates translation matrix
#'
#' @param x coordinate
#' @param y coordinate
#' @param z coordinate
#'
#' @noRd
#'
translation_matrix <- function(x, y, z) {
    .Call(`_rTwig_translation_matrix`, x, y, z)
}

#' @title User Matrix
#'
#' @description Generates user matrix to update RGL plot view
#'
#' @param x coordinate
#' @param y coordinate
#' @param z coordinate
#' @param start List of starting values
#'
#' @noRd
#'
user_matrix <- function(x, y, z, start) {
    .Call(`_rTwig_user_matrix`, x, y, z, start)
}

#' @title Connect Cylinders
#'
#' @description Connects cylinder endpoints and smooths axes
#'
#' @param branch_position position in the branch
#' @param branch branch id
#' @param start x x start
#' @param start y y start
#' @param start z z start
#' @param axis x x axis
#' @param axis y y axis
#' @param axis z z axis
#' @param end x x end
#' @param end y y end
#' @param end z z end
#' @return n x 3 point cloud matrix
#'
#' @noRd
#'
connect_cylinders <- function(cylinder) {
    invisible(.Call(`_rTwig_connect_cylinders`, cylinder))
}

