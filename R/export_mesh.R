#' Export Mesh
#'
#' @description Exports QSM cylinder mesh using the rgl library
#'
#' @param cylinder QSM cylinder data frame
#' @param filename File name and path for exporting. The .ply extension is automatically added if not present.
#' @param radius Vector of cylinder radii. Defaults to modified cylinders from the cylinder data frame.
#' @param color Optional cylinder color parameter. Colors must be a single hex color, a vector of hex colors, or a quoted column name. It can also be set to "random" to generate a random solid color, or FALSE to disable color on export. Vectors must have the same length as the cylinder data frame.
#' @param palette Optional color palette for numerical data. Palettes include: viridis, inferno, plasma, magma, cividis, and rainbow.
#' @param facets The number of facets in the polygon cross section. Defaults to 6, but can be increased to improve visual smoothness at the cost of performance and memory.
#' @param normals Option to export normals. Defaults to FALSE, but can be set to TRUE.
#'
#' @return A mesh .ply file
#' @export
#'
#' @examples
#'
#' ## TreeQSM Processing Chain
#' file <- system.file("extdata/QSM.mat", package = "rTwig")
#' qsm <- import_qsm(file)
#' cylinder <- qsm$cylinder
#' cylinder <- update_cylinders(cylinder)
#'
#' filename <- tempfile(pattern = "QSM_mesh")
#' export_mesh(cylinder, filename)
#'
#' ## SimpleForest Processing Chain
#' file <- system.file("extdata/QSM.csv", package = "rTwig")
#' cylinder2 <- read.csv(file)
#' cylinder2 <- update_cylinders(cylinder2)
#'
#' filename2 <- tempfile(pattern = "QSM_mesh2")
#' export_mesh(cylinder2, filename2)
#'
#' ## All Parameters
#'
#' filename3 <- tempfile(pattern = "QSM_mesh3")
#' export_mesh(
#'   cylinder = cylinder,
#'   filename = filename3,
#'   radius = "UnmodRadius",
#'   color = "growthLength",
#'   palette = "viridis"
#' )
#'
export_mesh <- function(
    cylinder,
    filename,
    radius = NULL,
    color = NULL,
    palette = NULL,
    facets = 6,
    normals = FALSE) {
  # Ensure filename ends in .ply
  if (substr(filename, nchar(filename) - 3, nchar(filename)) != ".ply") {
    filename <- paste0(filename, ".ply")
  }

  # rTwig ----------------------------------------------------------------------
  if (all(c("id", "parent", "start_x", "branch_order") %in% colnames(cylinder))) {
    plot_mesh(
      filename = filename, cylinder = cylinder, radius = radius,
      length = "length", branch_order = "branch_order",
      start_x = "start_x", start_y = "start_y", start_z = "start_z",
      axis_x = "axis_x", axis_y = "axis_y", axis_z = "axis_z",
      facets = facets, color = color, palette = palette, normals = normals
    )
  }
  # TreeQSM --------------------------------------------------------------------
  else if (all(c("parent", "extension", "branch", "BranchOrder") %in% colnames(cylinder))) {
    plot_mesh(
      filename = filename, cylinder = cylinder, radius = radius,
      length = "length", branch_order = "BranchOrder",
      start_x = "start.x", start_y = "start.y", start_z = "start.z",
      axis_x = "axis.x", axis_y = "axis.y", axis_z = "axis.z",
      facets = facets, color = color, palette = palette, normals = normals
    )
  }
  # SimpleForest ---------------------------------------------------------------
  else if (all(c("ID", "parentID", "branchID", "branchOrder") %in% colnames(cylinder))) {
    plot_mesh(
      filename = filename, cylinder = cylinder, radius = radius,
      length = "length", branch_order = "branchOrder",
      start_x = "startX", start_y = "startY", start_z = "startZ",
      axis_x = "axisX", axis_y = "axisY", axis_z = "axisZ",
      facets = facets, color = color, palette = palette, normals = normals
    )
  }
  # Treegraph ------------------------------------------------------------------
  else if (all(c("p1", "p2", "ninternode") %in% colnames(cylinder))) {
    plot_mesh(
      filename = filename, cylinder = cylinder, radius = radius,
      length = "length", branch_order = "branch_order",
      start_x = "sx", start_y = "sy", start_z = "sz",
      axis_x = "ax", axis_y = "ay", axis_z = "az",
      facets = facets, color = color, palette = palette, normals = normals
    )
  } else {
    message(
      "Invalid QSM Supplied!!!
      \nOnly TreeQSM, SimpleForest, or Treegraph QSMs are supported.
      \nMake sure the cylinder data frame and not the QSM list is supplied.
      \nMake sure the point cloud is a data frame with the first three columns as the x, y, and z coordinates."
    )
  }
}

#' Plot mesh
#' @param filename name of file to export
#' @param cylinder QSM cylinder data frame
#' @param radius cylinder radii
#' @param length cylinder length
#' @param branch_order cylinder branch order
#' @param start_x cylinder start x
#' @param start_y cylinder start y
#' @param start_z cylinder start z
#' @param axis_x cylinder axis x
#' @param axis_y cylinder axis y
#' @param axis_z cylinder axis z
#' @param facets cylinder facets
#' @param color cylinder color
#' @param palette color palette
#' @param normals normals logical
#' @returns a .ply file
#' @noRd
plot_mesh <- function(
    filename,
    cylinder,
    radius,
    length,
    branch_order,
    start_x,
    start_y,
    start_z,
    axis_x,
    axis_y,
    axis_z,
    facets,
    color,
    palette,
    normals) {
  message("Exporting Mesh")

  # Plotting radii -------------------------------------------------------------
  radius <- plotting_radii(cylinder, radius)

  # Plotting colors ------------------------------------------------------------
  if (!is.null(color) && color == FALSE) {
    colors <- NA
    c_check <- FALSE
  } else {
    colors <- plotting_colors(cylinder, color, palette, branch_order)
    c_check <- TRUE
  }

  # Open connection ------------------------------------------------------------
  rgl::open3d()

  # Plot cylinders -------------------------------------------------------------
  suppressMessages(
    plot_cylinders(
      cylinder, radius, length, start_x, start_y, start_z,
      axis_x, axis_y, axis_z, facets, colors,
      axes = FALSE
    )
  )

  # Export mesh ----------------------------------------------------------------
  rgl::writePLY(filename, withNormals = normals, withColors = c_check)

  # Close connection -----------------------------------------------------------
  rgl::close3d()
}
