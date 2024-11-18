#' Export Mesh
#'
#' @description Exports QSM cylinder mesh using the rgl library
#'
#' @param cylinder QSM cylinder data frame
#' @param filename File name and path for exporting. The .ply extension is automatically added if not present.
#' @param radius Radius column name either quoted or unquoted. Defaults to modified cylinders from the cylinder data frame.
#' @param color Optional cylinder color parameter. Colors must be a single hex color string, a `grDevices::colors()`, a vector of hex colors, or a quoted/unquoted column name. It can also be set to "random" to generate a random solid color, or FALSE to disable color on export. Vectors must have the same length as the cylinder data frame.
#' @param palette Optional color palette for numerical data. Palettes include `colourvalues::color_palettes()` or a user supplied RGB palette matrix with the length of cylinder.
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
#' filename <- tempfile(pattern = "TreeQSM_mesh")
#' export_mesh(cylinder, filename)
#'
#' ## SimpleForest Processing Chain
#' file <- system.file("extdata/QSM.csv", package = "rTwig")
#' cylinder <- read.csv(file)
#' cylinder <- update_cylinders(cylinder)
#'
#' filename <- tempfile(pattern = "SimpleForest_mesh")
#' export_mesh(cylinder, filename)
#'
export_mesh <- function(
    cylinder,
    filename,
    radius = NULL,
    color = NULL,
    palette = NULL,
    facets = 6,
    normals = FALSE) {
  # Check inputs ---------------------------------------------------------------
  if (is_missing(cylinder)) {
    message <- "argument `cylinder` is missing, with no default."
    abort(message, class = "missing_argument")
  }

  if (!is.data.frame(cylinder)) {
    message <- paste(
      paste0("`cylinder` must be a data frame, not ", class(cylinder), "."),
      "i Did you accidentally pass the QSM list instead of the cylinder data frame?",
      sep = "\n"
    )
    abort(message, class = "data_format_error")
  }

  if (is_missing(filename)) {
    message <- "argument `filename` is missing, with no default."
    abort(message, class = "missing_argument")
  }

  if (!is_string(filename)) {
    message <- paste0(
      "`filename` must be a string, not ", class(filename), "."
    )
    abort(message, class = "invalid_argument")
  }

  # Ensure filename ends with correct extension
  if (substr(filename, nchar(filename) - 3, nchar(filename)) != ".ply") {
    filename <- paste0(filename, ".ply")
  }

  # User selected columns
  radius <- select_column(rlang::enquo(radius))
  color <- select_column(rlang::enquo(color))

  if (!is_null(radius) & !any(radius %in% colnames(cylinder))) {
    abort(paste(
      "Can't select columns that don't exist.",
      paste0("X Column `", radius, "' doesn't exist."),
      "i Did you mistype your `radius` column name?`.",
      sep = "\n"
    ))
  }

  if (!is_null(color)) {
    if (color %in% grDevices::colors()) {
      message <- paste(
        "Hex colors (e.g. `#FF0000`) are preferred for plotting solid colors.",
        "Colors from `grDevices::colors()` (e.g. `red`) slow down plotting",
        "because 'grDevices::col2rgb()` must convert the color string into an RGB",
        "matrix for every cylinder times the number of facets per cylinder."
      )
      warn(message)
    }
  }

  if (!is_integerish(facets)) {
    message <- paste0(
      "`facets` must be an integer, not ", class(facets), "."
    )
    abort(message, class = "invalid_argument")
  }

  if (facets > 50) {
    warn("A large number of `facets` can quickly degrade plot performance.")
  }

  if (!is_logical(normals)) {
    message <- paste0(
      "`normals` must be a logical, not ", class(normals), "."
    )
    abort(message, class = "invalid_argument")
  }

  # Verify cylinders
  cylinder <- verify_cylinders(cylinder)

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
  }
  # aRchi ----------------------------------------------------------------------
  else if (all(c("cyl_ID", "parent_ID", "branching_order") %in% colnames(cylinder))) {
    plot_mesh(
      filename = filename, cylinder = cylinder, radius = radius,
      length = "length", branch_order = "branching_order",
      start_x = "startX", start_y = "startY", start_z = "startZ",
      axis_x = "axisX", axis_y = "axisY", axis_z = "axisZ",
      facets = facets, color = color, palette = palette, normals = normals
    )
  } else {
    message <- paste(
      "Unsupported QSM format provided.",
      "i Only TreeQSM, SimpleForest, Treegraph, or aRchi QSMs are supported.",
      sep = "\n"
    )
    abort(message, class = "data_format_error")
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
  inform("Exporting Mesh")

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
      lit = TRUE, alpha = 1
    )
  )

  # Export mesh ----------------------------------------------------------------
  rgl::writePLY(filename, withNormals = normals, withColors = c_check)

  # Close connection -----------------------------------------------------------
  rgl::close3d()
}
