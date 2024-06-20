#' Plot QSM
#'
#' @description Plots QSM cylinders and point clouds using the rgl library
#'
#' @param cylinder QSM cylinder data frame
#' @param radius Column name of radii as a quoted string. Defaults to the modified radii.
#' @param color Optional cylinder color parameter. Colors must be a single hex color, a vector of hex colors, or a quoted column name. It can also be set to "random" to generate a random solid color. Vectors must have the same length as the cylinder data frame.
#' @param palette Optional color palette for numerical data. Palettes include: viridis, inferno, plasma, magma, cividis, and rainbow.
#' @param facets The number of facets in the polygon cross section. Defaults to 6, but can be increased to improve visual smoothness at the cost of performance and memory.
#' @param cloud Point cloud data frame where the first three columns are the x, y, and z coordinates in the same coordinate system as the QSM. Defaults to NULL.
#' @param pt_color Color of the point cloud. Defaults to black. Can be set to "random".
#' @param pt_size Size of the points. Defaults to 0.1.
#' @param axes Show plot axes. Defaults to TRUE.
#' @param hover Show cylinder and branch id on mouse hover. Defaults to FALSE.
#' @param skeleton Plot the QSM skeleton instead of cylinders. Defaults to FALSE.
#' @param bg_color Set the background color of the rgl plot. Defaults to NULL.
#'
#' @return A rgl plot
#' @export
#'
#' @examples
#'
#' ## TreeQSM Processing Chain
#' file <- system.file("extdata/QSM.mat", package = "rTwig")
#' qsm <- import_qsm(file)
#' cylinder <- qsm$cylinder
#' cylinder <- update_cylinders(cylinder)
#' plot_qsm(cylinder)
#'
#' ## SimpleForest Processing Chain
#' file <- system.file("extdata/QSM.csv", package = "rTwig")
#' cylinder2 <- read.csv(file)
#' cylinder2 <- update_cylinders(cylinder2)
#' plot_qsm(cylinder2)
#'
#' ## All Parameters
#' file2 <- system.file("extdata/cloud.txt", package = "rTwig")
#' cloud <- read.table(file2, header = FALSE)
#'
#' plot_qsm(
#'   cylinder,
#'   radius = "UnmodRadius",
#'   color = "growthLength",
#'   palette = "viridis",
#'   facets = 100,
#'   cloud = cloud,
#'   pt_color = "random",
#'   pt_size = 1,
#'   axes = FALSE,
#'   hover = TRUE,
#'   bg_color = "black"
#' )
#'
plot_qsm <- function(
    cylinder,
    radius = NULL,
    color = NULL,
    palette = NULL,
    facets = 6,
    cloud = NULL,
    pt_color = NULL,
    pt_size = NULL,
    axes = TRUE,
    hover = FALSE,
    skeleton = FALSE,
    bg_color = NULL) {
  message("Plotting QSM")

  if (nrow(cylinder) == 0) {
    stop("Cylinder data frame empty!")
  }

  # rTwig ----------------------------------------------------------------------
  if (all(c("id", "parent", "start_x", "branch_order") %in% colnames(cylinder))) {
    plot_data(
      cylinder = cylinder, radius = radius, length = "length", id = "id",
      branch = "branch", branch_order = "branch_order",
      start_x = "start_x", start_y = "start_y", start_z = "start_z",
      axis_x = "axis_x", axis_y = "axis_y", axis_z = "axis_z",
      end_x = "end_x", end_y = "end_y", end_z = "end_z",
      facets = facets, color = color, palette = palette, axes = axes,
      hover = hover, skeleton = skeleton, cloud = cloud, pt_color = pt_color,
      pt_size = pt_size, bg_color = bg_color
    )
  }
  # TreeQSM --------------------------------------------------------------------
  else if (all(c("parent", "extension", "branch", "BranchOrder") %in% colnames(cylinder))) {
    plot_data(
      cylinder = cylinder, radius = radius, length = "length", id = "extension",
      branch = "branch", branch_order = "BranchOrder",
      start_x = "start.x", start_y = "start.y", start_z = "start.z",
      axis_x = "axis.x", axis_y = "axis.y", axis_z = "axis.z",
      end_x = "end.x", end_y = "end.y", end_z = "end.z",
      facets = facets, color = color, palette = palette, axes = axes,
      hover = hover, skeleton = skeleton, cloud = cloud, pt_color = pt_color,
      pt_size = pt_size, bg_color = bg_color
    )
  }
  # SimpleForest ---------------------------------------------------------------
  else if (all(c("ID", "parentID", "branchID", "branchOrder") %in% colnames(cylinder))) {
    plot_data(
      cylinder = cylinder, radius = radius, length = "length", id = "ID",
      branch = "branchID", branch_order = "branchOrder",
      start_x = "startX", start_y = "startY", start_z = "startZ",
      axis_x = "axisX", axis_y = "axisY", axis_z = "axisZ",
      end_x = "endX", end_y = "endY", end_z = "endZ",
      facets = facets, color = color, palette = palette, axes = axes,
      hover = hover, skeleton = skeleton, cloud = cloud, pt_color = pt_color,
      pt_size = pt_size, bg_color = bg_color
    )
  }
  # Treegraph ------------------------------------------------------------------
  else if (all(c("p1", "p2", "ninternode") %in% colnames(cylinder))) {
    plot_data(
      cylinder = cylinder, radius = radius, length = "length", id = "p1",
      branch = "nbranch", branch_order = "branch_order",
      start_x = "sx", start_y = "sy", start_z = "sz",
      axis_x = "ax", axis_y = "ay", axis_z = "az",
      end_x = "ex", end_y = "ey", end_z = "ez",
      facets = facets, color = color, palette = palette, axes = axes,
      hover = hover, skeleton = skeleton, cloud = cloud, pt_color = pt_color,
      pt_size = pt_size, bg_color = bg_color
    )
  } else {
    message(
      "Invalid QSM or Cloud Supplied!!!
      \nOnly TreeQSM, SimpleForest, or Treegraph QSMs are supported.
      \nMake sure the cylinder data frame and not the QSM list is supplied.
      \nMake sure the point cloud is a data frame or matrix with the first three columns as the x, y, and z coordinates."
    )
  }
}

#' Plot data
#' @param cylinder QSM cylinder data frame
#' @param radius cylinder radii
#' @param length cylinder length
#' @param id cylinder id
#' @param branch cylinder branch id
#' @param branch_order cylinder branch order
#' @param start_x cylinder start x
#' @param start_y cylinder start y
#' @param start_z cylinder start z
#' @param axis_x cylinder axis x
#' @param axis_y cylinder axis y
#' @param axis_z cylinder axis z
#' @param end_x cylinder end x
#' @param end_y cylinder end y
#' @param end_z cylinder end z
#' @param facets cylinder facets
#' @param color cylinder color
#' @param palette color palette
#' @param axes axes logical
#' @param hover hover logical
#' @param skeleton skeleton logical
#' @param cloud point cloud data frame or matrix
#' @param pt_color point cloud color
#' @param pt_size point cloud point size
#' @param bg_color rgl plot background color
#' @returns an rgl plot
#' @noRd
plot_data <- function(
    cylinder,
    radius,
    length,
    id,
    branch,
    branch_order,
    start_x,
    start_y,
    start_z,
    axis_x,
    axis_y,
    axis_z,
    end_x,
    end_y,
    end_z,
    facets,
    color,
    palette,
    axes,
    hover,
    skeleton,
    cloud,
    pt_color,
    pt_size,
    bg_color) {
  # Plotting radii -------------------------------------------------------------
  radius <- plotting_radii(cylinder, radius)

  # Plotting colors ------------------------------------------------------------
  colors <- plotting_colors(cylinder, color, palette, branch_order)

  # Plot skeleton --------------------------------------------------------------
  if (skeleton == TRUE) {
    plot_skeleton(
      cylinder, colors, axes,
      start_x, start_y, start_z, end_x, end_y, end_z
    )
  }
  # Plot cylinders -------------------------------------------------------------
  else {
    plot_cylinders(
      cylinder, radius, length, start_x, start_y, start_z,
      axis_x, axis_y, axis_z, facets, colors, axes
    )
  }

  # Plot cloud -----------------------------------------------------------------
  if (!is.null(cloud)) {
    plot_cloud(cloud, pt_size, pt_color)
  }

  # Mouse hover ----------------------------------------------------------------
  if (hover == TRUE) {
    hover(cylinder, id, branch, start_x, start_y, start_z)
  }

  # Background color -----------------------------------------------------------
  if (!is.null(bg_color)) {
    background_color(bg_color)
  }
}


#' Cylinder plotting radii
#' @param cylinder QSM cylinder data frame
#' @param radius cylinder radii column name
#' @returns a vector of cylinder radii
#' @noRd
plotting_radii <- function(cylinder, radius) {
  # Initialize cylinder radii
  if (is.null(radius)) {
    radius <- cylinder$radius
  } else {
    radius <- pull(cylinder, !!rlang::sym(radius))
  }

  return(radius)
}

#' Cylinder plotting colors
#' @param cylinder QSM cylinder data frame
#' @param radius cylinder radii column name
#' @param palette color palette
#' @param branch_order cylinder branch order column name
#' @returns a vector of hex colors
#' @noRd
plotting_colors <- function(cylinder, color, palette, branch_order) {
  if (is.null(color)) {
    default_color <- colourvalues::color_values(
      pull(cylinder, !!rlang::sym(branch_order)),
      palette = "rainbow"
    )
  } else if (color == "random") {
    color <- generate_random_colors(1)
  } else if (is.vector(color) & length(color) > 1) {
    if (length(color) != nrow(cylinder)) {
      stop("Supplied cylinder colors vector is not equal to the number of cylinders!")
    }
  } else if (is.vector(color) & length(color) == 1 & !(color %in% colnames(cylinder))) {
    err_test <- try(grDevices::col2rgb(color), silent = TRUE)

    if (is.matrix(err_test)) {
      color <- color
    } else {
      stop("Invalid color or column name supplied!")
    }
  } else if (color %in% colnames(cylinder)) {
    color <- pull(cylinder, !!rlang::sym(color))
  } else {
    stop("Invalid color or column name supplied!")
  }

  if (length(color) == 1) {
    plot_colors <- rep(color, nrow(cylinder))
  } else if (is.null(palette) & length(color) > 1 & !is.character(color)) {
    plot_colors <- colourvalues::color_values(color, palette = "rainbow")
  } else if (!is.null(palette) & length(color) > 1 & !is.character(color)) {
    plot_colors <- colourvalues::color_values(color, palette = palette)
  } else if (length(color) > 1 & is.character(color)) {
    plot_colors <- color
  } else {
    plot_colors <- default_color
  }

  return(plot_colors)
}

#' Plot skeleton
#' @param cylinder QSM cylinder data frame
#' @param colors cylinder colors
#' @param axes logical for plotting axes
#' @param start_x column name of start_x
#' @param start_y column name of start_y
#' @param start_z column name of start_z
#' @param end_x column name of end_x
#' @param end_y column name of end_y
#' @param end_z column name of end_z
#' @param branch_order cylinder branch order column name
#' @returns NA
#' @noRd
plot_skeleton <- function(
    cylinder,
    colors,
    axes,
    start_x,
    start_y,
    start_z,
    end_x,
    end_y,
    end_z) {
  message("Plotting Skeleton")

  # Plot Skeleton
  rgl::segments3d(
    x = as.vector(t(select(cylinder, all_of(c(start_x, end_x))))),
    y = as.vector(t(select(cylinder, all_of(c(start_y, end_y))))),
    z = as.vector(t(select(cylinder, all_of(c(start_z, end_z))))),
    col = rep(colors, each = 2)
  )

  # Plot axes
  if (axes == TRUE) {
    rgl::axes3d(edges = c("x", "y", "z"))
  }
}

#' Plot cylinders
#' @param cylinder QSM cylinder data frame
#' @param radius cylinder radii
#' @param length cylinder length
#' @param start_x column name of start_x
#' @param start_y column name of start_y
#' @param start_z column name of start_z
#' @param axis_x column name of axis_x
#' @param axis_y column name of axis_y
#' @param axis_z column name of axis_z
#' @param facets cylinder facets
#' @param colors cylinder colors
#' @param axes logical for plotting axes
#' @returns NA
#' @noRd
plot_cylinders <- function(
    cylinder,
    radius,
    length,
    start_x,
    start_y,
    start_z,
    axis_x,
    axis_y,
    axis_z,
    facets,
    colors,
    axes) {
  message("Plotting Cylinders")

  # Extract required variables
  start <- cbind(
    pull(cylinder, !!rlang::sym(start_x)),
    pull(cylinder, !!rlang::sym(start_y)),
    pull(cylinder, !!rlang::sym(start_z))
  )

  axis <- cbind(
    pull(cylinder, !!rlang::sym(axis_x)),
    pull(cylinder, !!rlang::sym(axis_y)),
    pull(cylinder, !!rlang::sym(axis_z))
  )

  length <- pull(cylinder, !!rlang::sym(length))

  # Create cylinder mesh
  vertices <- generate_mesh(start, axis, length, radius, facets)
  colors <- rep(colors, each = facets * 6)

  # Plot cylinders
  rgl::triangles3d(vertices, col = colors)

  # Plot axes
  if (axes == TRUE) {
    rgl::axes3d(edges = c("x", "y", "z"))
  }
}

#' Plot cloud
#' @param cloud point cloud data frame or matrix
#' @param pt_size point size
#' @param pt_color point cloud color
#' @returns NA
#' @noRd
plot_cloud <- function(cloud, pt_size, pt_color) {
  message("Plotting Cloud")

  if (is.matrix(cloud)) {
    cloud <- as_tidytable(cloud)
  }

  cloud <- rename(cloud, x = 1, y = 2, z = 3)

  # Initialize cloud inputs
  if (is.null(pt_color)) {
    pt_color <- "#000000"
  } else if (pt_color == "random") {
    pt_color <- generate_random_colors(1)
  } else {
    pt_color <- pt_color
  }

  if (is.null(pt_size)) {
    pt_size <- 0.1
  } else {
    pt_size <- pt_size
  }

  # Plot cloud
  rgl::plot3d(
    x = cloud$x,
    y = cloud$y,
    z = cloud$z,
    col = pt_color,
    size = pt_size,
    add = TRUE,
    aspect = FALSE
  )
}

#' Plot hover
#' @param cylinder QSM cylinder data frame
#' @param id cylinder id
#' @param branch cylinder branch id
#' @param start_x column name of start_x
#' @param start_y column name of start_y
#' @param start_z column name of start_z
#' @returns NA
#' @noRd
hover <- function(cylinder, id, branch, start_x, start_y, start_z) {
  rgl::hover3d(
    x = pull(cylinder, !!rlang::sym(start_x)),
    y = pull(cylinder, !!rlang::sym(start_y)),
    z = pull(cylinder, !!rlang::sym(start_z)),
    labels = paste0(
      "ID:",
      pull(cylinder, !!rlang::sym(id)),
      " - Branch:",
      pull(cylinder, !!rlang::sym(branch))
    )
  )
}

#' Background color
#' @param bg_color background color
#' @returns NA
#' @noRd
background_color <- function(bg_color) {
  rgl::bg3d(bg_color)
}
