#' Plot QSM
#'
#' @description Efficiently plot QSMs and point clouds. Uses the Rcpp and RGL libraries as backends.
#'
#' @param cylinder A QSM cylinder data frame.
#' @param radius Column name of radii as a quoted string. Defaults to the modified radii.
#' @param color Optional cylinder color parameter. Colors must be a single hex color, a vector of hex colors, or a quoted column name. It can also be set to "random" to generate a random solid color. Vectors must have the same length as the cylinder data frame.
#' @param palette Optional color palette for numerical data. Palettes include colourvalues::color_palettes() or any user supplied palette.
#' @param alpha Set the transparency of the cylinders. Defaults to 1. 1 is opaque and 0 is fully transparent.
#' @param facets The number of facets in the polygon cross section. Defaults to 6, but can be increased to improve visual smoothness at the cost of performance and memory.
#' @param skeleton Plot the QSM skeleton instead of cylinders. Defaults to FALSE.
#' @param skeleton_lwd Skeleton line width. Defaults to 1.
#' @param cloud Point cloud data frame where the first three columns are the x, y, and z coordinates in the same coordinate system as the QSM. Defaults to NULL.
#' @param pt_color Color of the point cloud. Defaults to black. Can be set to "random".
#' @param pt_size Size of the points. Defaults to 0.1.
#' @param triangulation Plot the stem triangulation mesh from TreeQSM. Defaults to NULL.
#' @param tri_color Color of the triangulation mesh. Colors must be a single hex color.
#' @param tri_palette Optional triangulation color palette for z values. Supports the same inputs as palettes.
#' @param axes Show plot axes. Defaults to TRUE.
#' @param axes_color Set the axes color. Defaults to black.
#' @param grid Show plot grid lines. Defaults to FALSE.
#' @param grid_color Set grid lines color. Defaults to grey.
#' @param hover Show cylinder and branch id on mouse hover. Defaults to FALSE.
#' @param bg_color Set the background color of the plot. Defaults to white.
#' @param lit Enable light source in plot. Defaults to TRUE. Can be set to FALSE.
#' @param pan Use right mouse button to pan plot. Defaults to TRUE, but is disabled when hover is enabled.
#' @param normalize Normalize the QSM to 0,0,0 based on the provided data. Defaults to FALSE.
#'
#' @return A rgl plot
#' @export
#'
#' @examples
#'
#' ## TreeQSM Processing Chain & Triangulation
#' file <- system.file("extdata/QSM.mat", package = "rTwig")
#' qsm <- import_qsm(file)
#' cylinder <- qsm$cylinder
#' cylinder <- update_cylinders(cylinder)
#' plot_qsm(cylinder)
#'
#' triangulation <- qsm$triangulation
#' plot_qsm(triangulation = triangulation)
#'
#' ## SimpleForest Processing Chain
#' file <- system.file("extdata/QSM.csv", package = "rTwig")
#' cylinder <- read.csv(file)
#' cylinder <- update_cylinders(cylinder)
#' plot_qsm(cylinder)
#'
#' ## aRchi Processing Chain
#' file <- system.file("extdata/QSM2.csv", package = "rTwig")
#' cylinder <- read.csv(file)
#' cylinder <- update_cylinders(cylinder)
#' plot_qsm(cylinder)
#'
plot_qsm <- function(
    cylinder = NULL,
    radius = NULL,
    color = NULL,
    palette = NULL,
    alpha = 1,
    facets = 6,
    skeleton = FALSE,
    skeleton_lwd = NULL,
    cloud = NULL,
    pt_color = NULL,
    pt_size = NULL,
    triangulation = NULL,
    tri_color = NULL,
    tri_palette = NULL,
    axes = TRUE,
    axes_color = NULL,
    grid = FALSE,
    grid_color = NULL,
    hover = FALSE,
    bg_color = NULL,
    lit = TRUE,
    pan = TRUE,
    normalize = FALSE) {
  if (!is.null(cylinder)) {
    if (nrow(cylinder) == 0) {
      stop("Cylinder data frame empty!")
    }
  }

  # User selected columns
  radius <- select_column(rlang::enquo(radius))
  color <- select_column(rlang::enquo(color))

  # rTwig ----------------------------------------------------------------------
  if (all(c("id", "parent", "start_x", "branch_order") %in% colnames(cylinder))) {
    plot_data(
      cylinder = cylinder, radius = radius, length = "length", id = "id",
      branch = "branch", branch_order = "branch_order",
      start_x = "start_x", start_y = "start_y", start_z = "start_z",
      axis_x = "axis_x", axis_y = "axis_y", axis_z = "axis_z",
      end_x = "end_x", end_y = "end_y", end_z = "end_z",
      facets = facets, skeleton = skeleton, skeleton_lwd = skeleton_lwd,
      color = color, palette = palette, alpha = alpha,
      axes = axes, axes_color = axes_color,
      grid = grid, grid_color = grid_color, hover = hover,
      cloud = cloud, pt_color = pt_color, pt_size = pt_size,
      bg_color = bg_color, lit = lit, pan = pan, normalize = normalize,
      triangulation = triangulation, tri_color = tri_color,
      tri_palette = tri_palette
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
      facets = facets, skeleton = skeleton, skeleton_lwd = skeleton_lwd,
      color = color, palette = palette, alpha = alpha,
      axes = axes, axes_color = axes_color,
      grid = grid, grid_color = grid_color, hover = hover,
      cloud = cloud, pt_color = pt_color, pt_size = pt_size,
      bg_color = bg_color, lit = lit, pan = pan, normalize = normalize,
      triangulation = triangulation, tri_color = tri_color,
      tri_palette = tri_palette
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
      facets = facets, skeleton = skeleton, skeleton_lwd = skeleton_lwd,
      color = color, palette = palette, alpha = alpha,
      axes = axes, axes_color = axes_color,
      grid = grid, grid_color = grid_color, hover = hover,
      cloud = cloud, pt_color = pt_color, pt_size = pt_size,
      bg_color = bg_color, lit = lit, pan = pan, normalize = normalize,
      triangulation = triangulation, tri_color = tri_color,
      tri_palette = tri_palette
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
      facets = facets, skeleton = skeleton, skeleton_lwd = skeleton_lwd,
      color = color, palette = palette, alpha = alpha,
      axes = axes, axes_color = axes_color,
      grid = grid, grid_color = grid_color, hover = hover,
      cloud = cloud, pt_color = pt_color, pt_size = pt_size,
      bg_color = bg_color, lit = lit, pan = pan, normalize = normalize,
      triangulation = triangulation, tri_color = tri_color,
      tri_palette = tri_palette
    )
  }
  # aRchi ----------------------------------------------------------------------
  else if (all(c("cyl_ID", "parent_ID", "branching_order") %in% colnames(cylinder))) {
    plot_data(
      cylinder = cylinder, radius = radius, length = "length", id = "cyl_ID",
      branch = "branch_ID", branch_order = "branching_order",
      start_x = "startX", start_y = "startY", start_z = "startZ",
      axis_x = "axisX", axis_y = "axisY", axis_z = "axisZ",
      end_x = "endX", end_y = "endY", end_z = "endZ",
      facets = facets, skeleton = skeleton, skeleton_lwd = skeleton_lwd,
      color = color, palette = palette, alpha = alpha,
      axes = axes, axes_color = axes_color,
      grid = grid, grid_color = grid_color, hover = hover,
      cloud = cloud, pt_color = pt_color, pt_size = pt_size,
      bg_color = bg_color, lit = lit, pan = pan, normalize = normalize,
      triangulation = triangulation, tri_color = tri_color,
      tri_palette = tri_palette
    )
  }
  # Point Cloud & Triangulation ------------------------------------------------
  else if (any(!is.null(cloud) | !is.null(triangulation))) {
    plot_data(
      cylinder = cylinder,
      axes = axes, axes_color = axes_color,
      grid = grid, grid_color = grid_color, hover = hover,
      cloud = cloud, pt_color = pt_color, pt_size = pt_size,
      bg_color = bg_color, lit = lit, pan = pan, normalize = normalize,
      triangulation = triangulation, tri_color = tri_color,
      tri_palette = tri_palette
    )
  } else {
    message(
      "Invalid QSM or Cloud Supplied!!!
      \nOnly TreeQSM, SimpleForest, Treegraph, or aRchi QSMs are supported.
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
#' @param skeleton skeleton logical
#' @param skeleton_lwd skeleton line width
#' @param color cylinder color
#' @param palette color palette
#' @param alpha cylinder transparency
#' @param axes axes logical
#' @param axes_color axes color
#' @param grid grid lines
#' @param grid_color grid lines color
#' @param hover hover logical
#' @param cloud point cloud data frame or matrix
#' @param pt_color point cloud color
#' @param pt_size point cloud point size
#' @param bg_color plot background color
#' @param lit plot lighting
#' @param pan pan plot on right click
#' @param normalize normalize QSM
#' @param triangulation plot triangulation mesh
#' @param tri_color triangulation mesh color
#' @param tri_palette triangulation palette
#' @returns an rgl plot
#' @noRd
plot_data <- function(
    cylinder = NULL,
    radius = NULL,
    length = NULL,
    id = NULL,
    branch = NULL,
    branch_order = NULL,
    start_x = NULL,
    start_y = NULL,
    start_z = NULL,
    axis_x = NULL,
    axis_y = NULL,
    axis_z = NULL,
    end_x = NULL,
    end_y = NULL,
    end_z = NULL,
    facets = NULL,
    skeleton = NULL,
    skeleton_lwd = NULL,
    color = NULL,
    palette = NULL,
    alpha = NULL,
    axes = NULL,
    axes_color = NULL,
    grid = NULL,
    grid_color = NULL,
    hover = NULL,
    cloud = NULL,
    pt_color = NULL,
    pt_size = NULL,
    bg_color = NULL,
    lit = NULL,
    pan = NULL,
    normalize = NULL,
    triangulation = NULL,
    tri_color = NULL,
    tri_palette = NULL) {
  if (!is.null(cylinder)) {
    # Plotting radii -----------------------------------------------------------
    radius <- plotting_radii(cylinder, radius)

    # Plotting colors ----------------------------------------------------------
    colors <- plotting_colors(cylinder, color, palette, branch_order)

    # Normalize QSM ------------------------------------------------------------
    if (normalize == TRUE) {
      cylinder <- normalize_qsm(
        cylinder = cylinder, id = id,
        start_x = start_x, start_y = start_y, start_z = start_z,
        end_x = end_x, end_y = end_y, end_z = end_z
      )
    }

    # Plot skeleton ------------------------------------------------------------
    if (skeleton == TRUE) {
      plot_skeleton(
        cylinder, colors, alpha, skeleton_lwd,
        start_x, start_y, start_z, end_x, end_y, end_z
      )
    }

    # Plot cylinders -----------------------------------------------------------
    else {
      plot_cylinders(
        cylinder, radius, length, start_x, start_y, start_z,
        axis_x, axis_y, axis_z, facets, colors, alpha, lit
      )
    }

    # Mouse hover --------------------------------------------------------------
    if (hover == TRUE) {
      hover(cylinder, id, branch, start_x, start_y, start_z)
    }
  }

  # Plot triangulation ---------------------------------------------------------
  if (!is.null(triangulation)) {
    plot_triangulation(triangulation, tri_color, tri_palette, lit)
  }

  # Plot cloud -----------------------------------------------------------------
  if (!is.null(cloud)) {
    plot_cloud(cloud, cylinder, pt_size, pt_color)
  }

  # Background color -----------------------------------------------------------
  if (!is.null(bg_color)) {
    background_color(bg_color)
  }

  # Plot axes ------------------------------------------------------------------
  if (axes == TRUE) {
    if (is.null(axes_color)) {
      axes(axes, axes_color = "#000000")
    } else {
      axes(axes, axes_color)
    }
  }

  # Plot grid lines ------------------------------------------------------------
  if (grid == TRUE) {
    if (is.null(grid_color)) {
      grid_lines(grid, grid_color = "grey")
    } else {
      grid_lines(grid, grid_color)
    }
  }

  # Pan plot -------------------------------------------------------------------
  if (pan == TRUE & hover == FALSE) {
    pan_plot(button = 2)
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
#' @param alpha cylinder transparency
#' @param skeleton_lwd skeleton line width
#' @param start_x column name of start_x
#' @param start_y column name of start_y
#' @param start_z column name of start_z
#' @param end_x column name of end_x
#' @param end_y column name of end_y
#' @param end_z column name of end_z
#' @returns NA
#' @noRd
plot_skeleton <- function(
    cylinder,
    colors,
    alpha,
    skeleton_lwd,
    start_x,
    start_y,
    start_z,
    end_x,
    end_y,
    end_z) {
  message("Plotting Skeleton")

  if (is.null(skeleton_lwd)) {
    lwd <- 1
  } else {
    lwd <- skeleton_lwd
  }

  # Plot Skeleton
  rgl::segments3d(
    x = as.vector(t(select(cylinder, all_of(c(start_x, end_x))))),
    y = as.vector(t(select(cylinder, all_of(c(start_y, end_y))))),
    z = as.vector(t(select(cylinder, all_of(c(start_z, end_z))))),
    col = rep(colors, each = 2), lwd = lwd, alpha = alpha
  )
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
#' @param alpha cylinder transparency
#' @param lit plot lighting
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
    alpha,
    lit) {
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
  cylinder_mesh <- generate_mesh(start, axis, length, radius, facets)
  colors <- rep(colors, each = facets * 6)

  # Plot cylinders
  rgl::triangles3d(cylinder_mesh, col = colors, lit = lit, alpha = alpha)
}

#' Plot cloud
#' @param cloud point cloud data frame or matrix
#' @param cylinder cylinder data frame to check for existing axes
#' @param pt_size point size
#' @param pt_color point cloud color
#' @returns NA
#' @noRd
plot_cloud <- function(cloud, cylinder, pt_size, pt_color) {
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

#' Plot triangulation
#' @param triangulation triangulation list
#' @param tri_color triangulation color
#' @param tri_palette triangulation palette
#' @param lit plot lighting
#' @returns NA
#' @noRd
plot_triangulation <- function(triangulation, tri_color, tri_palette, lit) {
  message("Plotting Triangulation")

  # Extract TreeQSM triangulation data
  v <- as.matrix(triangulation$vert)
  f <- as.matrix(triangulation$facet)

  # Create triangulation mesh
  triangulation_mesh <- v[as.vector(t(f)), ]

  # Colors
  z <- triangulation_mesh[, 3]
  z_normalized <- (z - min(z)) / (max(z) - min(z))

  if (is.null(tri_color) & is.null(tri_palette)) {
    colors <- colourvalues::color_values(z_normalized, palette = "rainbow")
  } else if (is.null(tri_color) & !is.null(tri_palette)) {
    colors <- colourvalues::color_values(z_normalized, palette = tri_palette)
  } else {
    colors <- tri_color
  }

  # Plot triangulation mesh
  rgl::triangles3d(triangulation_mesh, color = colors, lit = lit)
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

#' Axes
#' @param axes plot axes
#' @param axes_color axes color
#' @returns NA
#' @noRd
axes <- function(axes, axes_color) {
  if (axes == TRUE) {
    rgl::axes3d(edges = c("x", "y", "z"), col = axes_color)
  }
}

#' Grid lines
#' @param grid plot grid lines
#' @param grid_color axes color
#' @returns NA
#' @noRd
grid_lines <- function(grid, grid_color) {
  if (grid == TRUE) {
    rgl::grid3d(side = c("x", "y", "z"), col = grid_color)
  }
}

#' Background color
#' @param bg_color background color
#' @returns NA
#' @noRd
background_color <- function(bg_color) {
  rgl::bg3d(bg_color)
}

#' Pan plot
#' @param button mouse button
#' @returns NA
#' @noRd
pan_plot <- function(button = 2) {
  start <- vector(mode = "list", length = 5)

  begin <- function(x, y) {
    par <- rgl::par3d(no.readonly = TRUE)

    start[[1]] <<- par$userMatrix
    start[[2]] <<- par$viewport
    start[[3]] <<- par$scale
    start[[4]] <<- rgl::rgl.projection()
    start[[5]] <<- rtwig_window2user(
      x = x / start[[2]][3],
      y = 1 - y / start[[2]][4],
      z = 0.5,
      projection = start[[4]]
    )
  }

  update <- function(x, y) {
    rgl::par3d(userMatrix = user_matrix(x, y, 0.5, start))
  }

  rgl::rgl.setMouseCallbacks(button, begin, update)
  # cat("Callbacks set on button", button, "of rgl device", rgl.cur(), "")
  cat("Panning plot on rgl device:", rgl.cur())
}

#' Normalize QSM
#' @param cylinder QSM cylinder data frame
#' @param id cylinder id
#' @param start_x column name of start_x
#' @param start_y column name of start_y
#' @param start_z column name of start_z
#' @param end_x column name of end_x
#' @param end_y column name of end_y
#' @param end_z column name of end_z
#' @returns normalized coordinates or tree start
#' @noRd
normalize_qsm <- function(
    cylinder,
    id = NULL,
    start_x = NULL,
    start_y = NULL,
    start_z = NULL,
    end_x = NULL,
    end_y = NULL,
    end_z = NULL) {
  # Update cylinder coordinates
  coords <- cylinder %>%
    select(
      "id" = all_of(id),
      "start_x" = all_of(start_x),
      "start_y" = all_of(start_y),
      "start_z" = all_of(start_z),
      "end_x" = all_of(end_x),
      "end_y" = all_of(end_y),
      "end_z" = all_of(end_z)
    ) %>%
    mutate(
      start_x = start_x - min(.data$start_x, na.rm = FALSE),
      start_y = start_y - min(.data$start_y, na.rm = FALSE),
      start_z = start_z - min(.data$start_z, na.rm = FALSE),
      end_x = end_x - min(.data$start_x, na.rm = FALSE),
      end_y = end_y - min(.data$start_y, na.rm = FALSE),
      end_z = end_z - min(.data$start_z, na.rm = FALSE)
    ) %>%
    rename(
      !!rlang::sym(id) := "id",
      !!rlang::sym(start_x) := "start_x",
      !!rlang::sym(start_y) := "start_y",
      !!rlang::sym(start_z) := "start_z",
      !!rlang::sym(end_x) := "end_x",
      !!rlang::sym(end_y) := "end_y",
      !!rlang::sym(end_z) := "end_z",
    )

  cylinder %>%
    select(-all_of(c(start_x, start_y, start_z, end_x, end_y, end_z))) %>%
    left_join(coords, by = id)
}
