#' Plot QSM
#'
#' @description Efficiently plot QSMs, point clouds, leaves, and stem
#'  triangulation meshes. Plots can be combined and extensively customized.
#'
#' @param cylinder QSM cylinder data frame.
#'
#' @param radius Radius column name either quoted or unquoted.
#'  Defaults to the modified radii.
#'
#' @param color Optional cylinder color parameter.
#'  `color` must be a single hex color string, a `grDevices::colors()`, a vector
#'  of hex colors, or a quoted/unquoted column name.
#'  Vectors must have the same length as the cylinder data frame.
#'  `color = "random"` will generate a random color applied to all cylinders.
#'  Defaults to branching order.
#'
#' @param palette Optional cylinder color palette for numerical data.
#'  Palettes include `colourvalues::color_palettes()` or a user supplied RGB
#'  palette matrix with the length of cylinder. It can also be set to "random"
#'  to generate a random palette. If combined with `color = "random"`, each
#'  cylinder will have a random, distinct color.
#'
#' @param alpha Set the transparency of the cylinders.
#'  Defaults to 1. 1 is opaque and 0 is fully transparent.
#'
#' @param facets The number of facets in the polygon cross section.
#'  Defaults to 6. A higher number of facets improves visual smoothness at the
#'  cost of plotting speed, performance and memory.
#'
#' @param skeleton Plot the QSM skeleton instead of cylinders. Defaults to FALSE.
#'
#' @param skeleton_lwd Skeleton line width. Defaults to 1.
#'
#' @param cloud Point cloud data frame or matrix where the first three columns
#'  are the x, y, and z coordinates.
#'
#' @param pt_color Optional point cloud color parameter.
#'  `pt_color` must be a single hex color string, a `grDevices::colors()`, a vector
#'  of hex colors, or a quoted/unquoted column name.
#'  `pt_color = "random"` will generate a random color for all points.
#'  Vectors must have the same length as the point cloud data frame.
#'
#' @param pt_palette Optional point cloud color palette for numerical data.
#'  `pt_palette` includes `colourvalues::color_palettes()` or a user supplied RGB
#'  palette matrix with the length of the points. It can also be set to "random"
#'  to generate a random palette. If combined with `color = "random"`, each
#'  point will have a random, distinct color.
#'
#' @param pt_size Size of the points. Defaults to 0.1.
#'
#' @param pt_alpha Set the transparency of the point cloud.
#'  Defaults to 1. 1 is opaque and 0 is fully transparent.
#'
#' @param triangulation Stem triangulation mesh from TreeQSM. Defaults to NULL.
#'
#' @param tri_color Color of the triangulation mesh.
#'  Accepts hex colors, `grDevices::colors()`, or "random".
#'  Defaults to rainbow z-axis.
#'
#' @param tri_palette Optional triangulation color palette for z values.
#'  Supports the same inputs as palette.
#'
#' @param tri_alpha Set the transparency of the triangulation mesh.
#'  Defaults to 1. 1 is opaque and 0 is fully transparent.
#'
#' @param leaves Leaf mesh in the `rgl::mesh3d()` format. Defaults to NULL.
#'
#' @param lf_color Color of the leaves. Defaults to leaf green.
#'  Accepts hex colors, `grDevices::colors()`,"random", or "random_vertex".
#'  `lf_color = "random"` assigns a random color to all leaves, while
#'  `lf_color = "random_vertex"` which assigns a random color to each vertex.
#'
#' @param lf_alpha Set the transparency of the leaves.
#'  Defaults to 1. 1 is opaque and 0 is fully transparent.
#'
#' @param axes Show plot axes. Defaults to TRUE.
#' @param axes_color Set the axes color. Defaults to black.
#'
#' @param grid Show plot grid lines. Defaults to FALSE.
#' @param grid_color Set grid lines color. Defaults to grey.
#'
#' @param hover Show cylinder and branch id on mouse hover. Defaults to FALSE.
#'
#' @param bg_color Set the background color of the plot.
#'  Accepts hex colors or `grDevices::colors()`. Defaults to white.
#'
#' @param lit Enable light source in plot. Defaults to FALSE.
#'
#' @param pan Use right mouse button to pan the plot. Defaults to TRUE.
#'  Panning is disabled when `hover = TRUE`.
#'
#' @param normalize Normalize the cylinders to 0,0,0. Defaults to FALSE.
#'
#' @param mesh Plot any `rgl::mesh3d()` object.
#'
#' @return An `rgl::open3d()` plot
#' @export
#'
#' @examples
#'
#' ## TreeQSM
#' file <- system.file("extdata/QSM.mat", package = "rTwig")
#' qsm <- import_treeqsm(file)
#' cylinder <- qsm$cylinder
#' cylinder <- update_cylinders(cylinder)
#' plot_qsm(cylinder)
#'
#' triangulation <- qsm$triangulation
#' plot_qsm(triangulation = triangulation)
#'
plot_qsm <- function(
    cylinder = NULL,
    radius = NULL,
    color = NULL,
    palette = NULL,
    alpha = 1,
    facets = 6,
    skeleton = FALSE,
    skeleton_lwd = 1,
    cloud = NULL,
    pt_color = "#000000",
    pt_palette = NULL,
    pt_size = NULL,
    pt_alpha = 1,
    triangulation = NULL,
    tri_color = NULL,
    tri_palette = NULL,
    tri_alpha = 1,
    leaves = NULL,
    lf_color = "#5BA803",
    lf_alpha = 1,
    axes = TRUE,
    axes_color = "#000000",
    grid = FALSE,
    grid_color = "#D3D3D3",
    hover = FALSE,
    bg_color = "#FFFFFF",
    lit = FALSE,
    pan = TRUE,
    normalize = FALSE,
    mesh = NULL) {
  # Check inputs ---------------------------------------------------------------
  if (!is_null(cylinder)) {
    if (!is.data.frame(cylinder)) {
      message <- paste(
        paste0("`cylinder` must be a data frame, not ", class(cylinder), "."),
        "i Did you accidentally pass the QSM list instead of the cylinder data frame?",
        sep = "\n"
      )
      abort(message, class = "data_format_error")
    }

    if (nrow(cylinder) == 0) {
      abort("'cylinder' data frame empty!")
    }

    cylinder <- verify_cylinders(cylinder)
  }

  # User selected columns
  radius <- select_column(rlang::enquo(radius))
  color <- select_column(rlang::enquo(color))
  pt_color <- select_column(rlang::enquo(pt_color))

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
        paste(
          "Hex colors (e.g. `#FF0000`) are preferred for plotting",
          "solid colors."
        ),
        paste(
          "Colors from `grDevices::colors()` (e.g. `red`) slow down plotting",
          "because `grDevices::col2rgb()` must convert the color string into",
          "an RGB matrix for every vertex in the cylinder mesh."
        ),
        sep = "\n"
      )
      warn(message)
    }
  }

  if (!is_null(alpha)) {
    if (!is_double(alpha)) {
      message <- paste0(
        "`alpha` must be a double, not ", class(alpha), "."
      )
      abort(message, class = "invalid_argument")
    }

    if (alpha != 1) {
      message <- paste0(
        "Alpha transparency signifcantly degrades plot performance ",
        "for a large number of cylinders."
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
    message <- paste0(
      "A large value of `facets` signifcantly degrades plot ",
      "performance for a large number of cylinders."
    )
    warn(message)
  }

  if (!is_logical(skeleton)) {
    message <- paste0(
      "`skeleton` must be logical, not ", class(skeleton), "."
    )
    abort(message, class = "invalid_argument")
  }

  if (!is_null(skeleton_lwd)) {
    if (!is_double(skeleton_lwd)) {
      message <- paste0(
        "`skeleton_lwd` must be a double, not ", class(skeleton_lwd), "."
      )
      abort(message, class = "invalid_argument")
    }
  }

  if (!is_null(cloud)) {
    if (all(!is.data.frame(cloud), !is.matrix(cloud))) {
      message <- paste0(
        "`cloud` must be a data frame or matrix, not a ", class(cloud), "."
      )
      abort(message, class = "data_format_error")
    }
  }

  if (!is_null(pt_size) & !is_double(pt_size)) {
    message <- paste0(
      "`pt_size` must be a double, not ", class(pt_size), "."
    )
    abort(message, class = "invalid_argument")
  }

  if (!is_null(pt_alpha)) {
    if (!is_double(pt_alpha)) {
      message <- paste0(
        "`pt_alpha` must be a double, not ", class(pt_alpha), "."
      )
      abort(message, class = "invalid_argument")
    }

    if (pt_alpha != 1) {
      message <- paste0(
        "Alpha transparency signifcantly degrades plot performance ",
        "for a large number of points."
      )
      warn(message)
    }
  }

  if (!is.null(triangulation)) {
    if (!is_list(triangulation)) {
      message <- paste(
        paste0("`triangulation` must be a list, not ", class(triangulation), "."),
        "i `triangulation` must be created by `import_treeqsm()`.",
        sep = "\n"
      )
      abort(message, class = "data_format_error")
    }
    if (is_list(triangulation) & length(triangulation) == 0) {
      abort("`triangulation` is an empty list!")
    }
  }

  if (!is_null(tri_alpha)) {
    if (!is_double(tri_alpha)) {
      message <- paste0(
        "`tri_alpha` must be a double, not ", class(tri_alpha), "."
      )
      abort(message, class = "invalid_argument")
    }

    if (tri_alpha != 1) {
      message <- paste0(
        "Alpha transparency signifcantly degrades plot performance ",
        "for a large number of triangles."
      )
      warn(message)
    }
  }

  if (!is.null(leaves)) {
    leaf_class <- class(leaves)
    if (!("mesh3d" %in% leaf_class || "shape3d" %in% leaf_class)) {
      message <- paste(
        paste0("`leaves` must be an rgl mesh3d, not ", class(leaves)[1], "."),
        "i `leaves` can be created with `import_leaves()`.",
        sep = "\n"
      )
      abort(message, class = "data_format_error")
    }
  }

  if (!is_null(lf_alpha)) {
    if (!is_double(lf_alpha)) {
      message <- paste0(
        "`lf_alpha` must be a double, not ", class(lf_alpha), "."
      )
      abort(message, class = "invalid_argument")
    }

    if (lf_alpha != 1) {
      message <- paste0(
        "Alpha transparency signifcantly degrades plot performance ",
        "for a large number of leaves."
      )
      warn(message)
    }
  }

  if (!is_logical(axes)) {
    message <- paste0(
      "`axes` must be logical, not ", class(axes), "."
    )
    abort(message, class = "invalid_argument")
  }

  if (!is_logical(grid)) {
    message <- paste0(
      "`grid` must be logical, not ", class(grid), "."
    )
    abort(message, class = "invalid_argument")
  }

  if (!is_logical(hover)) {
    message <- paste0(
      "`hover` must be logical, not ", class(hover), "."
    )
    abort(message, class = "invalid_argument")
  }

  if (!is_logical(lit)) {
    message <- paste0(
      "`lit` must be logical, not ", class(lit), "."
    )
    abort(message, class = "invalid_argument")
  }

  if (!is_logical(pan)) {
    message <- paste0(
      "`pan` must be logical, not ", class(pan), "."
    )
    abort(message, class = "invalid_argument")
  }

  if (!is_logical(normalize)) {
    message <- paste0(
      "`normalize` must be logical, not ", class(normalize), "."
    )
    abort(message, class = "invalid_argument")
  }

  if (!is.null(mesh)) {
    mesh_class <- class(mesh)
    if (!("mesh3d" %in% mesh_class || "shape3d" %in% mesh_class)) {
      message <- paste(
        paste0("`mesh` must be an rgl mesh3d, not ", class(mesh)[1], "."),
        "i `mesh` can be created with `rgl::as.mesh3d()`.",
        sep = "\n"
      )
      abort(message, class = "data_format_error")
    }
  }

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
      cloud = cloud, pt_color = pt_color, pt_palette = pt_palette,
      pt_size = pt_size, pt_alpha = pt_alpha,
      bg_color = bg_color, lit = lit, pan = pan, normalize = normalize,
      triangulation = triangulation, tri_color = tri_color,
      tri_palette = tri_palette, tri_alpha = tri_alpha,
      leaves = leaves, lf_color = lf_color, lf_alpha = lf_alpha, mesh = mesh
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
      cloud = cloud, pt_color = pt_color, pt_palette = pt_palette,
      pt_size = pt_size, pt_alpha = pt_alpha,
      bg_color = bg_color, lit = lit, pan = pan, normalize = normalize,
      triangulation = triangulation, tri_color = tri_color,
      tri_palette = tri_palette, tri_alpha = tri_alpha,
      leaves = leaves, lf_color = lf_color, lf_alpha = lf_alpha, mesh = mesh
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
      cloud = cloud, pt_color = pt_color, pt_palette = pt_palette,
      pt_size = pt_size, pt_alpha = pt_alpha,
      bg_color = bg_color, lit = lit, pan = pan, normalize = normalize,
      triangulation = triangulation, tri_color = tri_color,
      tri_palette = tri_palette, tri_alpha = tri_alpha,
      leaves = leaves, lf_color = lf_color, lf_alpha = lf_alpha, mesh = mesh
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
      cloud = cloud, pt_color = pt_color, pt_palette = pt_palette,
      pt_size = pt_size, pt_alpha = pt_alpha,
      bg_color = bg_color, lit = lit, pan = pan, normalize = normalize,
      triangulation = triangulation, tri_color = tri_color,
      tri_palette = tri_palette, tri_alpha = tri_alpha,
      leaves = leaves, lf_color = lf_color, lf_alpha = lf_alpha, mesh = mesh
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
      cloud = cloud, pt_color = pt_color, pt_palette = pt_palette,
      pt_size = pt_size, pt_alpha = pt_alpha,
      bg_color = bg_color, lit = lit, pan = pan, normalize = normalize,
      triangulation = triangulation, tri_color = tri_color,
      tri_palette = tri_palette, tri_alpha = tri_alpha,
      leaves = leaves, lf_color = lf_color, lf_alpha = lf_alpha, mesh = mesh
    )
  }
  # Point Cloud, Triangulation, Leaves, or Mesh --------------------------------
  else if (any(
    !is.null(cloud) | !is.null(triangulation) | !is.null(leaves) | !is.null(mesh)
  )) {
    plot_data(
      cylinder = cylinder,
      axes = axes, axes_color = axes_color,
      grid = grid, grid_color = grid_color, hover = hover,
      cloud = cloud, pt_color = pt_color, pt_palette = pt_palette,
      pt_size = pt_size, pt_alpha = pt_alpha,
      bg_color = bg_color, lit = lit, pan = pan, normalize = normalize,
      triangulation = triangulation, tri_color = tri_color,
      tri_palette = tri_palette, tri_alpha = tri_alpha,
      leaves = leaves, lf_color = lf_color, lf_alpha = lf_alpha, mesh = mesh
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
#' @param pt_palette point cloud palette
#' @param pt_size point cloud point size
#' @param pt_alpha point cloud alpha
#' @param bg_color plot background color
#' @param lit plot lighting
#' @param pan pan plot on right click
#' @param normalize normalize QSM
#' @param triangulation plot triangulation mesh
#' @param tri_color triangulation mesh color
#' @param tri_palette triangulation palette
#' @param tri_alpha triangulation alpha
#' @param leaves plot leaf mesh
#' @param lf_color leaf mesh color
#' @param lf_alpha leaf alpha
#' @param mesh rgl::mesh3d object
#' @returns an rgl::open3d plot
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
    pt_palette = NULL,
    pt_size = NULL,
    pt_alpha = NULL,
    bg_color = NULL,
    lit = NULL,
    pan = NULL,
    normalize = NULL,
    triangulation = NULL,
    tri_color = NULL,
    tri_palette = NULL,
    tri_alpha = NULL,
    leaves = NULL,
    lf_color = NULL,
    lf_alpha = NULL,
    mesh = NULL) {
  if (!is.null(cylinder)) {
    # Plotting radii -----------------------------------------------------------
    radius <- plotting_radii(cylinder, radius)

    # Plot colors --------------------------------------------------------------
    colors <- plot_colors(cylinder, color, palette, branch_order)

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
    plot_triangulation(triangulation, tri_color, tri_palette, tri_alpha, lit)
  }

  # Plot cloud -----------------------------------------------------------------
  if (!is.null(cloud)) {
    plot_cloud(cloud, pt_size, pt_color, pt_palette, pt_alpha)
  }

  # Plot leaves ----------------------------------------------------------------
  if (!is.null(leaves)) {
    plot_leaves(leaves, lf_color, lf_alpha, lit)
  }

  # Plot additional mesh -------------------------------------------------------
  if (!is.null(mesh)) {
    rgl::plot3d(mesh, add = TRUE, lit = lit)
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
      grid_lines(grid, grid_color = "#D3D3D3")
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
    radius <- cylinder$radius # Note: partial matching includes aRchi radius_cyl
  } else {
    radius <- pull(cylinder, {{ radius }})
  }

  return(radius)
}

#' Plot colors
#' @param data QSM or point cloud data data frame
#' @param radius data radii column name
#' @param palette color palette
#' @param branch_order branch order column name
#' @returns a vector of hex colors
#' @noRd
plot_colors <- function(data, color, palette, branch_order) {
  message <- paste(
    "`color` is invalid.",
    "! `color` vectors must have length == nrow(data).",
    paste0(
      "i Valid inputs for `color` include: hex colors, `grDevices::colors()`, ",
      "colnames(data), or `random`."
    ),
    sep = "\n"
  )

  if (is.null(color)) {
    color <- NULL
  } else {
    if (color %in% colnames(data)) {
      color <- color
    } else {
      eval_check <- try(eval(rlang::parse_expr(color)), silent = TRUE)

      if (!inherits(eval_check, "try-error")) {
        color <- eval_check
      }
    }
  }

  if (is.null(color)) {
    if (!is.null(branch_order)) { # cylinder defaults
      default_color <- colourvalues::color_values(
        pull(data, {{ branch_order }}),
        palette = "rainbow"
      )
    } else if (is.null(branch_order)) { # cloud defaults
      default_color <- "#000000"
    }
  } else if (is.vector(color) & length(color) > 1) {
    if (length(color) != nrow(data)) {
      abort(message)
    }
  } else if (color == "random") {
    color <- generate_random_colors(1)

    if (!is.null(palette) && palette == "random") {
      colors <- generate_random_colors(nrow(data))
      return(colors)
    }
  } else if (is.vector(color) & length(color) == 1 & !(color %in% colnames(data))) {
    rgb_check <- try(grDevices::col2rgb(color), silent = TRUE)

    if (is.matrix(rgb_check)) {
      color <- color
    } else {
      abort(message)
    }
  } else if (color %in% colnames(data)) {
    color <- pull(data, {{ color }})
  } else {
    abort(message)
  }

  if (length(color) == 1) {
    colors <- rep(color, nrow(data))
  } else if (is.null(palette) & length(color) > 1 & !is.character(color)) {
    colors <- colourvalues::color_values(color, palette = "rainbow")
  } else if (!is.null(palette) & length(color) > 1 & !is.character(color)) {
    if (palette == "random") {
      n <- length(colourvalues::color_palettes())
      palettes <- colourvalues::color_palettes()
      palette <- palettes[sample(1:n, 1)]
    }

    colors <- colourvalues::color_values(color, palette = palette)
  } else if (length(color) > 1 & is.character(color)) {
    colors <- color
  } else {
    colors <- default_color
  }

  return(colors)
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
  inform("Plotting Skeleton")

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
  inform("Plotting Cylinders")

  # Extract required variables
  start <- cbind(
    pull(cylinder, {{ start_x }}),
    pull(cylinder, {{ start_y }}),
    pull(cylinder, {{ start_z }})
  )

  axis <- cbind(
    pull(cylinder, {{ axis_x }}),
    pull(cylinder, {{ axis_y }}),
    pull(cylinder, {{ axis_z }})
  )

  length <- pull(cylinder, {{ length }})

  # Create cylinder mesh
  cylinder_mesh <- generate_mesh(start, axis, length, radius, facets)
  colors <- rep(colors, each = facets * 6)

  # Alpha transparency
  if (is.null(alpha)) {
    alpha <- 1
  } else {
    alpha <- alpha
  }

  # Plot cylinders
  rgl::triangles3d(cylinder_mesh, col = colors, lit = lit, alpha = alpha)
}

#' Plot cloud
#' @param cloud point cloud data frame or matrix
#' @param pt_size point size
#' @param pt_color point cloud color
#' @param pt_palette point cloud palette
#' @param pt_alpha point cloud alpha
#' @returns NA
#' @noRd
plot_cloud <- function(cloud, pt_size, pt_color, pt_palette, pt_alpha) {
  inform("Plotting Cloud")

  if (!is.data.frame(cloud)) {
    cloud <- as_tidytable(cloud)
  }

  if (!all(map_lgl(cloud[, 1:3], is.numeric))) {
    message <- paste0(
      "The first three columns of `cloud` must be x, y, z."
    )
    abort(message, class = "data_format_error")
  }

  # Initialize cloud inputs
  pt_color <- plot_colors(cloud, pt_color, pt_palette, NULL)

  if (is.null(pt_size)) {
    pt_size <- 0.1
  } else {
    pt_size <- pt_size
  }

  if (is.null(pt_alpha)) {
    pt_alpha <- 1
  } else {
    pt_alpha <- pt_alpha
  }

  # Plot cloud
  rgl::plot3d(
    x = cloud,
    col = pt_color,
    size = pt_size,
    alpha = pt_alpha,
    add = TRUE,
    aspect = FALSE
  )
}

#' Plot triangulation
#' @param triangulation triangulation list
#' @param tri_color triangulation color
#' @param tri_palette triangulation palette
#' @param tri_alpha triangulation alpha
#' @param lit plot lighting
#' @returns NA
#' @noRd
plot_triangulation <- function(
    triangulation,
    tri_color,
    tri_palette,
    tri_alpha,
    lit) {
  inform("Plotting Triangulation")

  # Extract TreeQSM triangulation data
  v <- as.matrix(triangulation$vert)
  f <- as.matrix(triangulation$facet)

  # Create triangulation mesh
  triangulation_mesh <- v[as.vector(t(f)), ]

  # Colors
  z <- triangulation_mesh[, 3]
  z_normalized <- (z - min(z)) / (max(z) - min(z))

  if (is.null(tri_color) & is.null(tri_palette)) {
    tri_color <- colourvalues::color_values(z_normalized, palette = "rainbow")
  } else if (!is.null(tri_color) & is.null(tri_palette)) {
    if (tri_color == "random") {
      tri_color <- generate_random_colors(1)
    } else {
      tri_color <- tri_color
    }
  }

  if (!is.null(tri_palette)) {
    tri_color <- colourvalues::color_values(z_normalized, palette = tri_palette)
  }

  # Alpha transparency
  if (is.null(tri_alpha)) {
    tri_alpha <- 1
  } else {
    tri_alpha <- tri_alpha
  }

  # Plot triangulation mesh
  rgl::triangles3d(
    triangulation_mesh,
    color = tri_color,
    alpha = tri_alpha,
    lit = lit
  )
}

#' Plot leaves
#' @param leaves leaf rgl mesh3d
#' @param lf_color leaf color
#' @param lf_alpha leaf alpha
#' @param lit plot lighting
#' @returns NA
#' @noRd
plot_leaves <- function(
    leaves,
    lf_color,
    lf_alpha,
    lit) {
  inform("Plotting Leaves")

  # Get leaf colors
  if (!is.null(lf_color)) {
    if (lf_color == "random") {
      lf_color <- generate_random_colors(1)
    } else if (lf_color == "random_vertex") {
      lf_color <- rep(generate_random_colors(length(leaves$vb) / 3), each = 3)
    } else {
      lf_color <- lf_color
    }
  } else {
    lf_color <- "#5BA803"
  }

  # Plot leaf mesh
  rgl::plot3d(
    x = leaves,
    col = lf_color,
    alpha = lf_alpha,
    lit = lit,
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
    x = pull(cylinder, {{ start_x }}),
    y = pull(cylinder, {{ start_y }}),
    z = pull(cylinder, {{ start_z }}),
    labels = paste0(
      "ID:",
      pull(cylinder, {{ id }}),
      " - Branch:",
      pull(cylinder, {{ branch }})
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
    id,
    start_x,
    start_y,
    start_z,
    end_x,
    end_y,
    end_z) {
  # Update cylinder coordinates
  coords <- cylinder %>%
    select(
      "id" = {{ id }},
      "start_x" = {{ start_x }},
      "start_y" = {{ start_y }},
      "start_z" = {{ start_z }},
      "end_x" = {{ end_x }},
      "end_y" = {{ end_y }},
      "end_z" = {{ end_z }}
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
