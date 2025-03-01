#' Export Mesh
#'
#' @description Exports a QSM cylinder mesh in various formats
#'
#' @param cylinder QSM cylinder data frame.
#'
#' @param filename File name and path for exporting.
#'  The file extension is automatically added if not present.
#'
#' @param format Mesh file format. Defaults to `ply`.
#'  Supported formats include `ply`, `obj`, `stl`, and `blender`.
#'  `format = "blender"` exports the mesh in the qsm-blender-addons format.
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
#' @param facets The number of facets in the polygon cross section.
#'  Defaults to 6. A higher number of facets improves visual smoothness at the
#'  cost of plotting speed, performance and memory.
#'
#' @param normals Export surface normals per vertex. Defaults to FALSE.
#'
#' @param alpha Set the transparency of the cylinders used in the "ply" format.
#'  Defaults to 1. 1 is opaque and 0 is fully transparent.
#'
#' @return A mesh file
#' @export
#'
#' @examples
#'
#' ## Load QSM
#' file <- system.file("extdata/QSM.mat", package = "rTwig")
#' qsm <- import_treeqsm(file)
#' cylinder <- qsm$cylinder
#' cylinder <- update_cylinders(cylinder)
#'
#' # PLY
#' filename <- tempfile(pattern = "QSM_ply")
#' export_mesh(
#'   cylinder = cylinder,
#'   filename = filename,
#'   format = "ply",
#'   color = "distanceToTwig",
#'   palette = "viridis",
#'   normals = TRUE
#' )
#'
#' # OBJ
#' filename <- tempfile(pattern = "QSM_obj")
#' export_mesh(
#'   cylinder = cylinder,
#'   filename = filename,
#'   format = "obj",
#'   normals = TRUE
#' )
#'
#' # STL
#' filename <- tempfile(pattern = "QSM_stl")
#' export_mesh(
#'   cylinder = cylinder,
#'   filename = filename,
#'   format = "stl",
#'   normals = TRUE
#' )
#'
#' # QSM Blender Addons
#' filename <- tempfile(pattern = "QSM_blender")
#' export_mesh(
#'   cylinder = cylinder,
#'   filename = filename,
#'   format = "blender",
#'   normals = TRUE
#' )
#'
export_mesh <- function(
    cylinder,
    filename,
    format = "ply",
    radius = NULL,
    color = NULL,
    palette = NULL,
    facets = 6,
    normals = FALSE,
    alpha = 1) {
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

  if (!is_null(format)) {
    if (!format %in% c("ply", "obj", "stl", "blender")) {
      message <- paste(
        "`format` is invalid!",
        "i supported formats include: `ply`, `obj`, `stl`, `blender`",
        sep = "\n"
      )
      abort(message, class = "data_format_error")
    }
  }

  if (!is_null(format) & !is_string(format)) {
    message <- paste0(
      "`format` must be a string, not ", class(format), "."
    )
    abort(message, class = "invalid_argument")
  }

  if (!is_string(filename)) {
    message <- paste0(
      "`filename` must be a string, not ", class(filename), "."
    )
    abort(message, class = "invalid_argument")
  }

  # Ensure correct file extension
  base_name <- basename(filename)
  new_name <- sub("\\.[^.]*$", "", base_name)
  filename <- file.path(dirname(filename), new_name)

  if (format == "ply") {
    filename <- paste(filename, format, sep = ".")
  } else if (format == "obj") {
    filename <- paste(filename, format, sep = ".")
  } else if (format == "stl") {
    filename <- paste(filename, format, sep = ".")
  } else if (format == "blender") {
    filename <- paste0(filename, ".txt")
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

  if (!is_null(alpha)) {
    if (!is_double(alpha)) {
      message <- paste0(
        "`alpha` must be a double, not ", class(alpha), "."
      )
      abort(message, class = "invalid_argument")
    }
  }

  # Verify cylinders
  cylinder <- verify_cylinders(cylinder)

  # rTwig ----------------------------------------------------------------------
  if (all(c("id", "parent", "start_x", "branch_order") %in% colnames(cylinder))) {
    format_mesh(
      filename = filename, format = format, cylinder = cylinder, radius = radius,
      length = "length", branch = "branch", branch_order = "branch_order",
      start_x = "start_x", start_y = "start_y", start_z = "start_z",
      axis_x = "axis_x", axis_y = "axis_y", axis_z = "axis_z",
      facets = facets, color = color, palette = palette,
      normals = normals, alpha = alpha
    )
  }
  # TreeQSM --------------------------------------------------------------------
  else if (all(c("parent", "extension", "branch", "BranchOrder") %in% colnames(cylinder))) {
    format_mesh(
      filename = filename, format = format, cylinder = cylinder, radius = radius,
      length = "length", branch = "branch", branch_order = "BranchOrder",
      start_x = "start.x", start_y = "start.y", start_z = "start.z",
      axis_x = "axis.x", axis_y = "axis.y", axis_z = "axis.z",
      facets = facets, color = color, palette = palette,
      normals = normals, alpha = alpha
    )
  }
  # SimpleForest ---------------------------------------------------------------
  else if (all(c("ID", "parentID", "branchID", "branchOrder") %in% colnames(cylinder))) {
    format_mesh(
      filename = filename, format = format, cylinder = cylinder, radius = radius,
      length = "length", branch = "branchID", branch_order = "branchOrder",
      start_x = "startX", start_y = "startY", start_z = "startZ",
      axis_x = "axisX", axis_y = "axisY", axis_z = "axisZ",
      facets = facets, color = color, palette = palette,
      normals = normals, alpha = alpha
    )
  }
  # Treegraph ------------------------------------------------------------------
  else if (all(c("p1", "p2", "ninternode") %in% colnames(cylinder))) {
    format_mesh(
      filename = filename, format = format, cylinder = cylinder, radius = radius,
      length = "length", branch = "nbranch", branch_order = "branch_order",
      start_x = "sx", start_y = "sy", start_z = "sz",
      axis_x = "ax", axis_y = "ay", axis_z = "az",
      facets = facets, color = color, palette = palette,
      normals = normals, alpha = alpha
    )
  }
  # aRchi ----------------------------------------------------------------------
  else if (all(c("cyl_ID", "parent_ID", "branching_order") %in% colnames(cylinder))) {
    format_mesh(
      filename = filename, format = format, cylinder = cylinder, radius = radius,
      length = "length", branch = "n_branch", branch_order = "branching_order",
      start_x = "startX", start_y = "startY", start_z = "startZ",
      axis_x = "axisX", axis_y = "axisY", axis_z = "axisZ",
      facets = facets, color = color, palette = palette,
      normals = normals, alpha = alpha
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

#' Format mesh
#' @param filename name of file to export
#' @param format export format
#' @param cylinder QSM cylinder data frame
#' @param radius cylinder radii
#' @param length cylinder length
#' @param branch cylinder branch id
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
#' @param alpha cylinder alpha
#' @returns a mesh file
#' @noRd
format_mesh <- function(
    filename,
    format,
    cylinder,
    radius,
    length,
    branch,
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
    normals,
    alpha) {
  inform("Exporting Mesh")

  if (format == "ply") {
    # Create cylinder vertices
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
    radius <- plotting_radii(cylinder, radius)

    cylinder_mesh <- generate_mesh(start, axis, length, radius, facets)

    # Plotting colors and transparency
    if (is.null(color)) {
      colors <- NULL
    } else {
      if (!is.null(color) && color == FALSE) {
        colors <- NULL
      } else {
        colors <- plot_colors(cylinder, color, palette, branch_order)
        colors <- rep(colors, each = facets * 6)

        if (!is.null(alpha)) {
          colors <- cbind(t(grDevices::col2rgb(colors, alpha = FALSE)), alpha)
        } else {
          colors <- cbind(t(grDevices::col2rgb(colors, alpha = FALSE)), 1)
        }
      }
    }

    # Surface normals
    if (normals == TRUE) {
      normals <- calculate_normals(cylinder_mesh)
    } else {
      normals <- NULL
    }

    # Export cylinder mesh
    write_ply(
      vertices = cylinder_mesh,
      colors = colors,
      normals = normals,
      filename = filename
    )
  } else if (format == "obj") {
    # Create cylinder vertices
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
    radius <- plotting_radii(cylinder, radius)

    cylinder_mesh <- generate_mesh(start, axis, length, radius, facets)

    # Surface normals
    if (normals == TRUE) {
      normals <- calculate_normals(cylinder_mesh)
    } else {
      normals <- NULL
    }

    # Export cylinder mesh
    write_obj(
      vertices = cylinder_mesh,
      normals = normals,
      filename = filename
    )
  } else if (format == "stl") {
    # Create cylinder vertices
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
    radius <- plotting_radii(cylinder, radius)

    cylinder_mesh <- generate_mesh(start, axis, length, radius, facets)

    # Surface normals
    normals <- calculate_normals(cylinder_mesh)

    # Export cylinder mesh
    write_stl(
      vertices = cylinder_mesh,
      normals = normals,
      filename = filename
    )
  } else if (format == "blender") {
    select(
      cylinder,
      branch = {{ branch }},
      start_x = {{ start_x }}, start_y = {{ start_y }}, start_z = {{ start_z }},
      axis_x = {{ axis_x }}, axis_y = {{ axis_y }}, axis_z = {{ axis_z }},
      length = {{ length }},
      radius = {{ radius }}
    ) %>%
      fwrite(
        file = filename,
        col.names = FALSE,
        row.names = FALSE,
        sep = " "
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
