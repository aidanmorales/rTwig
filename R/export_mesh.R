#' Export Mesh
#'
#' @description Exports QSM cylinder mesh using the rgl library
#'
#' @param cylinder QSM cylinder data frame
#' @param filename Desired name of file. The .ply extension is automatically added.
#' @param radius Vector of cylinder radii. Defaults to modified cylinders from the cylinder data frame.
#' @param cyl_color Optional cylinder color parameter. Colors must be a single hex color, or a vector or column of hex colors with the same length as the cylinder data frame.
#' @param cyl_palette Optional color palette for numerical data. Palettes include: viridis, inferno, plasma, magma, cividis, and rainbow.
#' @param cyl_sides The number of sides in the polygon cross section. Defaults to 8, but can be increased to improve visual smoothness.
#'
#' @return A mesh .ply file
#' @export
#'
#' @import colourvalues
#' @import Morpho
#' @import rgl
#'
#' @examples
#' \donttest{
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
#'   radius = cylinder$UnmodRadius,
#'   cyl_color = cylinder$GrowthLength,
#'   cyl_palette = "viridis"
#' )
#'
#' }
#'
export_mesh <- function(cylinder, filename, radius = NULL, cyl_color = NULL, cyl_palette = NULL, cyl_sides = 8) {
  message("Plotting QSM")

  # TreeQSM --------------------------------------------------------------------
  if (all(c("parent", "extension", "branch", "BranchOrder") %in% colnames(cylinder))) {
    # Error message if cylinders have not been updated
    if (any(cylinder$extension == 1)) {
      stopifnot("Cylinder indexes have not been updated! Please run update_cylinders() before proceeding." = pull(slice_head(cylinder, n = 1), .data$extension) == 1)
    }

    # Initialize cylinder radii
    if (is.null(radius)) {
      radius <- cylinder$radius
    } else {
      if (length(radius) != nrow(cylinder)) {
        stop("Supplied cylinder radii vector is not equal to the number of cylinders!")
      }
      radius <- radius
    }

    # Initialize cylinder colors
    if (is.null(cyl_color)) {
      colors <- color_values(cylinder$BranchOrder, palette = "rainbow")
    } else {
      if (length(cyl_color) > 1 & length(cyl_color) != nrow(cylinder)) {
        stop("Supplied cylinder colors vector is not equal to the number of cylinders!")
      } else {
        cyl_color <- cyl_color
      }

      if (length(cyl_color) == 1) {
        colors <- rep(cyl_color, nrow(cylinder))
      } else if (is.null(cyl_palette) & length(cyl_color) > 1 & !is.character(cyl_color)) {
        colors <- color_values(cyl_color, palette = "rainbow")
      } else if (!is.null(cyl_palette) & length(cyl_color) > 1 & !is.character(cyl_color)) {
        colors <- color_values(cyl_color, palette = cyl_palette)
      } else if (length(cyl_color) > 1 & is.character(cyl_color)) {
        colors <- cyl_color
      }
    }

    message("Creating Cylinder Meshes")

    # Create RGL cylinders
    plot_data <- lapply(1:nrow(cylinder), function(i) {
      cyl <- cylinder3d(
        center = cbind(
          c(cylinder$start.x[i], cylinder$end.x[i]),
          c(cylinder$start.y[i], cylinder$end.y[i]),
          c(cylinder$start.z[i], cylinder$end.z[i])
        ),
        radius = radius[i],
        sides = cyl_sides,
        closed = 0,
        rotationMinimizing = TRUE
      )
      cyl$material$color <- colors[i]
      cyl
    })

    message("Exporting Cylinder Mesh")

    # Combine cylinder meshes into one
    mesh <- Reduce(merge, plot_data)

    # Export mesh
    Morpho::mesh2ply(mesh, filename = filename, writeNormals = FALSE)

  # SimpleForest ---------------------------------------------------------------
  } else if (all(c("ID", "parentID", "branchID", "branchOrder") %in% colnames(cylinder))) {
    # Initialize cylinder radii
    if (is.null(radius)) {
      radius <- cylinder$radius
    } else {
      if (length(radius) != nrow(cylinder)) {
        stop("Supplied cylinder radii vector is not equal to the number of cylinders!")
      }
      radius <- radius
    }

    # Initialize cylinder colors
    if (is.null(cyl_color)) {
      colors <- color_values(cylinder$branchOrder, palette = "rainbow")
    } else {
      if (length(cyl_color) > 1 & length(cyl_color) != nrow(cylinder)) {
        stop("Supplied cylinder colors vector is not equal to the number of cylinders!")
      } else {
        cyl_color <- cyl_color
      }

      if (length(cyl_color) == 1) {
        colors <- rep(cyl_color, nrow(cylinder))
      } else if (is.null(cyl_palette) & length(cyl_color) > 1 & !is.character(cyl_color)) {
        colors <- color_values(cyl_color, palette = "rainbow")
      } else if (!is.null(cyl_palette) & length(cyl_color) > 1 & !is.character(cyl_color)) {
        colors <- color_values(cyl_color, palette = cyl_palette)
      } else if (length(cyl_color) > 1 & is.character(cyl_color)) {
        colors <- cyl_color
      }
    }

    message("Creating Cylinder Meshes")

    plot_data <- lapply(1:nrow(cylinder), function(i) {
      cyl <- cylinder3d(
        center = cbind(
          c(cylinder$startX[i], cylinder$endX[i]),
          c(cylinder$startY[i], cylinder$endY[i]),
          c(cylinder$startZ[i], cylinder$endZ[i])
        ),
        radius = radius[i],
        sides = cyl_sides,
        closed = 0,
        rotationMinimizing = TRUE
      )
      cyl$material$color <- colors[i]
      cyl
    })

    message("Exporting Cylinder Mesh")

    # Combine cylinder meshes into one
    mesh <- Reduce(merge, plot_data)

    # Export mesh
    Morpho::mesh2ply(mesh, filename = filename, writeNormals = FALSE)

  } else {
    message(
      "Invalid QSM Supplied!!!
      \nMake sure the cylinder data frame and not the QSM list is supplied.
      \nMake sure the point cloud is a data frame with the first three columns as the x, y, and z coordinates."
    )
  }
}
