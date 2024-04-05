#' Plot QSM
#'
#' @description Plots QSM cylinders using the rgl library
#'
#' @param cylinder QSM cylinder data frame
#' @param radius Vector of cylinder radii. Defaults to modified cylinders from the cylinder data frame.
#' @param cyl_color Optional cylinder color parameter. Colors must be a single hex color, or a vector or column of hex colors with the same length as the cylinder data frame.
#' @param cyl_palette Optional color palette for numerical data. Palettes include: viridis, inferno, plasma, magma, cividis, and rainbow.
#' @param cyl_sides The number of sides in the polygon cross section. Defaults to 8, but can be increased to improve visual smoothness.
#' @param cloud Point cloud data frame where the first three columns are the x, y, and z coordinates in the same coordinate system as the QSM.
#' @param pt_color Color of the point cloud. Defaults to black.
#' @param pt_size Size of the points as a number. Defaults to 0.1.
#' @param axes Show plot axes. Defaults to TRUE.
#' @param hover Show cylinder ID and branch on mouse hover. Defaults to FALSE.
#'
#' @return A rgl QSM plot
#' @export
#'
#' @import colourvalues
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
#' cylinder <- correct_radii(cylinder, twigRad = 4.23)
#' plot_qsm(cylinder)
#'
#' ## SimpleForest Processing Chain
#' file <- system.file("extdata/QSM.csv", package = "rTwig")
#' cylinder2 <- read.csv(file)
#' cylinder2 <- update_cylinders(cylinder2)
#' cylinder2 <- correct_radii(cylinder2, twigRad = 4.23)
#' plot_qsm(cylinder2)
#'
#' ## All Parameters
#' file2 <- system.file("extdata/cloud.txt", package = "rTwig")
#' cloud <- read.table(file2, header = FALSE)
#'
#' plot_qsm(
#'   cylinder,
#'   radius = cylinder$UnmodRadius,
#'   cyl_color = cylinder$GrowthLength,
#'   cyl_palette = "viridis",
#'   cyl_sides = 100,
#'   cloud = cloud,
#'   pt_color = "white",
#'   pt_size = 1,
#'   axes = FALSE,
#'   hover = TRUE
#' )
#'
#' }
#'
plot_qsm <- function(cylinder, radius = NULL, cyl_color = NULL, cyl_palette = NULL, cyl_sides = 8, cloud = NULL, pt_color = NULL, pt_size = NULL, axes = TRUE, hover = FALSE) {
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

    message("Plotting Cylinder Meshes")

    # Plot cylinders
    open3d()
    par3d(skipRedraw=TRUE)
    shade3d(shapelist3d(plot_data, plot = FALSE))
    par3d(skipRedraw=FALSE)

    # Plot axes
    if (axes == TRUE) {
      axes3d(edges = c("x", "y", "z"))
    }

    # Plot cloud
    if (!is.null(cloud)) {
      cloud <- rename(cloud, x = 1, y = 2, z = 3)

      # Initialize cloud inputs
      if (is.null(pt_color)) {
        pt_color <- "#000000"
      } else {
        pt_color <- pt_color
      }

      if (is.null(pt_size)) {
        pt_size <- 0.1
      } else {
        pt_size <- pt_size
      }

      # Plot cloud
      plot3d(cloud$x, cloud$y, cloud$z, col = pt_color, size = pt_size, add = TRUE, aspect = FALSE)
    }

    # Set mouse hover
    if (hover == TRUE) {
      hover3d(
        cylinder$start.x,
        cylinder$start.y,
        cylinder$start.z,
        labels = paste0("ID:", cylinder$extension, " - Branch:", cylinder$branch)
      )
    }

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

    message("Plotting Cylinder Meshes")

    # Plot cylinders
    open3d()
    par3d(skipRedraw=TRUE)
    shade3d(shapelist3d(plot_data, plot = FALSE))
    par3d(skipRedraw=FALSE)

    # Plot axes
    if (axes == TRUE) {
      axes3d(edges = c("x", "y", "z"))
    }

    # Plot cloud
    if (!is.null(cloud)) {
      cloud <- rename(cloud, x = 1, y = 2, z = 3)

      # Initialize cloud inputs
      if (is.null(pt_color)) {
        pt_color <- "#000000"
      } else {
        pt_color <- pt_color
      }

      if (is.null(pt_size)) {
        pt_size <- 0.1
      } else {
        pt_size <- pt_size
      }

      # Plot cloud
      plot3d(cloud$x, cloud$y, cloud$z, col = pt_color, size = pt_size, add = TRUE, aspect = FALSE)
    }

    # Set mouse hover
    if (hover == TRUE) {
      hover3d(
        cylinder$startX,
        cylinder$startY,
        cylinder$startZ,
        labels = paste0("ID:", cylinder$ID, " - Branch:", cylinder$branchID)
      )
    }
  } else {
    message(
      "Invalid QSM or Cloud Supplied!!!
      \nMake sure the cylinder data frame and not the QSM list is supplied.
      \nMake sure the point cloud is a data frame with the first three columns as the x, y, and z coordinates."
    )
  }
}
