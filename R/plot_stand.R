#' Plot Stand
#'
#' @description Plots multiple QSMs in a forest stand using the rgl library
#'
#' @param qsms A list of QSM cylinder data frames.
#' @param radius_type Radius type as either "modified", "unmodified", or "old". Defaults to "modified".
#' @param qsm_colors Optional qsm color parameters. Colors vector of hex colors with the same length as the qsms list. Defaults to distinct random colors.
#' @param cyl_sides The number of sides in the polygon cross section. Defaults to 8, but can be increased to improve visual smoothness.
#' @param clouds A list of point cloud data frames where the first three columns are the x, y, and z coordinates in the same coordinate system as the QSMs.
#' @param pt_colors A vector of hex colors. Defaults to the same random color as the QSM.
#' @param pt_sizes Size of the points as a number. Defaults to 0.1.
#' @param axes Show plot axes. Defaults to TRUE.
#'
#' @return A rgl QSM plot
#' @export
#'
#' @import randomcoloR
#' @import rgl
#'
#' @examples
#' \donttest{
#'
#' # Import QSMs
#' file1 <- system.file("extdata/QSM.mat", package = "rTwig")
#' qsm <- import_qsm(file1)
#' cylinder <- qsm$cylinder
#' cylinder <- update_cylinders(cylinder)
#'
#' # Create a list of QSM cylinder data frames
#' qsms <- list(cylinder)
#'
#' # Optionally import and create a list of point clouds
#' file2 <- system.file("extdata/cloud.txt", package = "rTwig")
#' cloud <- read.table(file2, header = FALSE)
#' clouds <- list(cloud)
#'
#' # Plot QSMs and clouds
#' plot_stand(qsms = qsms, clouds = clouds)
#'
#' }
#'
plot_stand <- function(qsms, radius_type = "modified", qsm_colors = NULL, cyl_sides = 8, clouds = NULL, pt_colors = NULL, pt_sizes = NULL, axes = TRUE) {
  # Number of QSMs or Clouds to plot
  n_qsms <- length(qsms)
  n_clouds <- length(clouds)

  # Unique colors
  palette <- distinctColorPalette(n_qsms)

  # Start RGL Plot

  open3d()
  par3d(skipRedraw=TRUE)

  # Plot QSMs
  for (j in 1:length(qsms)) {
    message(paste("Plotting Tree", j, "of", n_qsms, sep = " "))

    cylinder <- qsms[[j]]

    # TreeQSM ------------------------------------------------------------------
    if (all(c("parent", "extension", "branch", "BranchOrder") %in% colnames(cylinder))) {
      # Setup Radius
      if (radius_type == "modified") {
        cyl_radius <- cylinder$radius
      } else if (radius_type == "unmodified") {
        cyl_radius <- cylinder$UnmodRadius
      } else if (radius_type == "old") {
        cyl_radius <- cylinder$OldRadius
      }

      # Setup QSM Colors
      if (is.null(qsm_colors)) {
        colors <- rep(palette[j], nrow(cylinder))
      } else {
        if (length(qsm_colors) != length(qsms)) {
          stop("Supplied QSM colors vector is not equal to the number of QSMs!")
        }
        colors <- rep(qsm_colors[j], nrow(cylinder))
      }

      # Create RGL cylinders
      plot_data <- lapply(1:nrow(cylinder), function(i) {
        cyl <- cylinder3d(
          center = cbind(
            c(cylinder$start.x[i], cylinder$end.x[i]),
            c(cylinder$start.y[i], cylinder$end.y[i]),
            c(cylinder$start.z[i], cylinder$end.z[i])
          ),
          radius = cyl_radius[i],
          sides = cyl_sides,
          closed = 0,
          rotationMinimizing = TRUE
        )
        cyl$material$color <- colors[i]
        cyl
      })

      shapelist3d(plot_data, plot = TRUE)

      # SimpleForest -------------------------------------------------------------
    } else if (all(c("ID", "parentID", "branchID", "branchOrder") %in% colnames(cylinder))) {
      # Setup Radius
      if (radius_type == "modified") {
        cyl_radius <- cylinder$radius
      } else if (radius_type == "unmodified") {
        cyl_radius <- cylinder$UnmodRadius
      } else if (radius_type == "old") {
        cyl_radius <- cylinder$OldRadius
      }

      # Setup QSM Colors
      if (is.null(qsm_colors)) {
        colors <- rep(palette[j], nrow(cylinder))
      } else {
        if (length(qsm_colors) != length(qsms)) {
          stop("Supplied QSM colors vector is not equal to the number of QSMs!")
        }
        colors <- rep(qsm_colors[j], nrow(cylinder))
      }

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

      shapelist3d(plot_data, plot = TRUE)
    }
  }

  par3d(skipRedraw=FALSE)

  # Plot Clouds
  for (j in 1:length(clouds)) {
    message(paste("Plotting Cloud", j, "of", n_qsms, sep = " "))

    # Plot cloud
    if (!is.null(clouds)) {
      cloud <- clouds[[j]]
      cloud <- rename(cloud, x = 1, y = 2, z = 3)

      # Initialize cloud inputs
      if (is.null(pt_colors)) {
        pt_color <- palette[j]
      } else {
        pt_color <- pt_colors[j]
      }

      if (is.null(pt_sizes)) {
        pt_sizes <- 0.1
      } else {
        pt_sizes <- pt_sizes
      }

      # Plot cloud
      plot3d(cloud$x, cloud$y, cloud$z, col = pt_color, size = pt_sizes, add = TRUE, aspect = FALSE)
    }
  }

  # Plot Axes
  if (axes == TRUE) {
    axes3d(edges = c("x", "y", "z"))
  }
}
