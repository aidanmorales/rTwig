#' Plot QSM and Point Cloud
#'
#' @description Plots QSM cylinders and point cloud using the rgl library
#'
#' @param cylinder QSM cylinder data frame
#' @param cloud Point cloud data frame where the first three columns are the x, y, and z coordinates in the same coordinate system as the QSM.
#' @param radius Radius type, as either "modified" or "unmodified". Defaults to modified.
#' @param cyl_color Color QSM by either "BranchOrder" or "GrowthLength". Defaults to BranchOrder.
#' @param pt_color Color of the point cloud. Defaults to black.
#' @param pt_size Size of the points. Defaults to 0.1.
#'
#' @return A rgl QSM plot with overlayed point cloud
#' @export
#'
#' @import colourvalues
#' @import rgl
#'
#' @examples
#' \dontrun{
#' ## TreeQSM Processing Chain
#' file <- system.file("extdata/QSM.mat", package = "rTwig")
#' qsm <- import_qsm(file)
#' cylinder <- qsm$cylinder
#' cylinder <- update_cylinders(cylinder)
#' cylinder <- growth_length(cylinder)
#' plot_qsm_cloud(
#'   cylinder = cylinder,
#'   cloud = cloud,
#'   radius = "unmodified",
#'   cyl_color = "GrowthLength",
#'   pt_color = "blue",
#'   pt_size = 0.2
#' )
#'
#' ## SimpleForest Processing Chain
#' file <- system.file("extdata/QSM.csv", package = "rTwig")
#' cylinder <- read.csv(file)
#' cylinder <- update_cylinders(cylinder)
#' plot_qsm_cloud(
#'   cylinder = cylinder,
#'   cloud = cloud,
#'   radius = "unmodified",
#'   cyl_color = "GrowthLength",
#'   pt_color = "blue",
#'   pt_size = 0.2
#' )
#' }
plot_qsm_cloud <- function(cylinder, cloud, radius = "modified", cyl_color = "BranchOrder", pt_color = "black", pt_size = 0.1) {
  message("Plotting QSM")

  # TreeQSM --------------------------------------------------------------------
  if (all(c("parent", "extension", "branch", "BranchOrder") %in% colnames(cylinder))) {
    if (cyl_color == "GrowthLength") {
      colors <- colour_values(cylinder$GrowthLength, palette = "viridis")
    } else {
      colors <- color_values(cylinder$BranchOrder, palette = "rainbow")
    }

    if (radius == "modified") {
      radius <- cylinder$radius
    } else if (radius == "unmodified") {
      radius <- cylinder$UnmodRadius
    }

    plot_data <- lapply(1:nrow(cylinder), function(i) {
      cyl <- cylinder3d(
        center = cbind(
          c(cylinder$start.x[i], cylinder$end.x[i]),
          c(cylinder$start.y[i], cylinder$end.y[i]),
          c(cylinder$start.z[i], cylinder$end.z[i])
        ),
        radius = radius[i],
        sides = 100,
        closed = -1
      )
      cyl$material$color <- colors[i]
      cyl
    })

    open3d()
    shade3d(shapelist3d(plot_data, plot = FALSE))
    axes3d(edges = c("x", "y", "z"))

    cloud <- rename(cloud, x = 1, y = 2, z = 3)
    plot3d(cloud$x, cloud$y, cloud$z, col = pt_color, size = pt_size, add = TRUE)

  # SimpleForest ---------------------------------------------------------------
  } else if (all(c("ID", "parentID", "branchID", "branchOrder") %in% colnames(cylinder))) {
    if (cyl_color == "GrowthLength") {
      colors <- colour_values(cylinder$growthLength, palette = "viridis")
    } else {
      colors <- color_values(cylinder$branchOrder, palette = "rainbow")
    }

    if (radius == "modified") {
      radius <- cylinder$radius
    } else if (radius == "unmodified") {
      radius <- cylinder$UnmodRadius
    }

    plot_data <- lapply(1:nrow(cylinder), function(i) {
      cyl <- cylinder3d(
        center = cbind(
          c(cylinder$startX[i], cylinder$endX[i]),
          c(cylinder$startY[i], cylinder$endY[i]),
          c(cylinder$startZ[i], cylinder$endZ[i])
        ),
        radius = radius[i],
        sides = 100,
        closed = -1
      )
      cyl$material$color <- colors[i]
      cyl
    })

    open3d()
    shade3d(shapelist3d(plot_data, plot = FALSE))
    axes3d(edges = c("x", "y", "z"))

    cloud <- rename(cloud, x = 1, y = 2, z = 3)
    plot3d(cloud$x, cloud$y, cloud$z, col = pt_color, size = pt_size, add = TRUE)
  } else {
    message("Invalid QSM or Cloud Supplied!!!")
  }
}
