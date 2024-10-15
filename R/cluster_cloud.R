#' Cluster Cloud
#'
#' @description Cluster a point cloud or simulate a point cloud based on its QSM.
#' If using the input point cloud, the the cylinder ids are transferred to the
#' cloud using the nearest neighbors. If simulating a cloud, the points are built
#' directly from the cylinders with user controlled spacing. The cylinder id can
#' be used to easily join the desired variables from the QSM to the cloud.
#' The nearest neighbor search uses the C++ nanoflann library.
#'
#' @param cylinder A QSM cylinder data frame.
#' @param cloud The input point cloud for the QSM to cluster. If NULL (default), the simulated cloud is returned.
#' @param spacing The point spacing in meters for the simulated cloud. Defaults to 0.02 meters.
#'
#' @return A point cloud data frame
#' @export
#'
#' @references
#' \insertRef{blanco2014nanoflann}{rTwig}
#'
#' @examples
#'
#' ## Load Data
#' file <- system.file("extdata/QSM.mat", package = "rTwig")
#' file2 <- system.file("extdata/cloud.txt", package = "rTwig")
#'
#' qsm <- run_rtwig(file, twig_radius = 4.23, metrics = FALSE)
#' cloud <- read.table(file2)
#'
#' ## Clustered Cloud
#' clustered_cloud <- cluster_cloud(cylinder = qsm, cloud = cloud)
#'
#' # # Join QSM variables and export
#' # filename <- tempfile(pattern = "clustered_cloud", fileext = ".txt")
#' # clustered_cloud %>%
#' #   left_join(qsm) %>%
#' #   fwrite(file = filename)
#'
#' ## Simulated Cloud
#' simulated_cloud <- cluster_cloud(cylinder = qsm, spacing = 0.01)
#'
#' # Plot Simulated Cloud
#' plot_qsm(cloud = simulated_cloud)
#'
#' # # Join QSM variables and export
#' # filename2 <- tempfile(pattern = "simulated_cloud", fileext = ".txt")
#' # simulated_cloud %>%
#' #   left_join(qsm) %>%
#' #   fwrite(file = filename2)
#'
cluster_cloud <- function(cylinder, cloud = NULL, spacing = NULL) {
  # rTwig ----------------------------------------------------------------------
  if (all(c("id", "parent", "start_x", "branch_order") %in% colnames(cylinder))) {
    cluster_data(
      cylinder = cylinder, radius = "radius", length = "length", id = "id",
      start_x = "start_x", start_y = "start_y", start_z = "start_z",
      axis_x = "axis_x", axis_y = "axis_y", axis_z = "axis_z",
      cloud = cloud, spacing = spacing
    )
  }
  # TreeQSM --------------------------------------------------------------------
  else if (all(c("parent", "extension", "branch", "BranchOrder") %in% colnames(cylinder))) {
    cluster_data(
      cylinder = cylinder, radius = "radius", length = "length", id = "extension",
      start_x = "start.x", start_y = "start.y", start_z = "start.z",
      axis_x = "axis.x", axis_y = "axis.y", axis_z = "axis.z",
      cloud = cloud, spacing = spacing
    )
  }
  # SimpleForest ---------------------------------------------------------------
  else if (all(c("ID", "parentID", "branchID", "branchOrder") %in% colnames(cylinder))) {
    cluster_data(
      cylinder = cylinder, radius = "radius", length = "length", id = "ID",
      start_x = "startX", start_y = "startY", start_z = "startZ",
      axis_x = "axisX", axis_y = "axisY", axis_z = "axisZ",
      cloud = cloud, spacing = spacing
    )
  }
  # Treegraph ------------------------------------------------------------------
  else if (all(c("p1", "p2", "ninternode") %in% colnames(cylinder))) {
    cluster_data(
      cylinder = cylinder, radius = "radius", length = "length", id = "p1",
      start_x = "sx", start_y = "sy", start_z = "sz",
      axis_x = "ax", axis_y = "ay", axis_z = "az",
      cloud = cloud, spacing = spacing
    )
  }
  # aRchi ----------------------------------------------------------------------
  else if (all(c("cyl_ID", "parent_ID", "branching_order") %in% colnames(cylinder))) {
    cluster_data(
      cylinder = cylinder, radius = "radius_cyl", length = "length", id = "cyl_ID",
      start_x = "startX", start_y = "startY", start_z = "startZ",
      axis_x = "axisX", axis_y = "axisY", axis_z = "axisZ",
      cloud = cloud, spacing = spacing
    )
  } else {
    message(
      "Invalid QSM or Cloud Supplied!!!
      \nOnly TreeQSM, SimpleForest, Treegraph, or aRchi QSMs are supported.
      \nMake sure the cylinder data frame and not the QSM list is supplied.
      \nPlease supply either the input cloud, or spacing for a simulated cloud.
      \nMake sure the point cloud is a data frame or matrix with the first three columns as the x, y, and z coordinates."
    )
  }
}

#' Cluster data
#' @param cylinder QSM cylinder data frame
#' @param cloud point cloud data frame or matrix
#' @param id cylinder id
#' @param axis_x cylinder axis x
#' @param axis_y cylinder axis y
#' @param axis_z cylinder axis z
#' @param start_x cylinder start x
#' @param start_y cylinder start y
#' @param start_z cylinder start z
#' @param length cylinder length
#' @param radius cylinder radii
#' @param spacing point spacing in meters
#' @noRd
cluster_data <- function(
    cylinder,
    cloud,
    id,
    axis_x,
    axis_y,
    axis_z,
    start_x,
    start_y,
    start_z,
    length,
    radius,
    spacing) {
  # Select required variables --------------------------------------------------
  cylinder <- cylinder %>%
    select(
      id = {{ id }},
      radius = {{ radius }},
      length = {{ length }},
      start_x = {{ start_x }},
      start_y = {{ start_y }},
      start_z = {{ start_z }},
      axis_x = {{ axis_x }},
      axis_y = {{ axis_y }},
      axis_z = {{ axis_z }},
    )

  axis <- cbind(cylinder$axis_x, cylinder$axis_y, cylinder$axis_z)
  start <- cbind(cylinder$start_x, cylinder$start_y, cylinder$start_z)
  mat <- matrix() # Placeholder to satisfy generate_cloud matrix inputs

  # Simulated Cloud ------------------------------------------------------------
  if (is.null(cloud)) {
    message("Simulating Point Cloud")

    if (is.null(spacing)) {
      spacing <- 0.02
    }

    generate_cloud(
      start = start, axis = axis, tips = mat, length = cylinder$length,
      radius = cylinder$radius, branch = mat, metrics = FALSE, spacing = spacing
    ) %>%
      as_tidytable(cloud) %>%
      rename("x" = 1, "y" = 2, "z" = 3, {{ id }} := 4)
  }
  # Clustered Cloud ------------------------------------------------------------
  else {
    message("Clustering Point Cloud")

    cloud_ref <- generate_cloud(
      start = start, axis = axis, tips = mat, length = cylinder$length,
      radius = cylinder$radius, branch = mat, metrics = FALSE, spacing = 0.02
    )

    if (!is.null(cloud)) {
      if (!is.matrix(cloud)) {
        cloud <- as.matrix(cloud[, 1:3])
      }
    }

    cbind(cloud, assign_cloud_ids(cloud_ref, cloud)) %>%
      as_tidytable(cloud) %>%
      rename("x" = 1, "y" = 2, "z" = 3, {{ id }} := 4)
  }
}
