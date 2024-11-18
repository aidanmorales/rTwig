#' Standardize QSM
#'
#' @description Standardizes QSM variable names and ordering across different QSM software
#'
#' @details Renames supported QSM software output columns to be consistent.
#' All names are lower case and underscore delimited. See the dictionary
#' vignette for a detailed description of column names. A consistent QSM format
#' ensures maximum compatibility when analyzing QSMs made with different
#' software. This function can be run either before or after
#' `update_cylinders()` has been run, or at any stage.
#'
#' @param cylinder QSM cylinder data frame
#'
#' @return Returns a data frame
#' @export
#'
#' @examples
#'
#' ## TreeQSM Processing Chain
#' file <- system.file("extdata/QSM.mat", package = "rTwig")
#' qsm <- import_qsm(file)
#' cylinder <- qsm$cylinder
#' cylinder <- standardize_qsm(cylinder)
#' str(cylinder)
#'
#' ## SimpleForest Processing Chain
#' file <- system.file("extdata/QSM.csv", package = "rTwig")
#' cylinder <- read.csv(file)
#' cylinder <- standardize_qsm(cylinder)
#' str(cylinder)
#'
#' ## aRchi Processing Chain
#' file <- system.file("extdata/QSM2.csv", package = "rTwig")
#' cylinder <- read.csv(file)
#' cylinder <- standardize_qsm(cylinder)
#' str(cylinder)
#'
standardize_qsm <- function(cylinder) {
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

  # Verify cylinders
  cylinder <- verify_cylinders(cylinder)

  # TreeQSM --------------------------------------------------------------------
  if (all(c("parent", "extension", "branch", "BranchOrder") %in% colnames(cylinder))) {
    select(cylinder,
      start_x = "start.x", start_y = "start.y", start_z = "start.z",
      axis_x = "axis.x", axis_y = "axis.y", axis_z = "axis.z",
      end_x = "end.x", end_y = "end.y", end_z = "end.z",
      id = "extension", parent = "parent", radius = "radius",
      raw_radius = "UnmodRadius", modified = any_of("modified"),
      length = "length", branch = "branch",
      branch_position = "PositionInBranch",
      branch_order = "BranchOrder", reverse_order = "reverseBranchOrder",
      branch_alt = "branch_alt",
      segment = "segment", parent_segment = "parentSegment",
      total_children = "totalChildren", growth_length = "growthLength",
      base_distance = "distanceFromBase", twig_distance = "distanceToTwig",
      vessel_volume = "vesselVolume",
      pipe_area = "reversePipeAreaBranchorder",
      pipe_radius = "reversePipeRadiusBranchorder"
    )
  }
  # SimpleForest ---------------------------------------------------------------
  else if (all(c("ID", "parentID", "branchID", "branchOrder") %in% colnames(cylinder))) {
    select(cylinder,
      start_x = "startX", start_y = "startY", start_z = "startZ",
      axis_x = "axisX", axis_y = "axisY", axis_z = "axisZ",
      end_x = "endX", end_y = "endY", end_z = "endZ",
      id = "ID", parent = "parentID", radius = "radius",
      raw_radius = "UnmodRadius", modified = any_of("modified"),
      length = "length", branch = "branchID",
      branch_position = "positionInBranch",
      branch_order = "branchOrder", reverse_order = "reverseBranchOrder",
      branch_alt = "branch_alt",
      segment = "segmentID", parent_segment = "parentSegmentID",
      total_children = "totalChildren", growth_length = "growthLength",
      base_distance = "distanceFromBase", twig_distance = "distanceToTwig",
      vessel_volume = "vesselVolume",
      pipe_area = "reversePipeAreaBranchorder",
      pipe_radius = "reversePipeRadiusBranchorder"
    )
  }
  # Treegraph ------------------------------------------------------------------
  else if (all(c("p1", "p2", "ninternode") %in% colnames(cylinder))) {
    select(cylinder,
      start_x = "sx", start_y = "sy", start_z = "sz",
      axis_x = "ax", axis_y = "ay", axis_z = "az",
      end_x = "ex", end_y = "ey", end_z = "ez",
      id = "p1", parent = "p2", radius = "radius",
      raw_radius = "UnmodRadius", modified = any_of("modified"),
      length = "length", branch = "nbranch",
      branch_position = "positionInBranch",
      branch_order = "branch_order", reverse_order = "reverseBranchOrder",
      branch_alt = "branch_alt",
      segment = "segment", parent_segment = "parentSegment",
      total_children = "totalChildren", growth_length = "growthLength",
      base_distance = "distanceFromBase", twig_distance = "distanceToTwig",
      vessel_volume = "vesselVolume",
      pipe_area = "reversePipeAreaBranchorder",
      pipe_radius = "reversePipeRadiusBranchorder"
    )
  }
  # aRchi ----------------------------------------------------------------------
  else if (all(c("cyl_ID", "parent_ID", "branching_order") %in% colnames(cylinder))) {
    select(cylinder,
      start_x = "startX", start_y = "startY", start_z = "startZ",
      axis_x = "axisX", axis_y = "axisY", axis_z = "axisZ",
      end_x = "endX", end_y = "endY", end_z = "endZ",
      id = "cyl_ID", parent = "parent_ID", radius = "radius_cyl",
      raw_radius = "UnmodRadius", modified = any_of("modified"),
      length = "length", branch = "branch_ID",
      branch_position = "positionInBranch",
      branch_order = "branching_order", reverse_order = "reverseBranchOrder",
      branch_alt = "branch_alt",
      segment = "segment", parent_segment = "parentSegment",
      total_children = "totalChildren", growth_length = "growthLength",
      base_distance = "distanceFromBase", twig_distance = "distanceToTwig",
      vessel_volume = "vesselVolume",
      pipe_area = "reversePipeAreaBranchorder",
      pipe_radius = "reversePipeRadiusBranchorder"
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
