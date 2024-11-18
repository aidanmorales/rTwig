#' Export MAT
#'
#' @description Exports the cylinder data to be visualized with TreeQSM's `plot_cylinder_model()`.
#'
#' @param cylinder QSM cylinder data frame
#' @param filename Desired name of file
#'
#' @return Returns a .mat file
#' @export
#'
#' @examples
#'
#' ## TreeQSM Processing Chain
#' file <- system.file("extdata/QSM.mat", package = "rTwig")
#' qsm <- import_qsm(file)
#' cylinder <- qsm$cylinder
#' cylinder <- update_cylinders(cylinder)
#'
#' filename <- tempfile(pattern = "TreeQSM_QSM", fileext = ".mat")
#' export_mat(cylinder, filename)
#'
#' ## SimpleForest Processing Chain
#' file <- system.file("extdata/QSM.csv", package = "rTwig")
#' cylinder <- read.csv(file)
#' cylinder <- update_cylinders(cylinder)
#'
#' filename <- tempfile(pattern = "SimpleForest_QSM", fileext = ".mat")
#' export_mat(cylinder, filename)
#'
export_mat <- function(cylinder, filename) {
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

  if (is_missing(filename)) {
    message <- "argument `filename` is missing, with no default."
    abort(message, class = "missing_argument")
  }

  if (!is_string(filename)) {
    message <- paste0(
      "`filename` must be a string, not ", class(filename), "."
    )
    abort(message, class = "invalid_argument")
  }

  # Ensure filename ends with correct extension
  if (substr(filename, nchar(filename) - 3, nchar(filename)) != ".mat") {
    filename <- paste0(filename, ".mat")
  }

  # Verify cylinders
  cylinder <- verify_cylinders(cylinder)

  inform("Exporting to .mat")

  # rTwig ----------------------------------------------------------------------
  if (all(c("id", "parent", "start_x", "branch_order") %in% colnames(cylinder))) {
    radius <- as.matrix(cylinder$radius)
    length <- as.matrix(cylinder$length)

    start <- cylinder %>%
      select(start.x = "start_x", start.y = "start_y", start.z = "start_z") %>%
      as.matrix()

    axis <- cylinder %>%
      select(axis.x = "axis_x", axis.y = "axis_y", axis.z = "axis_z") %>%
      as.matrix()

    parent <- as.matrix(cylinder$parent)
    extension <- as.matrix(cylinder$id)
    added <- NA
    UnmodRadius <- as.matrix(cylinder$raw_radius)
    branch <- as.matrix(cylinder$branch)
    SurfCov <- NA
    mad <- NA
    BranchOrder <- as.matrix(cylinder$branch_order)
    PositionInBranch <- as.matrix(cylinder$branch_position)

    output <- list(
      radius = radius,
      length = length,
      start = start,
      axis = axis,
      parent = parent,
      extension = extension,
      added = added,
      UnmodRadius = UnmodRadius,
      branch = branch,
      SurfCov = SurfCov,
      mad = mad,
      BranchOrder = BranchOrder,
      PositionInBranch = PositionInBranch
    )

    R.matlab::writeMat(filename, cylinder = output)
  }
  # TreeQSM --------------------------------------------------------------------
  else if (all(c("parent", "extension", "branch", "BranchOrder") %in% colnames(cylinder))) {
    radius <- as.matrix(cylinder$radius)
    length <- as.matrix(cylinder$length)

    start <- cylinder %>%
      select("start.x", "start.y", "start.z") %>%
      as.matrix()

    axis <- cylinder %>%
      select("axis.x", "axis.y", "axis.z") %>%
      as.matrix()

    parent <- as.matrix(cylinder$parent)
    extension <- as.matrix(cylinder$extension)
    added <- as.matrix(cylinder$added)
    UnmodRadius <- as.matrix(cylinder$UnmodRadius)
    branch <- as.matrix(cylinder$branch)
    BranchOrder <- as.matrix(cylinder$BranchOrder)
    PositionInBranch <- as.matrix(cylinder$PositionInBranch)

    # Checks for columns only in TreeQSM v2.4.0 and up
    if (all(c("SurfCov", "mad") %in% colnames(cylinder))) {
      SurfCov <- as.matrix(cylinder$SurfCov)
      mad <- as.matrix(cylinder$mad)

      output <- list(
        radius = radius,
        length = length,
        start = start,
        axis = axis,
        parent = parent,
        extension = extension,
        added = added,
        UnmodRadius = UnmodRadius,
        branch = branch,
        SurfCov = SurfCov,
        mad = mad,
        BranchOrder = BranchOrder,
        PositionInBranch = PositionInBranch
      )

      R.matlab::writeMat(filename, cylinder = output)
    } else {
      SurfCov <- NA
      mad <- NA

      output <- list(
        radius = radius,
        length = length,
        start = start,
        axis = axis,
        parent = parent,
        extension = extension,
        added = added,
        UnmodRadius = UnmodRadius,
        branch = branch,
        SurfCov = SurfCov,
        mad = mad,
        BranchOrder = BranchOrder,
        PositionInBranch = PositionInBranch
      )

      R.matlab::writeMat(filename, cylinder = output)
    }
  }
  # SimpleForest ---------------------------------------------------------------
  else if (all(c("ID", "parentID", "branchID", "branchOrder") %in% colnames(cylinder))) {
    radius <- as.matrix(cylinder$radius)
    length <- as.matrix(cylinder$length)

    start <- cylinder %>%
      select(start.x = "startX", start.y = "startY", start.z = "startZ") %>%
      as.matrix()

    axis <- cylinder %>%
      select(axis.x = "axisX", axis.y = "axisY", axis.z = "axisZ") %>%
      as.matrix()

    parent <- as.matrix(cylinder$parentID)
    extension <- as.matrix(cylinder$ID)
    added <- NA
    UnmodRadius <- as.matrix(cylinder$UnmodRadius)
    branch <- as.matrix(cylinder$branchID)
    SurfCov <- NA
    mad <- as.matrix(cylinder$averagePointDistance)
    BranchOrder <- as.matrix(cylinder$branchOrder)
    PositionInBranch <- as.matrix(cylinder$positionInBranch)

    output <- list(
      radius = radius,
      length = length,
      start = start,
      axis = axis,
      parent = parent,
      extension = extension,
      added = added,
      UnmodRadius = UnmodRadius,
      branch = branch,
      SurfCov = SurfCov,
      mad = mad,
      BranchOrder = BranchOrder,
      PositionInBranch = PositionInBranch
    )

    R.matlab::writeMat(filename, cylinder = output)
  }
  # Treegraph ------------------------------------------------------------------
  else if (all(c("p1", "p2", "ninternode") %in% colnames(cylinder))) {
    radius <- as.matrix(cylinder$radius)
    length <- as.matrix(cylinder$length)

    start <- cylinder %>%
      select(start.x = "sx", start.y = "sy", start.z = "sz") %>%
      as.matrix()

    axis <- cylinder %>%
      select(axis.x = "ax", axis.y = "ay", axis.z = "az") %>%
      as.matrix()

    parent <- as.matrix(cylinder$p2)
    extension <- as.matrix(cylinder$p1)
    added <- NA
    UnmodRadius <- as.matrix(cylinder$UnmodRadius)
    branch <- as.matrix(cylinder$nbranch)
    SurfCov <- NA
    mad <- NA
    BranchOrder <- as.matrix(cylinder$branch_order)
    PositionInBranch <- as.matrix(cylinder$positionInBranch)

    output <- list(
      radius = radius,
      length = length,
      start = start,
      axis = axis,
      parent = parent,
      extension = extension,
      added = added,
      UnmodRadius = UnmodRadius,
      branch = branch,
      SurfCov = SurfCov,
      mad = mad,
      BranchOrder = BranchOrder,
      PositionInBranch = PositionInBranch
    )

    R.matlab::writeMat(filename, cylinder = output)
  }
  # aRchi ----------------------------------------------------------------------
  else if (all(c("cyl_ID", "parent_ID", "branching_order") %in% colnames(cylinder))) {
    radius <- as.matrix(cylinder$radius_cyl)
    length <- as.matrix(cylinder$length)

    start <- cylinder %>%
      select(start.x = "startX", start.y = "startY", start.z = "startZ") %>%
      as.matrix()

    axis <- cylinder %>%
      select(axis.x = "axisX", axis.y = "axisY", axis.z = "axisZ") %>%
      as.matrix()

    parent <- as.matrix(cylinder$parent_ID)
    extension <- as.matrix(cylinder$cyl_ID)
    added <- NA
    UnmodRadius <- as.matrix(cylinder$UnmodRadius)
    branch <- as.matrix(cylinder$branch_ID)
    SurfCov <- NA
    mad <- NA
    BranchOrder <- as.matrix(cylinder$branching_order)
    PositionInBranch <- as.matrix(cylinder$positionInBranch)

    output <- list(
      radius = radius,
      length = length,
      start = start,
      axis = axis,
      parent = parent,
      extension = extension,
      added = added,
      UnmodRadius = UnmodRadius,
      branch = branch,
      SurfCov = SurfCov,
      mad = mad,
      BranchOrder = BranchOrder,
      PositionInBranch = PositionInBranch
    )

    R.matlab::writeMat(filename, cylinder = output)
  } else {
    message <- paste(
      "Unsupported QSM format provided.",
      "i Only TreeQSM, SimpleForest, Treegraph, or aRchi QSMs are supported.",
      sep = "\n"
    )
    abort(message, class = "data_format_error")
  }
}
