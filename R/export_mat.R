#' Export MAT
#'
#' @description Exports the cylinder data to be visualized with TreeQSM's plot_cylinder_model() function
#'
#' @param cylinder QSM cylinder data frame
#' @param filename Desired name of file
#'
#' @return Returns a .mat file
#' @export
#'
#' @import dplyr
#' @importFrom Matrix as.matrix
#' @importFrom R.matlab writeMat
#'
#' @examples
#' \dontrun{
#' ## TreeQSM Processing Chain
#' qsm <- import_qsm("foo.mat")
#' cylinder <- qsm$cylinder
#' cylinder <- update_cylinders(cylinder)
#' cylinder <- growth_length(cylinder)
#' cylinder <- correct_radii(cylinder, twigRad = 1.5)
#' export_mat(cylinder, "foo_corrected.mat")
#'
#' ## SimpleForest Processing Chain
#' cylinder <- read.csv("foo.csv")
#' cylinder <- update_cylinders(cylinder)
#' cylinder <- growth_length(cylinder)
#' cylinder <- correct_radii(cylinder, twigRad = 1.5)
#' export_mat(cylinder, "foo_corrected.mat")
#' }
export_mat <- function(cylinder, filename) {
  message("Exporting to .mat")

  # TreeQSM --------------------------------------------------------------------
  if (all(c("parent", "extension", "branch", "BranchOrder") %in% colnames(cylinder))) {
    radius <- as.matrix(cylinder$radius)
    length <- as.matrix(cylinder$length)

    start <- cylinder %>%
      select(.data$start.x, .data$start.y, .data$start.z) %>%
      as.matrix()

    axis <- cylinder %>%
      select(.data$axis.x, .data$axis.y, .data$axis.z) %>%
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

  # SimpleForest ---------------------------------------------------------------
  } else if (all(c("ID", "parentID", "branchID", "branchOrder") %in% colnames(cylinder))) {
    radius <- as.matrix(cylinder$radius)
    length <- as.matrix(cylinder$length)

    start <- cylinder %>%
      select(start.x = .data$startX, start.y = .data$startY, start.z = .data$startZ) %>%
      as.matrix()

    axis <- cylinder %>%
      select(axis.x = .data$axisX, axis.y = .data$axisY, axis.z = .data$axisZ) %>%
      as.matrix()

    parent <- as.matrix(cylinder$parentID)
    extension <- as.matrix(cylinder$ID)
    added <- NA
    UnmodRadius <- as.matrix(cylinder$UnmodRadius)
    branch <- as.matrix(cylinder$branchID)
    SurfCov <- NA
    mad <- as.matrix(cylinder$averagePointDistance)
    BranchOrder <- as.matrix(cylinder$branchOrder)
    PositionInBranch <- as.matrix(cylinder %>% group_by(.data$branchID) %>%
      reframe(PositionInBranch = 1:n()) %>%
      ungroup() %>%
      select(.data$PositionInBranch) %>%
      pull())

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
    message(
      "Invalid Dataframe Supplied!!!
      \nOnly TreeQSM or SimpleForest QSMs are supported.
      \nMake sure the cylinder data frame and not the QSM list is supplied."
    )
  }
}