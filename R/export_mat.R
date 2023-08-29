#' Export MAT
#'
#' @description Exports the cylinder data to be visualized with TreeQSM's plot_cylinder_model() function.
#'
#' @param df QSM cylinder data frame
#' @param filename Desired name of file
#'
#' @return Returns a .mat file
#' @export
#'
#' @import dplyr
#' @importFrom R.matlab writeMat
#'
#' @examples
#' \dontrun{
#' ## TreeQSM Processing Chain
#' df <- import_qsm("foo.mat")
#' df <- update_cylinders(df)
#' df <- growth_length(df)
#' df <- correct_radii(df, twigRad = 0.003)
#' export_mat(df, "foo_corrected.mat")
#'
#' ## SimpleForest Processing Chain
#' df <- read.csv("foo.csv")
#' df <- update_cylinders(df)
#' df <- growth_length(df)
#' df <- correct_radii(df, twigRad = 0.003)
#' export_mat(df, "foo_corrected.mat")
#' }
export_mat <- function(df, filename) {
  message("Exporting to .mat")

  if (all(c("parent", "extension", "branch", "BranchOrder") %in% colnames(df))) {
    radius <- as.matrix(df$radius)
    length <- as.matrix(df$length)

    start <- df %>%
      select(.data$start.x, .data$start.y, .data$start.z) %>%
      as.matrix()

    axis <- df %>%
      select(.data$axis.x, .data$axis.y, .data$axis.z) %>%
      as.matrix()

    parent <- as.matrix(df$parent)
    extension <- as.matrix(df$extension)
    added <- as.matrix(df$added)
    UnmodRadius <- as.matrix(df$UnmodRadius)
    branch <- as.matrix(df$branch)
    BranchOrder <- as.matrix(df$BranchOrder)
    PositionInBranch <- as.matrix(df$PositionInBranch)

    # Checks for columns only in TreeQSM v2.4.0 and up
    if (all(c("SurfCov", "mad") %in% colnames(df))) {
      SurfCov <- as.matrix(df$SurfCov)
      mad <- as.matrix(df$mad)

      R.matlab::writeMat(filename,
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
    } else {
      SurfCov <- NA
      mad <- NA

      R.matlab::writeMat(filename,
        radius = radius,
        length = length,
        start = start,
        axis = axis,
        parent = parent,
        extension = extension,
        added = added,
        UnmodRadius = UnmodRadius,
        branch = branch,
        BranchOrder = BranchOrder,
        PositionInBranch = PositionInBranch
      )
    }
  } else if (all(c("ID", "parentID", "branchID", "branchOrder") %in% colnames(df))) {
    radius <- as.matrix(df$radius)
    length <- as.matrix(df$length)

    start <- df %>%
      select(start.x = .data$startX, start.y = .data$startY, start.z = .data$startZ) %>%
      as.matrix()

    axis <- df %>%
      select(axis.x = .data$axisX, axis.y = .data$axisY, axis.z = .data$axisZ) %>%
      as.matrix()

    parent <- as.matrix(df$parentID)
    extension <- as.matrix(df$ID)
    added <- NA
    UnmodRadius <- as.matrix(df$UnmodRadius)
    branch <- as.matrix(df$branchID)
    SurfCov <- NA
    mad <- as.matrix(df$averagePointDistance)
    BranchOrder <- as.matrix(df$branchOrder)
    PositionInBranch <- as.matrix(df %>% group_by(.data$branchID) %>%
      reframe(PositionInBranch = 1:n()) %>%
      ungroup() %>%
      select(.data$PositionInBranch) %>%
      pull())

    R.matlab::writeMat(filename,
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
  } else {
    message("Invalid Dataframe Supplied!!!\nOnly TreeQSM or SimpleForest QSMs are supported.")
  }
}
