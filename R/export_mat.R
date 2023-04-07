#' Export MAT
#'
#' @param df QSM cylinder data frame
#' @param filename Desired name of file
#' @param method QSM type, as either "TreeQSM" or "SimpleForest". Defaults to TreeQSM.
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
#' df <- update_cylinders(df, method = "SimpleForest")
#' df <- growth_length(df, method = "SimpleForest")
#' df <- correct_radii(df, twigRad = 0.003, method = "SimpleForest")
#' export_mat(df, "foo_corrected.mat", method = "SimpleForest")
#' }
export_mat <- function(df, filename, method = "TreeQSM") {
  message("Exporting to .mat")
  if (method == "TreeQSM") {
    radius <- as.matrix(df$radius)
    length <- as.matrix(df$length)

    start <- df %>%
      select(.data$start.x, .data$start.y, .data$start.z) %>%
      as.matrix()

    axis <- df %>%
      select(.data$axis.x, .data$axis.y, .data$axis.z) %>%
      as.matrix()

    parent <- as.matrix(df$parent)
    extension <- as.matrix(df$id)
    added <- as.matrix(df$added)
    UnmodRadius <- as.matrix(df$UnmodRadius)
    branch <- as.matrix(df$branch)
    SurfCov <- as.matrix(df$SurfCov)
    mad <- as.matrix(df$mad)
    BranchOrder <- as.matrix(df$BranchOrder)
    PositionInBranch <- as.matrix(df$PositionInBranch)

    writeMat(filename,
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
  } else if (method == "SimpleForest") {
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
      summarize(PositionInBranch = 1:n()) %>%
      select(.data$PositionInBranch) %>%
      ungroup() %>%
      pull())

    writeMat(filename,
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
    message("Invalid Method Entered!!!\nValid Methods = TreeQSM or SimpleForest")
  }
}
