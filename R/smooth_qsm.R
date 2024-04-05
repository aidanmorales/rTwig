#' Smooth QSM
#'
#' @description Visual smoothing of a QSM by ensuring the midpoints of all cylinders are connected
#'
#' @param cylinder QSM cylinder data frame
#'
#' @return Returns a data frame
#' @export
#'
#' @rawNamespace import(tidytable, except=c(map_dfr))
#'
#' @examples
#' \donttest{
#'
#' file <- system.file("extdata/QSM.mat", package = "rTwig")
#' qsm <- import_qsm(file)
#' cylinder <- qsm$cylinder
#' cylinder <- update_cylinders(cylinder)
#'
#' ## Before Smoothing
#' plot_qsm(cylinder)
#'
#' ## After Smoothing
#' cylinder <- smooth_qsm(cylinder)
#' plot_qsm(cylinder)
#'
#' }
#'
smooth_qsm <- function(cylinder) {
  message("Smoothing QSM")

  if (all(c("parent", "extension", "branch", "BranchOrder") %in% colnames(cylinder))) {
    # Error message if cylinders have not been updated
    stopifnot("Cylinder indexes have not been updated! Please run update_cylinders() before proceeding." = pull(slice_head(cylinder, n = 1),.data$extension) == 1)

    cylinder <- cylinder %>%
      group_by(.data$branch) %>%
      mutate(
        start.x = case_when(.data$PositionInBranch > 1 ~ lag(.data$end.x, 1), TRUE ~ .data$start.x),
        start.y = case_when(.data$PositionInBranch > 1 ~ lag(.data$end.y, 1), TRUE ~ .data$start.y),
        start.z = case_when(.data$PositionInBranch > 1 ~ lag(.data$end.z, 1), TRUE ~ .data$start.z),
        end.x = case_when(.data$PositionInBranch > 1 & .data$PositionInBranch < max(.data$PositionInBranch) ~ (lead(.data$start.x, 1) + .data$end.x) / 2, TRUE ~ .data$end.x),
        end.y = case_when(.data$PositionInBranch > 1 & .data$PositionInBranch < max(.data$PositionInBranch) ~ (lead(.data$start.y, 1) + .data$end.y) / 2, TRUE ~ .data$end.y),
        end.z = case_when(.data$PositionInBranch > 1 & .data$PositionInBranch < max(.data$PositionInBranch) ~ (lead(.data$start.z, 1) + .data$end.z) / 2, TRUE ~ .data$end.z),
        axis.x = (.data$end.x - .data$start.x) / .data$length,
        axis.y = (.data$end.y - .data$start.y) / .data$length,
        axis.z = (.data$end.z - .data$start.z) / .data$length
      ) %>%
      ungroup()
  } else {
    message(
      "Invalid Dataframe Supplied!!!
      \nOnly TreeQSM is supported for smoothing.
      \nMake sure the cylinder data frame and not the QSM list is supplied."
    )
  }
  return(cylinder)
}
