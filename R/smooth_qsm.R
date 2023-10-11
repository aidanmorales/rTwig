#' Smooth QSM
#'
#' @description Visual smoothing of a QSM by ensuring the midpoints of all cylinders are connected
#'
#' @param cylinder QSM cylinder data frame
#'
#' @return Returns a data frame
#' @export
#'
#' @import dplyr
#'
#' @examples
#' \dontrun{
#' ## TreeQSM Smoothing
#' file <- system.file("extdata/QSM.mat", package = "rTwig")
#' qsm <- import_qsm(file)
#' cylinder <- qsm$cylinder
#' cylinder <- update_cylinders(cylinder)
#' cylinder <- growth_length(cylinder)
#' cylinder <- correct_radii(cylinder, twigRad = 1.5)
#'
#' ## Before Smoothing
#' plot_qsm(cylinder)
#'
#' ## After Smoothing
#' cylinder <- smooth_qsm(cylinder)
#' plot_qsm(cylinder)
#' }
smooth_qsm <- function(cylinder) {
  message("Smoothing QSM")

  if (all(c("parent", "extension", "branch", "BranchOrder") %in% colnames(cylinder))) {
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
    message("Invalid QSM Supplied!!!\nOnly TreeQSM is supported for smoothing.")
  }
  return(cylinder)
}
