#' Smooth QSM
#'
#' @description
#' Visual smoothing of a QSM by ensuring the midpoints of all branch cylinders are connected.
#'
#' @param df QSM cylinder data frame
#' @param method QSM type, as either "TreeQSM" or "SimpleForest". Only TreeQSM is currently implemented and is the default.
#'
#' @return Returns a data.frame
#' @export
#'
#' @import dplyr
#'
#' @examples
#' \dontrun{
#' ## TreeQSM Smoothing
#' file <- system.file("extdata/QSM.mat", package = "rTwig")
#' df <- import_qsm(file)
#' df <- update_cylinders(df)
#' df <- growth_length(df)
#' df <- correct_radii(df, twigRad = 0.003)
#'
#' ## Before Smoothing
#' plot_qsm(df)
#'
#' ## After Smoothing
#' df <- smooth_qsm(df)
#' plot_qsm(df)
#' }
smooth_qsm <- function(df, method = "TreeQSM") {
  if (method == "TreeQSM") {
    df <- df %>%
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
      # mutate(
      #   start.x = case_when(PositionInBranch > 1 & totChildren == 1 ~ lag(end.x, 1), TRUE ~ start.x),
      #   start.y = case_when(PositionInBranch > 1 & totChildren == 1 ~ lag(end.y, 1), TRUE ~ start.y),
      #   start.z = case_when(PositionInBranch > 1 & totChildren == 1 ~ lag(end.z, 1), TRUE ~ start.z),
      #   end.x = case_when(PositionInBranch > 1 & PositionInBranch < max(PositionInBranch) & totChildren == 1 ~ lead(start.x, 1), TRUE ~ end.x),
      #   end.y = case_when(PositionInBranch > 1 & PositionInBranch < max(PositionInBranch) & totChildren == 1 ~ lead(start.y, 1), TRUE ~ end.y),
      #   end.z = case_when(PositionInBranch > 1 & PositionInBranch < max(PositionInBranch) & totChildren == 1 ~ lead(start.z, 1), TRUE ~ end.z)
      # ) %>%
      # mutate(
      #   start.x = case_when(PositionInBranch > 1 ~ lag(end.x, 1), TRUE ~ start.x),
      #   start.y = case_when(PositionInBranch > 1 ~ lag(end.y, 1), TRUE ~ start.y),
      #   start.z = case_when(PositionInBranch > 1 ~ lag(end.z, 1), TRUE ~ start.z) # ,
      #   # end.x = case_when(PositionInBranch > 1 & PositionInBranch < max(PositionInBranch) ~ lead(start.x, 1), TRUE ~ end.x),
      #   # end.y = case_when(PositionInBranch > 1 & PositionInBranch < max(PositionInBranch) ~ lead(start.y, 1), TRUE ~ end.y),
      #   # end.z = case_when(PositionInBranch > 1 & PositionInBranch < max(PositionInBranch) ~ lead(start.z, 1), TRUE ~ end.z)
      # ) %>%
      ungroup()
  }
  return(df)
}
