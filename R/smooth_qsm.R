#' Smooth QSM
#'
#' @description Visual smoothing of a QSM by ensuring the midpoints of all cylinders are connected
#'
#' @param cylinder QSM cylinder data frame
#'
#' @return Returns a data frame
#' @export
#'
#' @examples
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
smooth_qsm <- function(cylinder) {
  message("Smoothing QSM")

  if (all(c("parent", "extension", "branch", "BranchOrder") %in% colnames(cylinder))) {
    # Error message if cylinders have not been updated
    stopifnot("Cylinder indexes have not been updated! Please run update_cylinders() before proceeding." = pull(slice_head(cylinder, n = 1), .data$extension) == 1)

    # Connect cylinder endpoints
    connect_cylinders(cylinder)

  } else {
    message(
      "Invalid Dataframe Supplied!!!
      \nOnly TreeQSM is supported for smoothing.
      \nMake sure the cylinder data frame and not the QSM list is supplied."
    )
  }
  return(cylinder)
}
