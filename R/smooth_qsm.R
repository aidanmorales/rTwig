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

  # TreeQSM --------------------------------------------------------------------
  if (all(c("parent", "extension", "branch", "BranchOrder") %in% colnames(cylinder))) {
    # Verify cylinders
    cylinder <- verify_cylinders(cylinder)

    inform("Smoothing QSM")

    # Connect cylinder endpoints
    connect_cylinders(cylinder)

    return(cylinder)
  } else {
    message <- paste(
      "Unsupported QSM format provided.",
      "Only TreeQSM is supported in `smooth_qsm()`.",
      sep = "\n"
    )
    warn(message, class = "data_format_error")

    return(cylinder)
  }
}
