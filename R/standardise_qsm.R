#' Standardise QSM
#'
#' @description All QSM variables are renamed and reordered a standardised
#'  format across the supported QSM software for a consistent experience.
#'  All internal rTwig functions use these standardised names for consistency.
#'
#' @details Renames supported QSM software output columns to be consistent.
#'  All names are lower case and underscore delimited. See the dictionary
#'  vignette for a detailed description of column names. A consistent QSM format
#'  ensures maximum compatibility when analyzing QSMs made with different
#'  software. This function can be run either before or after
#'  `update_cylinders()` has been run, or at any stage.
#'
#' `standardise_qsm()` and `standardise_qsm()` are synonyms.
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
#' cylinder <- standardise_qsm(cylinder)
#' str(cylinder)
#'
#' ## SimpleForest Processing Chain
#' file <- system.file("extdata/QSM.csv", package = "rTwig")
#' cylinder <- read.csv(file)
#' cylinder <- standardise_qsm(cylinder)
#' str(cylinder)
#'
#' ## aRchi Processing Chain
#' file <- system.file("extdata/QSM2.csv", package = "rTwig")
#' cylinder <- read.csv(file)
#' cylinder <- standardise_qsm(cylinder)
#' str(cylinder)
#'
standardise_qsm <- function(cylinder) {
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

  # Detect format and define columns -------------------------------------------
  qsm_format <- detect_format(cylinder)

  if (is.null(qsm_format) || qsm_format == "rtwig") {
    message <- paste(
      "Unsupported QSM format provided.",
      "i Only TreeQSM, SimpleForest, Treegraph, or aRchi QSMs are supported.",
      sep = "\n"
    )
    abort(message, class = "data_format_error")
  }

  cols <- unlist(define_columns(qsm_format), use.names = TRUE)

  # Only modified is optional; retain the existing column order.
  if (!cols[["modified"]] %in% colnames(cylinder)) {
    cols <- cols[names(cols) != "modified"]
  }

  select(cylinder, all_of(cols))
}

#' @rdname standardise_qsm
#' @export
standardize_qsm <- standardise_qsm
