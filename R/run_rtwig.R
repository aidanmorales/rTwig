#' Run Real Twig
#'
#' @description Run the Real Twig correction and calculate tree metrics for
#'  supported QSM formats.
#'
#' @param filename file path to QSM (.mat, .csv, .json)
#' @param twig_radius Twig radius in millimeters
#' @param metrics Calculate tree metrics. Defaults to TRUE.
#'
#' @param version If using a specific version of TreeQSM, the user can specify
#'  the version (e.g. 2.4.1, 2.0, etc.).
#'
#' @param smooth Defaults to TRUE if using TreeQSM. Can be set to FALSE.
#'
#' @param standardise Standardise QSM cylinder data.
#' Defaults to FALSE, but can be set to TRUE.
#'
#' @param broken_branch Enable or disable the broken branch filter.
#'  Defaults to TRUE.
#'
#' @param ... Additional arguments allowing standardise and standardize to be
#'  used as synonyms.
#'
#' @return Returns cylinder data frame or list if metrics is true.
#' @export
#'
#' @examples
#'
#' ## TreeQSM
#' file <- system.file("extdata/QSM.mat", package = "rTwig")
#' qsm <- run_rtwig(file, twig_radius = 4.23)
#' str(qsm$cylinder)
#'
run_rtwig <- function(
    filename,
    twig_radius,
    metrics = TRUE,
    version = NULL,
    smooth = TRUE,
    standardise = FALSE,
    broken_branch = TRUE,
    ...) {
  # Check inputs ---------------------------------------------------------------
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

  if (is_missing(twig_radius)) {
    message <- "argument `twig_radius` is missing, with no default."
    abort(message, class = "missing_argument")
  }

  if (!is_null(twig_radius)) {
    if (!is_scalar_double(twig_radius)) {
      message <- paste0(
        "`twig_radius` must be double, not ", class(twig_radius), "."
      )
      abort(message, class = "invalid_argument")
    }
  }

  if (!is_logical(metrics)) {
    message <- paste0(
      "`metrics` must be logical, not ", class(metrics), "."
    )
    abort(message, class = "invalid_argument")
  }

  if (!is_null(version)) {
    if (!is_string(version)) {
      message <- paste0(
        "`filename` must be a string, not ", class(version), "."
      )
      abort(message, class = "invalid_argument")
    }
  }

  if (!is_logical(smooth)) {
    message <- paste0(
      "`smooth` must be logical, not ", class(smooth), "."
    )
    abort(message, class = "invalid_argument")
  }

  # Ensure standardise and standardize are synonyms
  args <- as.list(match.call())[-1]

  # Define synonyms mapping
  synonyms <- list(standardise = c("standardise", "standardize"))

  # Normalize arguments
  for (key in names(synonyms)) {
    for (syn in synonyms[[key]]) {
      if (syn %in% names(args)) {
        args[[key]] <- args[[syn]]
        args[[syn]] <- NULL
      }
    }
  }

  # Assign normalized argument
  if (!is.null(args$standardise)) standardise <- args$standardise

  if (!is_logical(standardise)) {
    message <- paste0(
      "`standardise` must be logical, not ", class(standardise), "."
    )
    abort(message, class = "invalid_argument")
  }

  if (!is_logical(broken_branch)) {
    message <- paste0(
      "`broken_branch` must be logical, not ", class(broken_branch), "."
    )
    abort(message, class = "invalid_argument")
  }

  # Get file extension
  extension <- sub(".*\\.", "", basename(filename))
  file <- filename # file must be re-defined internally pass string checks

  # TreeQSM --------------------------------------------------------------------
  if (extension == "mat") {
    # Import QSM ---------------------------------------------------------------
    if (!is.null(version)) {
      version <- version
      cylinder <- import_treeqsm(file, version)$cylinder
    } else {
      cylinder <- import_treeqsm(file)$cylinder
    }

    # Update Cylinders ---------------------------------------------------------
    cylinder <- update_cylinders(cylinder)

    # Smooth QSM ---------------------------------------------------------------
    if (smooth == TRUE) {
      cylinder <- smooth_qsm(cylinder)
    }

    # Standardise QSM ----------------------------------------------------------
    if (standardise == TRUE) {
      cylinder <- standardise_qsm(cylinder)
    }

    # Correct Radii ------------------------------------------------------------
    cylinder <- correct_radii(
      cylinder = cylinder,
      twig_radius = twig_radius,
      broken_branch = broken_branch
    )

    # Tree Metrics -------------------------------------------------------------
    if (metrics == TRUE) {
      metrics <- tree_metrics(cylinder)
      return(list(cylinder = cylinder, metrics = metrics))
    } else {
      return(cylinder)
    }
  }
  # SimpleForest & aRchi -------------------------------------------------------
  else if (extension == "csv") {
    # Import QSM ---------------------------------------------------------------
    cylinder <- tidytable::fread(file)

    # Update Cylinders ---------------------------------------------------------
    cylinder <- update_cylinders(cylinder)

    # Standardise QSM ----------------------------------------------------------
    if (standardise == TRUE) {
      cylinder <- standardise_qsm(cylinder)
    }

    # Correct Radii ------------------------------------------------------------
    cylinder <- correct_radii(
      cylinder = cylinder,
      twig_radius = twig_radius,
      broken_branch = broken_branch
    )

    # Tree Metrics -------------------------------------------------------------
    if (metrics == TRUE) {
      metrics <- tree_metrics(cylinder)
      return(list(cylinder = cylinder, metrics = metrics))
    } else {
      return(cylinder)
    }
  }
  # Treegraph ------------------------------------------------------------------
  else if (extension == "json") {
    # Import QSM ---------------------------------------------------------------
    cylinder <- import_treegraph(file)$cyls

    # Update Cylinders ---------------------------------------------------------
    cylinder <- update_cylinders(cylinder)

    # Standardise QSM ----------------------------------------------------------
    if (standardise == TRUE) {
      cylinder <- standardise_qsm(cylinder)
    }

    # Correct Radii ------------------------------------------------------------
    cylinder <- correct_radii(
      cylinder = cylinder,
      twig_radius = twig_radius,
      broken_branch = broken_branch
    )

    # Tree Metrics -------------------------------------------------------------
    if (metrics == TRUE) {
      metrics <- tree_metrics(cylinder)
      return(list(cylinder = cylinder, metrics = metrics))
    } else {
      return(cylinder)
    }
  } else {
    message <- paste(
      "Unsupported QSM format provided.",
      "i Only TreeQSM, SimpleForest, Treegraph, or aRchi QSMs are supported.",
      sep = "\n"
    )
    abort(message, class = "data_format_error")
  }
}
