#' Run Real Twig
#'
#' @description Runs all Real Twig steps
#'
#' @param file file path to QSM (.mat, .csv, .json)
#' @param twig_radius Twig radius in millimeters
#' @param metrics Calculate tree metrics? Defaults to TRUE.
#' @param version Defaults to NULL. If using a specific version of TreeQSM, the user can specify the version (e.g. 2.4.1, 2.0, etc.).
#' @param smooth Defaults to TRUE, if using TreeQSM. Can be set to FALSE.
#' @param standardize Standardize QSM cylinder data? Defaults to FALSE. Can be set to TRUE.
#' @param broken_branch Enable or disable the broken branch filter. Defaults to TRUE.
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
#' ## SimpleForest
#' file <- system.file("extdata/QSM.csv", package = "rTwig")
#' qsm <- run_rtwig(file, twig_radius = 4.23)
#' str(qsm)
#'
run_rtwig <- function(
    file,
    twig_radius,
    metrics = TRUE,
    version = NULL,
    smooth = TRUE,
    standardize = FALSE,
    broken_branch = TRUE) {
  # Get file extension
  extension <- sub(".*\\.", "", basename(file))

  # TreeQSM --------------------------------------------------------------------
  if (extension == "mat") {
    # Import QSM ---------------------------------------------------------------
    if (!is.null(version)) {
      version <- version
      cylinder <- import_qsm(file, version)$cylinder
    } else {
      cylinder <- import_qsm(file)$cylinder
    }

    # Update Cylinders ---------------------------------------------------------
    cylinder <- update_cylinders(cylinder)

    # Smooth QSM ---------------------------------------------------------------
    if (smooth == TRUE) {
      cylinder <- smooth_qsm(cylinder)
    }

    # Standardize QSM ----------------------------------------------------------
    if (standardize == TRUE) {
      cylinder <- standardize_qsm(cylinder)
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

    # Standardize QSM ----------------------------------------------------------
    if (standardize == TRUE) {
      cylinder <- standardize_qsm(cylinder)
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

    # Standardize QSM ----------------------------------------------------------
    if (standardize == TRUE) {
      cylinder <- standardize_qsm(cylinder)
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
    message(
      "Unsupported QSM supplied!!!
      \nOnly TreeQSM (.mat), SimpleForest, aRchi (.csv), and Treegraph (.json) are supported."
    )
  }
}
