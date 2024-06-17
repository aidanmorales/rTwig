#' Run Real Twig
#'
#' @description Runs all Real Twig steps
#'
#' @param file file path to QSM (.mat, .csv, .json)
#' @param twig_radius Twig radius in millimeters
#' @param metrics Calculate tree metrics? Defaults to TRUE.
#' @param backend Parallel backend for multi-core processing. Defaults to "multisession" (all platforms), but can be set to "multicore" (MacOS & Linux), "cluster" (all platforms), or a "package::backend" string.
#' @param version Defaults to NULL. If using a specific version of TreeQSM, the user can specify the version (e.g. 2.4.1, 2.0, etc.).
#' @param smooth Defaults to NULL. If using TreeQSM, set to TRUE to smooth the QSM.
#'
#' @return Returns cylinder data frame or list if metrics is true.
#' @export
#'
#' @examples
#' \donttest{
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
#' }
#'
run_rtwig <- function(
    file,
    twig_radius,
    backend = "multisession",
    metrics = TRUE,
    version = NULL,
    smooth = NULL) {
  # Get file extension
  extension <- sub(".*\\.", "", basename(file))

  # Get parallel backend
  backend <- backend

  # Get twig radius
  twig_radius <- twig_radius

  # TreeQSM --------------------------------------------------------------------
  if (extension == "mat") {
    # Import QSM ---------------------------------------------------------------
    if (!is.null(version)) {
      version <- version
      qsm <- import_qsm(file, version)
    } else {
      cylinder <- import_qsm(file)$cylinder
    }

    # Update Cylinders ---------------------------------------------------------
    cylinder <- update_cylinders(cylinder)

    # Correct Radii ------------------------------------------------------------
    cylinder <- correct_radii(
      cylinder = cylinder,
      twig_radius = twig_radius,
      backend = backend
    )

    # Smooth QSM ---------------------------------------------------------------
    if (!is.null(smooth)) {
      cylinder <- smooth_qsm(cylinder)
    }

    # Tree Metrics -------------------------------------------------------------
    if (metrics == TRUE) {
      metrics <- tree_metrics(cylinder)
      return(list(cylinder = cylinder, metrics = metrics))
    } else{
      return(cylinder)
    }

  # SimpleForest ---------------------------------------------------------------
  } else if (extension == "csv") {
    # Import QSM ---------------------------------------------------------------
    cylinder <- tidytable::fread(file)

    # Update Cylinders ---------------------------------------------------------
    cylinder <- update_cylinders(cylinder)

    # Correct Radii ------------------------------------------------------------
    cylinder <- correct_radii(
      cylinder = cylinder,
      twig_radius = twig_radius,
      backend = backend
    )

    # Tree Metrics -------------------------------------------------------------
    if (metrics == TRUE) {
      metrics <- tree_metrics(cylinder)
      return(list(cylinder = cylinder, metrics = metrics))
    } else{
      return(cylinder)
    }

  # Treegraph ------------------------------------------------------------------
  } else if (extension == "json") {
    # Import QSM ---------------------------------------------------------------
    cylinder <- import_treegraph(file)$cyls

    # Update Cylinders ---------------------------------------------------------
    cylinder <- update_cylinders(cylinder)

    # Correct Radii ------------------------------------------------------------
    cylinder <- correct_radii(
      cylinder = cylinder,
      twig_radius = twig_radius,
      backend = backend
    )

    # Tree Metrics -------------------------------------------------------------
    if (metrics == TRUE) {
      metrics <- tree_metrics(cylinder)
      return(list(cylinder = cylinder, metrics = metrics))
    } else{
      return(cylinder)
    }

  } else {
    message(
      "Unsupported QSM supplied!!!
      \nOnly TreeQSM (.mat), SimpleForest (.mat), and Treegraph (.json) are supported."
    )
  }
}
