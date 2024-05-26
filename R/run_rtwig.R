#' Run Real Twig
#'
#' @description Runs all Real Twig steps
#'
#' @param file file path to QSM (.mat, .csv, .json)
#' @param twig_radius Twig radius in millimeters
#' @param backend Parallel backend for multi-core processing. Defaults to "multisession" (all platforms), but can be set to "multicore" (MacOS & Linux), "cluster" (all platforms), or a "package::backend" string.
#' @param version Defaults to NULL. If using a specific version of TreeQSM, the user can specify the version (e.g. 2.4.1, 2.0, etc.).
#' @param smooth Defaults to NULL. If using TreeQSM, set to TRUE to smooth the QSM.
#'
#' @return Returns a data frame
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
run_rtwig <- function(file, twig_radius, backend = "multisession", version = NULL, smooth = NULL) {
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
      qsm <- import_qsm(file)
    }

    # Update Cylinders ---------------------------------------------------------
    qsm$cylinder <- update_cylinders(qsm$cylinder)

    # Correct Radii ------------------------------------------------------------
    qsm$cylinder <- correct_radii(
      cylinder = qsm$cylinder,
      twig_radius = twig_radius,
      backend = backend
    )

    # Smooth QSM ---------------------------------------------------------------
    if(!is.null(smooth)){
      qsm$cylinder <- smooth_qsm(qsm$cylinder)
    }

    return(qsm)

  # SimpleForest ---------------------------------------------------------------
  } else if (extension == "csv") {

    # Import QSM ---------------------------------------------------------------
    qsm <- tidytable::fread(file)

    # Update Cylinders ---------------------------------------------------------
    qsm <- update_cylinders(qsm)

    # Correct Radii ------------------------------------------------------------
    qsm <- correct_radii(
      cylinder = qsm,
      twig_radius = twig_radius,
      backend = backend
    )

    return(qsm)

  # treegraph ------------------------------------------------------------------
  } else if (extension == "json") {

    # Import QSM ---------------------------------------------------------------
    qsm <- import_treegraph(file)

    # Update Cylinders ---------------------------------------------------------
    qsm$cyls <- update_cylinders(qsm$cyls)

    # Correct Radii ------------------------------------------------------------

    # IMPLEMENT LATER!!!

    # qsm$cyls <- correct_radii(
    #   cylinder = qsm$cyls,
    #   twig_radius = twig_radius,
    #   backend = backend
    # )

    return(qsm)

  } else {
    message(
      "Unsupported QSM supplied!!!
      \nOnly TreeQSM (.mat), SimpleForest (.mat), and treegraph (.json) are supported."
    )
  }
}
