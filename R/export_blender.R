#' Export Blender
#'
#' @description Exports a QSM in the QSM Blender Addons format
#'
#' @param cylinder QSM cylinder data frame
#' @param filename File name and path for exporting. The .txt extension is automatically added if not present.
#'
#' @return A .txt file
#' @export
#'
#' @examples
#'
#' file <- system.file("extdata/QSM.mat", package = "rTwig")
#' qsm <- run_rtwig(file, twig_radius = 4.23, metrics = FALSE, smooth = TRUE)
#'
#' filename <- tempfile(pattern = "qsm_blender")
#' export_blender(qsm, filename)
#'
export_blender <- function(cylinder, filename) {
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

  # Ensure filename ends with correct extension
  if (substr(filename, nchar(filename) - 3, nchar(filename)) != ".txt") {
    filename <- paste0(filename, ".txt")
  }

  # Verify cylinders
  cylinder <- verify_cylinders(cylinder)

  # rTwig ----------------------------------------------------------------------
  if (all(c("id", "parent", "start_x", "branch_order") %in% colnames(cylinder))) {
    select(
      cylinder,
      branch = "branch",
      start_x = "start_x", start_y = "start_y", start_z = "start_z",
      axis_x = "axis_x", axis_y = "axis_y", axis_z = "axis_z",
      length = "length",
      radius = "radius"
    ) %>%
      fwrite(
        file = filename,
        col.names = FALSE,
        row.names = FALSE,
        sep = " "
      )
  }
  # TreeQSM --------------------------------------------------------------------
  else if (all(c("parent", "extension", "branch", "BranchOrder") %in% colnames(cylinder))) {
    select(
      cylinder,
      branch = "branch",
      start_x = "start.x", start_y = "start.y", start_z = "start.z",
      axis_x = "axis.x", axis_y = "axis.y", axis_z = "axis.z",
      length = "length",
      radius = "radius"
    ) %>%
      fwrite(
        file = filename,
        col.names = FALSE,
        row.names = FALSE,
        sep = " "
      )
  }
  # SimpleForest ---------------------------------------------------------------
  else if (all(c("ID", "parentID", "branchID", "branchOrder") %in% colnames(cylinder))) {
    select(
      cylinder,
      branch = "branchID",
      start_x = "startX", start_y = "startY", start_z = "startZ",
      axis_x = "axisX", axis_y = "axisY", axis_z = "axisZ",
      length = "length",
      radius = "radius"
    ) %>%
      fwrite(
        file = filename,
        col.names = FALSE,
        row.names = FALSE,
        sep = " "
      )
  }
  # Treegraph ------------------------------------------------------------------
  else if (all(c("p1", "p2", "ninternode") %in% colnames(cylinder))) {
    select(
      cylinder,
      branch = "nbranch",
      start_x = "sx", start_y = "sy", start_z = "sz",
      axis_x = "ax", axis_y = "ay", axis_z = "az",
      length = "length",
      radius = "radius"
    ) %>%
      fwrite(
        file = filename,
        col.names = FALSE,
        row.names = FALSE,
        sep = " "
      )
  }
  # aRchi ----------------------------------------------------------------------
  else if (all(c("cyl_ID", "parent_ID", "branching_order") %in% colnames(cylinder))) {
    select(
      cylinder,
      branch = "branch_ID",
      start_x = "startX", start_y = "startY", start_z = "startZ",
      axis_x = "axisX", axis_y = "axisY", axis_z = "axisZ",
      length = "length",
      radius = "radius_cyl"
    ) %>%
      fwrite(
        file = filename,
        col.names = FALSE,
        row.names = FALSE,
        sep = " "
      )
  } else {
    message <- paste(
      "Unsupported QSM format provided.",
      "i Only TreeQSM, SimpleForest, Treegraph, or aRchi QSMs are supported.",
      sep = "\n"
    )
    abort(message, class = "data_format_error")
  }
}
