#' Import Leaves
#'
#' @description Imports leaves created by the QSM foliage and needles naive
#'  insertion (QSM-FaNNI) algorithm. The leaves are stored as a triangular mesh,
#'  compatible with both rTwig and RGL functions.
#'
#' @param filename a QSM-FaNNI .obj file
#' @param format .obj file format. Valid formats include `obj` and `obj_ext`.
#'  Defaults to `obj`.
#' @param simplify simplify the mesh by removing duplicate vertices. This can
#'  reduce memory usage at the cost of import speed. Defaults to FALSE.
#'
#' @return rgl::mesh3d object
#' @export
#'
#' @references
#' \insertRef{FaNNI}{rTwig}
#'
#' @examples
#' \dontrun{
#'
#' ## Import FaNNI leaves from the standard obj format
#' file <- "path_to_leaves.obj"
#' qsm <- import_leaves(file, format = "obj")
#'
#' ## Import FaNNI leaves from the extended obj format
#' file <- "path_to_leaves_extended.obj"
#' qsm <- import_leaves(file, format = "obj_ext")
#' }
#'
import_leaves <- function(filename, format = "obj", simplify = FALSE) {
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

  if (!file.exists(filename)) {
    message <- paste(
      "The file in `filename` does not exist.",
      "i Did you enter the correct path to your leaves?",
      sep = "\n"
    )
    abort(message, class = "file_error")
  }

  if (!is_string(format)) {
    message <- paste0(
      "`format` must be a string, not ", class(format), "."
    )
    abort(message, class = "invalid_argument")
  }

  if (!(format == "obj" || format == "obj_ext")) {
    message <- paste(
      "Invalid `format`!.",
      "i Valid formats include `obj` and `obj_ext`.",
      sep = "\n"
    )
    abort(message, class = "file_error")
  }

  # Get file extension
  extension <- sub(".*\\.", "", basename(filename))

  # Ensure filename ends with correct extension
  if (extension != "obj") {
    abort("`filename` must end in `.obj`.", class = "data_format_error")
  }

  if (!is_logical(simplify)) {
    message <- paste0(
      "`simplify` must be logical, not ", class(simplify), "."
    )
    abort(message, class = "invalid_argument")
  }

  inform("Importing Leaves")

  as.mesh3d(
    read_obj(filename, format),
    type = "triangles",
    smooth = FALSE,
    merge = simplify
  )
}
