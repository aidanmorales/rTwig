#' Import AdQSM
#'
#' @description Imports a QSM created by AdQSM
#'
#' @param filename an AdQSM .obj file
#'
#' @return Returns a data frame
#' @export
#'
#' @references
#' \insertRef{fan2020adqsm}{rTwig}
#'
#' @examples
#' \dontrun{
#'
#' # Import an AdQSM
#' qsm <- import_adqsm("path/to/obj/file")
#' }
#'
import_adqsm <- function(filename) {
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
      "i Did you enter the correct path to your QSM?",
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

  # Import QSM -----------------------------------------------------------------
  inform("Importing AdQSM")

  # Import vertices from obj file
  vertices <- read_obj(filename, format = "obj") %>%
    unique() %>%
    as_tidytable() %>%
    rename(x = 1, y = 2, z = 3)

  # Build basic QSM cylinders from vertices and reconstruct the entire QSM
  build_adqsm(vertices) %>%
  reconstruct_qsm(
    branch = "branch", id = "id", parent = "parent", radius = "radius",
    start_x = "start_x", start_y = "start_y", start_z = "start_z",
    end_x = "end_x", end_y = "end_y", end_z = "end_z"
  )
}
