#' Import Treegraph
#'
#' @description Imports a QSM created by treegraph
#'
#' @param filename a treegraph .json file
#'
#' @return Returns a list
#' @export
#'
#' @references
#' \insertRef{treegraph2}{rTwig}
#'
#' \insertRef{treegraph1}{rTwig}
#'
#' @examples
#' \dontrun{
#'
#' # Import a treegraph QSM
#' qsm <- import_treegraph("path/to/json/file")
#' }
#'
import_treegraph <- function(filename) {
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
  if (extension != "json") {
    abort("`filename` must end in `.json`.", class = "data_format_error")
  }

  # Import QSM -----------------------------------------------------------------
  inform("Importing Treegraph")

  # Import treegraph qsm from json
  qsm <- RcppSimdJson::fload(filename)

  # Parse json strings
  tree <- RcppSimdJson::fparse(qsm$tree)
  internode <- RcppSimdJson::fparse(qsm$internode)
  node <- RcppSimdJson::fparse(qsm$node)
  cyls <- RcppSimdJson::fparse(qsm$cyls)
  centres <- RcppSimdJson::fparse(qsm$centres)

  # Convert list sublists into a data frame
  name <- qsm$name
  created <- qsm$created
  tree <- flatten_json_list(tree)
  internode <- flatten_json_list(internode)
  node <- flatten_json_list(node)
  cyls <- flatten_json_list(cyls)
  centres <- flatten_json_list(centres)
  centres$centre_id <- 0:(nrow(centres) - 1) # fix null list values
  centres$idx <- 0:(nrow(centres) - 1) # fix null list values

  # Store all QSM info as a list
  list(
    name = name,
    created = created,
    tree = tree,
    internode = internode,
    node = node,
    cyls = cyls,
    centres = centres
  )
}

#' Flattens sublists of json files into vectors and combines into a data frame
#' @param list a list created from a json file
#' @returns returns a list
#' @noRd
flatten_json_list <- function(list) {
  # Extract names and values from the master list
  names <- names(list)
  values <- lapply(list, unlist)

  # Maximum length of the sublists
  max_length <- max(lengths(values))

  # Ensure all lists are the same length
  padded_values <- lapply(values, function(x) c(x, rep(NA, max_length - length(x))))

  # Combine all vectors into data frame
  df <- as_tidytable(padded_values)
  colnames(df) <- names

  return(df)
}
