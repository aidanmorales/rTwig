#' Import Treegraph
#'
#' @description Imports a QSM created by treegraph
#'
#' @param file a treegraph .json file
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
import_treegraph <- function(file) {
  message("Importing treegraph .json")

  # Import treegraph qsm from json
  qsm <- RcppSimdJson::fload(file)

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
  qsm <- list(
    name = name,
    created = created,
    tree = tree,
    internode = internode,
    node = node,
    cyls = cyls,
    centres = centres
  )

  return(qsm)
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
