#' Growth Length
#'
#' @description Calculates the growth length of each cylinder
#'
#' @details Growth length is the length of a parent cylinder, plus the lengths
#' of all of its child cylinders. It is a cumulative metric that was created by
#' Dr. Jan Hackenberg for use in the SimpleForest QSM software.
#'
#'
#' @param cylinder QSM cylinder data frame
#'
#' @return Returns a data frame
#' @export
#'
#' @import dplyr
#' @import tidyr
#' @import tibble
#' @rawNamespace import(igraph, except=c(union, as_data_frame, groups, crossing))
#'
#' @references {
#'   \insertRef{growth_parameter1}{rTwig}
#' }
#'
#' @examples
#' \dontrun{
#' ## TreeQSM Processing Chain
#' file <- system.file("extdata/QSM.mat", package = "rTwig")
#' qsm <- import_qsm(file)
#' cylinder <- qsm$cylinder
#' cylinder <- update_cylinders(cylinder)
#' cylinder <- growth_length(cylinder)
#' str(cylinder)
#'
#' ## SimpleForest Processing Chain
#' file <- system.file("extdata/QSM.csv", package = "rTwig")
#' cylinder <- read.csv(file)
#' cylinder <- update_cylinders(cylinder)
#' cylinder <- growth_length(cylinder)
#' str(cylinder)
#' }
growth_length <- function(cylinder) {
  message("Calculating Growth Length")

  # TreeQSM --------------------------------------------------------------------
  if (all(c("parent", "extension", "branch", "BranchOrder") %in% colnames(cylinder))) {
    g <- data.frame(parent = cylinder$parent, extension = cylinder$extension)
    g <- igraph::graph_from_data_frame(g) - 1
    g <- igraph::permute(g, match(igraph::V(g)$name, cylinder$extension))

    paths <- igraph::ego(g, order = igraph::vcount(g), mode = "out")

    GrowthLength <- paths %>%
      enframe() %>%
      unnest(cols = c(.data$value)) %>%
      rename(index = .data$name, extension = .data$value) %>%
      mutate_all(as.double) %>%
      left_join(cylinder %>%
        select(.data$extension, .data$length), by = "extension") %>%
      select(extension = .data$index, .data$length) %>%
      group_by(.data$extension) %>%
      summarize(GrowthLength = sum(.data$length, na.rm = TRUE))

    cylinder <- left_join(cylinder, GrowthLength, by = "extension")

  # SimpleForest ---------------------------------------------------------------
  } else if (all(c("ID", "parentID", "branchID", "branchOrder") %in% colnames(cylinder))) {
    cylinder <- cylinder %>%
      mutate(
        ID = .data$ID + 1,
        parentID = .data$parentID + 1
      )

    g <- data.frame(parent = cylinder$parentID, id = cylinder$ID)
    g <- igraph::graph_from_data_frame(g) - 1
    g <- igraph::permute(g, match(V(g)$name, cylinder$ID))

    paths <- igraph::ego(g, order = vcount(g), mode = "out")

    GrowthLength <- paths %>%
      enframe() %>%
      unnest(cols = c(.data$value)) %>%
      rename(index = .data$name, ID = .data$value) %>%
      mutate_all(as.double) %>%
      left_join(cylinder %>%
        select(.data$ID, .data$length), by = "ID") %>%
      select(ID = .data$index, .data$length) %>%
      group_by(.data$ID) %>%
      summarize(growthLength2 = sum(.data$length, na.rm = TRUE))

    cylinder <- left_join(cylinder, GrowthLength, by = "ID") %>%
      mutate(
        ID = .data$ID - 1,
        parentID = .data$parentID - 1
      ) %>%
      relocate(.data$growthLength2, .after = .data$growthLength)
  } else {
    message("Invalid Dataframe Supplied!!!\nOnly TreeQSM or SimpleForest QSMs are supported.")
  }
  return(cylinder)
}
