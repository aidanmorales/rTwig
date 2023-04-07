#' Growth Length
#'
#' @description Calculates the growth length of each cylinder.
#'
#' @param df QSM cylinder data.frame
#' @param method QSM type, as either "TreeQSM" or "SimpleForest". Defaults to TreeQSM.
#'
#' @return Returns a data.frame
#' @export
#'
#' @import dplyr
#' @import tidyr
#' @import tibble
#' @rawNamespace import(igraph, except=c(union, as_data_frame, groups, crossing))
#'
#' @examples
#' ## TreeQSM Processing Chain
#' file <- system.file("extdata/QSM.mat", package = "rTwig")
#' df <- import_qsm(file)
#' df <- update_cylinders(df)
#' df <- growth_length(df)
#' str(df)
#'
#' ## SimpleForest Processing Chain
#' file <- system.file("extdata/QSM.csv", package = "rTwig")
#' df <- read.csv(file)
#' df <- update_cylinders(df, method = "SimpleForest")
#' df <- growth_length(df, method = "SimpleForest")
#' str(df)
growth_length <- function(df, method = "TreeQSM") {
  message("Calculating Growth Length")

  if (method == "TreeQSM") {
    g <- data.frame(parent = df$parent, id = df$id)
    g <- igraph::graph_from_data_frame(g) - 1
    g <- igraph::permute(g, match(igraph::V(g)$name, df$id))

    paths <- igraph::ego(g, order = igraph::vcount(g), mode = "out")

    GrowthLength <- paths %>%
      enframe() %>%
      unnest(cols = c(.data$value)) %>%
      rename(index = .data$name, id = .data$value) %>%
      mutate_all(as.double) %>%
      left_join(df %>%
        select(.data$id, .data$length), by = "id") %>%
      select(id = .data$index, .data$length) %>%
      group_by(.data$id) %>%
      summarize(GrowthLength = sum(.data$length, na.rm = TRUE))

    df <- left_join(df, GrowthLength, by = "id")
  } else if (method == "SimpleForest") {
    df <- df %>%
      mutate(
        ID = .data$ID + 1,
        parentID = .data$parentID + 1
      )

    g <- data.frame(parent = df$parentID, id = df$ID)
    g <- igraph::graph_from_data_frame(g) - 1
    g <- igraph::permute(g, match(V(g)$name, df$ID))

    paths <- igraph::ego(g, order = vcount(g), mode = "out")

    GrowthLength <- paths %>%
      enframe() %>%
      unnest(cols = c(.data$value)) %>%
      rename(index = .data$name, ID = .data$value) %>%
      mutate_all(as.double) %>%
      left_join(df %>%
        select(.data$ID, .data$length), by = "ID") %>%
      select(ID = .data$index, .data$length) %>%
      group_by(.data$ID) %>%
      summarize(growthLength2 = sum(.data$length, na.rm = TRUE))

    df <- left_join(df, GrowthLength, by = "ID") %>%
      mutate(
        ID = .data$ID - 1,
        parentID = .data$parentID - 1
      ) %>%
      relocate(.data$growthLength2, .after = .data$growthLength)
  } else {
    message("Invalid Method Entered!!!\nValid Methods = TreeQSM or SimpleForest")
  }
  return(df)
}
