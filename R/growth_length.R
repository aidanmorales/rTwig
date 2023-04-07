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
    g <- data.frame(parent = df$parent, id = df$id) %>%
      igraph::graph_from_data_frame() - 1

    g <- permute(g, match(igraph::V(g)$name, df$id))

    paths <- g %>%
      igraph::ego(order = igraph::vcount(g), mode = "out")

    GrowthLength <- paths %>%
      enframe() %>%
      unnest(cols = c(value)) %>%
      rename("index" = name, id = "value") %>%
      mutate_all(as.double) %>%
      left_join(df %>%
                  select(id, length), by = "id") %>%
      select(id = index, length) %>%
      group_by(id) %>%
      summarize(GrowthLength = sum(length, na.rm = TRUE))

    df <- left_join(df, GrowthLength, by = "id")
  } else if (method == "SimpleForest") {
    df <- df %>%
      mutate(
        ID = ID + 1,
        parentID = parentID + 1
      )

    g <- data.frame(parent = df$parentID, id = df$ID) %>%
      graph_from_data_frame() - 1

    g <- permute(g, match(V(g)$name, df$ID))

    paths <- g %>%
      ego(order = vcount(g), mode = "out")

    GrowthLength <- paths %>%
      enframe() %>%
      unnest(cols = c(value)) %>%
      rename("index" = name, ID = "value") %>%
      mutate_all(as.double) %>%
      left_join(df %>%
                  select(ID, length), by = "ID") %>%
      select(ID = index, length) %>%
      group_by(ID) %>%
      summarize(growthLength2 = sum(length, na.rm = TRUE))

    df <- left_join(df, GrowthLength, by = "ID") %>%
      mutate(
        ID = ID - 1,
        parentID = parentID - 1
      ) %>%
      relocate(growthLength2, .after = growthLength)
  } else {
    message("Invalid Method Entered!!!\nValid Methods = TreeQSM or SimpleForest")
  }
  return(df)
}
