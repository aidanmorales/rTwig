#' Select Column
#'
#' @description Uses rlang to convert a variable to a string without evaluation.
#' It is used in conjunction with rlang::enquo() This allows the user to pass
#' quoted or unquoted column names to a function, making a variable selection
#' without the variable having to exist in the global environment. This makes
#' selection work with both traditional R strings and unquoted data frame
#' variables when using magrittr pipes.
#'
#' @param ... The column name either quoted or unquoted passed from rlang::enquo(...)
#'
#' @returns a string label or NULL
#' @noRd
#'
select_column <- function(...) {
  expr <- rlang::quo_get_expr(...)

  if (is.null(expr) | is.character(expr)) {
    return(expr)
  } else {
    rlang::as_label(expr)
  }
}

#' Builds QSM cylinder network with igraph
#' @param cylinder QSM cylinder data frame
#' @param id column name of parent cylinders
#' @param parent column name of parent cylinders
#' @param paths return only paths
#' @param pruning return only children
#' @param cache cache the network TRUE or FALSE
#' @returns cylinder network
#' @noRd
build_network <- function(
    cylinder,
    id,
    parent,
    paths = FALSE,
    pruning = FALSE,
    cache = TRUE) {
  inform("Building Cylinder Network")

  # Extract cylinder ids
  id <- pull(select(cylinder, {{ id }}))
  parent <- pull(select(cylinder, {{ parent }}))

  # Creates QSM cylinder network
  qsm_g <- tidytable(parent = parent, id = id)
  qsm_g <- igraph::graph_from_data_frame(qsm_g)

  # Find supported children
  child_g <- qsm_g - 1 # remove cylinder 0
  child_g <- igraph::permute(child_g, match(igraph::V(child_g)$name, id))
  child_g <- igraph::ego(child_g, order = igraph::vcount(child_g), mode = "out")

  child_id <- as.integer(unlist(child_g, FALSE, FALSE))
  child_index <- cumsum(duplicated(child_id) & !duplicated(child_id, fromLast = TRUE)) + 1
  child_df <- tidytable(index = child_index, id = child_id)

  if (pruning == TRUE) {
    if (cache == TRUE) {
      temp_file <- file.path(tempdir(), "network.rds")

      # Cache Network
      inform("Caching Network")
      saveRDS(child_df, temp_file)
    }

    return(child_df)
  }

  # Finds twigs cylinders
  twig_id_g <- igraph::V(qsm_g)[igraph::degree(qsm_g, mode = "out") == 0]
  twig_id_v <- as.integer(igraph::as_ids(twig_id_g))

  # Find all paths from base to twigs
  paths_g <- igraph::all_simple_paths(qsm_g, from = 1, to = twig_id_g)
  all_id <- as.integer(unlist(sapply(paths_g, igraph::as_ids), FALSE, FALSE))
  all_index <- cumsum(all_id == 0)
  all_df <- tidytable(index = all_index, id = all_id)

  if (paths == TRUE) {
    return(all_df)
  }

  # Find all paths from base to cylinder
  base_g <- igraph::all_simple_paths(qsm_g, from = 1, to = igraph::V(qsm_g))
  base_id <- as.integer(unlist(sapply(base_g, igraph::as_ids), FALSE, FALSE))
  base_index <- cumsum(base_id == 0)
  base_df <- tidytable(index = base_index, id = base_id)

  # Find number of paths cylinder occurs in and if it is a twig
  cylinder_info <- tabulate(all_id)
  cylinder_info <- tidytable(
    id = 1:length(cylinder_info),
    frequency = cylinder_info
  ) %>%
    mutate(twig = .data$id %in% !!twig_id_v) %>%
    select("id", "frequency", "twig")

  network <- list(
    qsm_g = qsm_g,
    child_df = child_df,
    all_df = all_df,
    base_df = base_df,
    cylinder_info = cylinder_info
  )

  if (cache == TRUE) {
    temp_file <- file.path(tempdir(), "network.rds")

    # Cache Network
    inform("Caching Network")
    saveRDS(network, temp_file)
  }

  return(network)
}

#' Verifies QSM cylinder network
#' @param cylinder QSM cylinder data frame
#' @param paths return only paths
#' @param pruning return only children
#' @returns cylinder network
#' @noRd
verify_network <- function(cylinder, pruning = FALSE, paths = FALSE) {
  temp_file <- file.path(tempdir(), "network.rds")

  if (pruning == TRUE) {
    # Import network from cache if it exists
    if (file.exists(temp_file)) {
      network <- readRDS(temp_file)

      if (length(network) == 5) {
        network <- network$child_df
      }

      # Check ids
      net_ids <- unique(network$id)
      cyl_ids <- unique(cylinder$id)

      # If ids mismatch, re-build the network, else return the existing network
      if (length(net_ids) != nrow(cylinder)) {
        build_network(cylinder, "id", "parent", pruning = TRUE, cache = TRUE)
      } else {
        return(network)
      }
    } else {
      # If network cache does not exist, build the network and cache it
      build_network(cylinder, "id", "parent", pruning = TRUE, cache = TRUE)
    }
  } else if (paths == TRUE) {
    # Import network from cache if it exists
    if (file.exists(temp_file)) {
      network <- readRDS(temp_file)

      if (length(network) == 5) {
        network$all_df
      } else {
        build_network(cylinder, "id", "parent", paths = TRUE)
      }
    } else {
      build_network(cylinder, "id", "parent", paths = TRUE)
    }
  }
}

#' Verify Cylinders
#' Checks if `update_cylinders()` has been run
#' @param cylinder QSM cylinder data frame
#' @param paths return only paths
#' @returns cylinder data frame
#' @noRd
verify_cylinders <- function(cylinder) {
  if (!all(c(
    "id", "parent", "branch_order", "reverse_order", "total_children",
    "growth_length"
  ) %in% colnames(cylinder))) {
    if (!("totalChildren" %in% colnames(cylinder))) {
      inform("Verifying Cylinders")

      cylinder <- suppressMessages(update_cylinders(cylinder))

      message <- paste(
        "Please run `update_cylinders()` before using rTwig functions.",
        "Running `update_cylinders()' will suppress this message.",
        sep = "\n"
      )
      inform(message)
    } else if ("GrowthLength" %in% colnames(cylinder)) {
      cylinder <- rename(cylinder, "growthLength" = "GrowthLength")

      message <- paste(
        "A legacy version of rTwig was detected (v1.0.x).",
        "Updating column names for compatibility.",
        sep = "\n"
      )
      inform(message)
    }
  }
  return(cylinder)
}
