#' Update Cylinders
#'
#' @description Updates the QSM cylinder data in preparation for radii correction
#'
#' @details Updates parent-child branch and cylinder relationships to fill in any gaps.
#' Four useful QSM metrics developed by Jan Hackenberg are also calculated.
#' Growth length is the length of a parent cylinder, plus the lengths of all of
#' its child cylinders. The segment is a portion of a branch between two branching nodes.
#' The reverse branch order assigns twigs as order 1 and works backwards at each
#' branching junction to the base of the stem, which has the largest reverse branch order.
#' Distance from twig is the average distance to all connected twigs for a given cylinder.
#' Two new metrics, distance from base, and total children, are also calculated.
#'
#' @param cylinder QSM cylinder data frame
#'
#' @return Returns a data frame
#' @export
#'
#' @references
#' \insertRef{growth_parameter1}{rTwig}
#'
#' \insertRef{growth_parameter2}{rTwig}
#'
#' @examples
#'
#' ## TreeQSM Processing Chain
#' file <- system.file("extdata/QSM.mat", package = "rTwig")
#' qsm <- import_qsm(file)
#' cylinder <- qsm$cylinder
#' cylinder <- update_cylinders(cylinder)
#' str(cylinder)
#'
#' ## SimpleForest Processing Chain
#' file <- system.file("extdata/QSM.csv", package = "rTwig")
#' cylinder <- read.csv(file)
#' cylinder <- update_cylinders(cylinder)
#' str(cylinder)
#'
update_cylinders <- function(cylinder) {
  # TreeQSM --------------------------------------------------------------------
  if (all(c("parent", "extension", "branch", "BranchOrder") %in% colnames(cylinder))) {
    # Save All Radii -----------------------------------------------------------
    cylinder <- mutate(cylinder, OldRadius = .data$radius, radius = .data$UnmodRadius)

    # Fill Extension Gaps ------------------------------------------------------
    cylinder <- cylinder %>%
      mutate(extension = row_number()) %>%
      distinct(.keep_all = TRUE)

    # Branch Ordering ----------------------------------------------------------
    # Relabels branches consecutively
    branches <- unique(cylinder$branch)
    branch_id <- tidytable(
      branch = branches,
      branch_new = 1:length(branches)
    )

    # Updates branch ordering
    cylinder <- left_join(cylinder, branch_id, by = "branch") %>%
      select(-.data$branch) %>%
      rename("branch" = .data$branch_new)

    # Plotting Info ------------------------------------------------------------
    cylinder <- cylinder %>%
      mutate(
        end.x = .data$start.x + (.data$axis.x * .data$length),
        end.y = .data$start.y + (.data$axis.y * .data$length),
        end.z = .data$start.z + (.data$axis.z * .data$length)
      )

    # Cylinder Ordering --------------------------------------------------------
    cylinder <- arrange(cylinder, .data$extension)
    cylinder <- update_ordering(cylinder, "extension", "parent")

    # Total Children -----------------------------------------------------------
    cylinder <- total_children(cylinder, "parent")

    # Build QSM Cylinder Network  ----------------------------------------------
    network <- build_network(cylinder, "extension", "parent")

    # Growth Length ------------------------------------------------------------
    cylinder <- growth_length(network, cylinder, "extension", "length")

    # Reverse Branch Order -----------------------------------------------------
    cylinder <- reverse_branch_order(network, cylinder, "extension")

    # Branch Segments ----------------------------------------------------------
    cylinder <- branch_segments(cylinder, "extension", "parent", "branch", "reverseBranchOrder")

    # Path Metrics -------------------------------------------------------------
    cylinder <- path_metrics(network, cylinder, "extension", "length")

    # Organize Cylinders  ------------------------------------------------------

    if (all(c("SurfCov", "mad") %in% colnames(cylinder))) {
      cylinder <- cylinder %>%
        relocate(.data$OldRadius, .after = .data$UnmodRadius) %>%
        relocate(.data$reverseBranchOrder, .after = .data$BranchOrder) %>%
        relocate(.data$segment, .after = .data$PositionInBranch) %>%
        relocate(.data$parentSegment, .after = .data$segment) %>%
        relocate(.data$UnmodRadius:.data$mad, .after = .data$end.z) %>%
        relocate(.data$growthLength, .after = .data$mad) %>%
        relocate(.data$mad, .after = .data$end.z) %>%
        relocate(.data$SurfCov, .after = .data$mad) %>%
        relocate(.data$branch, .after = .data$growthLength) %>%
        relocate(.data$parent, .after = .data$branch) %>%
        relocate(.data$extension, .after = .data$parent) %>%
        relocate(.data$end.x, .after = .data$axis.z) %>%
        relocate(.data$end.y, .after = .data$end.x) %>%
        relocate(.data$end.z, .after = .data$end.y)
    } else {
      cylinder <- cylinder %>%
        relocate(.data$OldRadius, .after = .data$UnmodRadius) %>%
        relocate(.data$reverseBranchOrder, .after = .data$BranchOrder) %>%
        relocate(.data$segment, .after = .data$PositionInBranch) %>%
        relocate(.data$parentSegment, .after = .data$segment) %>%
        relocate(.data$UnmodRadius, .after = .data$end.z) %>%
        relocate(.data$OldRadius, .after = .data$UnmodRadius) %>%
        relocate(.data$growthLength, .after = .data$OldRadius) %>%
        relocate(.data$branch, .after = .data$growthLength) %>%
        relocate(.data$parent, .after = .data$branch) %>%
        relocate(.data$extension, .after = .data$parent) %>%
        relocate(.data$end.x, .after = .data$axis.z) %>%
        relocate(.data$end.y, .after = .data$end.x) %>%
        relocate(.data$end.z, .after = .data$end.y)
    }
  # SimpleForest ---------------------------------------------------------------
  } else if (all(c("ID", "parentID", "branchID", "branchOrder") %in% colnames(cylinder))) {
    # Adjusts indexing to be compatible with igraph
    cylinder <- cylinder %>%
      mutate(
        ID = .data$ID + 1,
        parentID = .data$parentID + 1
      )

    # Adds cylinder info for plotting
    cylinder <- cylinder %>%
      mutate(
        UnmodRadius = .data$radius,
        axisX = (.data$endX - .data$startX) / .data$length,
        axisY = (.data$endY - .data$startY) / .data$length,
        axisZ = (.data$endZ - .data$startZ) / .data$length
      ) %>%
      relocate(.data$axisX, .after = .data$endZ) %>%
      relocate(.data$axisY, .after = .data$axisX) %>%
      relocate(.data$axisZ, .after = .data$axisY) %>%
      relocate(.data$radius, .before = .data$radius)

    # Add position in branch segment
    cylinder <- cylinder %>%
      group_by(.data$branchOrder, .data$branchID, .data$segmentID) %>%
      mutate(branchNew = cur_group_id()) %>%
      group_by(.data$branchNew) %>%
      mutate(positionInBranch = 1:n()) %>%
      ungroup() %>%
      relocate(.data$branchNew, .after = .data$branchID) %>%
      relocate(.data$positionInBranch, .after = .data$branchNew)

    # Total Children -----------------------------------------------------------
    cylinder <- total_children(cylinder, "parentID")

    # Build QSM Cylinder Network  ----------------------------------------------
    network <- build_network(cylinder, "ID", "parentID")

    # Growth Length (if missing) -----------------------------------------------
    if (!"growthLength" %in% colnames(cylinder)) {
      cylinder <- growth_length(network, cylinder, "ID", "parentID")
    }

    # Reverse Branch Order (if missing) ----------------------------------------
    if (!"reverseBranchOrder" %in% colnames(cylinder)) {
      cylinder <- reverse_branch_order(network, cylinder, "ID")
    }

    # Branch Segments (if missing) ---------------------------------------------
    if (!"segmentID" %in% colnames(cylinder)) {
      cylinder <- branch_segments(cylinder, "ID", "parentID", "branchID", "reverseBranchOrder")
    }

    # Path Metrics -------------------------------------------------------------
    if (!"distanceFromBase" %in% colnames(cylinder) & !"distanceToTwig" %in% colnames(cylinder)) {
      cylinder <- path_metrics(network, cylinder, "ID", "length")
    }

    # Resets cylinder ordering
    cylinder <- cylinder %>%
      mutate(
        ID = .data$ID - 1,
        parentID = .data$parentID - 1
      )
  # treegraph ------------------------------------------------------------------
  } else if (all(c("p1", "p2", "ninternode") %in% colnames(cylinder))) {
    # Save All Radii ----------------------------------------------------------
    cylinder <- mutate(cylinder, OldRadius = .data$radius, UnmodRadius = .data$radius)

    # Plotting Info ------------------------------------------------------------
    cylinder <- cylinder %>%
      mutate(
        ex = .data$sx + (.data$ax * .data$length),
        ey = .data$sy + (.data$ay * .data$length),
        ez = .data$sz + (.data$az * .data$length)
      ) %>%
      relocate(.data$ex, .after = .data$az) %>%
      relocate(.data$ey, .after = .data$ex) %>%
      relocate(.data$ez, .after = .data$ey)

    # Cylinder Ordering --------------------------------------------------------
    cylinder <- arrange(cylinder, .data$nbranch, .data$ninternode)
    cylinder <- update_ordering(cylinder, "p1", "p2")

    # Total Children -----------------------------------------------------------
    cylinder <- total_children(cylinder, "p2")

    # Build QSM Cylinder Network  ----------------------------------------------
    network <- build_network(cylinder, "p1", "p2")

    # Growth Length ------------------------------------------------------------
    cylinder <- growth_length(network, cylinder, "p1", "length")

    # Reverse Branch Order -----------------------------------------------------
    cylinder <- reverse_branch_order(network, cylinder, "p1")

    # Branch Segments ----------------------------------------------------------
    cylinder <- branch_segments(cylinder, "p1", "p2", "nbranch", "reverseBranchOrder")

    # Path Metrics -------------------------------------------------------------
    cylinder <- path_metrics(network, cylinder, "p1", "length")
  } else {
    message(
      "Invalid Dataframe Supplied!!!
      \nOnly TreeQSM or SimpleForest QSMs are supported.
      \nMake sure the cylinder data frame and not the QSM list is supplied."
    )
  }
  return(cylinder)
}


#' Updates cylinder parent child ordering
#' @param cylinder QSM cylinder data frame
#' @param id column name of cylinder indexes
#' @param parent column name of parent cylinders
#' @returns cylinder data frame with new ids
#' @noRd
update_ordering <- function(cylinder, id, parent) {
  message("Updating Cylinder Ordering")

  # Link new id to original parents
  parent_old <- cylinder %>%
    select({{ parent }}, {{ id }}) %>%
    mutate(id_new = 1:n()) %>%
    rename(parent_old = !!rlang::sym(parent))

  # Creates new parent and child ids
  new_id <- parent_old %>%
    left_join(
      select(parent_old, parent_old = !!rlang::sym(id), parent_new = .data$id_new),
      by = "parent_old"
    ) %>%
    select(parent = .data$parent_new, id = .data$id_new) %>%
    mutate(parent = case_when(is.na(.data$parent) ~ 0, TRUE ~ as.double(.data$parent))) %>%
    mutate(parent = case_when(
      .data$parent == 0 & .data$id > 1 ~ lag(as.double(.data$id), 1),
      TRUE ~ as.double(.data$parent)
    ))

  # Join new ids
  cylinder <- cylinder %>%
    select(-c(!!rlang::sym(id), !!rlang::sym(parent))) %>%
    bind_cols(new_id) %>%
    rename(!!rlang::sym(parent) := .data$parent, !!rlang::sym(id) := .data$id)

  return(cylinder)
}


#' Finds total children for each cylinder
#' @param cylinder QSM cylinder data frame
#' @param parent column name of parent cylinders
#' @returns cylinder data frame with total children
#' @noRd
total_children <- function(cylinder, parent) {
  message("Calculating Total Children")

  # Adds supported children for each cylinder
  total_children <- cylinder %>%
    group_by(!!rlang::sym(parent)) %>%
    summarize(totalChildren = n())

  # Joins total children
  cylinder <- left_join(cylinder, total_children, by = parent)

  return(cylinder)
}


#' Builds QSM cylinder network with igraph
#' @param cylinder QSM cylinder data frame
#' @param id column name of parent cylinders
#' @param parent column name of parent cylinders
#' @returns list of cylinder networkd
#' @noRd
build_network <- function(cylinder, id, parent) {
  message("Building Cylinder Network")

  # Extract cylinder ids
  id <- pull(select(cylinder, !!rlang::sym(id)))
  parent <- pull(select(cylinder, !!rlang::sym(parent)))

  # Creates QSM cylinder network
  qsm_g <- tidytable(parent = parent, id = id)
  qsm_g <- igraph::graph_from_data_frame(qsm_g)

  # Finds twigs cylinders
  twig_id_g <- igraph::V(qsm_g)[igraph::degree(qsm_g, mode = "out") == 0]
  twig_id_v <- as.integer(igraph::as_ids(twig_id_g))

  # Find supported children
  child_g <- qsm_g - 1 # remove cylinder 0
  child_g <- igraph::permute(child_g, match(igraph::V(child_g)$name, id))
  child_g <- igraph::ego(child_g, order = igraph::vcount(child_g), mode = "out")

  child_id <- as.integer(unlist(child_g, FALSE, FALSE))
  child_index <- cumsum(duplicated(child_id) & !duplicated(child_id, fromLast = TRUE)) + 1
  child_df <- tidytable(index = child_index, id = child_id)

  # Find all paths from base to twigs
  paths_g <- igraph::all_simple_paths(qsm_g, from = 1, to = twig_id_g)
  all_id <- as.integer(unlist(sapply(paths_g, igraph::as_ids), FALSE, FALSE))
  all_index <- cumsum(all_id == 0)
  all_df <- tidytable(index = all_index, id = all_id)

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
    select(.data$id, .data$frequency, .data$twig)

  return(
    list(
      qsm_g = qsm_g,
      child_df = child_df,
      all_df = all_df,
      base_df = base_df,
      cylinder_info = cylinder_info
    )
  )
}


#' Calculates growth length for each cylinder
#' @param network QSM cylinder network list
#' @param cylinder QSM cylinder data frame
#' @param id column name of cylinder indexes
#' @param length column name of cylinder lengths
#' @returns cylinder data frame with growth length
#' @noRd
growth_length <- function(network, cylinder, id, length) {
  message("Calculating Growth Length")

  # Calculate growth length
  growth_length <- network$child_df %>%
    left_join(select(cylinder, id = !!rlang::sym(id), length = !!rlang::sym(length)), by = "id") %>%
    group_by(.data$index) %>%
    summarize(growthLength = sum(!!rlang::sym(length), na.rm = TRUE)) %>%
    rename(!!rlang::sym(id) := .data$index)

  # Joins growth length
  cylinder <- left_join(cylinder, growth_length, by = id)

  return(cylinder)
}


#' Calculates the reverse branch order for each cylinder
#' @param network QSM cylinder network list
#' @param cylinder QSM cylinder data frame
#' @param id column name of cylinder indexes
#' @returns cylinder data frame with reverse branch order
#' @noRd
reverse_branch_order <- function(network, cylinder, id) {
  message("Calculating Reverse Branch Order")

  # Calculates Branch Nodes & Node Depth
  reverse_branch_order <- network$all_df %>%
    left_join(select(cylinder, id = !!rlang::sym(id), .data$totalChildren), by = "id") %>%
    group_by(.data$index) %>%
    filter(.data$id == 1 | .data$totalChildren > 1) %>%
    mutate(
      depth = 1:n(),
      reverseBranchOrder = abs(.data$depth - max(.data$depth)) + 1
    ) %>%
    group_by(.data$id) %>%
    summarize(reverseBranchOrder = max(.data$reverseBranchOrder)) %>%
    rename(!!rlang::sym(id) := .data$id)

  # Joins reverse branch order
  cylinder <- left_join(cylinder, reverse_branch_order, by = id) %>%
    fill(.data$reverseBranchOrder, .direction = "down")

  return(cylinder)
}


#' Calculates branching segments or "nodes"
#' @param cylinder QSM cylinder data frame
#' @param id column name of cylinder indexes
#' @param branch column name of branch ids
#' @param rbo column name of reverse branch order
#' @returns cylinder data frame with branch segments
#' @noRd
branch_segments <- function(cylinder, id, parent, branch, rbo) {
  message("Calculating Branch Segments")

  # Calculates Branch Segments
  branch_segments <- cylinder %>%
    distinct(!!rlang::sym(branch), !!rlang::sym(rbo)) %>%
    mutate(segment = 1:n())

  # Joins branch segments
  cylinder <- left_join(cylinder, branch_segments, by = c(branch, rbo))

  # Calculates Parent Segments
  child_segments <- cylinder %>%
    select(id = !!rlang::sym(parent), childSegment = .data$segment) %>%
    distinct(.data$childSegment, .keep_all = TRUE)

  parent_segments <- left_join(
    select(cylinder, id = !!rlang::sym(id), .data$segment),
    child_segments,
    by = "id"
  ) %>%
    drop_na() %>%
    select(segment = .data$childSegment, parentSegment = .data$segment)

  # Joins parent segments
  cylinder <- left_join(cylinder, parent_segments, by = "segment")
  cylinder <- mutate(cylinder, parentSegment = replace_na(.data$parentSegment, 0))

  return(cylinder)
}


#' Calculates path based distance metrics
#' @param network QSM cylinder network list
#' @param cylinder QSM cylinder data frame
#' @param id column name of cylinder indexes
#' @param length column name of cylinder lengths
#' @returns cylinder data frame with branch segments
#' @noRd
path_metrics <- function(network, cylinder, id, length) {
  message("Calculating Distance From Base")

  # Calculate distance from base to cylinder
  base_distance <- network$base_df %>%
    left_join(select(cylinder, id = !!rlang::sym(id), length = !!rlang::sym(length)), by = "id") %>%
    group_by(.data$index) %>%
    summarize(distanceFromBase = sum(.data$length, na.rm = TRUE)) %>%
    rename(!!rlang::sym(id) := .data$index)

  # Joins distance from base
  cylinder <- left_join(cylinder, base_distance, by = id)

  message("Calculating Average Distance To Twigs")

  # Calculate average distance to twigs
  twig_distance <- left_join(network$child_df, network$cylinder_info, by = "id") %>%
    left_join(select(cylinder, id = !!rlang::sym(id), length = !!rlang::sym(length)), by = "id") %>%
    group_by(.data$index) %>%
    summarize(distanceToTwig = sum(.data$length * .data$frequency) / sum(.data$twig)) %>%
    rename(!!rlang::sym(id) := .data$index)

  # Joins distance from twig
  cylinder <- left_join(cylinder, twig_distance, by = id)

  return(cylinder)
}
