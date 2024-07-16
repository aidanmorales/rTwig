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
      select(-"branch") %>%
      rename("branch" = "branch_new")

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
    cylinder <- total_children(cylinder, "parent", "extension")

    # Build QSM Cylinder Network  ----------------------------------------------
    network <- build_network(cylinder, "extension", "parent")

    # Verify Topology ----------------------------------------------------------
    cylinder <- verify_topology(
      network, cylinder, "extension", "parent", "branch", "BranchOrder"
    )

    # Growth Length ------------------------------------------------------------
    cylinder <- growth_length(network, cylinder, "extension", "length")

    # Reverse Branch Order -----------------------------------------------------
    cylinder <- reverse_branch_order(network, cylinder, "extension", "parent")

    # Branch Segments ----------------------------------------------------------
    cylinder <- branch_segments(
      cylinder, "extension", "parent", "branch", "reverseBranchOrder"
    )

    # Alternate Branch Numbering -----------------------------------------------
    cylinder <- branch_alt(
      network, cylinder, "extension", "parent", "branch", "BranchOrder"
    )

    # Path Metrics -------------------------------------------------------------
    cylinder <- path_metrics(network, cylinder, "extension", "length")

    # Organize Cylinders  ------------------------------------------------------

    if (all(c("SurfCov", "mad") %in% colnames(cylinder))) {
      cylinder <- cylinder %>%
        relocate("OldRadius", .after = "UnmodRadius") %>%
        relocate("reverseBranchOrder", .after = "BranchOrder") %>%
        relocate("segment", .after = "PositionInBranch") %>%
        relocate("parentSegment", .after = "segment") %>%
        relocate("UnmodRadius":"mad", .after = "end.z") %>%
        relocate("growthLength", .after = "mad") %>%
        relocate("mad", .after = "end.z") %>%
        relocate("SurfCov", .after = "mad") %>%
        relocate("branch", .after = "growthLength") %>%
        relocate("parent", .after = "branch") %>%
        relocate("extension", .after = "parent") %>%
        relocate("end.x", .after = "axis.z") %>%
        relocate("end.y", .after = "end.x") %>%
        relocate("end.z", .after = "end.y") %>%
        relocate("branch_alt", .after = "branch")
    } else {
      cylinder <- cylinder %>%
        relocate("OldRadius", .after = "UnmodRadius") %>%
        relocate("reverseBranchOrder", .after = "BranchOrder") %>%
        relocate("segment", .after = "PositionInBranch") %>%
        relocate("parentSegment", .after = "segment") %>%
        relocate("UnmodRadius", .after = "end.z") %>%
        relocate("OldRadius", .after = "UnmodRadius") %>%
        relocate("growthLength", .after = "OldRadius") %>%
        relocate("branch", .after = "growthLength") %>%
        relocate("parent", .after = "branch") %>%
        relocate("extension", .after = "parent") %>%
        relocate("end.x", .after = "axis.z") %>%
        relocate("end.y", .after = "end.x") %>%
        relocate("end.z", .after = "end.y") %>%
        relocate("branch_alt", .after = "branch")
    }
  }
  # SimpleForest ---------------------------------------------------------------
  else if (all(c("ID", "parentID", "branchID", "branchOrder") %in% colnames(cylinder))) {
    # Adjusts indexing to be compatible with igraph ----------------------------
    cylinder <- cylinder %>%
      mutate(
        ID = .data$ID + 1,
        parentID = .data$parentID + 1,
        branchID = .data$branchID + 1,
        segmentID = .data$segmentID + 1,
        parentSegmentID = .data$parentSegmentID + 1
      )

    # Adds cylinder info for plotting ------------------------------------------
    cylinder <- cylinder %>%
      mutate(
        UnmodRadius = .data$radius,
        axisX = (.data$endX - .data$startX) / .data$length,
        axisY = (.data$endY - .data$startY) / .data$length,
        axisZ = (.data$endZ - .data$startZ) / .data$length
      ) %>%
      relocate("axisX", .after = "endZ") %>%
      relocate("axisY", .after = "axisX") %>%
      relocate("axisZ", .after = "axisY") %>%
      relocate("radius", .before = "radius")

    # Save Alternate Branch ID -------------------------------------------------
    cylinder <- cylinder %>%
      mutate(branch_alt = .data$branchID - 1) %>%
      select(-"branchID")

    # Generates new branch IDs -------------------------------------------------
    message("Updating Branch Ordering")

    # Initialize an empty list to store results
    connected_segments <- list()

    # Find unique branches
    for (order in unique(cylinder$branchOrder)) {
      order_df <- filter(cylinder, .data$branchOrder == order)
      edges <- tidytable(from = order_df$parentID, to = order_df$ID)
      g <- igraph::graph_from_data_frame(d = edges, directed = FALSE)
      components <- igraph::clusters(g)
      order_df$index <- components$membership[match(order_df$ID, igraph::V(g)$name)]
      connected_segments[[as.character(order)]] <- order_df
    }

    # Create unique branch ids
    branch_new <- bind_rows(connected_segments) %>%
      group_by("branchOrder", "index") %>%
      mutate(branchID = cur_group_id()) %>%
      ungroup() %>%
      select("ID", "branchID")

    # Join new branch ids and calculates position in branch
    cylinder <- left_join(cylinder, branch_new) %>%
      group_by("branchID") %>%
      mutate(positionInBranch = 1:n()) %>%
      ungroup()

    # Relabels branches consecutively
    branches <- unique(cylinder$branchID)
    branch_id <- tidytable(
      branchID = branches,
      branch_new = 1:length(branches)
    )

    # Updates branch ordering
    cylinder <- left_join(cylinder, branch_id, by = "branchID") %>%
      select(-"branchID") %>%
      rename("branchID" = "branch_new")

    # Total Children -----------------------------------------------------------
    cylinder <- total_children(cylinder, "parentID", "ID")

    # Build QSM Cylinder Network  ----------------------------------------------
    network <- build_network(cylinder, "ID", "parentID")

    # Verify Topology ----------------------------------------------------------
    cylinder <- verify_topology(
      network, cylinder, "ID", "parentID", "branchID", "branchOrder"
    )

    # Growth Length (if missing) -----------------------------------------------
    if (!"growthLength" %in% colnames(cylinder)) {
      cylinder <- growth_length(network, cylinder, "ID", "parentID")
    }

    # Reverse Branch Order (if missing) ----------------------------------------
    if (!"reverseBranchOrder" %in% colnames(cylinder)) {
      cylinder <- reverse_branch_order( network, cylinder, "ID", "parentID")
    }

    # Branch Segments (if missing) ---------------------------------------------
    if (!"segmentID" %in% colnames(cylinder)) {
      cylinder <- branch_segments(
        cylinder, "ID", "parentID", "branchID", "reverseBranchOrder"
      )
    }

    # Path Metrics -------------------------------------------------------------
    if (!"distanceFromBase" %in% colnames(cylinder) & !"distanceToTwig" %in% colnames(cylinder)) {
      cylinder <- path_metrics(network, cylinder, "ID", "length")
    }
  }
  # Treegraph ------------------------------------------------------------------
  else if (all(c("p1", "p2", "ninternode") %in% colnames(cylinder))) {
    # Save All Radii -----------------------------------------------------------
    cylinder <- mutate(cylinder, OldRadius = .data$radius, UnmodRadius = .data$radius)

    # Plotting Info ------------------------------------------------------------
    cylinder <- cylinder %>%
      mutate(
        ex = .data$sx + (.data$ax * .data$length),
        ey = .data$sy + (.data$ay * .data$length),
        ez = .data$sz + (.data$az * .data$length)
      ) %>%
      relocate("ex", .after = "az") %>%
      relocate("ey", .after = "ex") %>%
      relocate("ez", .after = "ey")

    # Update Branch and Node Index ---------------------------------------------
    cylinder <- cylinder %>%
      mutate(
        nbranch = .data$nbranch + 1,
        ninternode = .data$ninternode + 1
      ) %>%
      group_by("nbranch") %>%
      mutate(positionInBranch = 1:n()) %>%
      ungroup()

    # Branch Order -------------------------------------------------------------
    if (!"branch_order2" %in% colnames(cylinder)) {
      cylinder <- mutate(cylinder, branch_order = .data$branch_order - 1)
    } else if (all(c("branch_order2", "branch_order") %in% colnames(cylinder))) {
      cylinder <- cylinder %>%
        select(-"branch_order") %>%
        rename(branch_order = "branch_order2") %>%
        mutate(branch_order = .data$branch_order - 1)
    }

    # Cylinder Ordering --------------------------------------------------------
    cylinder <- arrange(cylinder, .data$nbranch, .data$ninternode)
    cylinder <- update_ordering(cylinder, "p1", "p2")

    # Total Children -----------------------------------------------------------
    cylinder <- total_children(cylinder, "p2", "p1")

    # Build QSM Cylinder Network  ----------------------------------------------
    network <- build_network(cylinder, "p1", "p2")

    # Verify Topology ----------------------------------------------------------
    cylinder <- verify_topology(
      network, cylinder, "p1", "p2", "nbranch", "branch_order"
    )

    # Growth Length ------------------------------------------------------------
    cylinder <- growth_length(network, cylinder, "p1", "length")

    # Reverse Branch Order -----------------------------------------------------
    cylinder <- reverse_branch_order(network, cylinder, "p1", "p2")

    # Branch Segments ----------------------------------------------------------
    cylinder <- branch_segments(
      cylinder, "p1", "p2", "nbranch", "reverseBranchOrder"
    )

    # Alternate Branch Numbering -----------------------------------------------
    cylinder <- branch_alt(
      network, cylinder, "p1", "p2", "nbranch", "branch_order"
    )

    # Path Metrics -------------------------------------------------------------
    cylinder <- path_metrics(network, cylinder, "p1", "length")
  } else {
    message(
      "Invalid Dataframe Supplied!!!
      \nOnly TreeQSM, SimpleForest, or Treegraph QSMs are supported.
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
      select(parent_old, parent_old = !!rlang::sym(id), parent_new = "id_new"),
      by = "parent_old"
    ) %>%
    select(parent = "parent_new", id = "id_new") %>%
    mutate(
      parent = case_when(is.na(.data$parent) ~ 0, TRUE ~ as.double(.data$parent))
    ) %>%
    mutate(
      parent = case_when(
        .data$parent == 0 & .data$id > 1 ~ lag(as.double(.data$id), 1),
        TRUE ~ as.double(.data$parent)
      )
    )

  # Join new ids
  cylinder <- cylinder %>%
    select(-c(!!rlang::sym(id), !!rlang::sym(parent))) %>%
    bind_cols(new_id) %>%
    rename(!!rlang::sym(parent) := "parent", !!rlang::sym(id) := "id")

  return(cylinder)
}

#' Finds total children for each cylinder
#' @param cylinder QSM cylinder data frame
#' @param parent column name of parent cylinders
#' @param id column name of cylinder indexes
#' @returns cylinder data frame with total children
#' @noRd
total_children <- function(cylinder, parent, id) {
  message("Calculating Total Children")

  # Adds supported children for each cylinder
  total_children <- cylinder %>%
    group_by(!!rlang::sym(parent)) %>%
    summarize(totalChildren = n()) %>%
    rename(!!rlang::sym(id) := !!rlang::sym(parent))

  # Joins total children
  cylinder <- left_join(cylinder, total_children, by = id)

  # Fill NA with 0
  cylinder$totalChildren <- replace_na(cylinder$totalChildren, 0)

  return(cylinder)
}

#' Builds QSM cylinder network with igraph
#' @param cylinder QSM cylinder data frame
#' @param id column name of parent cylinders
#' @param parent column name of parent cylinders
#' @param all_paths return all paths or just branching paths
#' @returns list of cylinder networkd
#' @noRd
build_network <- function(cylinder, id, parent, all_paths = TRUE) {
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

  # Find all paths from base to twigs
  paths_g <- igraph::all_simple_paths(qsm_g, from = 1, to = twig_id_g)
  all_id <- as.integer(unlist(sapply(paths_g, igraph::as_ids), FALSE, FALSE))
  all_index <- cumsum(all_id == 0)
  all_df <- tidytable(index = all_index, id = all_id)

  if (all_paths == FALSE) {
    return(all_df)
  }

  # Find supported children
  child_g <- qsm_g - 1 # remove cylinder 0
  child_g <- igraph::permute(child_g, match(igraph::V(child_g)$name, id))
  child_g <- igraph::ego(child_g, order = igraph::vcount(child_g), mode = "out")

  child_id <- as.integer(unlist(child_g, FALSE, FALSE))
  child_index <- cumsum(duplicated(child_id) & !duplicated(child_id, fromLast = TRUE)) + 1
  child_df <- tidytable(index = child_index, id = child_id)

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
    left_join(
      select(cylinder, id = !!rlang::sym(id), length = !!rlang::sym(length)),
      by = "id"
    ) %>%
    group_by("index") %>%
    summarize(growthLength = sum(!!rlang::sym(length), na.rm = TRUE)) %>%
    rename(!!rlang::sym(id) := "index")

  # Joins growth length
  cylinder <- left_join(cylinder, growth_length, by = id)

  return(cylinder)
}

#' Calculates the reverse branch order for each cylinder
#' @param network QSM cylinder network list
#' @param cylinder QSM cylinder data frame
#' @param id column name of cylinder indexes
#' @param parent column name of parent cylinders
#' @returns cylinder data frame with reverse branch order
#' @noRd
reverse_branch_order <- function(network, cylinder, id, parent) {
  message("Calculating Reverse Branch Order")

  # Find branch break points
  breaks <- cylinder %>%
    rename(parent := !!rlang::sym(parent)) %>%
    group_by("parent") %>%
    summarize(breaks = n())

  # Calculates Branch Nodes & Node Depth
  reverse_branch_order <- network$all_df %>%
    left_join(
      select(cylinder, id = !!rlang::sym(id), parent = !!rlang::sym(parent)),
      by = "id"
    ) %>%
    left_join(breaks, by = "parent") %>%
    group_by("index") %>%
    filter(.data$id == 1 | .data$breaks > 1) %>%
    mutate(
      depth = 1:n(),
      reverseBranchOrder = abs(.data$depth - max(.data$depth)) + 1
    ) %>%
    group_by("id") %>%
    summarize(reverseBranchOrder = max(.data$reverseBranchOrder)) %>%
    rename(!!rlang::sym(id) := "id")

  # Joins reverse branch order
  cylinder <- left_join(cylinder, reverse_branch_order, by = id) %>%
    fill("reverseBranchOrder", .direction = "down")

  return(cylinder)
}

#' Calculates branching segments or "internodes"
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
    select(id = !!rlang::sym(parent), childSegment = "segment") %>%
    distinct("childSegment", .keep_all = TRUE)

  parent_segments <- left_join(
    select(cylinder, id = !!rlang::sym(id), "segment"),
    child_segments,
    by = "id"
  ) %>%
    drop_na() %>%
    select(segment = "childSegment", parentSegment = "segment")

  # Joins parent segments
  cylinder <- left_join(cylinder, parent_segments, by = "segment")
  cylinder$parentSegment <- replace_na(cylinder$parentSegment, 0)

  return(cylinder)
}

#' Calculates the alternate branch index
#' @param network QSM cylinder network list
#' @param cylinder QSM cylinder data frame
#' @param id column name of cylinder indexes
#' @param parent column name of parent cylinders
#' @param branch column name of branch ids
#' @param branch_order column name of cylinder branch order
#' @returns cylinder data frame with growth length
#' @noRd
branch_alt <- function(
    network,
    cylinder,
    id,
    parent,
    branch,
    branch_order) {
  message("Calculating Alternate Branch Numbers")

  # Find first order branch bases
  first_branch <- cylinder %>%
    filter(!!rlang::sym(branch_order) == 1) %>%
    group_by(!!rlang::sym(branch)) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    mutate(branch_alt = 1:n()) %>%
    select(index = !!rlang::sym(id), "branch_alt")

  # Find cylinder
  cylinder <- filter(network$child_df, .data$index %in% first_branch$index) %>%
    rename(!!rlang::sym(id) := "id") %>%
    right_join(cylinder, by = id) %>%
    left_join(first_branch, by = "index") %>%
    select(-"index") %>%
    relocate(!!rlang::sym(id), .before = !!rlang::sym(parent))

  cylinder$branch_alt <- replace_na(cylinder$branch_alt, 0)

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
    left_join(
      select(cylinder, id = !!rlang::sym(id), length = !!rlang::sym(length)),
      by = "id"
    ) %>%
    group_by("index") %>%
    summarize(distanceFromBase = sum(.data$length, na.rm = TRUE)) %>%
    rename(!!rlang::sym(id) := "index")

  # Joins distance from base
  cylinder <- left_join(cylinder, base_distance, by = id)

  message("Calculating Average Distance To Twigs")

  # Calculate average distance to twigs
  twig_distance <- left_join(network$child_df, network$cylinder_info, by = "id") %>%
    left_join(
      select(cylinder, id = !!rlang::sym(id), length = !!rlang::sym(length)),
      by = "id"
    ) %>%
    group_by("index") %>%
    summarize(
      distanceToTwig = sum(.data$length * .data$frequency) / sum(.data$twig)
    ) %>%
    rename(!!rlang::sym(id) := "index")

  # Joins distance from twig
  cylinder <- left_join(cylinder, twig_distance, by = id)

  return(cylinder)
}

#' Verify QSM topology
#' @param network QSM cylinder network list
#' @param cylinder QSM cylinder data frame
#' @param id column name of cylinder indexes
#' @param parent column name of parent cylinders
#' @param branch column name of branch ids
#' @param branch_order column name of cylinder branch order
#' @param verify dev Boolean to output error cylinders
#' @return cylinder data frame with corrected topology
#' @noRd
verify_topology <- function(
    network,
    cylinder,
    id,
    parent,
    branch,
    branch_order,
    verify = FALSE) {
  message("Verifying Topology")

  # Generate branch order topology
  topology <- cylinder %>%
    select(
      id = !!rlang::sym(id),
      parent = !!rlang::sym(parent),
      branch_order = !!rlang::sym(branch_order)
    ) %>%
    left_join(
      select(
        cylinder,
        parent = !!rlang::sym(id),
        parent_order = !!rlang::sym(branch_order)
      ),
      by = "parent"
    )

  # Check for topological errors
  error_topology <- topology %>%
    mutate(check = .data$branch_order - .data$parent_order) %>%
    filter(!.data$check %in% c(NA_integer_, 0, 1)) %>%
    pull("id")

  # Output error cylinders
  if (verify == TRUE) {
    return(error_topology)
  }

  if (length(error_topology) > 0) {
    message("Correcting Topology")

    # Correct topology
    corrected_topology <- network$child_df %>%
      filter(.data$index %in% !!error_topology) %>%
      left_join(
        select(topology, "id", "branch_order"),
        by = c("index" = "id")
      ) %>%
      left_join(
        select(topology, "id", "parent_order"),
        by = c("index" = "id")
      ) %>%
      mutate(branch_order = .data$branch_order + .data$parent_order + 1) %>%
      select(id, branch_order) %>%
      group_by("id") %>%
      filter(branch_order == max(.data$branch_order)) %>%
      rename(!!rlang::sym(id) := "id")

    # Update QSM topology
    cylinder <- cylinder %>%
      rename(branch_order = !!rlang::sym(branch_order)) %>%
      left_join(corrected_topology, by = id) %>%
      mutate(
        !!rlang::sym(branch_order) := coalesce(.data$branch_order.y, .data$branch_order.x)
      ) %>%
      select(-c("branch_order.x", "branch_order.y"))
  }

  return(cylinder)
}
