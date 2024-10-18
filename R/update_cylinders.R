#' Update Cylinders
#'
#' @description Updates the QSM cylinder data in preparation for radii correction
#'
#' @details Updates and verifies parent-child cylinder relationships and
#' calculates new variables and metrics found throughout the supported QSM software.
#' This function is required to run the rest of the rTwig functions.
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
#' ## aRchi Processing Chain
#' file <- system.file("extdata/QSM2.csv", package = "rTwig")
#' cylinder <- read.csv(file)
#' cylinder <- update_cylinders(cylinder)
#' str(cylinder)
#'
update_cylinders <- function(cylinder) {
  # Check inputs ---------------------------------------------------------------
  if (is_missing(cylinder)) {
    message <- "argument `cylinder` is missing, with no default."
    abort(message, class = "missing_argument")
  }

  if (!is.data.frame(cylinder)) {
    message <- paste(
      paste0("`cylinder` must be a data frame, not ", class(cylinder), "."),
      "i Did you accidentally pass the QSM list instead of the cylinder data frame?",
      sep = "\n"
    )
    abort(message, class = "data_format_error")
  }

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
    network <- build_network(cylinder, "extension", "parent", cache = TRUE)

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
      cylinder %>%
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
      cylinder %>%
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

    # Plotting Info ------------------------------------------------------------
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

    # Find Branches ------------------------------------------------------------
    cylinder <- branch_from_order(
      cylinder, "ID", "parentID", "branchOrder", "branchID"
    )

    # Total Children -----------------------------------------------------------
    cylinder <- total_children(cylinder, "parentID", "ID")

    # Build QSM Cylinder Network  ----------------------------------------------
    network <- build_network(cylinder, "ID", "parentID", cache = TRUE)

    # Verify Topology ----------------------------------------------------------
    cylinder <- verify_topology(
      network, cylinder, "ID", "parentID", "branchID", "branchOrder"
    )

    # Growth Length (if missing) -----------------------------------------------
    if (!"growthLength" %in% colnames(cylinder)) {
      cylinder <- growth_length(network, cylinder, "ID", "length")
    }

    # Reverse Branch Order (if missing) ----------------------------------------
    if (!"reverseBranchOrder" %in% colnames(cylinder)) {
      cylinder <- reverse_branch_order(network, cylinder, "ID", "parentID")
    }

    # Branch Segments (if missing) ---------------------------------------------
    if (!"segmentID" %in% colnames(cylinder)) {
      cylinder <- branch_segments(
        cylinder, "ID", "parentID", "branchID", "reverseBranchOrder"
      )
    }

    # Path Metrics -------------------------------------------------------------
    cylinder <- path_metrics(network, cylinder, "ID", "length")

    # Check for existing path metrics columns
    if (any(c("distanceToTwig.x", "distanceToTwig.y") %in% colnames(cylinder))) {
      cylinder <- cylinder %>%
        rename(
          distanceToTwigSF = "distanceToTwig.x",
          distanceToTwig = "distanceToTwig.y"
        )
    }

    if (any(c("reversePipeRadiusBranchorder.x", " reversePipeRadiusBranchorder.y") %in% colnames(cylinder))) {
      cylinder <- cylinder %>%
        select(-"reversePipeRadiusBranchorder.x") %>%
        rename(reversePipeRadiusBranchorder = "reversePipeRadiusBranchorder.y")
    }

    if (any(c("reversePipeAreaBranchorder.x", " reversePipeAreaBranchorder.y") %in% colnames(cylinder))) {
      cylinder <- cylinder %>%
        select(-"reversePipeAreaBranchorder.x") %>%
        rename(reversePipeAreaBranchorder = "reversePipeAreaBranchorder.y")
    }

    if (any(c("vesselVolume.x", " vesselVolume.y") %in% colnames(cylinder))) {
      cylinder <- cylinder %>%
        select(-"vesselVolume.x") %>%
        rename(vesselVolume = "vesselVolume.y")
    }

    return(cylinder)
  }
  # Treegraph ------------------------------------------------------------------
  else if (all(c("p1", "p2", "ninternode") %in% colnames(cylinder))) {
    # Save All Radii -----------------------------------------------------------
    cylinder <- mutate(cylinder, UnmodRadius = .data$radius)

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
    network <- build_network(cylinder, "p1", "p2", cache = TRUE)

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
    path_metrics(network, cylinder, "p1", "length")
  }
  # aRchi ----------------------------------------------------------------------
  else if (all(c("cyl_ID", "parent_ID", "branching_order") %in% colnames(cylinder))) {
    # Cylinder Ordering --------------------------------------------------------
    cylinder <- arrange(cylinder, .data$cyl_ID)
    cylinder <- update_ordering(cylinder, "cyl_ID", "parent_ID")

    # Branch Order -------------------------------------------------------------
    cylinder <- mutate(cylinder, branching_order = .data$branching_order - 1)

    # Adds cylinder info for plotting ------------------------------------------
    cylinder <- cylinder %>%
      mutate(
        UnmodRadius = .data$radius_cyl,
        axisX = (.data$endX - .data$startX) / .data$length,
        axisY = (.data$endY - .data$startY) / .data$length,
        axisZ = (.data$endZ - .data$startZ) / .data$length
      ) %>%
      relocate("axisX", .after = "endZ") %>%
      relocate("axisY", .after = "axisX") %>%
      relocate("axisZ", .after = "axisY") %>%
      relocate("radius_cyl", .before = "radius_cyl")

    # Find Branches ------------------------------------------------------------
    cylinder <- branch_from_order(
      cylinder, "cyl_ID", "parent_ID", "branching_order",
      branch_name = "branch_ID"
    )

    # Total Children -----------------------------------------------------------
    cylinder <- total_children(cylinder, "parent_ID", "cyl_ID")

    # Build QSM Cylinder Network  ----------------------------------------------
    network <- build_network(cylinder, "cyl_ID", "parent_ID", cache = TRUE)

    # Verify Topology ----------------------------------------------------------
    cylinder <- verify_topology(
      network, cylinder, "cyl_ID", "parent_ID", "branch_ID", "branching_order"
    )

    # Growth Length ------------------------------------------------------------
    cylinder <- growth_length(network, cylinder, "cyl_ID", "length")

    # Reverse Branch Order -----------------------------------------------------
    cylinder <- reverse_branch_order(network, cylinder, "cyl_ID", "parent_ID")

    # Branch Segments ----------------------------------------------------------
    cylinder <- branch_segments(
      cylinder, "cyl_ID", "parent_ID", "branch_ID", "reverseBranchOrder"
    )

    # Alternate Branch Numbering -----------------------------------------------
    cylinder <- branch_alt(
      network, cylinder, "cyl_ID", "parent_ID", "branch_ID", "branching_order"
    )

    # Path Metrics -------------------------------------------------------------
    path_metrics(network, cylinder, "cyl_ID", "length")
  } else {
    message <- paste(
      "Unsupported QSM format provided.",
      "i Only TreeQSM, SimpleForest, Treegraph, or aRchi QSMs are supported.",
      sep = "\n"
    )
    abort(message, class = "data_format_error")
  }
}

#' Updates cylinder parent child ordering
#' @param cylinder QSM cylinder data frame
#' @param id column name of cylinder indexes
#' @param parent column name of parent cylinders
#' @returns cylinder data frame with new ids
#' @noRd
update_ordering <- function(cylinder, id, parent) {
  inform("Updating Cylinder Ordering")

  # Link new id to original parents
  parent_old <- cylinder %>%
    select({{ parent }}, {{ id }}) %>%
    mutate(id_new = 1:n()) %>%
    rename(parent_old = {{ parent }})

  # Creates new parent and child ids
  new_id <- parent_old %>%
    left_join(
      select(parent_old, parent_old = {{ id }}, parent_new = "id_new"),
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
  cylinder %>%
    select(-c({{ id }}, {{ parent }})) %>%
    bind_cols(new_id) %>%
    rename({{ parent }} := "parent", {{ id }} := "id")
}

#' Finds total children for each cylinder
#' @param cylinder QSM cylinder data frame
#' @param parent column name of parent cylinders
#' @param id column name of cylinder indexes
#' @returns cylinder data frame with total children
#' @noRd
total_children <- function(cylinder, parent, id) {
  inform("Calculating Total Children")

  # Adds supported children for each cylinder
  total_children <- cylinder %>%
    group_by({{ parent }}) %>%
    summarise(totalChildren = n(), .groups = "drop") %>%
    rename({{ id }} := {{ parent }})

  # Joins total children and fill na with 0
  left_join(cylinder, total_children, by = id) %>%
    mutate(totalChildren = replace_na(.data$totalChildren, 0))
}

#' Calculates growth length for each cylinder
#' @param network QSM cylinder network list
#' @param cylinder QSM cylinder data frame
#' @param id column name of cylinder indexes
#' @param length column name of cylinder lengths
#' @returns cylinder data frame with growth length
#' @noRd
growth_length <- function(network, cylinder, id, length) {
  inform("Calculating Growth Length")

  # Calculate growth length
  growth_length <- network$child_df %>%
    left_join(
      select(cylinder, id = {{ id }}, length = {{ length }}),
      by = "id"
    ) %>%
    group_by("index") %>%
    summarise(
      growthLength = sum(!!rlang::sym(length), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    rename({{ id }} := "index")

  # Joins growth length
  left_join(cylinder, growth_length, by = id)
}

#' Calculates the reverse branch order for each cylinder
#' @param network QSM cylinder network list
#' @param cylinder QSM cylinder data frame
#' @param id column name of cylinder indexes
#' @param parent column name of parent cylinders
#' @returns cylinder data frame with reverse branch order
#' @noRd
reverse_branch_order <- function(network, cylinder, id, parent) {
  inform("Calculating Reverse Branch Order")

  # Find branch break points
  breaks <- cylinder %>%
    rename(parent := {{ parent }}) %>%
    group_by("parent") %>%
    summarise(breaks = n(), .groups = "drop")

  # Calculates Branch Nodes & Node Depth
  reverse_branch_order <- network$all_df %>%
    left_join(
      select(cylinder, id = {{ id }}, parent = {{ parent }}),
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
    summarise(
      reverseBranchOrder = max(.data$reverseBranchOrder),
      .groups = "drop"
    ) %>%
    rename({{ id }} := "id")

  # Joins reverse branch order
  left_join(cylinder, reverse_branch_order, by = id) %>%
    fill("reverseBranchOrder", .direction = "down")
}

#' Calculates branching segments or "internodes"
#' @param cylinder QSM cylinder data frame
#' @param id column name of cylinder indexes
#' @param branch column name of branch ids
#' @param rbo column name of reverse branch order
#' @returns cylinder data frame with branch segments
#' @noRd
branch_segments <- function(cylinder, id, parent, branch, rbo) {
  inform("Calculating Branch Segments")

  # Calculates Branch Segments
  branch_segments <- cylinder %>%
    distinct({{ branch }}, {{ rbo }}) %>%
    mutate(segment = 1:n())

  # Joins branch segments
  cylinder <- left_join(cylinder, branch_segments, by = c(branch, rbo))

  # Calculates Parent Segments
  child_segments <- cylinder %>%
    select(id = {{ parent }}, childSegment = "segment") %>%
    distinct("childSegment", .keep_all = TRUE)

  parent_segments <- left_join(
    select(cylinder, id = {{ id }}, "segment"),
    child_segments,
    by = "id"
  ) %>%
    drop_na() %>%
    select(segment = "childSegment", parentSegment = "segment")

  # Joins parent segments
  left_join(cylinder, parent_segments, by = "segment") %>%
    mutate(parentSegment = replace_na(.data$parentSegment, 0))
}

#' Calculates the alternate branch index
#' @param network QSM cylinder network list
#' @param cylinder QSM cylinder data frame
#' @param id column name of cylinder indexes
#' @param parent column name of parent cylinders
#' @param branch column name of branch ids
#' @param branch_order column name of cylinder branch order
#' @returns cylinder data frame with alterate branches
#' @noRd
branch_alt <- function(
    network,
    cylinder,
    id,
    parent,
    branch,
    branch_order) {
  inform("Calculating Alternate Branch Numbers")

  # Find first order branch bases
  first_branch <- cylinder %>%
    filter(!!rlang::sym(branch_order) == 1) %>%
    group_by({{ branch }}) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    mutate(branch_alt = 1:n()) %>%
    select(index = {{ id }}, "branch_alt")

  # Find cylinder
  filter(network$child_df, .data$index %in% first_branch$index) %>%
    rename({{ id }} := "id") %>%
    right_join(cylinder, by = id) %>%
    left_join(first_branch, by = "index") %>%
    select(-"index") %>%
    relocate({{ id }}, .before = {{ parent }}) %>%
    mutate(branch_alt = replace_na(.data$branch_alt, 0))
}

#' Calculates branch ids from branch order
#' @param cylinder QSM cylinder data frame
#' @param id column name of cylinder indexes
#' @param parent column name of parent cylinders
#' @param branch_order column name of cylinder branch order
#' @param branch_name output column name for the branches
#' @returns cylinder data frame with branches
#' @noRd
branch_from_order <- function(cylinder, id, parent, branch_order, branch_name) {
  inform("Finding Branches")

  # Dynamically select ids and branch order
  data <- select(cylinder, all_of(id), all_of(parent), all_of(branch_order)) %>%
    rename("id" = id, "parent" = parent, "branch_order" = branch_order)

  # Initialize an empty list to store results
  connected_segments <- list()

  # Find unique branches per order
  for (order in unique(data$branch_order)) {
    order_df <- filter(data, .data$branch_order == order)
    edges <- tidytable(from = order_df$parent, to = order_df$id)
    g <- igraph::graph_from_data_frame(d = edges, directed = FALSE)
    components <- igraph::components(g)
    order_df$index <- components$membership[match(order_df$id, igraph::V(g)$name)]
    connected_segments[[as.character(order)]] <- order_df
  }

  # Create unique branch ids
  branch <- bind_rows(connected_segments) %>%
    group_by("branch_order", "index") %>%
    mutate(branch = cur_group_id()) %>%
    ungroup() %>%
    select("id", "branch")

  # Join new branch ids and calculate position in branch
  data <- left_join(data, branch) %>%
    group_by("branch") %>%
    mutate(positionInBranch = 1:n()) %>%
    ungroup()

  # Relabels branches consecutively
  branches <- unique(data$branch)
  branch <- tidytable(
    branch = branches,
    branch_new = 1:length(branches)
  )

  # Updates branch ordering
  branch <- left_join(data, branch, by = "branch") %>%
    select("id", "branch_new", "positionInBranch") %>%
    rename(
      {{ id }} := "id",
      {{ branch_name }} := "branch_new"
    )

  # Joins branches
  left_join(cylinder, branch, by = id)
}

#' Calculates path based distance metrics
#' @param network QSM cylinder network list
#' @param cylinder QSM cylinder data frame
#' @param id column name of cylinder indexes
#' @param length column name of cylinder lengths
#' @returns cylinder data frame with branch segments
#' @noRd
path_metrics <- function(network, cylinder, id, length) {
  inform("Calculating Path Metrics")

  # Calculate distance from base to cylinder
  base_distance <- network$base_df %>%
    left_join(
      select(cylinder, id = {{ id }}, length = {{ length }}),
      by = "id"
    ) %>%
    group_by("index") %>%
    summarise(
      distanceFromBase = sum(.data$length, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    rename({{ id }} := "index")

  # Calculate allometric variables
  path_df <- network$child_df %>%
    left_join(network$cylinder_info, by = "id") %>%
    left_join(
      select(cylinder, id = {{ id }}, length = {{ length }}),
      by = "id"
    )

  # Calculate path metrics
  path_metrics <- path_df %>%
    group_by("index") %>%
    summarise(
      twig_sum = sum(.data$twig),
      length_freq_sum = sum(.data$length * .data$frequency),
      .groups = "drop"
    ) %>%
    mutate(
      distanceToTwig = .data$length_freq_sum / .data$twig_sum,
      reversePipeAreaBranchorder = .data$twig_sum,
      reversePipeRadiusBranchorder = sqrt(.data$twig_sum)
    ) %>%
    rename({{ id }} := .data$index) %>%
    select(-c("twig_sum", "length_freq_sum"))

  # Calculate vessel volume
  vessel_volume <- path_df %>%
    left_join(
      select(
        path_metrics,
        id = {{ id }},
        RBOPA = "reversePipeAreaBranchorder"
      ),
      by = "id"
    ) %>%
    group_by("index") %>%
    summarise(
      vesselVolume = sum(.data$RBOPA * .data$length), .groups = "drop"
    ) %>%
    rename({{ id }} := .data$index) %>%
    select({{ id }}, "vesselVolume")

  # Joins variables
  cylinder %>%
    left_join(base_distance, by = id) %>%
    left_join(path_metrics, by = id) %>%
    left_join(vessel_volume, by = id)
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
  inform("Verifying Topology")

  # Generate branch order topology
  topology <- cylinder %>%
    select(
      id = {{ id }},
      parent = {{ parent }},
      branch_order = {{ branch_order }}
    ) %>%
    left_join(
      select(
        cylinder,
        parent = {{ id }},
        parent_order = {{ branch_order }}
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
    inform("Correcting Topology")

    # Correct topology
    corrected_topology <- network$child_df %>%
      filter(.data$index %in% !!error_topology) %>%
      left_join(
        select(topology, "id", "branch_order", "parent_order"),
        by = c("index" = "id")
      ) %>%
      mutate(branch_order = .data$branch_order + .data$parent_order + 1) %>%
      select(id, branch_order) %>%
      group_by("id") %>%
      filter(branch_order == max(.data$branch_order)) %>%
      rename({{ id }} := "id")

    # Update QSM topology
    cylinder %>%
      rename(branch_order = {{ branch_order }}) %>%
      left_join(corrected_topology, by = id) %>%
      mutate(
        {{ branch_order }} := coalesce(.data$branch_order.y, .data$branch_order.x)
      ) %>%
      select(-c("branch_order.x", "branch_order.y"))
  } else {
    return(cylinder)
  }
}
