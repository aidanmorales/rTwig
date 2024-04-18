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
#' @rawNamespace import(tidytable, except=c(map_dfr))
#' @rawNamespace import(igraph, except=c(union, as_data_frame, groups, crossing, "%->%", "%<-%"))
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
    message("Updating Cylinder Ordering")

    # Branch Ordering ----------------------------------------------------------

    # Updates branch numbers
    cylinder <- cylinder %>%
      mutate(
        extension = row_number() + 1,
        id = .data$extension - 1
      ) %>%
      distinct(.keep_all = TRUE)

    # Relabels branches consecutively
    cylinder_id <- cylinder %>%
      arrange(.data$branch, .data$parent, .data$PositionInBranch) %>%
      group_by(.data$branch) %>%
      summarize(n = n()) %>%
      mutate(branch_new = 1:n())

    # Updates branch ordering
    cylinder <- left_join(cylinder, cylinder_id, by = "branch") %>%
      select(-c(.data$n, .data$branch, .data$extension)) %>%
      rename("branch" = .data$branch_new) %>%
      arrange(.data$id)

    # Parent Child Ordering ----------------------------------------------------

    # Updates parent child ordering
    parent_temp <- cylinder %>%
      select(.data$parent, .data$id) %>%
      mutate(id_new = 1:n()) %>%
      rename(parent_old = .data$parent)

    new_id <- parent_temp %>%
      left_join(
        select(parent_temp, parent_old = .data$id, parent_new = .data$id_new),
        by = "parent_old"
      ) %>%
      select(parent = .data$parent_new, id = .data$id_new) %>%
      mutate(parent = case_when(is.na(.data$parent) ~ 0, TRUE ~ as.double(.data$parent))) %>%
      mutate(parent = case_when(
        .data$parent == 0 & .data$id > 1 ~ lag(as.double(.data$id), 1),
        TRUE ~ as.double(.data$parent)
      ))

    cylinder <- cylinder %>%
      select(-.data$parent, -.data$id) %>%
      bind_cols(new_id) %>%
      rename(extension = .data$id)

    # Save All Radii -----------------------------------------------------------
    cylinder <- mutate(cylinder, OldRadius = .data$radius, radius = .data$UnmodRadius)

    # Total Children -----------------------------------------------------------

    # Adds supported children for each cylinder
    total_children <- cylinder %>%
      group_by(.data$parent) %>%
      summarize(totalChildren = n())

    # Joins total children
    cylinder <- cylinder %>%
      left_join(total_children, by = "parent")

    # Plotting Info ------------------------------------------------------------

    # Adds cylinder endpoints for plotting
    cylinder <- cylinder %>%
      mutate(
        end.x = .data$start.x + (.data$axis.x * .data$length),
        end.y = .data$start.y + (.data$axis.y * .data$length),
        end.z = .data$start.z + (.data$axis.z * .data$length)
      )

    # Build QSM Cylinder Network  ----------------------------------------------

    message("Building Cylinder Network")

    # Creates QSM cylinder network
    qsm_g <- tidytable(parent = cylinder$parent, extension = cylinder$extension)
    qsm_g <- igraph::graph_from_data_frame(qsm_g)

    # Finds twigs cylinders
    twig_id_g <- igraph::V(qsm_g)[igraph::degree(qsm_g, mode = "out") == 0]
    twig_id_v <- as.integer(as_ids(twig_id_g))

    # Find supported children
    child_g <- qsm_g - 1 # remove cylinder 0
    child_g <- igraph::permute(child_g, match(igraph::V(child_g)$name, cylinder$extension))
    child_g <- igraph::ego(child_g, order = igraph::vcount(child_g), mode = "out")

    child_id <- as.integer(unlist(child_g, FALSE, FALSE))
    child_index <- cumsum(duplicated(child_id) & !duplicated(child_id, fromLast = TRUE)) + 1
    child_df <- tidytable(index = child_index, extension = child_id)

    # Find all paths from base to twigs
    paths_g <- igraph::all_simple_paths(qsm_g, from = 1, to = twig_id_g)
    all_id <- as.integer(unlist(sapply(paths_g, as_ids), FALSE, FALSE))
    all_index <- cumsum(all_id == 0)
    all_df <- tidytable(index = all_index, extension = all_id)

    # Find all paths from base to cylinder
    base_g <- igraph::all_simple_paths(qsm_g, from = 1, to = igraph::V(qsm_g))
    base_id <- as.integer(unlist(sapply(base_g, as_ids), FALSE, FALSE))
    base_index <- cumsum(base_id == 0)

    # Find number of paths cylinder occurs in and if it is a twig
    cylinder_info <- table(all_id)
    cylinder_info <- tidytable(
      extension = as.integer(names(cylinder_info)),
      frequency = as.integer(cylinder_info)
    ) %>%
      mutate(twig = .data$extension %in% !!twig_id_v) %>%
      select(.data$extension, .data$frequency, .data$twig)

    # Growth Length ------------------------------------------------------------

    message("Calculating Growth Length")

    # Calculate growth length
    growth_length <- child_df %>%
      left_join(select(cylinder, .data$extension, .data$length), by = "extension") %>%
      group_by(.data$index) %>%
      summarize(GrowthLength = sum(.data$length, na.rm = TRUE)) %>%
      rename(extension = .data$index)

    # Joins growth length
    cylinder <- left_join(cylinder, growth_length, by = "extension")

    # Reverse Branch Order -----------------------------------------------------

    message("Calculating Reverse Branch Order")

    # Calculates Branch Nodes & Node Depth
    reverse_branch_order <- all_df %>%
      left_join(select(cylinder, .data$extension, .data$totalChildren), by = "extension") %>%
      group_by(.data$index) %>%
      filter(.data$extension == 1 | .data$totalChildren > 1) %>%
      mutate(
        depth = 1:n(),
        reverseBranchOrder = abs(.data$depth - max(.data$depth)) + 1
      ) %>%
      group_by(.data$extension) %>%
      summarize(reverseBranchOrder = max(.data$reverseBranchOrder))

    # Joins reverse branch order
    cylinder <- left_join(cylinder, reverse_branch_order, by = "extension") %>%
      fill(.data$reverseBranchOrder, .direction = "down")

    # Branch Segments ----------------------------------------------------------

    message("Calculating Branch Segments")

    # Calculates Branch Segments
    branch_segments <- cylinder %>%
      distinct(.data$branch, .data$reverseBranchOrder) %>%
      mutate(segment = 1:n())

    # Joins branch segments
    cylinder <- left_join(cylinder, branch_segments, by = c("branch", "reverseBranchOrder"))

    # Parent Segments ----------------------------------------------------------

    child_segments <- cylinder %>%
      select(extension = .data$parent, childSegment = .data$segment) %>%
      distinct(.data$childSegment, .keep_all = TRUE)

    parent_segments <- left_join(
      select(cylinder, .data$extension, .data$segment),
      child_segments,
      by = c("extension")
    ) %>%
      drop_na() %>%
      select(segment = .data$childSegment, parentSegment = .data$segment)

    # Joins parent segments
    cylinder <- left_join(cylinder, parent_segments, by = "segment")
    cylinder <- mutate(cylinder, parentSegment = replace_na(.data$parentSegment, 0))

    # Distance From Base -------------------------------------------------------

    message("Calculating Distance From Base")

    # Calculate distance from base to cylinder
    base_distance <- tidytable(index = base_index, extension = base_id) %>%
      left_join(select(cylinder, .data$extension, .data$length), by = "extension") %>%
      group_by(.data$index) %>%
      summarize(distanceFromBase = sum(.data$length, na.rm = TRUE)) %>%
      rename(extension = .data$index)

    # Joins distance from base
    cylinder <- left_join(cylinder, base_distance, by = "extension")

    # Distance To Twig ----------------------------------------------------------

    message("Calculating Average Distance To Twigs")

    # Calculate average distance to twigs
    twig_distance <- left_join(child_df, cylinder_info, by = "extension") %>%
      left_join(select(cylinder, .data$extension, .data$length), by = "extension") %>%
      group_by(.data$index) %>%
      summarize(distanceToTwig = sum(.data$length * .data$frequency) / sum(.data$twig)) %>%
      rename(extension = .data$index)

    # Joins distance from twig
    cylinder <- left_join(cylinder, twig_distance, by = "extension")

    # Organize Cylinders  ------------------------------------------------------

    if (all(c("SurfCov", "mad") %in% colnames(cylinder))) {
      cylinder <- cylinder %>%
        relocate(.data$OldRadius, .after = .data$UnmodRadius) %>%
        relocate(.data$reverseBranchOrder, .after = .data$BranchOrder) %>%
        relocate(.data$segment, .after = .data$PositionInBranch) %>%
        relocate(.data$parentSegment, .after = .data$segment) %>%
        relocate(.data$UnmodRadius:.data$mad, .after = .data$end.z) %>%
        relocate(.data$GrowthLength, .after = .data$mad) %>%
        relocate(.data$mad, .after = .data$end.z) %>%
        relocate(.data$SurfCov, .after = .data$mad) %>%
        relocate(.data$branch, .after = .data$GrowthLength) %>%
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
        relocate(.data$GrowthLength, .after = .data$OldRadius) %>%
        relocate(.data$branch, .after = .data$GrowthLength) %>%
        relocate(.data$parent, .after = .data$branch) %>%
        relocate(.data$extension, .after = .data$parent) %>%
        relocate(.data$end.x, .after = .data$axis.z) %>%
        relocate(.data$end.y, .after = .data$end.x) %>%
        relocate(.data$end.z, .after = .data$end.y)
    }
  # SimpleForest ---------------------------------------------------------------
  } else if (all(c("ID", "parentID", "branchID", "branchOrder") %in% colnames(cylinder))) {
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

    # Adds supported children for each cylinder
    total_children <- cylinder %>%
      group_by(.data$parentID) %>%
      summarize(totalChildren = n())

    # Joins total children
    cylinder <- left_join(cylinder, total_children, by = "parentID")

    # Add position in branch segment
    cylinder <- cylinder %>%
      group_by(.data$branchOrder, .data$branchID, .data$segmentID) %>%
      mutate(branchNew = cur_group_id()) %>%
      group_by(.data$branchNew) %>%
      mutate(positionInBranch = 1:n()) %>%
      ungroup() %>%
      relocate(.data$branchNew, .after = .data$branchID) %>%
      relocate(.data$positionInBranch, .after = .data$branchNew)

    # Build QSM Cylinder Network  ----------------------------------------------

    message("Building Cylinder Network")

    # Adjusts indexing to be compatible with igraph
    cylinder <- cylinder %>%
      mutate(
        ID = .data$ID + 1,
        parentID = .data$parentID + 1
      )

    # Creates QSM cylinder network
    qsm_g <- tidytable(parentID = cylinder$parentID, ID = cylinder$ID)
    qsm_g <- igraph::graph_from_data_frame(qsm_g)

    # Finds twigs cylinders
    twig_id_g <- igraph::V(qsm_g)[igraph::degree(qsm_g, mode = "out") == 0]
    twig_id_v <- as.integer(as_ids(twig_id_g))

    # Find supported children
    child_g <- qsm_g - 1 # remove cylinder 0
    child_g <- igraph::permute(child_g, match(igraph::V(child_g)$name, cylinder$ID))
    child_g <- igraph::ego(child_g, order = igraph::vcount(child_g), mode = "out")

    child_id <- as.integer(unlist(child_g, FALSE, FALSE))
    child_index <- cumsum(duplicated(child_id) & !duplicated(child_id, fromLast = TRUE)) + 1
    child_df <- tidytable(index = child_index, ID = child_id)

    # Find all paths from base to twigs
    paths_g <- igraph::all_simple_paths(qsm_g, from = 1, to = twig_id_g)
    all_id <- as.integer(unlist(sapply(paths_g, as_ids), FALSE, FALSE))
    all_index <- cumsum(all_id == 0)
    all_df <- tidytable(index = all_index, ID = all_id)

    # Find all paths from base to cylinder
    base_g <- igraph::all_simple_paths(qsm_g, from = 1, to = igraph::V(qsm_g))
    base_id <- as.integer(unlist(sapply(base_g, as_ids), FALSE, FALSE))
    base_index <- cumsum(base_id == 0)

    # Find number of paths cylinder occurs in and if it is a twig
    cylinder_info <- table(all_id)
    cylinder_info <- tidytable(
      ID = as.integer(names(cylinder_info)),
      frequency = as.integer(cylinder_info)
    ) %>%
      mutate(twig = .data$ID %in% !!twig_id_v) %>%
      select(.data$ID, .data$frequency, .data$twig)

    # Growth Length (if missing) -----------------------------------------------
    if (!"growthLength" %in% colnames(cylinder)) {
      message("Calculating Growth Length")

      growth_length <- child_df %>%
        left_join(select(cylinder, .data$ID, .data$length), by = "ID") %>%
        group_by(.data$index) %>%
        summarize(growthLength = sum(.data$length, na.rm = TRUE)) %>%
        rename(ID = .data$index)

      cylinder <- left_join(cylinder, growth_length, by = "ID")
    }

    # Reverse Branch Order (if missing) ----------------------------------------
    if (!"reverseBranchOrder" %in% colnames(cylinder)) {
      message("Calculating Reverse Branch Order")

      # Calculates Branch Nodes & Node Depth
      reverse_branch_order <- all_df %>%
        left_join(select(cylinder, .data$ID, .data$totalChildren), by = "ID") %>%
        group_by(.data$index) %>%
        filter(.data$ID == 1 | .data$totalChildren > 1) %>%
        mutate(
          depth = 1:n(),
          reverseBranchOrder = abs(.data$depth - max(.data$depth)) + 1
        ) %>%
        group_by(.data$ID) %>%
        summarize(reverseBranchOrder = max(.data$reverseBranchOrder))

      # Joins reverse branch order
      cylinder <- left_join(cylinder, reverse_branch_order, by = "ID") %>%
        fill(.data$reverseBranchOrder, .direction = "down")
    }

    # Branch Segments (if missing) ---------------------------------------------
    if (!"segmentID" %in% colnames(cylinder)) {
      message("Calculating Branch Segments")

      # Calculates Branch Segments
      branch_segments <- cylinder %>%
        distinct(.data$branchID, .data$reverseBranchOrder) %>%
        mutate(segmentID = 1:n())

      # Joins branch segments
      cylinder <- left_join(cylinder, branch_segments, by = c("branchID", "reverseBranchOrder"))

      # Parent Segments --------------------------------------------------------
      child_segments <- cylinder %>%
        select(ID = .data$parentID, childSegment = .data$segmentID) %>%
        distinct(.data$childSegment, .keep_all = TRUE)

      parent_segments <- left_join(
        select(cylinder, .data$ID, .data$segmentID),
        child_segments,
        by = c("ID")
      ) %>%
        drop_na() %>%
        select(segmentID = .data$childSegment, parentSegmentID = .data$segmentID)

      # Joins parent segments
      cylinder <- left_join(cylinder, parent_segments, by = "segmentID")
      cylinder <- mutate(cylinder, parentSegment = replace_na(.data$parentSegmentID, 0))
    }


    # Distance From Base (if missing) ------------------------------------------
    if (!"distanceFromBase" %in% colnames(cylinder)) {
      message("Calculating Distance From Base")

      # Calculate distance from base to cylinder
      base_distance <- tidytable(index = base_index, ID = base_id) %>%
        left_join(select(cylinder, .data$ID, .data$length), by = "ID") %>%
        group_by(.data$index) %>%
        summarize(distanceFromBase = sum(.data$length, na.rm = TRUE)) %>%
        rename(ID = .data$index)

      # Joins distance from base
      cylinder <- left_join(cylinder, base_distance, by = "ID")
    }

    # Distance To Twig (if missing) --------------------------------------------
    if (!"disnatnceToTwig" %in% colnames(cylinder)) {
      message("Calculating Average Distance To Twigs")

      # Calculate average distance to twigs
      twig_distance <- left_join(child_df, cylinder_info, by = "ID") %>%
        left_join(select(cylinder, .data$ID, .data$length), by = "ID") %>%
        group_by(.data$index) %>%
        summarize(distanceToTwig = sum(.data$length * .data$frequency) / sum(.data$twig)) %>%
        rename(ID = .data$index)

      # Joins distance from twig
      cylinder <- left_join(cylinder, twig_distance, by = "ID")
    }

    # Resets cylinder ordering
    cylinder <- cylinder %>%
      mutate(
        ID = .data$ID - 1,
        parentID = .data$parentID - 1
      )
  } else {
    message(
      "Invalid Dataframe Supplied!!!
      \nOnly TreeQSM or SimpleForest QSMs are supported.
      \nMake sure the cylinder data frame and not the QSM list is supplied."
    )
  }
  return(cylinder)
}
