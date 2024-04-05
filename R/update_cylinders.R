#' Update Cylinders
#'
#' @description Updates the QSM cylinder data in preparation for radii correction
#'
#' @details Updates parent-child branch and cylinder relationships to fill in any gaps.
#' Three additional useful QSM metrics developed by Jan Hackenberg are also calculated.
#' Growth length is the length of a parent cylinder, plus the lengths of all of
#' its child cylinders. The segment is a portion of a branch between two branching nodes.
#' The reverse branch order assigns twigs as order 1 and works backwards at each
#' branching junction to the base of the stem, which has the largest reverse branch order.
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
    cyl_id <- cylinder %>%
      arrange(.data$branch, .data$parent, .data$PositionInBranch) %>%
      group_by(.data$branch) %>%
      summarize(n = n()) %>%
      mutate(
        branch_new = 1:n()
      )

    # Updates branch ordering
    cylinder <- left_join(cylinder, cyl_id, by = "branch") %>%
      relocate(.data$id, .before = .data$extension) %>%
      relocate(.data$branch_new, .before = .data$parent) %>%
      select(-c(.data$n, .data$branch, .data$extension)) %>%
      rename("branch" = .data$branch_new) %>%
      arrange(.data$id)

    # Parent Child Ordering ----------------------------------------------------

    # Updates parent child ordering
    temp <- cylinder %>%
      select(.data$parent, .data$id) %>%
      mutate(id_new = 1:n()) %>%
      rename(parent_old = .data$parent)

    temp2 <- temp %>%
      select(parent_old = .data$id, parent_new = .data$id_new)

    newID <- temp %>%
      left_join(temp2, by = "parent_old") %>%
      select(parent = .data$parent_new, id = .data$id_new) %>%
      mutate(parent = case_when(is.na(.data$parent) ~ 0, TRUE ~ as.double(.data$parent))) %>%
      mutate(parent = case_when(.data$parent == 0 & .data$id > 1 ~ lag(as.double(.data$id), 1), TRUE ~ as.double(.data$parent)))

    cylinder <- cylinder %>%
      select(-.data$parent, -.data$id) %>%
      bind_cols(newID) %>%
      relocate(.data$parent, .after = .data$branch) %>%
      relocate(.data$id, .after = .data$parent) %>%
      rename(extension = .data$id)

    # Total Children -----------------------------------------------------------

    # Adds supported children for each cylinder
    tot_children <- cylinder %>%
      group_by(.data$parent) %>%
      summarize(totalChildren = n())

    # Joins total children
    cylinder <- cylinder %>%
      left_join(tot_children, by = "parent")

    # Growth Length ------------------------------------------------------------

    message("Calculating Growth Length")

    # Find cylinder relationships and paths
    g <- data.frame(parent = cylinder$parent, extension = cylinder$extension)
    g <- igraph::graph_from_data_frame(g) - 1
    g <- igraph::permute(g, match(igraph::V(g)$name, cylinder$extension))

    paths <- igraph::ego(g, order = igraph::vcount(g), mode = "out")

    # Calculate growth length
    GrowthLength <- paths %>%
      enframe() %>%
      unnest() %>%
      rename(index = .data$name, extension = .data$value) %>%
      mutate(index = as.double(.data$index), extension = as.double(.data$extension)) %>%
      left_join(cylinder %>%
        select(.data$extension, .data$length), by = "extension") %>%
      select(extension = .data$index, .data$length) %>%
      group_by(.data$extension) %>%
      summarize(GrowthLength = sum(.data$length, na.rm = TRUE))

    # Joins growth length
    cylinder <- left_join(cylinder, GrowthLength, by = "extension")

    # Reverse Branch Order -----------------------------------------------------

    message("Calculating Reverse Branch Order")

    # Creates path network
    g <- data.frame(parent = cylinder$parent, extension = cylinder$extension)
    g <- igraph::graph_from_data_frame(g)

    starts <- igraph::V(g)[igraph::degree(g, mode = "in") == 0]
    finals <- igraph::V(g)[igraph::degree(g, mode = "out") == 0]

    paths <- igraph::all_simple_paths(g, from = starts[[1]], to = finals)

    # Calculates Branch Nodes & Node Depth
    nodes <- sapply(paths, as_ids) %>%
      enframe() %>%
      unnest_longer(col = "value") %>%
      rename(index = .data$name, extension = .data$value) %>%
      mutate(index = as.double(.data$index), extension = as.double(.data$extension)) %>%
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
    cylinder <- left_join(cylinder, nodes, by = "extension") %>%
      fill(.data$reverseBranchOrder, .direction = "down")

    # Branch Segments ----------------------------------------------------------

    # Calculates Branch Segments
    segments <- cylinder %>%
      distinct(.data$branch, .data$reverseBranchOrder) %>%
      mutate(segment = 1:n())

    # Joins branch segments
    cylinder <- left_join(cylinder, segments, by = c("branch", "reverseBranchOrder"))

    # Calculates Parent Segments
    parents <- cylinder %>%
      select(.data$extension, .data$segment)

    children <- cylinder %>%
      select(extension = .data$parent, childSegment = .data$segment) %>%
      distinct(.data$childSegment, .keep_all = TRUE)

    segments2 <- left_join(parents, children, by = c("extension")) %>%
      drop_na() %>%
      select(segment = .data$childSegment, parentSegment = .data$segment)

    # Joins parent segments
    cylinder <- left_join(cylinder, segments2, by = c("segment"))
    cylinder <- mutate(cylinder, parentSegment = replace_na(.data$parentSegment, 0))

    # Plotting Info ------------------------------------------------------------

    # Adds cylinder endpoints for plotting
    cylinder <- cylinder %>%
      mutate(
        end.x = .data$start.x + (.data$axis.x * .data$length),
        end.y = .data$start.y + (.data$axis.y * .data$length),
        end.z = .data$start.z + (.data$axis.z * .data$length)
      ) %>%
      relocate(.data$end.x, .after = .data$axis.z) %>%
      relocate(.data$end.y, .after = .data$end.x) %>%
      relocate(.data$end.z, .after = .data$end.y)

    # Save All Radii -----------------------------------------------------------
    cylinder <- cylinder %>%
      mutate(OldRadius = .data$radius, radius = .data$UnmodRadius)

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
        relocate(.data$SurfCov, .after = .data$mad)
    } else {
      cylinder <- cylinder %>%
        relocate(.data$OldRadius, .after = .data$UnmodRadius) %>%
        relocate(.data$reverseBranchOrder, .after = .data$BranchOrder) %>%
        relocate(.data$segment, .after = .data$PositionInBranch) %>%
        relocate(.data$parentSegment, .after = .data$segment) %>%
        relocate(.data$UnmodRadius, .after = .data$end.z) %>%
        relocate(.data$OldRadius, .after = .data$UnmodRadius) %>%
        relocate(.data$GrowthLength, .after = .data$OldRadius)
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
    tot_children <- cylinder %>%
      group_by(.data$parentID) %>%
      summarize(totalChildren = n())

    # Joins total children
    cylinder <- cylinder %>%
      left_join(tot_children, by = "parentID")

    # Add position in branch segment
    cylinder <- cylinder %>%
      group_by(.data$branchOrder, .data$branchID, .data$segmentID) %>%
      mutate(branchNew = cur_group_id()) %>%
      group_by(.data$branchNew) %>%
      mutate(positionInBranch = 1:n()) %>%
      ungroup() %>%
      relocate(.data$branchNew, .after = .data$branchID) %>%
      relocate(.data$positionInBranch, .after = .data$branchNew)

    # Calculates Growth Length if Missing
    if (!"growthLength" %in% colnames(cylinder)) {
      message("Calculating Growth Length")

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
        unnest() %>%
        rename(index = .data$name, ID = .data$value) %>%
        mutate(index = as.double(.data$index), ID = as.double(.data$ID)) %>%
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
    }

    # Calculates Reverse Branch Order if Missing
    if (!"reverseBranchOrder" %in% colnames(cylinder)) {
      message("Calculating Reverse Branch Order")

      cylinder <- cylinder %>%
        mutate(
          ID = .data$ID + 1,
          parentID = .data$parentID + 1
        )

      # Creates path network
      g <- data.frame(parent = cylinder$parentID, extension = cylinder$ID)
      g <- igraph::graph_from_data_frame(g)

      starts <- igraph::V(g)[igraph::degree(g, mode = "in") == 0]
      finals <- igraph::V(g)[igraph::degree(g, mode = "out") == 0]

      paths <- igraph::all_simple_paths(g, from = starts[[1]], to = finals)

      # Calculates Branch Nodes & Node Depth
      nodes <- sapply(paths, as_ids) %>%
        enframe() %>%
        unnest_longer(col = "value") %>%
        rename(index = .data$name, ID = .data$value) %>%
        mutate(index = as.double(.data$index), ID = as.double(.data$ID)) %>%
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
      cylinder <- left_join(cylinder, nodes, by = "ID") %>%
        fill(.data$reverseBranchOrder, .direction = "down") %>%
        mutate(
          ID = .data$ID - 1,
          parentID = .data$parentID - 1
        )

      # Calculates Branch Segments
      segments <- cylinder %>%
        distinct(.data$branchID, .data$reverseBranchOrder) %>%
        mutate(segmentID = 1:n())

      # Joins branch segments
      cylinder <- left_join(cylinder, segments, by = c("branchID", "reverseBranchOrder"))

      # Calculates Parent Segments
      parents <- cylinder %>%
        select(.data$ID, .data$segmentID)

      children <- cylinder %>%
        select(ID = .data$parentID, childSegment = .data$segmentID) %>%
        distinct(.data$childSegment, .keep_all = TRUE)

      segments2 <- left_join(parents, children, by = c("ID")) %>%
        drop_na() %>%
        select(segmentID = .data$childSegment, parentSegmentID = .data$segment)

      # Joins parent segments
      cylinder <- left_join(cylinder, segments2, by = c("segment"))
      cylinder <- cylinder %>%
        mutate(
          parentSegmentID = replace_na(.data$parentSegmentID, 0),
          segmentID = .data$segmentID - 1,
          parentSegmentID = .data$parentSegmentID - 1
        )
    }
  } else {
    message(
      "Invalid Dataframe Supplied!!!
      \nOnly TreeQSM or SimpleForest QSMs are supported.
      \nMake sure the cylinder data frame and not the QSM list is supplied."
    )
  }
  return(cylinder)
}
