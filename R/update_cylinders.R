#' Update Cylinders
#'
#' @description Updates the QSM cylinder data in preparation for growth length and path analysis calculations
#'
#' @param cylinder QSM cylinder data frame
#'
#' @return Returns a data frame
#' @export
#'
#' @import dplyr
#'
#' @examples
#' \dontrun{
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
#' }
update_cylinders <- function(cylinder) {
  message("Updating cylinder")

  # TreeQSM --------------------------------------------------------------------
  if (all(c("parent", "extension", "branch", "BranchOrder") %in% colnames(cylinder))) {

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
      relocate(.data$id, .after = .data$parent)

    # Adds supported children for each cylinder
    tot_children <- cylinder %>%
      group_by(.data$parent) %>%
      summarize(totChildren = n())

    # Joins total children
    cylinder <- cylinder %>%
      left_join(tot_children, by = "parent")

    # Adds cylinder info for plotting and converts to local coordinate system
    cylinder <- cylinder %>%
      mutate(
        # start.z = .data$start.z - min(.data$start.z),
        end.x = .data$start.x + (.data$axis.x * .data$length),
        end.y = .data$start.y + (.data$axis.y * .data$length),
        end.z = .data$start.z + (.data$axis.z * .data$length)
      ) %>%
      relocate(.data$end.x, .after = .data$axis.z) %>%
      relocate(.data$end.y, .after = .data$end.x) %>%
      relocate(.data$end.z, .after = .data$end.y) %>%
      mutate(radius = .data$UnmodRadius) %>%
      rename(extension = id)

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
      summarize(totChildren = n())

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
  } else {
    message(
      "Invalid Dataframe Supplied!!!
      \nOnly TreeQSM or SimpleForest QSMs are supported.
      \nMake sure the cylinder data frame and not the QSM list is supplied."
    )
  }
  return(cylinder)
}
