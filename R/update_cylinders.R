#' Update Cylinders
#'
#' @description Updates the QSM cylinder data in preparation for growth length and path analysis calculations.
#'
#' @param df QSM cylinder data.frame
#' @param method QSM type, as either "TreeQSM" or "SimpleForest". Defaults to TreeQSM.
#'
#' @return Returns a data.frame
#' @export
#'
#' @import dplyr
#'
#' @examples
#' \dontrun{
#' ## TreeQSM Processing Chain
#' file <- system.file("extdata/QSM.mat", package = "rTwig")
#' df <- import_qsm(file)
#' df <- update_cylinders(df)
#' str(df)
#'
#' ## SimpleForest Processing Chain
#' file <- system.file("extdata/QSM.csv", package = "rTwig")
#' df <- read.csv(file)
#' df <- update_cylinders(df, method = "SimpleForest")
#' str(df)
#' }
update_cylinders <- function(df, method = "TreeQSM") {
  message("Updating Cylinders")

  if (method == "TreeQSM") {
    # Updates branch numbers
    df <- df %>%
      mutate(
        extension = row_number() + 1,
        id = .data$extension - 1
      ) %>%
      filter(!(.data$BranchOrder == 0 & .data$branch != 1)) %>%
      distinct(.keep_all = TRUE)

    # Relabels branches consecutively
    cyl_id <- df %>%
      arrange(.data$branch, .data$parent, .data$PositionInBranch) %>%
      group_by(.data$branch) %>%
      summarize(n = n()) %>%
      mutate(
        branch_new = 1:n()
      )

    # Updates branch ordering
    df <- left_join(df, cyl_id, by = "branch") %>%
      relocate(.data$id, .before = .data$extension) %>%
      relocate(.data$branch_new, .before = .data$parent) %>%
      select(-c(.data$n, .data$branch, .data$extension)) %>%
      rename("branch" = .data$branch_new) %>%
      arrange(.data$id)

    # Updates parent child ordering
    temp <- df %>%
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

    df <- df %>%
      select(-.data$parent, -.data$id) %>%
      bind_cols(newID) %>%
      relocate(.data$parent, .after = .data$branch) %>%
      relocate(.data$id, .after = .data$parent)

    # Adds cylinder info for plotting and converts to local coordinate system
    df <- df %>%
      mutate(
        start.z = .data$start.z - min(.data$start.z),
        end.x = .data$start.x + (.data$axis.x * .data$length),
        end.y = .data$start.y + (.data$axis.y * .data$length),
        end.z = .data$start.z + (.data$axis.z * .data$length)
      ) %>%
      relocate(.data$end.x, .after = .data$axis.z) %>%
      relocate(.data$end.y, .after = .data$end.x) %>%
      relocate(.data$end.z, .after = .data$end.y)

    temp <- df %>%
      group_by(.data$parent) %>%
      summarize(totChildren = n())

    # Joins total children and
    df <- df %>%
      left_join(temp, by = "parent") %>%
      mutate(radius = .data$UnmodRadius)
  } else if (method == "SimpleForest") {
    # Adds cylinder info for plotting
    df <- df %>%
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
  } else {
    message("Invalid Method Entered!!!\nValid Methods = TreeQSM or SimpleForest")
  }

  return(df)
}
