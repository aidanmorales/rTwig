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
update_cylinders <- function(df, method = "TreeQSM") {
  message("Updating Cylinders")

  if (method == "TreeQSM") {
    # Updates branch numbers
    df <- df %>%
      mutate(
        extension = row_number() + 1,
        id = extension - 1
      ) %>%
      filter(!(BranchOrder == 0 & branch != 1)) %>%
      distinct(.keep_all = TRUE)

    # Relabels branches consecutively
    cyl_id <- df %>%
      arrange(branch, parent, PositionInBranch) %>%
      group_by(branch) %>%
      summarize(n = n()) %>%
      mutate(
        branch_new = 1:n()
      )

    # Updates branch ordering
    df <- left_join(df, cyl_id, by = "branch") %>%
      relocate(id, .before = extension) %>%
      relocate(branch_new, .before = parent) %>%
      select(-c(n, branch, extension)) %>%
      rename("branch" = branch_new) %>%
      arrange(id)

    # Updates parent child ordering
    temp <- df %>%
      select(parent, id) %>%
      mutate(id_new = 1:n()) %>%
      rename(parent_old = parent)

    temp2 <- temp %>%
      select(parent_old = id, parent_new = id_new)

    newID <- temp %>%
      left_join(temp2, by = "parent_old") %>%
      select(parent = parent_new, id = id_new) %>%
      mutate(parent = case_when(is.na(parent) ~ 0, TRUE ~ as.double(parent))) %>%
      mutate(parent = case_when(parent == 0 & id > 1 ~ lag(as.double(id), 1), TRUE ~ as.double(parent)))

    df <- df %>%
      select(-parent, -id) %>%
      bind_cols(newID) %>%
      relocate(parent, .after = branch) %>%
      relocate(id, .after = parent)

    # Adds cylinder info for plotting and converts to local coordinate system
    df <- df %>%
      mutate(
        start.z = start.z - min(start.z),
        end.x = start.x + (axis.x * length),
        end.y = start.y + (axis.y * length),
        end.z = start.z + (axis.z * length)
      ) %>%
      relocate(end.x, .after = axis.z) %>%
      relocate(end.y, .after = end.x) %>%
      relocate(end.z, .after = end.y)

    temp <- df %>%
      group_by(parent) %>%
      summarize(totChildren = n())

    # Joins total children and
    df <- df %>%
      left_join(temp, by = "parent") %>%
      mutate(radius = UnmodRadius)
  } else if (method == "SimpleForest") {
    # Adds cylinder info for plotting
    df <- df %>%
      mutate(
        UnmodRadius = radius,
        axisX = (endX - startX) / length,
        axisY = (endY - startY) / length,
        axisZ = (endZ - startZ) / length
      ) %>%
      relocate(axisX, .after = endZ) %>%
      relocate(axisY, .after = axisX) %>%
      relocate(axisZ, .after = axisY) %>%
      relocate(radius, .before = radius)
  } else {
    message("Invalid Method Entered!!!\nValid Methods = TreeQSM or SimpleForest")
  }

  return(df)
}
