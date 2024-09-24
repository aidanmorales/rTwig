#' QSM Summary
#'
#' @description Generates a simple QSM summary (e.g. volume, surface area, dbh, etc.) by totals and branch order.
#'
#' @param cylinder QSM cylinder data frame
#' @param radius Radius column name either quoted or unquoted.
#' @param triangulation QSM triangulation list. Defaults to NULL. Only supports TreeQSM.
#'
#' @return Returns a list
#' @export
#'
#' @examples
#'
#' ## SimpleForest Processing Chain
#' file <- system.file("extdata/QSM.csv", package = "rTwig")
#' cylinder <- read.csv(file)
#' cylinder <- update_cylinders(cylinder)
#' qsm_summary(cylinder, radius)
#'
#' # TreeQSM Triangulation
#' file <- system.file("extdata/QSM.mat", package = "rTwig")
#' qsm <- import_qsm(file)
#' cylinder <- qsm$cylinder
#' cylinder <- update_cylinders(cylinder)
#' qsm_summary(cylinder, radius, triangulation = qsm$triangulation)
#'
qsm_summary <- function(cylinder, radius = NULL, triangulation = NULL) {
  message("Creating QSM Summary")

  # User selected radius
  radius <- select_column(rlang::enquo(radius))

  if (is.null(radius)) {
    stop("Please supply a radius column, either quoted or unquoted!")
  } else {
    radius <- pull(cylinder, !!radius)
  }

  # rTwig ----------------------------------------------------------------------
  if (all(c("id", "parent", "start_x", "branch_order") %in% colnames(cylinder))) {
    if (!is.null(triangulation)) {
      message("rTwig does not support triangulation of the main stem!")
    }

    data_summary(
      cylinder = cylinder,
      radius = radius, length = "length", branch = "branch",
      branch_order = "branch_order", branch_position = "branch_position",
      start_z = "start.z", id = "id", triangulation = NULL
    )
  }
  # TreeQSM --------------------------------------------------------------------
  else if (all(c("parent", "extension", "branch", "BranchOrder") %in% colnames(cylinder))) {
    data_summary(
      cylinder = cylinder,
      radius = radius, length = "length", branch = "branch",
      branch_order = "BranchOrder", branch_position = "PositionInBranch",
      start_z = "start.z", id = "extension", triangulation = triangulation
    )
  }
  # SimpleForest ---------------------------------------------------------------
  else if (all(c("ID", "parentID", "branchID", "branchOrder") %in% colnames(cylinder))) {
    if (!is.null(triangulation)) {
      message("SimpleForest does not support triangulation of the main stem!")
    }

    data_summary(
      cylinder = cylinder,
      radius = radius, length = "length", branch = "branchID",
      branch_order = "branchOrder", branch_position = "positionInBranch",
      start_z = "startZ", id = "ID", triangulation = NULL
    )
  }
  # Treegraph ------------------------------------------------------------------
  else if (all(c("p1", "p2", "ninternode") %in% colnames(cylinder))) {
    if (!is.null(triangulation)) {
      message("Treegraph does not support triangulation of the main stem!")
    }

    data_summary(
      cylinder = cylinder,
      radius = radius, length = "length", branch = "nbranch",
      branch_order = "branch_order", branch_position = "positionInBranch",
      start_z = "sz", id = "p1", triangulation = NULL
    )
  }
  # aRchi ----------------------------------------------------------------------
  else if (all(c("cyl_ID", "parent_ID", "branching_order") %in% colnames(cylinder))) {
    if (!is.null(triangulation)) {
      message("aRchi does not support triangulation of the main stem!")
    }

    data_summary(
      cylinder = cylinder,
      radius = radius, length = "length", branch = "branch_ID",
      branch_order = "branching_order", branch_position = "positionInBranch",
      start_z = "startZ", id = "cyl_ID", triangulation = NULL
    )
  } else {
    message(
      "Invalid Dataframe Supplied!!!
      \nOnly TreeQSM, SimpleForest, Treegraph, or aRchi QSMs are supported.
      \nMake sure the cylinder data frame and not the QSM list is supplied."
    )
  }
}

#' Data summary
#' @param cylinder QSM cylinder data frame
#' @param radius cylinder radii
#' @param length cylinder length
#' @param branch cylinder branch id
#' @param branch_order cylinder branch order
#' @param branch_position cylinder branch position
#' @param start_z column name of start_z
#' @param id cylinder id
#' @param triangulation TreeQSM triangulation
#'
#' @return a list
#' @noRd
#'
data_summary <- function(
    cylinder,
    radius,
    length,
    branch,
    branch_order,
    branch_position,
    start_z,
    id,
    triangulation) {
  # Subset cylinder data
  cyl_sub <- cylinder %>%
    select(
      all_of(
        c(length, branch, branch_order, branch_position, start_z, id)
      )
    ) %>%
    rename(
      "length" = !!rlang::sym(length),
      "branch" = !!rlang::sym(branch),
      "branch_order" = !!rlang::sym(branch_order),
      "branch_position" = !!rlang::sym(branch_position),
      "start_z" = !!rlang::sym(start_z),
      "id" = !!rlang::sym(id)
    ) %>%
    mutate(radius := radius)

  # Diameter at breast height (DBH)
  dbh <- cyl_sub %>%
    filter(.data$branch_order == 0 & .data$branch == 1) %>%
    arrange(.data$branch_position) %>%
    select("length", "radius")

  for (i in 1:nrow(dbh)) {
    DBHCyl <- sum(dbh$length[1:i])
    if (DBHCyl >= 1.37) {
      break
    }
  }

  dbh_qsm_cm <- dbh$radius[as.numeric(i)] * 200

  # Tree height
  tree_height_m <- max(cyl_sub$start_z) - min(cyl_sub$start_z)

  # Triangulation volumes
  if (!is.null(triangulation)) {
    # Finds the triangulation end cylinder
    cyl_end <- pull(triangulation$cylind - 1)

    # Gets the QSM volumes
    qsm_vol_sa <- cyl_sub %>%
      filter(.data$id %in% c(1:cyl_end)) %>%
      mutate(
        volume = pi * .data$radius^2 * .data$length * 1e3,
        surface_area = 2 * pi * .data$radius * .data$length
      ) %>%
      summarize(
        cyl_volume = sum(.data$volume),
        cyl_surface_area = sum(.data$surface_area)
      )

    # Triangulation Volume and Surface Area
    tri_volume <- pull(triangulation$volume)
    tri_surface_area <- pull(triangulation$SideArea)
  }

  # Branch order summary
  summary <- cyl_sub %>%
    mutate(
      volume = pi * .data$radius^2 * .data$length,
      surface_area = 2 * pi * .data$radius * .data$length
    ) %>%
    group_by("branch_order") %>%
    summarize(
      tree_volume_L = sum(.data$volume, na.rm = TRUE) * 1e3,
      tree_area_m2 = sum(.data$surface_area, na.rm = TRUE)
    )

  # Update with triangulation volumes
  if (!is.null(triangulation)) {
    summary$tree_volume_L[1] <- summary$tree_volume_L[1] - qsm_vol_sa$cyl_volume + tri_volume
    summary$tree_area_m2[1] <- summary$tree_area_m2[1] - qsm_vol_sa$cyl_surface_area + tri_surface_area
  }

  # Tree summary
  tree_volume_L <- summary %>%
    summarize(tree_volume_L = sum(.data$tree_volume_L))

  stem_volume_L <- summary %>%
    filter(.data$branch_order == 0) %>%
    summarize(stem_volume_L = sum(.data$tree_volume_L))

  branch_volume_L <- summary %>%
    filter(.data$branch_order != 0) %>%
    summarize(branch_volume_L = sum(.data$tree_volume_L))

  tree_area_m2 <- summary %>%
    summarize(tree_area_m2 = sum(.data$tree_area_m2))

  stem_area_m2 <- summary %>%
    filter(.data$branch_order == 0) %>%
    summarize(stem_area_m2 = sum(.data$tree_area_m2))

  branch_area_m2 <- summary %>%
    filter(.data$branch_order != 0) %>%
    summarize(branch_area_m2 = sum(.data$tree_area_m2))

  summary2 <- bind_cols(
    "dbh_qsm_cm" = dbh_qsm_cm,
    "tree_height_m" = tree_height_m,
    stem_volume_L,
    branch_volume_L,
    tree_volume_L,
    stem_area_m2,
    branch_area_m2,
    tree_area_m2
  )

  return(list(summary, summary2))
}
