#' QSM Summary
#'
#' @description Generates summaries of QSM features (e.g. volume, surface area, dbh, etc.) by totals and branch order
#'
#' @param cylinder QSM cylinder data frame
#' @param radius Radius type as either "modified", "unmodified", or "old". Defaults to "modified".
#' @param triangulation QSM triangulation list. Defaults to FALSE.
#'
#' @return Returns a list
#' @export
#'
#' @examples
#'
#' ## TreeQSM Processing Chain
#' file <- system.file("extdata/QSM.mat", package = "rTwig")
#' qsm <- import_qsm(file)
#' cylinder <- qsm$cylinder
#' cylinder <- update_cylinders(cylinder)
#' qsm_summary(cylinder)
#'
#' # TreeQSM Triangulation
#' file <- system.file("extdata/QSM.mat", package = "rTwig")
#' qsm <- import_qsm(file)
#' cylinder <- qsm$cylinder
#' cylinder <- update_cylinders(cylinder)
#' triangulation <- qsm$triangulation
#' qsm_summary(cylinder = cylinder, triangulation = triangulation)
#'
#' ## SimpleForest Processing Chain
#' file <- system.file("extdata/QSM.csv", package = "rTwig")
#' cylinder <- read.csv(file)
#' cylinder <- update_cylinders(cylinder)
#' qsm_summary(cylinder)
#'
qsm_summary <- function(cylinder, radius = "modified", triangulation = FALSE) {
  message("Creating QSM Summary")

  # rTwig ----------------------------------------------------------------------
  if (all(c("id", "parent", "start_x", "branch_order") %in% colnames(cylinder))) {
    # Setup Radius
    if (radius == "modified") {
      cylinder$sumrad <- cylinder$radius
    } else if (radius == "unmodified") {
      cylinder$sumrad <- cylinder$raw_radius
    } else if (radius == "old") {
      cylinder$sumrad <- cylinder$raw_radius
    }

    dbh <- cylinder %>%
      filter(.data$branch_order == 0 & .data$branch == 1) %>%
      arrange(.data$branch_position) %>%
      select(.data$length, .data$sumrad)

    # Finds the DBH cylinder
    for (i in 1:nrow(dbh)) {
      DBHCyl <- sum(dbh$length[1:i])
      if (DBHCyl >= 1.37) {
        break
      }
    }

    DBHCyl <- as.numeric(i)

    dbh_qsm_cm <- dbh$sumrad[DBHCyl] * 200

    tree_height_m <- max(cylinder$start_z) - min(cylinder$start_z)

    # Branch Order Summary
    summary <- cylinder %>%
      mutate(
        Volume = pi * .data$sumrad^2 * .data$length,
        SurfaceArea = 2 * pi * .data$sumrad * .data$length
      ) %>%
      group_by("branch_order") %>%
      summarize(
        tree_volume_L = sum(.data$Volume, na.rm = TRUE) * 1e3,
        tree_area_m2 = sum(.data$SurfaceArea, na.rm = TRUE)
      )

    # Total Summary
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
  }
  # TreeQSM --------------------------------------------------------------------
  else if (all(c("parent", "extension", "branch", "BranchOrder") %in% colnames(cylinder))) {
    # Setup Radius
    if (radius == "modified") {
      cylinder$sumrad <- cylinder$radius
    } else if (radius == "unmodified") {
      cylinder$sumrad <- cylinder$UnmodRadius
    } else if (radius == "old") {
      cylinder$sumrad <- cylinder$OldRadius
    }

    dbh <- cylinder %>%
      filter(.data$BranchOrder == 0 & .data$branch == 1) %>%
      arrange(.data$PositionInBranch) %>%
      select(.data$length, .data$sumrad)

    # Gets the triangulation and QSM volumes and surface areas to be swapped
    if (!is.logical(triangulation)) {
      # Finds the triangulation end cylinder
      cyl_end <- pull(triangulation$cylind - 2)

      # Gets the QSM volumes
      QSM_vol_sa <- cylinder %>%
        filter(.data$extension %in% c(1:cyl_end)) %>%
        mutate(
          Volume = pi * .data$sumrad^2 * .data$length * 1e3,
          SurfaceArea = 2 * pi * .data$sumrad * .data$length
        ) %>%
        summarize(
          CylVol = sum(.data$Volume),
          CylSA = sum(.data$SurfaceArea)
        )

      # Triangulation Volume and Surface Area
      TriVol <- pull(triangulation$volume)
      TriSA <- pull(triangulation$SideArea)
    }

    # Finds the DBH cylinder
    for (i in 1:nrow(dbh)) {
      DBHCyl <- sum(dbh$length[1:i])
      if (DBHCyl >= 1.37) {
        break
      }
    }

    DBHCyl <- as.numeric(i)

    dbh_qsm_cm <- dbh$sumrad[DBHCyl] * 200

    tree_height_m <- max(cylinder$start.z) - min(cylinder$start.z)

    # Branch Order Summary
    summary <- cylinder %>%
      mutate(
        Volume = pi * .data$sumrad^2 * .data$length,
        SurfaceArea = 2 * pi * .data$sumrad * .data$length
      ) %>%
      group_by("BranchOrder") %>%
      summarize(
        tree_volume_L = sum(.data$Volume, na.rm = TRUE) * 1e3,
        tree_area_m2 = sum(.data$SurfaceArea, na.rm = TRUE)
      )

    # Update with triangulation volumes
    if (!is.logical(triangulation)) {
      summary$tree_volume_L[1] <- summary$tree_volume_L[1] - QSM_vol_sa$CylVol + TriVol
      summary$tree_area_m2[1] <- summary$tree_area_m2[1] - QSM_vol_sa$CylSA + TriSA
    }

    # Total Summary
    tree_volume_L <- summary %>%
      summarize(tree_volume_L = sum(.data$tree_volume_L))

    stem_volume_L <- summary %>%
      filter(.data$BranchOrder == 0) %>%
      summarize(stem_volume_L = sum(.data$tree_volume_L))

    branch_volume_L <- summary %>%
      filter(.data$BranchOrder != 0) %>%
      summarize(branch_volume_L = sum(.data$tree_volume_L))

    tree_area_m2 <- summary %>%
      summarize(tree_area_m2 = sum(.data$tree_area_m2))

    stem_area_m2 <- summary %>%
      filter(.data$BranchOrder == 0) %>%
      summarize(stem_area_m2 = sum(.data$tree_area_m2))

    branch_area_m2 <- summary %>%
      filter(.data$BranchOrder != 0) %>%
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
  }
  # SimpleForest ---------------------------------------------------------------
  else if (all(c("ID", "parentID", "branchID", "branchOrder") %in% colnames(cylinder))) {
    # Stop on Triangulation
    if (!triangulation == FALSE) {
      stop("SimpleForest does not support triangulation of the main stem!")
    }

    # Setup Radius
    if (radius == "modified") {
      cylinder$sumrad <- cylinder$radius
    } else if (radius == "unmodified") {
      cylinder$sumrad <- cylinder$UnmodRadius
    } else if (radius == "old") {
      cylinder$sumrad <- cylinder$OldRadius
    }

    dbh <- cylinder %>%
      filter(.data$branchOrder == 0 & .data$branchID == 1) %>%
      arrange(.data$ID) %>%
      select("length", "sumrad")

    # Finds the DBH cylinder
    for (i in 1:nrow(dbh)) {
      DBHCyl <- sum(dbh$length[1:i])
      if (DBHCyl >= 1.37) {
        break
      }
    }

    DBHCyl <- as.numeric(i)

    dbh_qsm_cm <- dbh$sumrad[DBHCyl] * 200

    tree_height_m <- max(cylinder$startZ) - min(cylinder$startZ)

    summary <- cylinder %>%
      mutate(
        Volume = pi * .data$sumrad^2 * .data$length,
        SurfaceArea = 2 * pi * .data$sumrad * .data$length
      ) %>%
      group_by("branchOrder") %>%
      summarize(
        tree_volume_L = sum(.data$Volume, na.rm = TRUE) * 1e3,
        tree_area_m2 = sum(.data$SurfaceArea, na.rm = TRUE)
      ) %>%
      rename(BranchOrder = "branchOrder")

    tree_volume_L <- summary %>%
      summarize(tree_volume_L = sum(.data$tree_volume_L))

    stem_volume_L <- summary %>%
      filter(.data$BranchOrder == 0) %>%
      summarize(stem_volume_L = sum(.data$tree_volume_L))

    branch_volume_L <- summary %>%
      filter(.data$BranchOrder != 0) %>%
      summarize(branch_volume_L = sum(.data$tree_volume_L))

    tree_area_m2 <- summary %>%
      summarize(tree_area_m2 = sum(.data$tree_area_m2))

    stem_area_m2 <- summary %>%
      filter(.data$BranchOrder == 0) %>%
      summarize(stem_area_m2 = sum(.data$tree_area_m2))

    branch_area_m2 <- summary %>%
      filter(.data$BranchOrder != 0) %>%
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
  }
  # Treegraph ------------------------------------------------------------------
  else if (all(c("p1", "p2", "ninternode") %in% colnames(cylinder))) {
    # Stop on Triangulation
    if (!triangulation == FALSE) {
      stop("Treegraph does not support triangulation of the main stem!")
    }

    # Setup Radius
    if (radius == "modified") {
      cylinder$sumrad <- cylinder$radius
    } else if (radius == "unmodified") {
      cylinder$sumrad <- cylinder$UnmodRadius
    } else if (radius == "old") {
      cylinder$sumrad <- cylinder$OldRadius
    }

    dbh <- cylinder %>%
      filter(.data$branch_order == 0 & .data$nbranch == 1) %>%
      arrange(.data$p1) %>%
      select("length", "sumrad")

    # Finds the DBH cylinder
    for (i in 1:nrow(dbh)) {
      DBHCyl <- sum(dbh$length[1:i])
      if (DBHCyl >= 1.37) {
        break
      }
    }

    DBHCyl <- as.numeric(i)

    dbh_qsm_cm <- dbh$sumrad[DBHCyl] * 200

    tree_height_m <- max(cylinder$sz) - min(cylinder$sz)

    summary <- cylinder %>%
      mutate(
        Volume = pi * .data$sumrad^2 * .data$length,
        SurfaceArea = 2 * pi * .data$sumrad * .data$length
      ) %>%
      group_by("branch_order") %>%
      summarize(
        tree_volume_L = sum(.data$Volume, na.rm = TRUE) * 1e3,
        tree_area_m2 = sum(.data$SurfaceArea, na.rm = TRUE)
      ) %>%
      rename(BranchOrder = "branch_order")

    tree_volume_L <- summary %>%
      summarize(tree_volume_L = sum(.data$tree_volume_L))

    stem_volume_L <- summary %>%
      filter(.data$BranchOrder == 0) %>%
      summarize(stem_volume_L = sum(.data$tree_volume_L))

    branch_volume_L <- summary %>%
      filter(.data$BranchOrder != 0) %>%
      summarize(branch_volume_L = sum(.data$tree_volume_L))

    tree_area_m2 <- summary %>%
      summarize(tree_area_m2 = sum(.data$tree_area_m2))

    stem_area_m2 <- summary %>%
      filter(.data$BranchOrder == 0) %>%
      summarize(stem_area_m2 = sum(.data$tree_area_m2))

    branch_area_m2 <- summary %>%
      filter(.data$BranchOrder != 0) %>%
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
  } else {
    message(
      "Invalid Dataframe Supplied!!!
      \nOnly TreeQSM, SimpleForest, or Treegraph QSMs are supported.
      \nMake sure the cylinder data frame and not the QSM list is supplied."
    )
  }
  return(list(summary, summary2))
}
