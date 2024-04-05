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
#' @rawNamespace import(tidytable, except=c(map_dfr))
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

  # TreeQSM --------------------------------------------------------------------
  if (all(c("parent", "extension", "branch", "BranchOrder") %in% colnames(cylinder))) {

    # Setup Radius
    if (radius == "modified") {
      cylinder$sumrad <- cylinder$radius
    } else if (radius == "unmodified") {
      cylinder$sumrad<- cylinder$UnmodRadius
    } else if (radius == "old") {
      cylinder$sumrad <- cylinder$OldRadius
    }

    dbh <- cylinder %>%
      filter(.data$BranchOrder == 0 & .data$branch == 1) %>%
      arrange(.data$PositionInBranch) %>%
      select(.data$length, .data$sumrad)

    # Gets the triangulation and QSM volumes and surface areas to be swapped
    if(!is.logical(triangulation)){

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

    QSM.dbh.cm <- dbh$sumrad[DBHCyl] * 200

    QSM.ht.m <- max(cylinder$start.z) - min(cylinder$start.z)

    # Branch Order Summary
    summary <- cylinder %>%
      mutate(
        Volume = pi * .data$sumrad^2 * .data$length,
        SurfaceArea = 2 * pi * .data$sumrad * .data$length
      ) %>%
      group_by(.data$BranchOrder) %>%
      summarize(
        Tot.vol.L = sum(.data$Volume, na.rm = TRUE) * 1e3,
        Tot.sa.m2 = sum(.data$SurfaceArea, na.rm = TRUE)
      )

    # Update with triangulation volumes
    if(!is.logical(triangulation)){
      summary$Tot.vol.L[1] <- summary$Tot.vol.L[1] - QSM_vol_sa$CylVol + TriVol
      summary$Tot.sa.m2[1] <- summary$Tot.sa.m2[1] - QSM_vol_sa$CylSA + TriSA
    }

    # Total Summary
    Tot.vol.L <- summary %>%
      summarize(Tot.vol.L = sum(.data$Tot.vol.L))

    Stem.vol.L <- summary %>%
      filter(.data$BranchOrder == 0) %>%
      summarize(Stem.vol.L = sum(.data$Tot.vol.L))

    Branch.vol.L <- summary %>%
      filter(.data$BranchOrder != 0) %>%
      summarize(Branch.vol.L = sum(.data$Tot.vol.L))

    Tot.sa.m2 <- summary %>%
      summarize(Tot.sa.m2 = sum(.data$Tot.sa.m2))

    Stem.sa.m2 <- summary %>%
      filter(.data$BranchOrder == 0) %>%
      summarize(Stem.sa.m2 = sum(.data$Tot.sa.m2))

    Branch.sa.m2 <- summary %>%
      filter(.data$BranchOrder != 0) %>%
      summarize(Branch.sa.m2 = sum(.data$Tot.sa.m2))

    summary2 <- bind_cols(
      "QSM.dbh.cm" = QSM.dbh.cm,
      "QSM.ht.m" = QSM.ht.m,
      Stem.vol.L,
      Branch.vol.L,
      Tot.vol.L,
      Stem.sa.m2,
      Branch.sa.m2,
      Tot.sa.m2
    )

  # SimpleForest ---------------------------------------------------------------
  } else if (all(c("ID", "parentID", "branchID", "branchOrder") %in% colnames(cylinder))) {

    # Stop on Triangulation
    if(!triangulation == FALSE){
      stop("SimpleForest does not support triangulation of the main stem!")
    }

    # Setup Radius
    if (radius == "modified") {
      cylinder$sumrad <- cylinder$radius
    } else if (radius == "unmodified") {
      cylinder$sumrad<- cylinder$UnmodRadius
    } else if (radius == "old") {
      cylinder$sumrad <- cylinder$OldRadius
    }

    dbh <- cylinder %>%
      filter(.data$branchOrder == 0 & .data$branchID == 0) %>%
      arrange(.data$ID) %>%
      select(.data$length, .data$sumrad)

    # Finds the DBH cylinder
    for (i in 1:nrow(dbh)) {
      DBHCyl <- sum(dbh$length[1:i])
      if (DBHCyl >= 1.37) {
        break
      }
    }

    DBHCyl <- as.numeric(i)

    QSM.dbh.cm <- dbh$sumrad[DBHCyl] * 200

    QSM.ht.m <- max(cylinder$startZ) - min(cylinder$startZ)

    summary <- cylinder %>%
      mutate(
        Volume = pi * .data$sumrad^2 * .data$length,
        SurfaceArea = 2 * pi * .data$sumrad * .data$length
      ) %>%
      group_by(.data$branchOrder) %>%
      summarize(
        Tot.vol.L = sum(.data$Volume, na.rm = TRUE) * 1e3,
        Tot.sa.m2 = sum(.data$SurfaceArea, na.rm = TRUE)
      ) %>%
      rename(BranchOrder = .data$branchOrder)

    Tot.vol.L <- summary %>%
      summarize(Tot.vol.L = sum(.data$Tot.vol.L))

    Stem.vol.L <- summary %>%
      filter(.data$BranchOrder == 0) %>%
      summarize(Stem.vol.L = sum(.data$Tot.vol.L))

    Branch.vol.L <- summary %>%
      filter(.data$BranchOrder != 0) %>%
      summarize(Branch.vol.L = sum(.data$Tot.vol.L))

    Tot.sa.m2 <- summary %>%
      summarize(Tot.sa.m2 = sum(.data$Tot.sa.m2))

    Stem.sa.m2 <- summary %>%
      filter(.data$BranchOrder == 0) %>%
      summarize(Stem.sa.m2 = sum(.data$Tot.sa.m2))

    Branch.sa.m2 <- summary %>%
      filter(.data$BranchOrder != 0) %>%
      summarize(Branch.sa.m2 = sum(.data$Tot.sa.m2))

    summary2 <- bind_cols(
      "QSM.dbh.cm" = QSM.dbh.cm,
      "QSM.ht.m" = QSM.ht.m,
      Stem.vol.L,
      Branch.vol.L,
      Tot.vol.L,
      Stem.sa.m2,
      Branch.sa.m2,
      Tot.sa.m2
    )
  } else {
    message(
      "Invalid Dataframe Supplied!!!
      \nOnly TreeQSM or SimpleForest QSMs are supported.
      \nMake sure the cylinder data frame and not the QSM list is supplied."
    )
  }
  return(list(summary, summary2))
}
