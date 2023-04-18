#' QSM Summary
#'
#' @description Generates summaries of QSM features (e.g. volume, surface area, dbh, etc.) by totals and branch order.
#'
#' @param df QSM cylinder data frame
#'
#' @return Returns a list
#' @export
#'
#' @import dplyr
#'
#' @examples
#' \dontrun{
#' ## TreeQSM Processing Chain
#' file <- system.file("extdata/QSM.mat", package = "rTwig")
#' df <- import_qsm(file)
#' qsm_summary(df)
#'
#' ## SimpleForest Processing Chain
#' file <- system.file("extdata/QSM.csv", package = "rTwig")
#' df <- read.csv(file)
#' qsm_summary(df)
#' }
qsm_summary <- function(df) {
  message("Creating QSM Summary")

  if (all(c("parent", "extension", "branch", "BranchOrder") %in% colnames(df))) {
    dbh <- df %>%
      filter(.data$BranchOrder == 0) %>%
      arrange(.data$PositionInBranch) %>%
      select(.data$length, .data$radius)

    # Finds the DBH cylinder
    for (i in 1:nrow(dbh)) {
      DBHCyl <- sum(dbh$length[1:i])
      if (DBHCyl >= 1.37) {
        break
      }
    }

    DBHCyl <- as.numeric(i)

    QSM.dbh.cm <- dbh$radius[DBHCyl] * 200

    QSM.ht.m <- max(df$start.z) - min(df$start.z)

    summary <- df %>%
      mutate(
        Volume = pi * .data$radius^2 * .data$length,
        SurfaceArea = 2 * pi * .data$radius * .data$length
      ) %>%
      group_by(.data$BranchOrder) %>%
      summarize(
        Tot.vol.L = sum(.data$Volume, na.rm = TRUE) * 1e3,
        Tot.sa.m2 = sum(.data$SurfaceArea, na.rm = TRUE)
      )

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
  } else if (all(c("ID", "parentID", "branchID", "branchOrder") %in% colnames(df))) {
    dbh <- df %>%
      filter(.data$branchOrder == 0) %>%
      arrange(.data$ID) %>%
      select(.data$length, .data$radius)

    # Finds the DBH cylinder
    for (i in 1:nrow(dbh)) {
      DBHCyl <- sum(dbh$length[1:i])
      if (DBHCyl >= 1.37) {
        break
      }
    }

    DBHCyl <- as.numeric(i)

    QSM.dbh.cm <- dbh$radius[DBHCyl] * 200

    QSM.ht.m <- max(df$startZ) - min(df$startZ)

    summary <- df %>%
      mutate(
        Volume = pi * .data$radius^2 * .data$length,
        SurfaceArea = 2 * pi * .data$radius * .data$length
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
    message("Invalid Dataframe Supplied!!!\nOnly TreeQSM or SimpleForest QSMs are supported.")
  }
  return(list(summary, summary2))
}
