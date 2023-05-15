#' Correct Radii
#'
#' @description Corrects cylinder radii.
#'
#' @param df QSM cylinder data frame
#' @param twigRad Twig radius in meters.
#'
#' @return Returns a data.frame
#' @export
#'
#' @import dplyr
#' @import cobs
#' @import parallel
#' @import doParallel
#' @import doSNOW
#' @import foreach
#' @importFrom zoo na.approx
#' @importFrom data.table rbindlist
#' @importFrom stats IQR predict quantile
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @rawNamespace import(igraph, except=c(union, as_data_frame, groups, crossing))
#'
#' @examples
#' \dontrun{
#' ## TreeQSM Processing Chain
#' file <- system.file("extdata/QSM.mat", package = "rTwig")
#' df <- import_qsm(file)
#' df <- update_cylinders(df)
#' df <- growth_length(df)
#' df <- correct_radii(df, twigRad = 0.003)
#' str(df)
#'
#' ## SimpleForest Processing Chain
#' file <- system.file("extdata/QSM.csv", package = "rTwig")
#' df <- read.csv(file)
#' df <- update_cylinders(df)
#' df <- growth_length(df)
#' df <- correct_radii(df, twigRad = 0.003)
#' str(df)
#' }
correct_radii <- function(df, twigRad) {
  message("Correcting Cylinder Radii")

  if (all(c("parent", "extension", "branch", "BranchOrder") %in% colnames(df))) {
    # Creates path network
    g <- data.frame(parent = df$parent, extension = df$extension)
    g <- igraph::graph_from_data_frame(g)

    starts <- igraph::V(g)[igraph::degree(g, mode = "in") == 0]
    finals <- igraph::V(g)[igraph::degree(g, mode = "out") == 0]

    paths <- igraph::all_simple_paths(g, from = starts[[1]], to = finals)

    # Initialize parallel workers
    chk <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")

    if (nzchar(chk) && chk == "TRUE") {
      # Use 2 cores to pass checks
      n_cores <- 2L
    } else {
      # Use all cores for end-users
      n_cores <- parallel::detectCores(logical = FALSE)
    }

    cl <- makeCluster(n_cores)
    registerDoSNOW(cl)

    # Initializes the progress bar for the loop
    progress <- function(n) setTxtProgressBar(txtProgressBar(max = max(length(paths)), style = 3), n)
    opts <- list(progress = progress)

    # Fix for foreach package failing devtools::check()
    i <- NULL

    # Loops through the paths
    results <- foreach(
      i = 1:length(paths),
      .options.snow = opts,
      .inorder = FALSE,
      .packages = c("dplyr", "cobs")
    ) %dopar% {
      cyl_id <- sort(as.numeric(names(paths[[i]])))
      path_cyl <- filter(df, .data$extension %in% cyl_id)

      path_cyl <- path_cyl %>%
        mutate(
          row_num = n() - row_number() + 1,
          GrowthLength2 = sqrt(.data$GrowthLength) * .data$row_num,
          index = .data$radius / .data$GrowthLength
        ) %>%
        mutate(
          IQR = IQR(.data$index),
          upper = quantile(.data$index, probs = c(.75), na.rm = FALSE) + 1.5 * .data$IQR,
          lower = quantile(.data$index, probs = c(.25), na.rm = FALSE) - 1.5 * .data$IQR,
          bad_fit1 = case_when(.data$lower <= .data$index & .data$index <= .data$upper ~ 0, TRUE ~ 1),
          bad_fit2 = NA
        )

      # Identifies bad cylinder fits by tapering
      for (k in 1:nrow(path_cyl)) {
        z <- length(which((as.vector(path_cyl$radius[k] - path_cyl$radius[1:k]) / path_cyl$radius[k]) > 1 / k))

        if (z > 0) {
          path_cyl$bad_fit2[k] <- 1
        } else {
          path_cyl$bad_fit2[k] <- 0
        }
      }

      path_cyl <- path_cyl %>%
        mutate(bad_fit = case_when(.data$bad_fit1 == 1 & .data$bad_fit2 == 0 ~ 1, TRUE ~ .data$bad_fit2))

      # Extracts good cylinder fits to model paths
      path_temp <- path_cyl %>%
        mutate(weight = 100000) %>%
        filter(.data$bad_fit == 0) %>%
        select(x = .data$GrowthLength2, y = .data$radius, .data$weight)

      x <- path_temp$x
      x[length(x) + 1] <- 0
      y <- path_temp$y
      y[length(y) + 1] <- twigRad
      weights <- path_temp$weight
      weights[length(weights) + 1] <- 1

      # Forces model intercept through twigRad
      matrix <- matrix(ncol = 3, nrow = 1, byrow = TRUE)
      matrix[1, ] <- c(0, 0, twigRad)

      # Updates cylinder radii
      if (length(x) >= 3) {
        model <- cobs(x, y, lambda = 1, degree = 2, constraint = "increase", pointwise = matrix, ic = "SIC", print.mesg = FALSE, repeat.delete.add = FALSE, w = weights)
        path_cyl$radius <- predict(model, path_cyl$GrowthLength2)[, 2]
      } else {
        path_cyl$radius <- twigRad
      }

      return(path_cyl)
    }

    # Ends parallel cluster
    stopCluster(cl)

    # Calculates single cylinder radii from all paths
    cyl_radii <- data.table::rbindlist(results) %>%
      group_by(.data$extension) %>%
      mutate(
        IQR = IQR(.data$radius),
        upper = quantile(.data$radius, probs = c(.75), na.rm = FALSE) + 1.5 * .data$IQR,
        lower = quantile(.data$radius, probs = c(.25), na.rm = FALSE) - 1.5 * .data$IQR,
        bad_fit = case_when(.data$lower <= .data$radius & .data$radius <= .data$upper ~ 0, TRUE ~ 1)
      ) %>%
      filter(.data$bad_fit == 0) %>%
      summarize(
        radius = max(.data$radius, na.rm = TRUE)
      ) %>%
      mutate(radius = case_when(.data$radius < twigRad ~ twigRad, TRUE ~ .data$radius))

    # Updates the QSM with new radii
    df <- df %>%
      select(-.data$radius) %>%
      left_join(cyl_radii, by = "extension") %>%
      group_by(.data$branch) %>%
      mutate(radius = zoo::na.approx(.data$radius, rule = 2)) %>%
      ungroup()
  } else if (all(c("ID", "parentID", "branchID", "branchOrder") %in% colnames(df))) {
    # Creates path network
    g <- data.frame(parent = df$parentID, id = df$ID)
    g <- igraph::graph_from_data_frame(g)

    starts <- igraph::V(g)[igraph::degree(g, mode = "in") == 0]
    finals <- igraph::V(g)[igraph::degree(g, mode = "out") == 0]

    paths <- igraph::all_simple_paths(g, from = starts[[1]], to = finals)

    # Initialize parallel workers
    chk <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")

    if (nzchar(chk) && chk == "TRUE") {
      # Use 2 cores to pass checks
      n_cores <- 2L
    } else {
      # Use all cores for end-user
      n_cores <- parallel::detectCores(logical = FALSE)
    }

    cl <- makeCluster(n_cores)
    registerDoSNOW(cl)

    # Initializes the progress bar for the loop
    progress <- function(n) setTxtProgressBar(txtProgressBar(max = max(length(paths)), style = 3), n)
    opts <- list(progress = progress)

    # Fix for foreach package failing devtools::check()
    i <- NULL

    # Loops through the paths
    results <- foreach(
      i = 1:length(paths),
      .options.snow = opts,
      .inorder = FALSE,
      .packages = c("dplyr", "cobs")
    ) %dopar% {
      cyl_id <- sort(as.numeric(names(paths[[i]])))
      path_cyl <- filter(df, .data$ID %in% cyl_id)

      path_cyl <- path_cyl %>%
        mutate(
          row_num = n() - row_number() + 1,
          GrowthLength2 = sqrt(.data$growthLength) * .data$row_num,
          index = .data$radius / .data$growthLength
        ) %>%
        mutate(
          IQR = IQR(.data$index),
          upper = quantile(.data$index, probs = c(.75), na.rm = FALSE) + 1.5 * .data$IQR,
          lower = quantile(.data$index, probs = c(.25), na.rm = FALSE) - 1.5 * .data$IQR,
          bad_fit1 = case_when(.data$lower <= .data$index & .data$index <= .data$upper ~ 0, TRUE ~ 1),
          bad_fit2 = NA
        )

      # Identifies bad cylinder fits by tapering
      for (k in 1:nrow(path_cyl)) {
        z <- length(which((as.vector(path_cyl$radius[k] - path_cyl$radius[1:k]) / path_cyl$radius[k]) > 1 / k))

        if (z > 0) {
          path_cyl$bad_fit2[k] <- 1
        } else {
          path_cyl$bad_fit2[k] <- 0
        }
      }

      path_cyl <- path_cyl %>%
        mutate(bad_fit = case_when(.data$bad_fit1 == 1 & .data$bad_fit2 == 0 ~ 1, TRUE ~ .data$bad_fit2))

      # Extracts good cylinder fits to model paths
      path_temp <- path_cyl %>%
        mutate(weight = 100000) %>%
        filter(.data$bad_fit == 0) %>%
        select(x = .data$GrowthLength2, y = .data$radius, .data$weight)

      x <- path_temp$x
      x[length(x) + 1] <- 0
      y <- path_temp$y
      y[length(y) + 1] <- twigRad
      weights <- path_temp$weight
      weights[length(weights) + 1] <- 1

      # Forces model intercept through twigRad
      matrix <- matrix(ncol = 3, nrow = 1, byrow = TRUE)
      matrix[1, ] <- c(0, 0, twigRad)

      # Updates cylinder radii
      if (length(x) >= 3) {
        model <- cobs(x, y, lambda = 1, degree = 2, constraint = "increase", pointwise = matrix, ic = "SIC", print.mesg = FALSE, repeat.delete.add = FALSE, w = weights)
        path_cyl$radius <- predict(model, path_cyl$GrowthLength2)[, 2]
      } else {
        path_cyl$radius <- twigRad
      }l

      return(path_cyl)
    }

    # Ends parallel cluster
    stopCluster(cl)

    # Calculates single cylinder radii from all paths
    cyl_radii <- data.table::rbindlist(results) %>%
      group_by(.data$ID) %>%
      mutate(
        IQR = IQR(.data$radius),
        upper = quantile(.data$radius, probs = c(.75), na.rm = FALSE) + 1.5 * .data$IQR,
        lower = quantile(.data$radius, probs = c(.25), na.rm = FALSE) - 1.5 * .data$IQR,
        bad_fit = case_when(.data$lower <= .data$radius & .data$radius <= .data$upper ~ 0, TRUE ~ 1)
      ) %>%
      filter(.data$bad_fit == 0) %>%
      summarize(
        radius = max(.data$radius, na.rm = TRUE),
        bad_fit = mean(.data$bad_fit, na.rm = TRUE)
      ) %>%
      select(-.data$bad_fit) %>%
      mutate(radius = case_when(.data$radius < twigRad ~ twigRad, TRUE ~ .data$radius))

    # Updates the QSM with new radii
    df <- df %>%
      select(-.data$radius) %>%
      left_join(cyl_radii, by = "ID") %>%
      mutate(modVolume = pi * .data$radius^2 * .data$length) %>%
      relocate(.data$modVolume, .after = .data$volume) %>%
      relocate(.data$radius, .after = .data$radius) %>%
      group_by(.data$segmentID) %>%
      mutate(radius = zoo::na.approx(.data$radius, rule = 2)) %>%
      ungroup()
  } else {
    message("Invalid Dataframe Supplied!!!\nOnly TreeQSM or SimpleForest QSMs are supported.")
  }
  return(df)
}
