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
  message("Calculating Branch Paths")

  # TreeQSM --------------------------------------------------------------------
  if (all(c("parent", "extension", "branch", "BranchOrder") %in% colnames(df))) {
    # Finds end of buttress at first branch for better main stem modeling
    stem <- filter(df, .data$branch == 1)
    stem <- min(which(stem$totChildren > 1))

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
      .options.snow = opts, # .verbose = TRUE,
      .inorder = FALSE,
      .packages = c("dplyr", "cobs")
    ) %dopar% {
      # Identify Good Cylinder Fits --------------------------------------------

      # Extracts cylinders for each unique path
      cyl_id <- sort(as.numeric(names(paths[[i]])))

      # Creates indexes to identify poorly fit cylinders
      path_cyl <- filter(df, .data$extension %in% cyl_id) %>%
        mutate(
          index0 = .data$radius / .data$GrowthLength / (.data$BranchOrder + 1),
          index1 = log(.data$GrowthLength) / .data$radius^2,
          index2 = .data$radius^2 / log(.data$GrowthLength)
        )

      # Identifies poorly modeled cylinders
      path_temp <- path_cyl %>%
        mutate( # general cylinder pass
          IQR = IQR(.data$index0),
          upper = quantile(.data$index0, probs = c(.75), na.rm = FALSE) + 1.5 * .data$IQR,
          lower = quantile(.data$index0, probs = c(.25), na.rm = FALSE) - 1.5 * .data$IQR,
          bad_fit0 = case_when(.data$lower <= .data$index0 & .data$index0 >= .data$upper ~ 1, TRUE ~ 0)
        ) %>%
        filter(.data$bad_fit0 == 0) %>%
        group_by(.data$BranchOrder) %>%
        mutate( # removes small cylinders
          IQR = IQR(.data$index1),
          upper = quantile(.data$index1, probs = c(.75), na.rm = FALSE) + 1.5 * .data$IQR,
          lower = quantile(.data$index1, probs = c(.25), na.rm = FALSE) - 1.5 * .data$IQR,
          bad_fit1 = case_when(.data$lower <= .data$index1 & .data$index1 >= .data$upper ~ 1, TRUE ~ 0)
        ) %>%
        filter(.data$bad_fit1 == 0) %>%
        mutate( # removes large cylinders
          IQR = IQR(.data$index2),
          upper = quantile(.data$index2, probs = c(.75), na.rm = FALSE) + 1.5 * .data$IQR,
          lower = quantile(.data$index2, probs = c(.25), na.rm = FALSE) - 1.5 * .data$IQR,
          bad_fit2 = case_when(.data$lower <= .data$index2 & .data$index2 <= .data$upper ~ 0, TRUE ~ 1),
          bad_fit3 = NA
        ) %>%
        filter(.data$bad_fit2 == 0) %>%
        ungroup()

      # Identifies bad cylinder fits by tapering
      for (k in 1:nrow(path_temp)) {
        z <- length(which((as.vector(path_temp$radius[k] - path_temp$radius[1:k]) / path_temp$radius[k]) > 1 / sqrt(k)))

        if (z > 0) {
          path_temp$bad_fit3[k] <- 1
        } else {
          path_temp$bad_fit3[k] <- 0
        }
      }

      # Joins bad fits and accounts for buttress flare
      path_cyl <- path_cyl %>%
        left_join(select(path_temp, .data$extension, bad_fit = .data$bad_fit3), by = "extension") %>%
        mutate(
          bad_fit = tidyr::replace_na(.data$bad_fit, 1),
          bad_fit = case_when(.data$branch == 1 & .data$PositionInBranch <= !!stem ~ 0, TRUE ~ .data$bad_fit)
        )

      # Uses good cylinder fits to model paths
      # We rename growth length and radius to x a y for shorter labels
      path_temp <- path_cyl %>%
        filter(.data$bad_fit == 0) %>%
        select(x = .data$GrowthLength, y = .data$radius)

      # Dead Branch Filter -----------------------------------------------------

      # Identifies dead or broken branches and removes any real twig tapering
      # Dead branches have <= 1 child branch in the 1st order branches
      max_order <- slice_tail(path_cyl, n = 1) %>%
        select(.data$BranchOrder) %>%
        pull()

      if (max_order %in% c(1)) {
        x <- path_temp$x
        y <- path_temp$y
        min_rad <- y[length(y)]
        max_rad_ord <- path_cyl %>%
          filter(radius == !!min_rad) %>%
          select(.data$BranchOrder) %>%
          distinct() %>%
          pull()

        # Dead branch radii are 25% less in each new order if there are no good cylinders
        min_rad <- (y[length(y)] - (0.25 * y[length(y)])) / max_order
        max_children <- df %>%
          filter(.data$branch == c(
            pull(slice_tail(path_cyl, n = 1), .data$branch),
            pull(path_cyl %>%
              filter(!.data$branch == 1) %>%
              slice_head(n = 1), .data$branch)
          )) %>%
          filter(.data$totChildren >= 2) %>%
          nrow()

        # Bypasses dead branch filter if branch is alive in 1st order
        if (max_children > 3) {
          x <- path_temp$x
          x[length(x) + 1] <- x
          y <- path_temp$y
          y[length(y) + 1] <- twigRad

          min_rad <- y[length(y)]
          max_rad_ord <- 0
        }
      } else { # Bypasses dead branch filter for alive branches
        x <- path_temp$x
        x[length(x) + 1] <- min(path_cyl$GrowthLength)
        y <- path_temp$y
        y[length(y) + 1] <- twigRad

        min_rad <- y[length(y)]
        max_rad_ord <- 0
      }

      # Fit Monotonic GAM ------------------------------------------------------

      # Forces model intercept through the minimum twig diameter (twigRad)
      matrix <- matrix(ncol = 3, nrow = 1, byrow = TRUE)
      matrix[1, ] <- c(0, min(path_cyl$GrowthLength), twigRad)

      # Models new cylinder radii
      if (length(x) >= 3) {
        # Fits monotonic GAM using the cobs package
        model <- cobs::cobs(x, y,
          lambda = 0.01,
          degree = 1,
          constraint = "increase",
          pointwise = matrix,
          print.mesg = FALSE,
          repeat.delete.add = FALSE,
          nknots = length(x) - 1
        )
        path_cyl$radius <- predict(model, path_cyl$GrowthLength)[, 2]
      } else { # Ignores paths that are too short to be modeled
        path_cyl$radius <- path_cyl$radius
      }

      #  Removes tapering on main stem, broken, and dead branches
      path_cyl <- path_cyl %>%
        mutate(radius = case_when(
          .data$bad_fit == 0 & .data$BranchOrder == 0 ~ .data$UnmodRadius,
          TRUE ~ .data$radius
        )) %>%
        mutate(radius = case_when(
          .data$radius < !!min_rad & !(!!max_rad_ord == 0) ~ !!min_rad,
          TRUE ~ .data$radius
        ))


      # Diagnostic Graph -------------------------------------------------------

      # path_cyl %>%
      #   mutate(fit = predict(model, path_cyl$GrowthLength)[, 2]) %>%
      #   ggplot() +
      #   geom_line(aes(y = fit, x = GrowthLength), linewidth = 0.5, color = "black") +
      #   geom_point(aes(x = GrowthLength, y = UnmodRadius, color = as.factor(BranchOrder), shape = as.factor(bad_fit))) +
      #   labs(
      #     x = "Growth Length (m)",
      #     y = "Radius (m)",
      #     title = "Real Twig Path Correction",
      #     color = "Branch Order",
      #     shape = "Cylinder Fit", subtitle = "TreeQSM v2.4.1"
      #   ) +
      #   theme_classic()

      # Path Index
      path_cyl$pathIndex <- i

      return(path_cyl)
    }

    # Ends parallel cluster
    stopCluster(cl)

    # Update Radii -------------------------------------------------------------

    message("Updating Cylinder Radii")

    # Combines all paths into one data frame
    all_cyl <- data.table::rbindlist(results)

    # Finds main stem path index for separate modeling
    main_stem_path <- all_cyl %>%
      group_by(.data$branch, .data$pathIndex) %>%
      summarize(max_cyl = max(.data$PositionInBranch), .groups = "drop") %>%
      arrange(desc(.data$max_cyl)) %>%
      arrange(.data$branch) %>%
      group_by(.data$branch) %>%
      slice_head(n = 1) %>%
      filter(.data$branch == 1) %>%
      pull(.data$pathIndex)

    # Gets main stem cylinder ids
    main_stem_cyl <- all_cyl %>%
      filter(.data$pathIndex == !!main_stem_path) %>%
      select(.data$extension, .data$radius)

    # Calculates weighted mean for each cylinder
    cyl_radii <- all_cyl %>%
      group_by(.data$extension) %>%
      summarize(radius = stats::weighted.mean(w = .data$radius, .data$radius, na.rm = TRUE))

    # Updates the main stem with its own path
    cyl_radii <- rows_update(cyl_radii, main_stem_cyl, by = "extension")

    # Updates the QSM with new radii and interpolates any missing radii
    df <- df %>%
      select(-.data$radius) %>%
      left_join(cyl_radii, by = c("extension")) %>%
      group_by(.data$branch) %>%
      mutate(radius = case_when(n() > 1 ~ zoo::na.approx(.data$radius, rule = 2), TRUE ~ .data$radius)) %>%
      ungroup() %>%
      distinct(.data$extension, .keep_all = TRUE)
  } else if (all(c("ID", "parentID", "branchID", "branchOrder") %in% colnames(df))) {
    # Simple Forest  -------------------------------------------------------------
    # Finds end of buttress at first branch for better main stem modeling
    stem <- filter(df, .data$branchID == 0)
    stem <- min(which(stem$totChildren > 1))

    # Creates path network
    g <- data.frame(parent = df$parentID, extension = df$ID)
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
      .options.snow = opts, # .verbose = TRUE,
      .inorder = FALSE,
      .packages = c("dplyr", "cobs")
    ) %dopar% {
      # Identify Good Cylinder Fits --------------------------------------------

      # Extracts cylinders for each unique path
      cyl_id <- sort(as.numeric(names(paths[[i]])))

      # Creates indexes to identify poorly fit cylinders
      path_cyl <- filter(df, .data$ID %in% cyl_id) %>%
        mutate(
          index0 = .data$radius / .data$growthLength / (.data$branchOrder + 1),
          index1 = log(.data$growthLength) / .data$radius^2,
          index2 = .data$radius^2 / log(.data$growthLength)
        )

      # Identifies poorly modeled cylinders
      path_temp <- path_cyl %>%
        mutate( # general cylinder pass
          IQR = IQR(.data$index0),
          upper = quantile(.data$index0, probs = c(.75), na.rm = FALSE) + 1.5 * .data$IQR,
          lower = quantile(.data$index0, probs = c(.25), na.rm = FALSE) - 1.5 * .data$IQR,
          bad_fit0 = case_when(.data$lower <= .data$index0 & .data$index0 >= .data$upper ~ 1, TRUE ~ 0)
        ) %>%
        filter(.data$bad_fit0 == 0) %>%
        group_by(.data$branchOrder) %>%
        mutate( # removes small cylinders
          IQR = IQR(.data$index1),
          upper = quantile(.data$index1, probs = c(.75), na.rm = FALSE) + 1.5 * .data$IQR,
          lower = quantile(.data$index1, probs = c(.25), na.rm = FALSE) - 1.5 * .data$IQR,
          bad_fit1 = case_when(.data$lower <= .data$index1 & .data$index1 >= .data$upper ~ 1, TRUE ~ 0)
        ) %>%
        filter(.data$bad_fit1 == 0) %>%
        mutate( # removes large cylinders
          IQR = IQR(.data$index2),
          upper = quantile(.data$index2, probs = c(.75), na.rm = FALSE) + 1.5 * .data$IQR,
          lower = quantile(.data$index2, probs = c(.25), na.rm = FALSE) - 1.5 * .data$IQR,
          bad_fit2 = case_when(.data$lower <= .data$index2 & .data$index2 <= .data$upper ~ 0, TRUE ~ 1),
          bad_fit3 = NA
        ) %>%
        filter(.data$bad_fit2 == 0) %>%
        ungroup()

      # Identifies bad cylinder fits by tapering
      for (k in 1:nrow(path_temp)) {
        z <- length(which((as.vector(path_temp$radius[k] - path_temp$radius[1:k]) / path_temp$radius[k]) > 1 / sqrt(k)))

        if (z > 0) {
          path_temp$bad_fit3[k] <- 1
        } else {
          path_temp$bad_fit3[k] <- 0
        }
      }

      # Joins bad fits and accounts for buttress flare
      path_cyl <- path_cyl %>%
        left_join(select(path_temp, .data$ID, bad_fit = .data$bad_fit3), by = "ID") %>%
        mutate(
          bad_fit = tidyr::replace_na(.data$bad_fit, 1),
          bad_fit = case_when(.data$branchID == 1 & .data$positionInBranch <= !!stem ~ 0, TRUE ~ .data$bad_fit)
        )

      # Uses good cylinder fits to model paths
      # We rename growth length and radius to x a y for shorter labels
      path_temp <- path_cyl %>%
        filter(.data$bad_fit == 0) %>%
        select(x = .data$growthLength, y = .data$radius)

      # Dead Branch Filter -----------------------------------------------------

      # Identifies dead or broken branches and removes any real twig tapering
      # Dead branches have <= 1 child branch in the 1st order branches
      max_order <- slice_tail(path_cyl, n = 1) %>%
        select(.data$branchOrder) %>%
        pull()

      if (max_order %in% c(1)) {
        x <- path_temp$x
        y <- path_temp$y
        min_rad <- y[length(y)]
        max_rad_ord <- path_cyl %>%
          filter(radius == !!min_rad) %>%
          select(.data$branchOrder) %>%
          distinct() %>%
          pull()

        # Dead branch radii are 25% less in each new order if there are no good cylinders
        min_rad <- (y[length(y)] - (0.25 * y[length(y)])) / max_order
        max_children <- df %>%
          filter(.data$branchID == c(
            pull(slice_tail(path_cyl, n = 1), .data$branchID),
            pull(path_cyl %>%
              filter(!.data$branchID == 1) %>%
              slice_head(n = 1), .data$branchID)
          )) %>%
          filter(.data$totChildren >= 2) %>%
          nrow()

        # Bypasses dead branch filter if branch is alive in 1st order
        if (max_children > 3) {
          x <- path_temp$x
          x[length(x) + 1] <- x
          y <- path_temp$y
          y[length(y) + 1] <- twigRad

          min_rad <- y[length(y)]
          max_rad_ord <- 0
        }
      } else { # Bypasses dead branch filter for alive branches
        x <- path_temp$x
        x[length(x) + 1] <- min(path_cyl$growthLength)
        y <- path_temp$y
        y[length(y) + 1] <- twigRad

        min_rad <- y[length(y)]
        max_rad_ord <- 0
      }

      # Fit Monotonic GAM ------------------------------------------------------

      # Forces model intercept through the minimum twig diameter (twigRad)
      matrix <- matrix(ncol = 3, nrow = 1, byrow = TRUE)
      matrix[1, ] <- c(0, min(path_cyl$growthLength), twigRad)

      # Models new cylinder radii
      if (length(x) >= 3) {
        # Fits monotonic GAM using the cobs package
        model <- cobs::cobs(x, y,
          lambda = 0.01,
          degree = 1,
          constraint = "increase",
          pointwise = matrix,
          print.mesg = FALSE,
          repeat.delete.add = FALSE,
          nknots = length(x) - 1
        )
        path_cyl$radius <- predict(model, path_cyl$growthLength)[, 2]
      } else { # Ignores paths that are too short to be modeled
        path_cyl$radius <- path_cyl$radius
      }

      #  Removes tapering on main stem, broken, and dead branches
      path_cyl <- path_cyl %>%
        mutate(radius = case_when(
          .data$bad_fit == 0 & .data$branchOrder == 0 ~ .data$UnmodRadius,
          TRUE ~ .data$radius
        )) %>%
        mutate(radius = case_when(
          .data$radius < !!min_rad & !(!!max_rad_ord == 0) ~ !!min_rad,
          TRUE ~ .data$radius
        ))


      # Diagnostic Graph -------------------------------------------------------

      # path_cyl %>%
      #   mutate(fit = predict(model, path_cyl$growthLength)[, 2]) %>%
      #   ggplot() +
      #   geom_line(aes(y = fit, x = GrowthLength), linewidth = 0.5, color = "black") +
      #   geom_point(aes(x = GrowthLength, y = UnmodRadius, color = as.factor(branchOrder), shape = as.factor(bad_fit))) +
      #   labs(
      #     x = "Growth Length (m)",
      #     y = "Radius (m)",
      #     title = "Real Twig Path Correction",
      #     color = "Branch Order",
      #     shape = "Cylinder Fit", subtitle = "SimpleForest"
      #   ) +
      #   theme_classic()

      # Path Index
      path_cyl$pathIndex <- i

      return(path_cyl)
    }

    # Ends parallel cluster
    stopCluster(cl)

    # Update Radii -------------------------------------------------------------

    message("Updating Cylinder Radii")

    # Combines all paths into one data frame
    all_cyl <- data.table::rbindlist(results)

    # Finds main stem path index for separate modeling
    main_stem_path <- all_cyl %>%
      group_by(.data$branchID, .data$pathIndex) %>%
      summarize(max_cyl = max(.data$positionInBranch), .groups = "drop") %>%
      arrange(desc(.data$max_cyl)) %>%
      arrange(.data$branchID) %>%
      group_by(.data$branchID) %>%
      slice_head(n = 1) %>%
      filter(.data$branchID == 1) %>%
      pull(.data$pathIndex)

    # Gets main stem cylinder ids
    main_stem_cyl <- all_cyl %>%
      filter(.data$pathIndex == !!main_stem_path) %>%
      select(.data$ID, .data$radius)

    # Calculates weighted mean for each cylinder
    cyl_radii <- all_cyl %>%
      group_by(.data$ID) %>%
      summarize(radius = stats::weighted.mean(w = .data$radius, .data$radius, na.rm = TRUE))

    # Updates the main stem with its own path
    cyl_radii <- rows_update(cyl_radii, main_stem_cyl, by = "ID")

    # Updates the QSM with new radii and interpolates any missing radii
    df <- df %>%
      select(-.data$radius) %>%
      left_join(cyl_radii, by = c("ID")) %>%
      group_by(.data$branchID) %>%
      mutate(radius = case_when(n() > 1 ~ zoo::na.approx(.data$radius, rule = 2), TRUE ~ .data$radius)) %>%
      ungroup() %>%
      distinct(.data$ID, .keep_all = TRUE)
  } else {
    message("Invalid Dataframe Supplied!!!\nOnly TreeQSM or SimpleForest QSMs are supported.")
  }
  return(df)
}
