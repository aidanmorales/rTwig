#' Correct Radii
#'
#' @description Corrects cylinder radii
#'
#' @param cylinder QSM cylinder data frame
#' @param twigRad Twig radius in millimeters
#' @param backend Parallel backend for multi-core processing. Defaults to "multisession" (all platforms), but can be set to "multicore" (MacOS & Linux), "cluster" (all platforms), or a "package::backend" string.
#'
#' @return Returns a data frame
#' @export
#'
#' @import cobs
#' @import future
#' @import foreach
#' @import doFuture
#' @import progressr
#' @importFrom data.table rbindlist
#' @importFrom stats IQR predict quantile
#' @rawNamespace import(tidytable, except=c(map_dfr))
#' @rawNamespace import(igraph, except=c(union, as_data_frame, groups, crossing, "%->%", "%<-%"))
#'
#' @examples
#' \donttest{
#'
#' ## TreeQSM Processing Chain
#' file <- system.file("extdata/QSM.mat", package = "rTwig")
#' qsm <- import_qsm(file)
#' cylinder <- qsm$cylinder
#' cylinder <- update_cylinders(cylinder)
#' cylinder <- correct_radii(cylinder, twigRad = 4.23)
#' str(cylinder)
#'
#' ## SimpleForest Processing Chain
#' file <- system.file("extdata/QSM.csv", package = "rTwig")
#' cylinder <- read.csv(file)
#' cylinder <- update_cylinders(cylinder)
#' cylinder <- correct_radii(cylinder, twigRad = 4.23)
#' str(cylinder)
#'
#' }
#'
correct_radii <- function(cylinder, twigRad, backend = "multisession") {

  # Converts twig radius to meters
  twigRad <- twigRad / 1000

  # TreeQSM --------------------------------------------------------------------
  if (all(c("parent", "extension", "branch", "BranchOrder") %in% colnames(cylinder))) {

    message("Generating Branch Paths")

    # Error message if cylinders have not been updated or growth length has not been calculated
    stopifnot("Cylinder indexes have not been updated! Please run update_cylinders() before proceeding." = pull(slice_head(cylinder, n = 1), .data$extension) == 1)
    stopifnot("Growth length missing! Please run growth_length() before proceeding." = "GrowthLength" %in% colnames(cylinder))

    # Finds end of buttress at first branch for better main stem modeling
    stem <- filter(cylinder, .data$branch == 1)
    stem <- min(which(stem$totalChildren > 1))

    # Creates path network
    g <- data.frame(parent = cylinder$parent, extension = cylinder$extension)
    g <- igraph::graph_from_data_frame(g)

    starts <- igraph::V(g)[igraph::degree(g, mode = "in") == 0]
    finals <- igraph::V(g)[igraph::degree(g, mode = "out") == 0]

    paths <- igraph::all_simple_paths(g, from = starts[[1]], to = finals)

    message("Starting Parallel Workers")

    # Initialize parallel workers
    chk <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")

    if (nzchar(chk) && chk == "TRUE") {
      # Use sequential to pass checks
      oplan <- future::plan("sequential")
    } else {
      # Use all cores for end-users
      if(backend == "sequential"){
        oplan <- future::plan(backend)
      } else{
        oplan <- future::plan(backend, workers = availableCores())
      }
    }

    # Set dynamic progress bar
    if ((Sys.getenv("RSTUDIO") == "1") &&
      !nzchar(Sys.getenv("RSTUDIO_TERM"))) {
      progressr::handlers("rstudio", append = TRUE)
    }

    # Ignore future random seed warning
    options(future.rng.onMisuse = "ignore")

    # Define global variables to pass devtools::check()
    i <- NULL
    extension <- NULL
    GrowthLength <- NULL
    BranchOrder <- NULL
    branch <- NULL
    index0 <- NULL
    index1 <- NULL
    index2 <- NULL
    bad_fit <- NULL
    bad_fit0 <- NULL
    bad_fit1 <- NULL
    bad_fit2 <- NULL
    bad_fit3 <- NULL
    totalChildren <- NULL

    message("Correcting Branch Paths")

    # Loops through the paths
    with_progress({
      # Progress Bar
      p <- progressor(along = 1:length(paths))

      results <- foreach(i = 1:length(paths), .inorder = FALSE, .options.future = list(packages = c("dplyr", "cobs"))) %dofuture% {
        # Identify Good Cylinder Fits --------------------------------------------

        # Extracts cylinders for each unique path
        cyl_id <- sort(as.numeric(names(paths[[i]])))

        # Creates indexes to identify poorly fit cylinders
        path_cyl <- filter(cylinder, extension %in% cyl_id) %>%
          mutate(
            index0 = radius / GrowthLength / (BranchOrder + 1),
            index1 = log(GrowthLength) / radius^2,
            index2 = radius^2 / log(GrowthLength)
          )

        # Identifies poorly modeled cylinders
        path_temp <- path_cyl %>%
          mutate( # general cylinder pass
            IQR = IQR(index0),
            upper = quantile(index0, probs = c(.75), na.rm = FALSE) + 1.5 * IQR,
            lower = quantile(index0, probs = c(.25), na.rm = FALSE) - 1.5 * IQR,
            bad_fit0 = case_when(lower <= index0 & index0 >= upper ~ 1, TRUE ~ 0)
          ) %>%
          filter(bad_fit0 == 0) %>%
          group_by(BranchOrder) %>%
          mutate( # removes small cylinders
            IQR = IQR(index1),
            upper = quantile(index1, probs = c(.75), na.rm = FALSE) + 1.5 * IQR,
            lower = quantile(index1, probs = c(.25), na.rm = FALSE) - 1.5 * IQR,
            bad_fit1 = case_when(lower <= index1 & index1 >= upper ~ 1, TRUE ~ 0)
          ) %>%
          filter(bad_fit1 == 0) %>%
          mutate( # removes large cylinders
            IQR = IQR(index2),
            upper = quantile(index2, probs = c(.75), na.rm = FALSE) + 1.5 * IQR,
            lower = quantile(index2, probs = c(.25), na.rm = FALSE) - 1.5 * IQR,
            bad_fit2 = case_when(lower <= index2 & index2 <= upper ~ 0, TRUE ~ 1),
            bad_fit3 = NA
          ) %>%
          filter(bad_fit2 == 0) %>%
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
          left_join(select(path_temp, extension, bad_fit = bad_fit3), by = "extension") %>%
          mutate(
            bad_fit = replace_na(bad_fit, 1),
            bad_fit = case_when(branch == 1 & PositionInBranch <= !!stem ~ 0, TRUE ~ bad_fit)
          )

        # Uses good cylinder fits to model paths
        # We rename growth length and radius to x a y for shorter labels
        path_temp <- path_cyl %>%
          filter(bad_fit == 0) %>%
          select(x = GrowthLength, y = radius)

        # Dead Branch Filter -----------------------------------------------------

        # Identifies dead or broken branches and removes any real twig tapering
        # Dead branches have <= 1 child branch in the 1st order branches
        max_order <- slice_tail(path_cyl, n = 1) %>%
          select(BranchOrder) %>%
          pull()

        if (max_order %in% c(1)) {
          x <- path_temp$x
          y <- path_temp$y
          min_rad <- y[length(y)]
          max_rad_ord <- path_cyl %>%
            filter(radius == !!min_rad) %>%
            select(BranchOrder) %>%
            distinct() %>%
            pull()

          # Dead branch radii are 25% less in each new order if there are no good cylinders
          min_rad <- (y[length(y)] - (0.25 * y[length(y)])) / max_order
          max_children <- cylinder %>%
            filter(branch %in% c(
              pull(slice_tail(path_cyl, n = 1), branch),
              pull(path_cyl %>%
                filter(!branch == 1) %>%
                slice_head(n = 1), branch)
            )) %>%
            filter(totalChildren >= 2) %>%
            nrow()

          # Bypasses dead branch filter if branch is alive in 1st order
          if (max_children > 3) {
            x <- path_temp$x
            x[length(x) + 1] <- min(path_cyl$GrowthLength)
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
        if (length(x) > 3) {
          # Fits monotonic GAM using the cobs package
          model <- suppressWarnings(
            cobs::cobs(x, y,
              lambda = 0.01,
              degree = 1,
              constraint = "increase",
              pointwise = matrix,
              print.warn = FALSE,
              print.mesg = FALSE,
              repeat.delete.add = FALSE,
              nknots = length(x) - 1
            )
          )
          path_cyl$radius <- predict(model, path_cyl$GrowthLength)[, 2]
        } else { # Ignores paths that are too short to be modeled
          path_cyl$radius <- twigRad
        }

        #  Removes tapering on main stem, broken, and dead branches
        path_cyl <- path_cyl %>%
          mutate(radius = case_when(
            bad_fit == 0 & BranchOrder == 0 ~ UnmodRadius,
            TRUE ~ radius
          )) %>%
          mutate(radius = case_when(
            radius < !!min_rad & !(!!max_rad_ord == 0) ~ !!min_rad,
            TRUE ~ radius
          ))

        # Diagnostic Graph -----------------------------------------------------

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

        # Update Loop Progress
        p()

        return(path_cyl)
      }
    })

    # Update Radii -------------------------------------------------------------

    message("Updating Cylinder Radii")

    # Combines all paths into one data frame
    all_cyl <- data.table::rbindlist(results)

    # # Finds main stem path index for separate modeling
    # main_stem_path <- all_cyl %>%
    #   group_by(.data$branch, .data$pathIndex) %>%
    #   summarize(max_cyl = max(.data$PositionInBranch), .groups = "drop") %>%
    #   arrange(desc(.data$max_cyl)) %>%
    #   arrange(.data$branch) %>%
    #   group_by(.data$branch) %>%
    #   slice_head(n = 1) %>%
    #   filter(.data$branch == 1) %>%
    #   pull(.data$pathIndex)
    #
    # # Gets main stem cylinder ids
    # main_stem_cyl <- all_cyl %>%
    #   filter(.data$pathIndex == !!main_stem_path) %>%
    #   select(.data$extension, .data$radius)

    # Calculates weighted mean for each cylinder
    cyl_radii <- all_cyl %>%
      group_by(.data$extension) %>%
      summarize(radius = stats::weighted.mean(w = .data$radius, .data$radius, na.rm = TRUE))

    # Updates the main stem with its own path
    #cyl_radii <- rows_update(cyl_radii, main_stem_cyl, by = "extension")

    # Updates the QSM with new radii and interpolates any missing radii
    cylinder <- cylinder %>%
      select(-.data$radius) %>%
      left_join(cyl_radii, by = c("extension"))

    message("Done!")

    # SimpleForest  --------------------------------------------------------------
  } else if (all(c("ID", "parentID", "branchID", "branchOrder") %in% colnames(cylinder))) {

    message("Generating Branch Paths")

    # Finds end of buttress at first branch for better main stem modeling
    stem <- filter(cylinder, .data$branchID == 0)
    stem <- min(which(stem$totalChildren > 1))

    # Creates path network
    g <- data.frame(parent = cylinder$parentID, extension = cylinder$ID)
    g <- igraph::graph_from_data_frame(g)

    starts <- igraph::V(g)[igraph::degree(g, mode = "in") == 0]
    finals <- igraph::V(g)[igraph::degree(g, mode = "out") == 0]

    paths <- igraph::all_simple_paths(g, from = starts[[1]], to = finals)

    message("Starting Parallel Workers")

    # Initialize parallel workers
    chk <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")

    if (nzchar(chk) && chk == "TRUE") {
      # Use sequential to pass checks
      oplan <- future::plan("sequential")
    } else {
      # Use all cores for end-users
      if(backend == "sequential"){
        oplan <- future::plan(backend)
      } else{
        oplan <- future::plan(backend, workers = availableCores())
      }
    }

    # Set dynamic progress bar
    if ((Sys.getenv("RSTUDIO") == "1") &&
      !nzchar(Sys.getenv("RSTUDIO_TERM"))) {
      progressr::handlers("rstudio", append = TRUE)
    }

    # Ignore future random seed warning
    options(future.rng.onMisuse = "ignore")

    # Define global variables to pass devtools::check()
    i <- NULL
    ID <- NULL
    growthLength <- NULL
    branchOrder <- NULL
    branchID <- NULL
    index0 <- NULL
    index1 <- NULL
    index2 <- NULL
    bad_fit <- NULL
    bad_fit0 <- NULL
    bad_fit1 <- NULL
    bad_fit2 <- NULL
    bad_fit3 <- NULL
    totalChildren <- NULL

    message("Correcting Branch Paths")

    # Loops through the paths
    with_progress({
      # Progress Bar
      p <- progressor(along = 1:length(paths))

      results <- foreach(i = 1:length(paths), .inorder = FALSE) %dofuture% {
        # Identify Good Cylinder Fits ------------------------------------------

        # Extracts cylinders for each unique path
        cyl_id <- sort(as.numeric(names(paths[[i]])))

        # Creates indexes to identify poorly fit cylinder
        path_cyl <- filter(cylinder, ID %in% cyl_id) %>%
          mutate(
            index0 = radius / growthLength / (branchOrder + 1),
            index1 = log(growthLength) / radius^2,
            index2 = radius^2 / log(growthLength)
          )

        # Identifies poorly modeled cylinders
        path_temp <- path_cyl %>%
          mutate( # general cylinder pass
            IQR = IQR(index0),
            upper = quantile(index0, probs = c(.75), na.rm = FALSE) + 1.5 * IQR,
            lower = quantile(index0, probs = c(.25), na.rm = FALSE) - 1.5 * IQR,
            bad_fit0 = case_when(lower <= index0 & index0 >= upper ~ 1, TRUE ~ 0)
          ) %>%
          filter(bad_fit0 == 0) %>%
          group_by(branchOrder) %>%
          mutate( # removes small cylinders
            IQR = IQR(index1),
            upper = quantile(index1, probs = c(.75), na.rm = FALSE) + 1.5 * IQR,
            lower = quantile(index1, probs = c(.25), na.rm = FALSE) - 1.5 * IQR,
            bad_fit1 = case_when(lower <= index1 & index1 >= upper ~ 1, TRUE ~ 0)
          ) %>%
          filter(bad_fit1 == 0) %>%
          mutate( # removes large cylinders
            IQR = IQR(index2),
            upper = quantile(index2, probs = c(.75), na.rm = FALSE) + 1.5 * IQR,
            lower = quantile(index2, probs = c(.25), na.rm = FALSE) - 1.5 * IQR,
            bad_fit2 = case_when(lower <= index2 & index2 <= upper ~ 0, TRUE ~ 1),
            bad_fit3 = NA
          ) %>%
          filter(bad_fit2 == 0) %>%
          ungroup()

        # Identifies bad cylinder fits by tapering
        # This taper is more aggressive than the TreeQSM taper as SimpleForest
        # uses a sphere following system which tends to overestimate cylinder size
        for (k in 1:nrow(path_temp)) {
          z <- length(which((as.vector(path_temp$radius[k] - path_temp$radius[1:k]) / path_temp$radius[k]) > 0))

          if (z > 0) {
            path_temp$bad_fit3[k] <- 1
          } else {
            path_temp$bad_fit3[k] <- 0
          }
        }

        # Joins bad fits and accounts for buttress flare
        path_cyl <- path_cyl %>%
          left_join(select(path_temp, ID, bad_fit = bad_fit3), by = "ID") %>%
          mutate(bad_fit = replace_na(bad_fit, 1))

        # Uses good cylinder fits to model paths
        # We rename growth length and radius to x a y for shorter labels
        path_temp <- path_cyl %>%
          filter(bad_fit == 0) %>%
          select(x = growthLength, y = radius)

        # Dead Branch Filter ---------------------------------------------------

        # Identifies dead or broken branches and removes any real twig tapering
        # Dead branches have <= 1 child branch in the 1st order branches
        max_order <- slice_tail(path_cyl, n = 1) %>%
          select(branchOrder) %>%
          pull()

        if (max_order %in% c(1)) {
          x <- path_temp$x
          y <- path_temp$y
          min_rad <- y[length(y)]
          max_rad_ord <- path_cyl %>%
            filter(radius == !!min_rad) %>%
            select(branchOrder) %>%
            distinct() %>%
            pull()

          # Dead branch radii are 25% less in each new order if there are no good cylinders
          min_rad <- (y[length(y)] - (0.25 * y[length(y)])) / max_order
          max_children <- cylinder %>%
            filter(branchID %in% c(
              pull(slice_tail(path_cyl, n = 1), branchID),
              pull(path_cyl %>%
                filter(!branchID == 0) %>%
                slice_head(n = 1), branchID)
            )) %>%
            filter(totalChildren >= 2) %>%
            nrow()

          # Bypasses dead branch filter if branch is alive in 1st order
          if (max_children > 3) {
            x <- path_temp$x
            x[length(x) + 1] <- min(path_cyl$growthLength)
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

        # Fit Monotonic GAM ----------------------------------------------------

        # Forces model intercept through the minimum twig diameter (twigRad)
        matrix <- matrix(ncol = 3, nrow = 1, byrow = TRUE)
        matrix[1, ] <- c(0, min(path_cyl$growthLength), twigRad)

        # Models new cylinder radii
        if (length(x) > 3) {
          # Fits monotonic GAM using the cobs package
          model <- suppressWarnings(
            cobs::cobs(x, y,
              lambda = 0.01,
              degree = 1,
              constraint = "increase",
              pointwise = matrix,
              print.mesg = FALSE,
              repeat.delete.add = FALSE,
              nknots = length(x) - 1
            )
          )
          path_cyl$radius <- predict(model, path_cyl$growthLength)[, 2]
        } else { # Ignores paths that are too short to be modeled
          path_cyl$radius <- twigRad
        }

        #  Removes tapering on main stem, broken, and dead branches
        path_cyl <- path_cyl %>%
          mutate(radius = case_when(
            bad_fit == 0 & branchOrder == 0 ~ UnmodRadius,
            TRUE ~ radius
          )) %>%
          mutate(radius = case_when(
            radius < !!min_rad & !(!!max_rad_ord == 0) ~ !!min_rad,
            TRUE ~ radius
          ))

        # Diagnostic Graph -----------------------------------------------------

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

        # Update Loop Progress
        p()

        return(path_cyl)
      }
    })

    # Update Radii -------------------------------------------------------------

    message("Updating Cylinder Radii")

    # Combines all paths into one data frame
    all_cyl <- data.table::rbindlist(results)

    # # Finds main stem path index for separate modeling
    # main_stem_path <- all_cyl %>%
    #   group_by(.data$branchID, .data$pathIndex) %>%
    #   summarize(max_cyl = max(.data$positionInBranch), .groups = "drop") %>%
    #   arrange(desc(.data$max_cyl)) %>%
    #   arrange(.data$branchID) %>%
    #   group_by(.data$branchID) %>%
    #   slice_head(n = 1) %>%
    #   filter(.data$branchID == 0) %>%
    #   pull(.data$pathIndex)
    #
    # # Gets main stem cylinder ids
    # main_stem_cyl <- all_cyl %>%
    #   filter(.data$pathIndex == !!main_stem_path) %>%
    #   select(.data$ID, .data$radius, .data$UnmodRadius)
    #
    # # Updates poorly modeled main stem cylinders
    # main_stem_cyl <- main_stem_cyl %>%
    #   mutate(
    #     IQR = IQR(.data$radius),
    #     upper = quantile(.data$radius, probs = c(.75), na.rm = FALSE) + 1.5 * .data$IQR,
    #     radius = case_when(
    #       .data$radius >= .data$upper ~ .data$UnmodRadius,
    #       TRUE ~ .data$radius
    #     )
    #   ) %>%
    #   select(.data$ID, .data$radius)

    # Calculates weighted mean for each cylinder
    cyl_radii <- all_cyl %>%
      group_by(.data$ID) %>%
      summarize(radius = stats::weighted.mean(w = .data$radius, .data$radius, na.rm = TRUE))

    # Updates the main stem with its own path
    #cyl_radii <- rows_update(cyl_radii, main_stem_cyl, by = "ID")

    # Updates the QSM with new radii and interpolates any missing radii
    cylinder <- cylinder %>%
      select(-.data$radius) %>%
      left_join(cyl_radii, by = c("ID"))

    message("Done!")

  } else {
    message(
      "Invalid Dataframe Supplied!!!
      \nOnly TreeQSM or SimpleForest QSMs are supported.
      \nMake sure the cylinder data frame and not the QSM list is supplied."
    )
  }
  return(cylinder)

  # Future Package Cleanup
  chk <- Sys.getenv("_R_CHECK_CONNECTIONS_LEFT_OPEN_", "")

  if (nzchar(chk) && chk == "TRUE") {
    future::plan("sequential")
  } else{
    on.exit(plan(oplan), add = TRUE)
  }
}
