#' Correct Radii
#'
#' @description Corrects cylinder radii
#'
#' @param cylinder QSM cylinder data frame
#' @param twig_radius Twig radius in millimeters
#' @param backend Parallel backend for multi-core processing. Defaults to "multisession" (all platforms), but can be set to "multicore" (MacOS & Linux), "cluster" (all platforms), or a "package::backend" string.
#'
#' @return Returns a data frame
#' @export
#'
#' @examples
#' \donttest{
#'
#' ## TreeQSM Processing Chain
#' file <- system.file("extdata/QSM.mat", package = "rTwig")
#' qsm <- import_qsm(file)
#' cylinder <- qsm$cylinder
#' cylinder <- update_cylinders(cylinder)
#' cylinder <- correct_radii(cylinder, twig_radius = 4.23)
#' str(cylinder)
#'
#' ## SimpleForest Processing Chain
#' file <- system.file("extdata/QSM.csv", package = "rTwig")
#' cylinder <- read.csv(file)
#' cylinder <- update_cylinders(cylinder)
#' cylinder <- correct_radii(cylinder, twig_radius = 4.23)
#' str(cylinder)
#' }
#'
correct_radii <- function(cylinder, twig_radius, backend = "multisession") {
  # Converts twig radius to meters
  twig_radius <- twig_radius / 1000

  # Gets parallel backend
  backend <- backend

  # rTwig ----------------------------------------------------------------------
  if (all(c("id", "parent", "start_x", "branch_order") %in% colnames(cylinder))) {
    message("Generating Branch Paths")

    # Finds end of buttress at first branch for better main stem modeling
    stem <- filter(cylinder, .data$branch == 1)
    stem <- min(which(stem$total_children > 1))

    # Creates path network
    g <- data.frame(parent = cylinder$parent, id = cylinder$id)
    g <- igraph::graph_from_data_frame(g)

    starts <- igraph::V(g)[igraph::degree(g, mode = "in") == 0]
    finals <- igraph::V(g)[igraph::degree(g, mode = "out") == 0]

    paths <- igraph::all_simple_paths(g, from = starts[[1]], to = finals)

    # Start parallel workers
    oplan <- parallel_workers(backend, start_workers = TRUE)

    # Define global variables to pass devtools::check()
    i <- NULL

    message("Correcting Branch Paths")

    # Loops through the paths
    progressr::with_progress({
      # Progress Bar
      p <- progressr::progressor(along = 1:length(paths))

      results <- foreach::foreach(i = 1:length(paths), .inorder = FALSE) %dofuture% {
        # Identify Good Cylinder Fits ------------------------------------------

        # Extracts cylinders for each unique path
        cyl_id <- sort(as.numeric(names(paths[[i]])))

        # Creates indexes to identify poorly fit cylinders
        path_cyl <- filter(cylinder, .data$id %in% !!cyl_id) %>%
          mutate(
            index0 = .data$radius / .data$growth_length / (.data$branch_order + 1),
            index1 = log(.data$growth_length) / .data$radius^2,
            index2 = .data$radius^2 / log(.data$growth_length)
          )

        # Identifies poorly modeled cylinders
        path_temp <- path_cyl %>%
          mutate( # general cylinder pass
            IQR = stats::IQR(.data$index0),
            upper = stats::quantile(.data$index0, probs = c(.75), na.rm = FALSE) + 1.5 * .data$IQR,
            lower = stats::quantile(.data$index0, probs = c(.25), na.rm = FALSE) - 1.5 * .data$IQR,
            bad_fit0 = case_when(.data$lower <= .data$index0 & .data$index0 >= .data$upper ~ 1, TRUE ~ 0)
          ) %>%
          filter(.data$bad_fit0 == 0) %>%
          group_by("branch_order") %>%
          mutate( # removes small cylinders
            IQR = stats::IQR(.data$index1),
            upper = stats::quantile(.data$index1, probs = c(.75), na.rm = FALSE) + 1.5 * .data$IQR,
            lower = stats::quantile(.data$index1, probs = c(.25), na.rm = FALSE) - 1.5 * .data$IQR,
            bad_fit1 = case_when(.data$lower <= .data$index1 & .data$index1 >= .data$upper ~ 1, TRUE ~ 0)
          ) %>%
          filter(.data$bad_fit1 == 0) %>%
          mutate( # removes large cylinders
            IQR = stats::IQR(.data$index2),
            upper = stats::quantile(.data$index2, probs = c(.75), na.rm = FALSE) + 1.5 * .data$IQR,
            lower = stats::quantile(.data$index2, probs = c(.25), na.rm = FALSE) - 1.5 * .data$IQR,
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
          left_join(select(path_temp, "id", bad_fit = "bad_fit3"), by = "id") %>%
          mutate(
            bad_fit = replace_na(.data$bad_fit, 1),
            bad_fit = case_when(.data$branch == 1 & .data$branch_position <= !!stem ~ 0, TRUE ~ .data$bad_fit)
          )

        # Uses good cylinder fits to model paths
        # We rename growth length and radius to x a y for shorter labels
        path_temp <- path_cyl %>%
          filter(.data$bad_fit == 0) %>%
          select(x = "growth_length", y = "radius")

        # Broken Branch Filter -------------------------------------------------

        # Identifies broken branches and removes any real twig tapering
        # Broken branches have <= 1 child branch in the 1st order branches
        max_order <- slice_tail(path_cyl, n = 1) %>%
          select("branch_order") %>%
          pull()

        if (max_order %in% c(1)) {
          x <- path_temp$x
          y <- path_temp$y
          min_rad <- y[length(y)]
          max_rad_ord <- path_cyl %>%
            filter(.data$radius == !!min_rad) %>%
            select("branch_order") %>%
            distinct() %>%
            pull()

          # Broken branch radii are 25% less in each new order if there are no good cylinders
          min_rad <- (y[length(y)] - (0.25 * y[length(y)])) / max_order
          max_children <- cylinder %>%
            filter(.data$branch %in% c(
              pull(slice_tail(path_cyl, n = 1), .data$branch),
              pull(path_cyl %>%
                     filter(!.data$branch == 1) %>%
                     slice_head(n = 1), .data$branch)
            )) %>%
            filter(.data$total_children >= 2) %>%
            nrow()

          # Bypasses broken branch filter if branch is alive in 1st order
          if (max_children > 3) {
            x <- path_temp$x
            x[length(x) + 1] <- min(path_cyl$growth_length)
            y <- path_temp$y
            y[length(y) + 1] <- twig_radius

            min_rad <- y[length(y)]
            max_rad_ord <- 0
          }
        } else { # Bypasses broken branch filter for alive branches
          x <- path_temp$x
          x[length(x) + 1] <- min(path_cyl$growth_length)
          y <- path_temp$y
          y[length(y) + 1] <- twig_radius

          min_rad <- y[length(y)]
          max_rad_ord <- 0
        }

        # Fit Monotonic GAM ----------------------------------------------------

        # Forces model intercept through the minimum twig diameter (twig_radius)
        matrix <- matrix(ncol = 3, nrow = 1, byrow = TRUE)
        matrix[1, ] <- c(0, min(path_cyl$growth_length), twig_radius)

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
          path_cyl$radius <- stats::predict(model, path_cyl$growth_length)[, 2]
        } else { # Ignores paths that are too short to be modeled
          path_cyl$radius <- twig_radius
        }

        #  Removes tapering on main stem, broken, and dead branches
        path_cyl <- path_cyl %>%
          mutate(radius = case_when(
            .data$bad_fit == 0 & .data$branch_order == 0 ~ .data$raw_radius,
            TRUE ~ .data$radius
          )) %>%
          mutate(radius = case_when(
            .data$radius < !!min_rad & !(!!max_rad_ord == 0) ~ !!min_rad,
            TRUE ~ .data$radius
          ))

        # Diagnostic Graph -----------------------------------------------------

        # path_cyl %>%
        #   mutate(fit = stats::predict(model, path_cyl$growthLength)[, 2]) %>%
        #   ggplot() +
        #   geom_line(aes(y = fit, x = growthLength), linewidth = 0.5, color = "black") +
        #   geom_point(aes(x = growth_length, y = raw_radius, color = as.factor(branch_order), shape = as.factor(bad_fit))) +
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

    # Combines all paths and calculates weighted mean for each cylinder
    cyl_radii <- bind_rows(results) %>%
      group_by("id") %>%
      summarize(radius = stats::weighted.mean(w = .data$radius, .data$radius, na.rm = TRUE))

    # Updates the QSM with new radii and interpolates any missing radii
    cylinder <- cylinder %>%
      select(-"radius") %>%
      left_join(cyl_radii, by = "id") %>%
      relocate("radius", .before = "length")

    message("Done!")
  }
  # TreeQSM --------------------------------------------------------------------
  else if (all(c("parent", "extension", "branch", "BranchOrder") %in% colnames(cylinder))) {
    message("Generating Branch Paths")

    # Finds end of buttress at first branch for better main stem modeling
    stem <- filter(cylinder, .data$branch == 1)
    stem <- min(which(stem$totalChildren > 1))

    # Creates path network
    g <- data.frame(parent = cylinder$parent, extension = cylinder$extension)
    g <- igraph::graph_from_data_frame(g)

    starts <- igraph::V(g)[igraph::degree(g, mode = "in") == 0]
    finals <- igraph::V(g)[igraph::degree(g, mode = "out") == 0]

    paths <- igraph::all_simple_paths(g, from = starts[[1]], to = finals)

    # Start parallel workers
    oplan <- parallel_workers(backend, start_workers = TRUE)

    # Define global variables to pass devtools::check()
    i <- NULL

    message("Correcting Branch Paths")

    # Loops through the paths
    progressr::with_progress({
      # Progress Bar
      p <- progressr::progressor(along = 1:length(paths))

      results <- foreach::foreach(i = 1:length(paths), .inorder = FALSE) %dofuture% {
        # Identify Good Cylinder Fits ------------------------------------------

        # Extracts cylinders for each unique path
        cyl_id <- sort(as.numeric(names(paths[[i]])))

        # Creates indexes to identify poorly fit cylinders
        path_cyl <- filter(cylinder, .data$extension %in% !!cyl_id) %>%
          mutate(
            index0 = .data$radius / .data$growthLength / (.data$BranchOrder + 1),
            index1 = log(.data$growthLength) / .data$radius^2,
            index2 = .data$radius^2 / log(.data$growthLength)
          )

        # Identifies poorly modeled cylinders
        path_temp <- path_cyl %>%
          mutate( # general cylinder pass
            IQR = stats::IQR(.data$index0),
            upper = stats::quantile(.data$index0, probs = c(.75), na.rm = FALSE) + 1.5 * .data$IQR,
            lower = stats::quantile(.data$index0, probs = c(.25), na.rm = FALSE) - 1.5 * .data$IQR,
            bad_fit0 = case_when(.data$lower <= .data$index0 & .data$index0 >= .data$upper ~ 1, TRUE ~ 0)
          ) %>%
          filter(.data$bad_fit0 == 0) %>%
          group_by("BranchOrder") %>%
          mutate( # removes small cylinders
            IQR = stats::IQR(.data$index1),
            upper = stats::quantile(.data$index1, probs = c(.75), na.rm = FALSE) + 1.5 * .data$IQR,
            lower = stats::quantile(.data$index1, probs = c(.25), na.rm = FALSE) - 1.5 * .data$IQR,
            bad_fit1 = case_when(.data$lower <= .data$index1 & .data$index1 >= .data$upper ~ 1, TRUE ~ 0)
          ) %>%
          filter(.data$bad_fit1 == 0) %>%
          mutate( # removes large cylinders
            IQR = stats::IQR(.data$index2),
            upper = stats::quantile(.data$index2, probs = c(.75), na.rm = FALSE) + 1.5 * .data$IQR,
            lower = stats::quantile(.data$index2, probs = c(.25), na.rm = FALSE) - 1.5 * .data$IQR,
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
          left_join(select(path_temp, "extension", bad_fit = "bad_fit3"), by = "extension") %>%
          mutate(
            bad_fit = replace_na(.data$bad_fit, 1),
            bad_fit = case_when(.data$branch == 1 & .data$PositionInBranch <= !!stem ~ 0, TRUE ~ .data$bad_fit)
          )

        # Uses good cylinder fits to model paths
        # We rename growth length and radius to x a y for shorter labels
        path_temp <- path_cyl %>%
          filter(.data$bad_fit == 0) %>%
          select(x = "growthLength", y = "radius")

        # Broken Branch Filter -------------------------------------------------

        # Identifies broken branches and removes any real twig tapering
        # Broken branches have <= 1 child branch in the 1st order branches
        max_order <- slice_tail(path_cyl, n = 1) %>%
          select("BranchOrder") %>%
          pull()

        if (max_order %in% c(1)) {
          x <- path_temp$x
          y <- path_temp$y
          min_rad <- y[length(y)]
          max_rad_ord <- path_cyl %>%
            filter(.data$radius == !!min_rad) %>%
            select("BranchOrder") %>%
            distinct() %>%
            pull()

          # Broken branch radii are 25% less in each new order if there are no good cylinders
          min_rad <- (y[length(y)] - (0.25 * y[length(y)])) / max_order
          max_children <- cylinder %>%
            filter(.data$branch %in% c(
              pull(slice_tail(path_cyl, n = 1), .data$branch),
              pull(path_cyl %>%
                filter(!.data$branch == 1) %>%
                slice_head(n = 1), .data$branch)
            )) %>%
            filter(.data$totalChildren >= 2) %>%
            nrow()

          # Bypasses bropken branch filter if branch is alive in 1st order
          if (max_children > 3) {
            x <- path_temp$x
            x[length(x) + 1] <- min(path_cyl$growthLength)
            y <- path_temp$y
            y[length(y) + 1] <- twig_radius

            min_rad <- y[length(y)]
            max_rad_ord <- 0
          }
        } else { # Bypasses broken branch filter for alive branches
          x <- path_temp$x
          x[length(x) + 1] <- min(path_cyl$growthLength)
          y <- path_temp$y
          y[length(y) + 1] <- twig_radius

          min_rad <- y[length(y)]
          max_rad_ord <- 0
        }

        # Fit Monotonic GAM ----------------------------------------------------

        # Forces model intercept through the minimum twig diameter (twig_radius)
        matrix <- matrix(ncol = 3, nrow = 1, byrow = TRUE)
        matrix[1, ] <- c(0, min(path_cyl$growthLength), twig_radius)

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
          path_cyl$radius <- stats::predict(model, path_cyl$growthLength)[, 2]
        } else { # Ignores paths that are too short to be modeled
          path_cyl$radius <- twig_radius
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

        # Diagnostic Graph -----------------------------------------------------

        # path_cyl %>%
        #   mutate(fit = stats::predict(model, path_cyl$growthLength)[, 2]) %>%
        #   ggplot() +
        #   geom_line(aes(y = fit, x = growthLength), linewidth = 0.5, color = "black") +
        #   geom_point(aes(x = growthLength, y = UnmodRadius, color = as.factor(BranchOrder), shape = as.factor(bad_fit))) +
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

    # Combines all paths and calculates weighted mean for each cylinder
    cyl_radii <- bind_rows(results) %>%
      group_by("extension") %>%
      summarize(radius = stats::weighted.mean(w = .data$radius, .data$radius, na.rm = TRUE))

    # Updates the QSM with new radii and interpolates any missing radii
    cylinder <- cylinder %>%
      select(-"radius") %>%
      left_join(cyl_radii, by = "extension") %>%
      relocate("radius", .before = "length")

    message("Done!")
  }
  # SimpleForest  --------------------------------------------------------------
  else if (all(c("ID", "parentID", "branchID", "branchOrder") %in% colnames(cylinder))) {
    message("Generating Branch Paths")

    # Finds end of buttress at first branch for better main stem modeling
    stem <- filter(cylinder, .data$branchID == 1)
    stem <- min(which(stem$totalChildren > 1))

    # Creates path network
    g <- data.frame(parent = cylinder$parentID, extension = cylinder$ID)
    g <- igraph::graph_from_data_frame(g)

    starts <- igraph::V(g)[igraph::degree(g, mode = "in") == 0]
    finals <- igraph::V(g)[igraph::degree(g, mode = "out") == 0]

    paths <- igraph::all_simple_paths(g, from = starts[[1]], to = finals)

    # Initialize parallel workers
    parallel_workers(backend, start_workers = TRUE)

    # Define global variables to pass devtools::check()
    i <- NULL

    message("Correcting Branch Paths")

    # Loops through the paths
    progressr::with_progress({
      # Progress Bar
      p <- progressr::progressor(along = 1:length(paths))

      results <- foreach::foreach(i = 1:length(paths), .inorder = FALSE) %dofuture% {
        # Identify Good Cylinder Fits ------------------------------------------

        # Extracts cylinders for each unique path
        cyl_id <- sort(as.numeric(names(paths[[i]])))

        # Creates indexes to identify poorly fit cylinder
        path_cyl <- filter(cylinder, .data$ID %in% .data$cyl_id) %>%
          mutate(
            index0 = .data$radius / .data$growthLength / (.data$branchOrder + 1),
            index1 = log(.data$growthLength) / .data$radius^2,
            index2 = .data$radius^2 / log(.data$growthLength)
          )

        # Identifies poorly modeled cylinders
        path_temp <- path_cyl %>%
          mutate( # general cylinder pass
            IQR = stats::IQR(.data$index0),
            upper = stats::quantile(.data$index0, probs = c(.75), na.rm = FALSE) + 1.5 * .data$IQR,
            lower = stats::quantile(.data$index0, probs = c(.25), na.rm = FALSE) - 1.5 * .data$IQR,
            bad_fit0 = case_when(.data$lower <= .data$index0 & .data$index0 >= .data$upper ~ 1, TRUE ~ 0)
          ) %>%
          filter(.data$bad_fit0 == 0) %>%
          group_by("branchOrder") %>%
          mutate( # removes small cylinders
            IQR = stats::IQR(.data$index1),
            upper = stats::quantile(.data$index1, probs = c(.75), na.rm = FALSE) + 1.5 * .data$IQR,
            lower = stats::quantile(.data$index1, probs = c(.25), na.rm = FALSE) - 1.5 * .data$IQR,
            bad_fit1 = case_when(.data$lower <= .data$index1 & .data$index1 >= .data$upper ~ 1, TRUE ~ 0)
          ) %>%
          filter(.data$bad_fit1 == 0) %>%
          mutate( # removes large cylinders
            IQR = stats::IQR(.data$index2),
            upper = stats::quantile(.data$index2, probs = c(.75), na.rm = FALSE) + 1.5 * .data$IQR,
            lower = stats::quantile(.data$index2, probs = c(.25), na.rm = FALSE) - 1.5 * .data$IQR,
            bad_fit2 = case_when(.data$lower <= .data$index2 & .data$index2 <= .data$upper ~ 0, TRUE ~ 1),
            bad_fit3 = NA
          ) %>%
          filter(.data$bad_fit2 == 0) %>%
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
          left_join(select(path_temp, "ID", bad_fit = "bad_fit3"), by = "ID") %>%
          mutate(bad_fit = replace_na(.data$bad_fit, 1))

        # Uses good cylinder fits to model paths
        # We rename growth length and radius to x a y for shorter labels
        path_temp <- path_cyl %>%
          filter(.data$bad_fit == 0) %>%
          select(x = "growthLength", y = "radius")

        # Dead Branch Filter ---------------------------------------------------

        # Identifies dead or broken branches and removes any real twig tapering
        # Dead branches have <= 1 child branch in the 1st order branches
        max_order <- slice_tail(path_cyl, n = 1) %>%
          select("branchOrder") %>%
          pull()

        if (max_order %in% c(1)) {
          x <- path_temp$x
          y <- path_temp$y
          min_rad <- y[length(y)]
          max_rad_ord <- path_cyl %>%
            filter(.data$radius == !!min_rad) %>%
            select("branchOrder") %>%
            distinct() %>%
            pull()

          # Dead branch radii are 25% less in each new order if there are no good cylinders
          min_rad <- (y[length(y)] - (0.25 * y[length(y)])) / max_order
          max_children <- cylinder %>%
            filter(.data$branchID %in% c(
              pull(slice_tail(path_cyl, n = 1), .data$branchID),
              pull(path_cyl %>%
                filter(!.data$branchID == 1) %>%
                slice_head(n = 1), .data$branchID)
            )) %>%
            filter(.data$totalChildren >= 2) %>%
            nrow()

          # Bypasses dead branch filter if branch is alive in 1st order
          if (max_children > 3) {
            x <- path_temp$x
            x[length(x) + 1] <- min(path_cyl$growthLength)
            y <- path_temp$y
            y[length(y) + 1] <- twig_radius

            min_rad <- y[length(y)]
            max_rad_ord <- 0
          }
        } else { # Bypasses dead branch filter for alive branches
          x <- path_temp$x
          x[length(x) + 1] <- min(path_cyl$growthLength)
          y <- path_temp$y
          y[length(y) + 1] <- twig_radius

          min_rad <- y[length(y)]
          max_rad_ord <- 0
        }

        # Fit Monotonic GAM ----------------------------------------------------

        # Forces model intercept through the minimum twig diameter (twig_radius)
        matrix <- matrix(ncol = 3, nrow = 1, byrow = TRUE)
        matrix[1, ] <- c(0, min(path_cyl$growthLength), twig_radius)

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
          path_cyl$radius <- stats::predict(model, path_cyl$growthLength)[, 2]
        } else { # Ignores paths that are too short to be modeled
          path_cyl$radius <- twig_radius
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

        # Diagnostic Graph -----------------------------------------------------

        # path_cyl %>%
        #   mutate(fit = stats::predict(model, path_cyl$growthLength)[, 2]) %>%
        #   ggplot() +
        #   geom_line(aes(y = fit, x = growthLength), linewidth = 0.5, color = "black") +
        #   geom_point(aes(x = growthLength, y = UnmodRadius, color = as.factor(branchOrder), shape = as.factor(bad_fit))) +
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

    # Combines all paths and calculates weighted mean for each cylinder
    cyl_radii <- bind_rows(results) %>%
      group_by("ID") %>%
      summarize(radius = stats::weighted.mean(w = .data$radius, .data$radius, na.rm = TRUE))

    # Updates the QSM with new radii and interpolates any missing radii
    cylinder <- cylinder %>%
      select(-"radius") %>%
      left_join(cyl_radii, by = c("ID"))

    message("Done!")
  }
  # Treegraph ------------------------------------------------------------------
  else if (all(c("p1", "p2", "ninternode") %in% colnames(cylinder))) {
    message("Generating Branch Paths")

    # Finds end of buttress at first branch for better main stem modeling
    stem <- filter(cylinder, .data$nbranch == 1)
    stem <- min(which(stem$totalChildren > 1))

    # Creates path network
    g <- data.frame(parent = cylinder$p2, extension = cylinder$p1)
    g <- igraph::graph_from_data_frame(g)

    starts <- igraph::V(g)[igraph::degree(g, mode = "in") == 0]
    finals <- igraph::V(g)[igraph::degree(g, mode = "out") == 0]

    paths <- igraph::all_simple_paths(g, from = starts[[1]], to = finals)

    # Initialize parallel workers
    parallel_workers(backend, start_workers = TRUE)

    # Define global variables to pass devtools::check()
    i <- NULL

    message("Correcting Branch Paths")

    # Loops through the paths
    progressr::with_progress({
      # Progress Bar
      p <- progressr::progressor(along = 1:length(paths))

      results <- foreach::foreach(i = 1:length(paths), .inorder = FALSE) %dofuture% {
        # Identify Good Cylinder Fits ------------------------------------------

        # Extracts cylinders for each unique path
        cyl_id <- sort(as.numeric(names(paths[[i]])))

        # Creates indexes to identify poorly fit cylinder
        path_cyl <- filter(cylinder, .data$p1 %in% .data$cyl_id) %>%
          mutate(
            index0 = .data$radius / .data$growthLength / (.data$branch_order + 1),
            index1 = log(.data$growthLength) / .data$radius^2,
            index2 = .data$radius^2 / log(.data$growthLength)
          )

        # Identifies poorly modeled cylinders
        path_temp <- path_cyl %>%
          mutate( # general cylinder pass
            IQR = stats::IQR(.data$index0),
            upper = stats::quantile(.data$index0, probs = c(.75), na.rm = FALSE) + 1.5 * .data$IQR,
            lower = stats::quantile(.data$index0, probs = c(.25), na.rm = FALSE) - 1.5 * .data$IQR,
            bad_fit0 = case_when(.data$lower <= .data$index0 & .data$index0 >= .data$upper ~ 1, TRUE ~ 0)
          ) %>%
          filter(.data$bad_fit0 == 0) %>%
          group_by("branch_order") %>%
          mutate( # removes small cylinders
            IQR = stats::IQR(.data$index1),
            upper = stats::quantile(.data$index1, probs = c(.75), na.rm = FALSE) + 1.5 * .data$IQR,
            lower = stats::quantile(.data$index1, probs = c(.25), na.rm = FALSE) - 1.5 * .data$IQR,
            bad_fit1 = case_when(.data$lower <= .data$index1 & .data$index1 >= .data$upper ~ 1, TRUE ~ 0)
          ) %>%
          filter(.data$bad_fit1 == 0) %>%
          mutate( # removes large cylinders
            IQR = stats::IQR(.data$index2),
            upper = stats::quantile(.data$index2, probs = c(.75), na.rm = FALSE) + 1.5 * .data$IQR,
            lower = stats::quantile(.data$index2, probs = c(.25), na.rm = FALSE) - 1.5 * .data$IQR,
            bad_fit2 = case_when(.data$lower <= .data$index2 & .data$index2 <= .data$upper ~ 0, TRUE ~ 1),
            bad_fit3 = NA
          ) %>%
          filter(.data$bad_fit2 == 0) %>%
          ungroup()

        # Identifies bad cylinder fits by tapering
        # This taper is more aggressive than the TreeQSM taper
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
          left_join(select(path_temp, "p1", bad_fit = "bad_fit3"), by = "p1") %>%
          mutate(bad_fit = replace_na(.data$bad_fit, 1))

        # Uses good cylinder fits to model paths
        # We rename growth length and radius to x a y for shorter labels
        path_temp <- path_cyl %>%
          filter(.data$bad_fit == 0) %>%
          select(x = "growthLength", y = "radius")

        # Dead Branch Filter ---------------------------------------------------

        # Identifies dead or broken branches and removes any real twig tapering
        # Dead branches have <= 1 child branch in the 1st order branches
        max_order <- slice_tail(path_cyl, n = 1) %>%
          select("branch_order") %>%
          pull()

        if (max_order %in% c(1)) {
          x <- path_temp$x
          y <- path_temp$y
          min_rad <- y[length(y)]
          max_rad_ord <- path_cyl %>%
            filter(.data$radius == !!min_rad) %>%
            select("branch_order") %>%
            distinct() %>%
            pull()

          # Dead branch radii are 25% less in each new order if there are no good cylinders
          min_rad <- (y[length(y)] - (0.25 * y[length(y)])) / max_order
          max_children <- cylinder %>%
            filter(.data$nbranch %in% c(
              pull(slice_tail(path_cyl, n = 1), .data$nbranch),
              pull(path_cyl %>%
                filter(!.data$nbranch == 1) %>%
                slice_head(n = 1), .data$nbranch)
            )) %>%
            filter(.data$totalChildren >= 2) %>%
            nrow()

          # Bypasses dead branch filter if branch is alive in 1st order
          if (max_children > 3) {
            x <- path_temp$x
            x[length(x) + 1] <- min(path_cyl$growthLength)
            y <- path_temp$y
            y[length(y) + 1] <- twig_radius

            min_rad <- y[length(y)]
            max_rad_ord <- 0
          }
        } else { # Bypasses dead branch filter for alive branches
          x <- path_temp$x
          x[length(x) + 1] <- min(path_cyl$growthLength)
          y <- path_temp$y
          y[length(y) + 1] <- twig_radius

          min_rad <- y[length(y)]
          max_rad_ord <- 0
        }

        # Fit Monotonic GAM ----------------------------------------------------

        # Forces model intercept through the minimum twig diameter (twig_radius)
        matrix <- matrix(ncol = 3, nrow = 1, byrow = TRUE)
        matrix[1, ] <- c(0, min(path_cyl$growthLength), twig_radius)

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
          path_cyl$radius <- stats::predict(model, path_cyl$growthLength)[, 2]
        } else { # Ignores paths that are too short to be modeled
          path_cyl$radius <- twig_radius
        }

        #  Removes tapering on main stem, broken, and dead branches
        path_cyl <- path_cyl %>%
          mutate(radius = case_when(
            .data$bad_fit == 0 & .data$branch_order == 0 ~ .data$UnmodRadius,
            TRUE ~ .data$radius
          )) %>%
          mutate(radius = case_when(
            .data$radius < !!min_rad & !(!!max_rad_ord == 0) ~ !!min_rad,
            TRUE ~ .data$radius
          ))

        # Diagnostic Graph -----------------------------------------------------

        # path_cyl %>%
        #   mutate(fit = stats::predict(model, path_cyl$growthLength)[, 2]) %>%
        #   ggplot() +
        #   geom_line(aes(y = fit, x = growthLength), linewidth = 0.5, color = "black") +
        #   geom_point(aes(x = growthLength, y = UnmodRadius, color = as.factor(branch_order), shape = as.factor(bad_fit))) +
        #   labs(
        #     x = "Growth Length (m)",
        #     y = "Radius (m)",
        #     title = "Real Twig Path Correction",
        #     color = "Branch Order",
        #     shape = "Cylinder Fit", subtitle = "Treegraph"
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

    # Combines all paths and calculates weighted mean for each cylinder
    cyl_radii <- bind_rows(results) %>%
      group_by("p1") %>%
      summarize(radius = stats::weighted.mean(w = .data$radius, .data$radius, na.rm = TRUE))

    # Updates the QSM with new radii and interpolates any missing radii
    cylinder <- cylinder %>%
      select(-"radius") %>%
      left_join(cyl_radii, by = c("p1"))

    message("Done!")
  } else {
    message(
      "Invalid Dataframe Supplied!!!
      \nOnly TreeQSM, SimpleForest, or Treegraph QSMs are supported.
      \nMake sure the cylinder data frame and not the QSM list is supplied."
    )
  }
  return(cylinder)

  # Future Package Cleanup
  parallel_workers(end_workers = TRUE)
}


#' Starts parallel workers
#' @param backend user defined parallel backend
#' @param start_workers TRUE or NULL
#' @param end_workers TRUE or NULL
#' @returns cylinder data frame with reverse branch order
#' @noRd
parallel_workers <- function(
    backend = NULL,
    start_workers = NULL,
    end_workers = NULL) {
  message("Starting Parallel Workers")

  if (start_workers == TRUE) {
    # Initialize parallel workers
    chk <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")

    if (nzchar(chk) && chk == "TRUE") {
      # Use sequential to pass checks
      oplan <- future::plan("sequential")
    } else {
      # Use all cores for end-users
      if (backend == "sequential") {
        oplan <- future::plan(backend)
      } else {
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

    return(oplan)
  }

  if (end_workers == TRUE) {
    # Future Package Cleanup
    chk <- Sys.getenv("_R_CHECK_CONNECTIONS_LEFT_OPEN_", "")

    if (nzchar(chk) && chk == "TRUE") {
      future::plan("sequential")
    } else {
      on.exit(plan(!!oplan), add = TRUE)
    }
  }
}

#' Combine all paths into one data frame
#' @param backend user defined parallel backend
#' @param id column name of parent cylinders
#' @param parent column name of parent cylinders
#' @param radius column name of the cylinder radii
#' @param branch column name of the branches
#' @param branch_order column name of the branch orders
#' @param reverse_order column name of the reverse branch order
#' @param branch_position column name of the branch position
#' @param growth_length column name of growth length
#' @param total_children column name of total children
#'
#' @returns cylinder data frame with all paths
#' @noRd
combine_paths <- function(
    cylinder,
    id,
    parent,
    radius,
    branch,
    branch_order,
    reverse_order,
    branch_position,
    growth_length,
    total_children) {
  # Find all paths
  paths <- build_network(
    cylinder = cylinder,
    id = id,
    parent = parent,
    all_paths = FALSE
  )

  # Extract required variables
  path_vars <- cylinder %>%
    select(
      id = !!rlang::sym(id),
      radius = !!rlang::sym(radius),
      branch = !!rlang::sym(branch),
      branch_order = !!rlang::sym(branch_order),
      reverse_order = !!rlang::sym(reverse_order),
      branch_position = !!rlang::sym(branch_position),
      growth_length = !!rlang::sym(growth_length),
      total_children = !!rlang::sym(total_children)
    )

  # Combine all path data
  path_data <- left_join(paths, path_vars) %>%
    drop_na() %>%
    rename(path = "index")

  return(path_data)
}
