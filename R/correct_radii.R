#' Correct Radii
#'
#' @description Corrects cylinder radii
#'
#' @param cylinder QSM cylinder data frame
#' @param twig_radius Twig radius in millimeters
#' @param broken_branch Enable or disable the broken branch filter. Defaults to enabled (TRUE).
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
correct_radii <- function(
    cylinder,
    twig_radius,
    broken_branch = TRUE,
    backend = "multisession") {
  # Converts twig radius to meters
  twig_radius <- twig_radius / 1000

  # Gets broken branch filter
  broken_branch <- broken_branch

  # Gets parallel backend
  backend <- backend

  # rTwig ----------------------------------------------------------------------
  if (all(c("id", "parent", "start_x", "branch_order") %in% colnames(cylinder))) {
    model_paths(
      cylinder = cylinder, id = "id", parent = "parent",
      radius = "radius", raw_radius = "raw_radius",
      growth_length = "growth_length",
      branch = "branch", branch_order = "branch_order",
      branch_position = "branch_position",
      total_children = "total_children",
      twig_radius = twig_radius, backend = backend, taper = 1,
      broken_branch = broken_branch
    )
  }
  # TreeQSM --------------------------------------------------------------------
  else if (all(c("parent", "extension", "branch", "BranchOrder") %in% colnames(cylinder))) {
    model_paths(
      cylinder = cylinder, id = "extension", parent = "parent",
      radius = "radius", raw_radius = "UnmodRadius",
      growth_length = "growthLength",
      branch = "branch", branch_order = "BranchOrder",
      branch_position = "PositionInBranch",
      total_children = "totalChildren",
      twig_radius = twig_radius, backend = backend, taper = 1,
      broken_branch = broken_branch
    )
  }
  # SimpleForest  --------------------------------------------------------------
  else if (all(c("ID", "parentID", "branchID", "branchOrder") %in% colnames(cylinder))) {
    model_paths(
      cylinder = cylinder, id = "ID", parent = "parentID",
      radius = "radius", raw_radius = "UnmodRadius",
      growth_length = "growthLength",
      branch = "branchID", branch_order = "branchOrder",
      branch_position = "positionInBranch",
      total_children = "totalChildren",
      twig_radius = twig_radius, backend = backend, taper = 2,
      broken_branch = broken_branch
    )
  }
  # Treegraph ------------------------------------------------------------------
  else if (all(c("p1", "p2", "ninternode") %in% colnames(cylinder))) {
    model_paths(
      cylinder = cylinder, id = "p1", parent = "p2",
      radius = "radius", raw_radius = "UnmodRadius",
      growth_length = "growthLength",
      branch = "nbranch", branch_order = "branch_order",
      branch_position = "positionInBranch",
      total_children = "totalChildren",
      twig_radius = twig_radius, backend = backend, taper = 2,
      broken_branch = broken_branch
    )
  }
  # aRchi ----------------------------------------------------------------------
  else if (all(c("cyl_ID", "parent_ID", "branching_order") %in% colnames(cylinder))) {
    model_paths(
      cylinder = cylinder, id = "cyl_ID", parent = "parent_ID",
      radius = "radius_cyl", raw_radius = "UnmodRadius",
      growth_length = "growthLength",
      branch = "branch_ID", branch_order = "branching_order",
      branch_position = "positionInBranch",
      total_children = "totalChildren",
      twig_radius = twig_radius, backend = backend, taper = 2,
      broken_branch = broken_branch
    )
  } else {
    message(
      "Invalid Dataframe Supplied!!!
      \nOnly TreeQSM, SimpleForest, or Treegraph QSMs are supported.
      \nMake sure the cylinder data frame and not the QSM list is supplied."
    )
  }
}

#' Model paths
#' @param cylinder QSM cylinder data frame
#' @param id column name of cylinder ids
#' @param parent column name of parent cylinders
#' @param radius column name of the cylinder radii
#' @param raw_radius column name of the raw cylinder radii
#' @param growth_length column name of growth length
#' @param branch column name of the branches
#' @param branch_order column name of the branch orders
#' @param branch_position column name of the branch position
#' @param total_children column name of total children
#' @param twig_radius twig radius in meters
#' @param taper taper method
#' @param broken_branch broken branch filter
#' @param backend user defined parallel backend
#' @returns cylinder data frame with corrected radii
#' @noRd
model_paths <- function(
    cylinder,
    id,
    parent,
    radius,
    raw_radius,
    growth_length,
    branch,
    branch_order,
    branch_position,
    total_children,
    twig_radius,
    backend = "multisession",
    taper = 1,
    broken_branch = TRUE) {
  message("Generating Branch Paths")

  # Select required variables
  cyl_sub <- cylinder %>%
    select(
      "id" = all_of(id),
      "parent" = all_of(parent),
      "radius" = all_of(radius),
      "raw_radius" = all_of(raw_radius),
      "growth_length" = all_of(growth_length),
      "branch" = all_of(branch),
      "branch_order" = all_of(branch_order),
      "branch_position" = all_of(branch_position),
      "total_children" = all_of(total_children)
    )

  # Finds end of buttress at first branch for better main stem modeling
  stem <- filter(cyl_sub, .data$branch == 1)
  stem <- min(which(stem$total_children > 1))

  # Creates path network
  g <- data.frame(parent = cyl_sub$parent, id = cyl_sub$id)
  g <- igraph::graph_from_data_frame(g)

  starts <- igraph::V(g)[igraph::degree(g, mode = "in") == 0]
  finals <- igraph::V(g)[igraph::degree(g, mode = "out") == 0]

  paths <- igraph::all_simple_paths(g, from = starts[[1]], to = finals)

  # Define global variables to pass devtools::check()
  i <- NULL

  # Start parallel workers
  oplan <- parallel_workers(backend, start_workers = TRUE)

  if (!backend == "sequential") {
    options <- list(
      .options.future = list(
        globals = structure(c("paths", "cyl_sub"), add = TRUE),
        packages = c("tidytable", "cobs")
      ),
      .inorder = FALSE
    )
  } else {
    options <- NULL
  }

  message("Correcting Branch Paths")

  # Loops through the paths
  progressr::with_progress({
    # Progress Bar
    p <- progressr::progressor(along = 1:length(paths))

    results <- foreach::foreach(
      i = 1:length(paths), .options.future = options, .verbose = FALSE
    ) %dofuture% {
      # Identify Good Cylinder Fits --------------------------------------------
      # Extracts cylinders for each unique path
      cyl_id <- sort(as.numeric(names(paths[[i]])))

      # Creates indexes to identify poorly fit cylinders
      path_cyl <- filter(cyl_sub, .data$id %in% !!cyl_id) %>%
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
          bad_fit0 = case_when(
            .data$lower <= .data$index0 & .data$index0 >= .data$upper ~ 1, TRUE ~ 0
          )
        ) %>%
        filter(.data$bad_fit0 == 0) %>%
        group_by("branch_order") %>%
        mutate( # removes small cylinders
          IQR = stats::IQR(.data$index1),
          upper = stats::quantile(.data$index1, probs = c(.75), na.rm = FALSE) + 1.5 * .data$IQR,
          lower = stats::quantile(.data$index1, probs = c(.25), na.rm = FALSE) - 1.5 * .data$IQR,
          bad_fit1 = case_when(
            .data$lower <= .data$index1 & .data$index1 >= .data$upper ~ 1, TRUE ~ 0
          )
        ) %>%
        filter(.data$bad_fit1 == 0) %>%
        mutate( # removes large cylinders
          IQR = stats::IQR(.data$index2),
          upper = stats::quantile(.data$index2, probs = c(.75), na.rm = FALSE) + 1.5 * .data$IQR,
          lower = stats::quantile(.data$index2, probs = c(.25), na.rm = FALSE) - 1.5 * .data$IQR,
          bad_fit2 = case_when(
            .data$lower <= .data$index2 & .data$index2 <= .data$upper ~ 0, TRUE ~ 1
          ),
          bad_fit3 = NA
        ) %>%
        filter(.data$bad_fit2 == 0) %>%
        ungroup()

      if (taper == 1) {
        # Identifies bad cylinder fits by tapering
        for (k in 1:nrow(path_temp)) {
          z <- length(
            which(
              (as.vector(path_temp$radius[k] - path_temp$radius[1:k]) / path_temp$radius[k]) > 1 / sqrt(k)
            )
          )

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
            bad_fit = case_when(
              .data$branch == 1 & .data$branch_position <= !!stem ~ 0, TRUE ~ .data$bad_fit
            )
          )
      } else if (taper == 2) {
        # Identifies bad cylinder fits by tapering
        # This taper is more aggressive when unmodified cylinders are not available
        for (k in 1:nrow(path_temp)) {
          z <- length(
            which(
              (as.vector(path_temp$radius[k] - path_temp$radius[1:k]) / path_temp$radius[k]) > 0
            )
          )

          if (z > 0) {
            path_temp$bad_fit3[k] <- 1
          } else {
            path_temp$bad_fit3[k] <- 0
          }
        }

        # Joins bad fits and accounts for buttress flare
        path_cyl <- path_cyl %>%
          left_join(select(path_temp, "id", bad_fit = "bad_fit3"), by = "id") %>%
          mutate(bad_fit = replace_na(.data$bad_fit, 1))
      }

      # Uses good cylinder fits to model paths
      # We rename growth length and radius to x a y for shorter labels
      path_temp <- path_cyl %>%
        filter(.data$bad_fit == 0) %>%
        select(x = "growth_length", y = "radius")

      # Broken Branch Filter ---------------------------------------------------
      if (broken_branch == TRUE) {
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

          # Broken branch radii are 25% less in new orders when no good cylinders exist
          min_rad <- (y[length(y)] - (0.25 * y[length(y)])) / max_order
          max_children <- cyl_sub %>%
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
      } else { # Bypass broken branch filter
        x <- path_temp$x
        y <- path_temp$y

        min_rad <- twig_radius

        max_rad_ord <- slice_tail(path_cyl, n = 1) %>%
          select("branch_order") %>%
          pull()
      }

      # Fit Monotonic GAM ------------------------------------------------------
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

      # Removes tapering on main stem, broken, and dead branches
      path_cyl <- path_cyl %>%
        mutate(radius = case_when(
          .data$bad_fit == 0 & .data$branch_order == 0 ~ .data$raw_radius,
          TRUE ~ .data$radius
        )) %>%
        mutate(radius = case_when(
          .data$radius < !!min_rad & !(!!max_rad_ord == 0) ~ !!min_rad,
          TRUE ~ .data$radius
        ))

      # Diagnostic Graph -------------------------------------------------------
      # path_cyl %>%
      #   mutate(fit = stats::predict(model, path_cyl$growth_length)[, 2]) %>%
      #   ggplot() +
      #   geom_line(aes(y = fit, x = growth_length), linewidth = 0.5, color = "black") +
      #   geom_point(aes(x = growth_length, y = raw_radius, color = as.factor(branch_order), shape = as.factor(bad_fit))) +
      #   labs(
      #     x = "Growth Length (m)",
      #     y = "Radius (m)",
      #     title = "Real Twig Path Correction",
      #     color = "Branch Order",
      #     shape = "Cylinder Fit"
      #   ) +
      #   theme_classic()

      # Path Index
      path_cyl$pathIndex <- i

      # Update Loop Progress
      p()

      return(path_cyl)
    }
  })

  # Future Package Cleanup
  parallel_workers(end_workers = TRUE, oplan = oplan)

  # Update Radii ---------------------------------------------------------------
  message("Updating Cylinder Radii")

  # Combines all paths and calculates weighted mean for each cylinder
  cyl_radii <- bind_rows(results) %>%
    group_by("id") %>%
    summarize(
      radius = stats::weighted.mean(w = .data$radius, .data$radius, na.rm = TRUE)
    ) %>%
    rename(
      !!rlang::sym(id) := "id",
      !!rlang::sym(radius) := "radius"
    )

  # Updates the QSM cylinder data with new radii
  cylinder %>%
    rename("radius" := !!rlang::sym(radius)) %>%
    select(-"radius") %>%
    left_join(cyl_radii, by = id)
}

#' Starts parallel workers
#' @param backend user defined parallel backend
#' @param start_workers TRUE or NULL
#' @param end_workers TRUE or NULL
#' @param oplan oplan
#' @returns cylinder data frame with reverse branch order
#' @noRd
parallel_workers <- function(
    backend = NULL,
    start_workers = NULL,
    end_workers = NULL,
    oplan = NULL) {
  if (!is.null(start_workers)) {
    message("Starting Workers")

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
        oplan <- future::plan(backend, workers = future::availableCores())
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

  if (!is.null(end_workers)) {
    message("Closing Workers")

    # Future Package Cleanup
    chk <- Sys.getenv("_R_CHECK_CONNECTIONS_LEFT_OPEN_", "")

    if (nzchar(chk) && chk == "TRUE") {
      future::plan("sequential")
    } else {
      on.exit(future::plan(oplan), add = TRUE)
    }
  }
}

#' Combine all paths into one data frame
#' @param backend user defined parallel backend
#' @param id column name of cylinder ids
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
  left_join(paths, path_vars) %>%
    drop_na() %>%
    rename(path = "index")
}
