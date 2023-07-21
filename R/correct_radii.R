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
      # Extracts cylinders for each unique path
      cyl_id <- sort(as.numeric(names(paths[[i]])))
      path_cyl <- filter(df, .data$extension %in% cyl_id)

      # Creates indexes to identify poorly fit cylinders
      path_cyl <- path_cyl %>%
        mutate(

          # Bad fit based on ratio of growth length to radius
          row_num = n() - row_number() + 1,
          GrowthLength2 = sqrt(.data$GrowthLength) * .data$row_num,
          index = .data$radius / .data$GrowthLength,
          IQR = IQR(.data$index),
          upper = quantile(.data$index, probs = c(.75), na.rm = FALSE) + 1.5 * .data$IQR,
          lower = quantile(.data$index, probs = c(.25), na.rm = FALSE) - 1.5 * .data$IQR,
          bad_fit1 = case_when(.data$lower <= .data$index & .data$index <= .data$upper ~ 0, TRUE ~ 1),

          # Bad fit based on position in path versus the radius
          index2 = .data$row_num / radius,
          IQR = IQR(.data$index2),
          upper = quantile(.data$index2, probs = c(.75), na.rm = FALSE) + 1.5 * .data$IQR,
          lower = quantile(.data$index2, probs = c(.25), na.rm = FALSE) - 1.5 * .data$IQR,
          bad_fit2 = case_when(.data$lower <= .data$index2 & .data$index2 <= .data$upper ~ 0, TRUE ~ 1),

          # Take cylinders that pass both checks to send into a taper filter
          bad_fit3 = case_when(.data$bad_fit1 == 1 & .data$bad_fit2 == 0 ~ 1, TRUE ~ .data$bad_fit2),
          bad_fit4 = NA
        )

      # Identifies bad cylinder fits by tapering
      path_temp <- filter(path_cyl, .data$bad_fit3 == 0)

      for (k in 1:nrow(path_temp)) {
        z <- length(which((as.vector(path_temp$radius[k] - path_temp$radius[1:k]) / path_temp$radius[k]) > 1 / k))

        if (z > 0) {
          path_temp$bad_fit4[k] <- 1
        } else {
          path_temp$bad_fit4[k] <- 0
        }
      }

      # Uses good cylinder fits to model path
      path_temp <- path_temp %>%
        #mutate(weight = 100000) %>%
        filter(.data$bad_fit4 == 0) %>%
        select(x = .data$GrowthLength2, y = .data$radius)#, .data$weight)

      x <- path_temp$x
      x[length(x) + 1] <- 0
      y <- path_temp$y
      y[length(y) + 1] <- twigRad
      # weights <- path_temp$weight
      # weights[length(weights) + 1] <- 1

      # Forces model intercept through the minimum twig diameter (twigRad)
      matrix <- matrix(ncol = 3, nrow = 1, byrow = TRUE)
      matrix[1, ] <- c(0, 0, twigRad)

      # Models new cylinder radii
      if (length(x) >= 3) {
        # Fits monotonic GAM to the good cylinders to correct poor fits
        model <- cobs(x, y, lambda = 1, degree = 2, constraint = "increase", pointwise = matrix, ic = "SIC", print.mesg = FALSE, repeat.delete.add = FALSE, nknots = length(x) - 1)#, w = weights)
        path_cyl$radius <- predict(model, path_cyl$GrowthLength2)[, 2]
      } else {
        # Ignores paths that are too short to be modeled
        path_cyl$radius <- path_cyl$radius
      }

      # Index of path
      path_cyl$pathIndex <- i

      return(path_cyl)
    }

    # Ends parallel cluster
    stopCluster(cl)

    # Finds path that contains the entire branch
    path_index <- data.table::rbindlist(results) %>%
      group_by(.data$branch, .data$pathIndex) %>%
      summarize(max_cyl = max(.data$PositionInBranch), .groups = "drop") %>%
      arrange(desc(.data$max_cyl)) %>%
      arrange(.data$branch) %>%
      group_by(.data$branch) %>%
      slice_head(n = 1) %>%
      rename(pathIndex2 = .data$pathIndex)

    # Calculates single cylinder radii from max branch paths
    cyl_radii <- data.table::rbindlist(results) %>%
      left_join(path_index, by = "branch") %>%
      group_by(.data$branch) %>%
      filter(.data$pathIndex == .data$pathIndex2) %>%
      ungroup() %>%
      mutate(radius = case_when(.data$radius < twigRad ~ twigRad, TRUE ~ .data$radius)) %>%
      select(.data$extension, .data$radius)

    # Updates the QSM with new radii and interpolates any missing radii
    df <- df %>%
      select(-.data$radius) %>%
      left_join(cyl_radii, by = c("extension")) %>%
      group_by(.data$branch) %>%
      mutate(radius = case_when(n() > 1 ~ zoo::na.approx(.data$radius, rule = 2), TRUE ~ radius)) %>%
      ungroup() %>%
      distinct(.data$extension, .keep_all = TRUE)
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
      # Extracts cylinders for each unique path
      cyl_id <- sort(as.numeric(names(paths[[i]])))
      path_cyl <- filter(df, .data$ID %in% cyl_id)

      # Creates indexes to identify poorly fit cylinders
      path_cyl <- path_cyl %>%
        mutate(

          # Bad fit based on ratio of growth length to radius
          row_num = n() - row_number() + 1,
          GrowthLength2 = sqrt(.data$growthLength) * .data$row_num,
          index = .data$radius / .data$growthLength,
          IQR = IQR(.data$index),
          upper = quantile(.data$index, probs = c(.75), na.rm = FALSE) + 1.5 * .data$IQR,
          lower = quantile(.data$index, probs = c(.25), na.rm = FALSE) - 1.5 * .data$IQR,
          bad_fit1 = case_when(.data$lower <= .data$index & .data$index <= .data$upper ~ 0, TRUE ~ 1),

          # Bad fit based on position in path versus the radius
          index2 = .data$row_num / radius,
          IQR = IQR(.data$index2),
          upper = quantile(.data$index2, probs = c(.75), na.rm = FALSE) + 1.5 * .data$IQR,
          lower = quantile(.data$index2, probs = c(.25), na.rm = FALSE) - 1.5 * .data$IQR,
          bad_fit2 = case_when(.data$lower <= .data$index2 & .data$index2 <= .data$upper ~ 0, TRUE ~ 1),

          # Take cylinders that pass both checks to send into a taper filter
          bad_fit3 = case_when(.data$bad_fit1 == 1 & .data$bad_fit2 == 0 ~ 1, TRUE ~ .data$bad_fit2),
          bad_fit4 = NA
        )

      # Identifies bad cylinder fits by tapering
      path_temp <- filter(path_cyl, .data$bad_fit3 == 0)

      for (k in 1:nrow(path_temp)) {
        z <- length(which((as.vector(path_temp$radius[k] - path_temp$radius[1:k]) / path_temp$radius[k]) > 1 / k))

        if (z > 0) {
          path_temp$bad_fit4[k] <- 1
        } else {
          path_temp$bad_fit4[k] <- 0
        }
      }

      # Uses good cylinder fits to model path
      path_temp <- path_temp %>%
        mutate(weight = 100000) %>%
        filter(.data$bad_fit4 == 0) %>%
        select(x = .data$GrowthLength2, y = .data$radius, .data$weight)

      x <- path_temp$x
      x[length(x) + 1] <- 0
      y <- path_temp$y
      y[length(y) + 1] <- twigRad
      weights <- path_temp$weight
      weights[length(weights) + 1] <- 1

      # Forces model intercept through the minimum twig diameter (twigRad)
      matrix <- matrix(ncol = 3, nrow = 1, byrow = TRUE)
      matrix[1, ] <- c(0, 0, twigRad)

      # Models new cylinder radii
      if (length(x) >= 3) {
        # Fits monotonic GAM to the good cylinders to correct poor fits
        model <- cobs(x, y, lambda = 1, degree = 2, constraint = "increase", pointwise = matrix, ic = "SIC", print.mesg = FALSE, repeat.delete.add = FALSE, w = weights)
        path_cyl$radius <- predict(model, path_cyl$GrowthLength2)[, 2]
      } else {
        # Ignores paths that are too short to be modeled
        path_cyl$radius <- path_cyl$radius
      }

      # Index of path
      path_cyl$pathIndex <- i

      return(path_cyl)
    }

    # Ends parallel cluster
    stopCluster(cl)

    # Finds path that contains the entire branch
    path_index <- data.table::rbindlist(results) %>%
      group_by(.data$branchNew, .data$pathIndex) %>%
      summarize(max_cyl = max(.data$positionInBranch), .groups = "drop") %>%
      arrange(desc(.data$max_cyl)) %>%
      arrange(.data$branchNew) %>%
      group_by(.data$branchNew) %>%
      slice_head(n = 1) %>%
      rename(pathIndex2 = .data$pathIndex)

    # Calculates single cylinder radii from max branch paths
    cyl_radii <- data.table::rbindlist(results) %>%
      left_join(path_index, by = "branchNew") %>%
      group_by(.data$branchNew) %>%
      filter(.data$pathIndex == .data$pathIndex2) %>%
      ungroup() %>%
      mutate(radius = case_when(.data$radius < twigRad ~ twigRad, TRUE ~ .data$radius)) %>%
      select(.data$ID, .data$radius)

    # Updates the QSM with new radii and interpolates any missing radii
    df <- df %>%
      select(-.data$radius) %>%
      left_join(cyl_radii, by = c("ID")) %>%
      group_by(.data$branchNew) %>%
      mutate(radius = case_when(n() > 1 ~ zoo::na.approx(.data$radius, rule = 2), TRUE ~ radius)) %>%
      ungroup() %>%
      distinct(.data$ID, .keep_all = TRUE)
  } else {
    message("Invalid Dataframe Supplied!!!\nOnly TreeQSM or SimpleForest QSMs are supported.")
  }
  return(df)
}
