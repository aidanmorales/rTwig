#' Correct Radii
#'
#' @description Corrects cylinder radii.
#'
#' @param df QSM cylinder data frame
#' @param twigRad Twig radius in meters.
#' @param method QSM type, as either "TreeQSM" or "SimpleForest". Defaults to TreeQSM.
#'
#' @return Returns a data.frame
#' @export
#'
#' @import dplyr
#' @rawNamespace import(igraph, except=c(union, as_data_frame, groups, crossing))
#' @import cobs
#' @import parallel
#' @import doParallel
#' @import doSNOW
#' @import foreach
#' @importFrom zoo na.approx
#' @importFrom data.table rbindlist
#' @importFrom stats IQR predict quantile
#' @importFrom utils setTxtProgressBar txtProgressBar
#'
#' @examples
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
#' df <- update_cylinders(df, method = "SimpleForest")
#' df <- growth_length(df, method = "SimpleForest")
#' df <- correct_radii(df, twigRad = 0.003, method = "SimpleForest")
#' str(df)
correct_radii <- function(df, twigRad, method = "TreeQSM") {
  message("Correcting Cylinder Radii")

  if (method == "TreeQSM") {
    # Creates path network
    g <- data.frame(parent = df$parent, id = df$id)
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

    # Loops through the paths
    results <- foreach(
      i = 1:length(paths),
      .options.snow = opts,
      .inorder = FALSE,
      .packages = c("dplyr", "cobs")
    ) %dopar% {
      cyl_id <- sort(as.numeric(names(paths[[i]])))
      path_cyl <- filter(df, id %in% cyl_id)

      path_cyl <- path_cyl %>%
        mutate(
          row_num = n() - row_number() + 1,
          GrowthLength2 = sqrt(GrowthLength) * row_num,
          index = radius / GrowthLength
        ) %>%
        mutate(
          IQR = IQR(index),
          upper = quantile(index, probs = c(.75), na.rm = FALSE) + 1.5 * IQR,
          lower = quantile(index, probs = c(.25), na.rm = FALSE) - 1.5 * IQR,
          bad_fit1 = case_when(lower <= index & index <= upper ~ 0, TRUE ~ 1),
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
        mutate(bad_fit = case_when(bad_fit1 == 1 & bad_fit2 == 0 ~ 1, TRUE ~ bad_fit2))

      # Extracts good cylinder fits to model paths
      path_temp <- path_cyl %>%
        mutate(weight = 100000) %>%
        filter(bad_fit == 0) %>%
        select(x = GrowthLength2, y = radius, weight)

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
      model <- cobs(x, y, lambda = 1, degree = 2, constraint = "increase", pointwise = matrix, ic = "SIC", print.mesg = FALSE, repeat.delete.add = FALSE, w = weights)
      path_cyl$radius <- predict(model, path_cyl$GrowthLength2)[, 2]

      return(path_cyl)
    }

    # Ends parallel cluster
    stopCluster(cl)

    # Calculates single cylinder radii from all paths
    cyl_radii <- data.table::rbindlist(results) %>%
      group_by(id) %>%
      mutate(
        IQR = IQR(radius),
        upper = quantile(radius, probs = c(.75), na.rm = FALSE) + 1.5 * IQR,
        lower = quantile(radius, probs = c(.25), na.rm = FALSE) - 1.5 * IQR,
        bad_fit = case_when(lower <= radius & radius <= upper ~ 0, TRUE ~ 1)
      ) %>%
      filter(bad_fit == 0) %>%
      summarize(
        radius = max(radius, na.rm = TRUE)
      ) %>%
      mutate(radius = case_when(radius < twigRad ~ twigRad, TRUE ~ radius))

    # Updates the QSM with new radii
    df <- df %>%
      select(-radius) %>%
      left_join(cyl_radii, by = "id") %>%
      group_by(branch) %>%
      mutate(radius = zoo::na.approx(radius, rule = 2))
  } else if (method == "SimpleForest") {
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
      # Use all cores for enduser
      n_cores <- parallel::detectCores(logical = FALSE)
    }

    cl <- makeCluster(n_cores)
    registerDoSNOW(cl)

    # Initializes the progress bar for the loop
    progress <- function(n) setTxtProgressBar(txtProgressBar(max = max(length(paths)), style = 3), n)
    opts <- list(progress = progress)

    # Loops through the paths
    results <- foreach(
      i = 1:length(paths),
      .options.snow = opts,
      .inorder = FALSE,
      .packages = c("dplyr", "cobs")
    ) %dopar% {
      cyl_id <- sort(as.numeric(names(paths[[i]])))
      path_cyl <- filter(df, ID %in% cyl_id)

      path_cyl <- path_cyl %>%
        mutate(
          row_num = n() - row_number() + 1,
          GrowthLength2 = sqrt(growthLength) * row_num,
          index = radius / growthLength
        ) %>%
        mutate(
          IQR = IQR(index),
          upper = quantile(index, probs = c(.75), na.rm = FALSE) + 1.5 * IQR,
          lower = quantile(index, probs = c(.25), na.rm = FALSE) - 1.5 * IQR,
          bad_fit1 = case_when(lower <= index & index <= upper ~ 0, TRUE ~ 1),
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
        mutate(bad_fit = case_when(bad_fit1 == 1 & bad_fit2 == 0 ~ 1, TRUE ~ bad_fit2))

      # Extracts good cylinder fits to model paths
      path_temp <- path_cyl %>%
        mutate(weight = 100000) %>%
        filter(bad_fit == 0) %>%
        select(x = GrowthLength2, y = radius, weight)

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
      model <- cobs(x, y, lambda = 1, degree = 2, constraint = "increase", pointwise = matrix, ic = "SIC", print.mesg = FALSE, repeat.delete.add = FALSE, w = weights)
      path_cyl$radius <- predict(model, path_cyl$GrowthLength2)[, 2]

      return(path_cyl)
    }

    # Ends parallel cluster
    stopCluster(cl)

    # Calculates single cylider radii from all paths
    cyl_radii <- data.table::rbindlist(results) %>%
      group_by(ID) %>%
      mutate(
        IQR = IQR(radius),
        upper = quantile(radius, probs = c(.75), na.rm = FALSE) + 1.5 * IQR,
        lower = quantile(radius, probs = c(.25), na.rm = FALSE) - 1.5 * IQR,
        bad_fit = case_when(lower <= radius & radius <= upper ~ 0, TRUE ~ 1)
      ) %>%
      filter(bad_fit == 0) %>%
      summarize(
        radius = max(radius, na.rm = TRUE),
        bad_fit = mean(bad_fit, na.rm = TRUE)
      ) %>%
      select(-bad_fit) %>%
      mutate(radius = case_when(radius < twigRad ~ twigRad, TRUE ~ radius))

    # Updates the QSM with new radii
    df <- df %>%
      select(-radius) %>%
      left_join(cyl_radii, by = "ID") %>%
      mutate(modVolume = pi * radius^2 * length) %>%
      relocate(modVolume, .after = volume) %>%
      relocate(radius, .after = radius)
  } else {
    message("Invalid Method Entered!!!\nValid Methods = TreeQSM or SimpleForest")
  }

  return(df)
}
