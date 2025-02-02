#' Tree Metrics
#'
#' @description Calculates tree metrics from a QSM
#'
#' @details Calculates detailed tree, branch, and segment metrics from a QSM.
#'  Valid inputs require a connected QSM, which can be a whole tree or an
#'  individual branch. The outputs include all of the standard outputs from
#'  TreeQSM, and also additional variables, including, but not limited to,
#'  growth length, reverse branch order, branch segment or node relationships,
#'  and distances from twigs and the base of the tree, across various
#'  distribution metrics. Also included is a simulated point cloud of the tree,
#'  based on the QSM cylinder radii. When corrected with Real Twig, this allows
#'  for the testing and validation of point cloud diameter overestimation
#'  throughout the tree.
#'
#' @param cylinder QSM cylinder data frame
#' @param verify Verify QSM topology. Defaults to TRUE. If `verify = TRUE`,
#'  only topologically connected structures (e.g. a whole a tree or an
#'  individual branch) are eligible to run. This ensures all metrics are correct
#'  and verified. If `verify = FALSE`, the metrics are  run, but not verified.
#'  This is strongly discouraged, but can enable the calculation of tree metrics
#'  on topologically disconnected structures.
#'
#' @return Returns a list of tree metric data frames and a synthetic point cloud
#' @export
#'
#' @references
#' \insertRef{TreeQSM}{rTwig}
#'
#' \insertRef{growth_parameter1}{rTwig}
#'
#' \insertRef{growth_parameter2}{rTwig}
#'
#' \insertRef{treegraph2}{rTwig}
#'
#' \insertRef{path_fraction}{rTwig}
#'
#' @examples
#'
#' ## TreeQSM Processing Chain
#' file <- system.file("extdata/QSM.mat", package = "rTwig")
#' cylinder <- import_treeqsm(file)$cylinder
#' cylinder <- update_cylinders(cylinder)
#' metrics <- tree_metrics(cylinder)
#' names(metrics)
#'
#' ## SimpleForest Processing Chain
#' file <- system.file("extdata/QSM.csv", package = "rTwig")
#' cylinder <- read.csv(file)
#' cylinder <- update_cylinders(cylinder)
#' metrics <- tree_metrics(cylinder)
#' names(metrics)
#'
tree_metrics <- function(cylinder, verify = TRUE) {
  # Check inputs ---------------------------------------------------------------
  if (is_missing(cylinder)) {
    message <- "argument `cylinder` is missing, with no default."
    abort(message, class = "missing_argument")
  }

  if (!is.data.frame(cylinder)) {
    message <- paste(
      paste0("`cylinder` must be a data frame, not ", class(cylinder), "."),
      "i Did you accidentally pass the QSM list instead of the cylinder data frame?",
      sep = "\n"
    )
    abort(message, class = "data_format_error")
  }

  # Verify cylinders -----------------------------------------------------------
  cylinder <- verify_cylinders(cylinder)

  # rTwig ----------------------------------------------------------------------
  if (all(c("id", "parent", "start_x", "branch_order") %in% colnames(cylinder))) {
    calculate_tree_metrics(
      cylinder = cylinder, id = "id", parent = "parent",
      branch = "branch", branch_alt = "branch_alt",
      radius = "radius", raw_radius = "raw_radius",
      length = "length", segment = "segment",
      branch_position = "branch_position",
      growth_length = "growth_length", branch_order = "branch_order",
      reverse_order = "reverse_order", total_children = "total_children",
      base_distance = "base_distance", twig_distance = "twig_distance",
      vessel_volume = "vessel_volume", pipe_area = "pipe_area",
      pipe_radius = "pipe_radius",
      start_x = "start_x", start_y = "start_y", start_z = "start_z",
      axis_x = "axis_x", axis_y = "axis_y", axis_z = "axis_z",
      end_x = "end_x", end_y = "end_y", end_z = "end_z", verify
    )
  }
  # TreeQSM --------------------------------------------------------------------
  else if (all(c("parent", "extension", "branch", "BranchOrder") %in% colnames(cylinder))) {
    calculate_tree_metrics(
      cylinder = cylinder, id = "extension", parent = "parent",
      branch = "branch", branch_alt = "branch_alt",
      radius = "radius", raw_radius = "UnmodRadius",
      length = "length", segment = "segment",
      branch_position = "PositionInBranch",
      growth_length = "growthLength", branch_order = "BranchOrder",
      reverse_order = "reverseBranchOrder", total_children = "totalChildren",
      base_distance = "distanceFromBase", twig_distance = "distanceToTwig",
      vessel_volume = "vesselVolume", pipe_area = "reversePipeAreaBranchorder",
      pipe_radius = "reversePipeRadiusBranchorder",
      start_x = "start.x", start_y = "start.y", start_z = "start.z",
      axis_x = "axis.x", axis_y = "axis.y", axis_z = "axis.z",
      end_x = "end.x", end_y = "end.y", end_z = "end.z", verify
    )
  }
  # SimpleForest ---------------------------------------------------------------
  else if (all(c("ID", "parentID", "branchID", "branchOrder") %in% colnames(cylinder))) {
    calculate_tree_metrics(
      cylinder = cylinder, id = "ID", parent = "parentID",
      branch = "branchID", branch_alt = "branch_alt",
      radius = "radius", raw_radius = "UnmodRadius",
      length = "length", segment = "segmentID",
      branch_position = "positionInBranch",
      growth_length = "growthLength", branch_order = "branchOrder",
      reverse_order = "reverseBranchOrder", total_children = "totalChildren",
      base_distance = "distanceFromBase", twig_distance = "distanceToTwig",
      vessel_volume = "vesselVolume", pipe_area = "reversePipeAreaBranchorder",
      pipe_radius = "reversePipeRadiusBranchorder",
      start_x = "startX", start_y = "startY", start_z = "startZ",
      axis_x = "axisX", axis_y = "axisY", axis_z = "axisZ",
      end_x = "endX", end_y = "endY", end_z = "endZ", verify
    )
  }
  # Treegraph ------------------------------------------------------------------
  else if (all(c("p1", "p2", "ninternode") %in% colnames(cylinder))) {
    calculate_tree_metrics(
      cylinder = cylinder, id = "p1", parent = "p2",
      branch = "nbranch", branch_alt = "branch_alt",
      radius = "radius", raw_radius = "UnmodRadius",
      length = "length", segment = "segment",
      branch_position = "positionInBranch",
      growth_length = "growthLength", branch_order = "branch_order",
      reverse_order = "reverseBranchOrder", total_children = "totalChildren",
      base_distance = "distanceFromBase", twig_distance = "distanceToTwig",
      vessel_volume = "vesselVolume", pipe_area = "reversePipeAreaBranchorder",
      pipe_radius = "reversePipeRadiusBranchorder",
      start_x = "sx", start_y = "sy", start_z = "sz",
      axis_x = "ax", axis_y = "ay", axis_z = "az",
      end_x = "ex", end_y = "ey", end_z = "ez", verify
    )
  }
  # aRchi ----------------------------------------------------------------------
  else if (all(c("cyl_ID", "parent_ID", "branching_order") %in% colnames(cylinder))) {
    calculate_tree_metrics(
      cylinder = cylinder, id = "cyl_ID", parent = "parent_ID",
      branch = "branch_ID", branch_alt = "branch_alt",
      radius = "radius_cyl", raw_radius = "UnmodRadius",
      length = "length", segment = "segment",
      branch_position = "positionInBranch",
      growth_length = "growthLength", branch_order = "branching_order",
      reverse_order = "reverseBranchOrder", total_children = "totalChildren",
      base_distance = "distanceFromBase", twig_distance = "distanceToTwig",
      vessel_volume = "vesselVolume", pipe_area = "reversePipeAreaBranchorder",
      pipe_radius = "reversePipeRadiusBranchorder",
      start_x = "startX", start_y = "startY", start_z = "startZ",
      axis_x = "axisX", axis_y = "axisY", axis_z = "axisZ",
      end_x = "endX", end_y = "endY", end_z = "endZ", verify
    )
  } else {
    message <- paste(
      "Unsupported QSM format provided.",
      "i Only TreeQSM, SimpleForest, Treegraph, or aRchi QSMs are supported.",
      sep = "\n"
    )
    abort(message, class = "data_format_error")
  }
}

#' Calculates tree metrics
#' @param cylinder QSM cylinder data frame
#' @param id cylinder ids
#' @param parent cylinder parent ids
#' @param branch cylinder branch id
#' @param branch_alt cylinder alternate branch id
#' @param radius cylinder radius
#' @param raw_radius cylinder unmodified radius
#' @param length cylinder length
#' @param segment cylinder segment id
#' @param branch_position cylinder position in branch
#' @param growth_length cylinder growth length
#' @param branch_order cylinder branch order
#' @param reverse_order cylinder reverse branch order
#' @param total_children cylinder total children
#' @param base_distance cylinder distance from base of the tree
#' @param twig_distance cylinder average distance from the twigs
#' @param vessel_volume cylinder vessel volume
#' @param pipe_area cylinder pipe area
#' @param pipe_radius cylinder pipe radius
#' @param start_x cylinder start x position
#' @param start_y cylinder start y position
#' @param start_z cylinder start z position
#' @param axis_x cylinder x axis
#' @param axis_y cylinder y axis
#' @param axis_z cylinder z axis
#' @param end_x cylinder end x position
#' @param end_y cylinder end y position
#' @param end_z cylinder end z position
#' @param verify verify connectivity
#' @returns list of tree metrics
#' @noRd
calculate_tree_metrics <- function(
    cylinder,
    id,
    parent,
    branch,
    branch_alt,
    radius,
    raw_radius,
    length,
    segment,
    branch_position,
    growth_length,
    branch_order,
    reverse_order,
    total_children,
    base_distance,
    twig_distance,
    vessel_volume,
    pipe_area,
    pipe_radius,
    start_x,
    start_y,
    start_z,
    axis_x,
    axis_y,
    axis_z,
    end_x,
    end_y,
    end_z,
    verify) {
  # Dynamically select cylinder variables --------------------------------------
  cylinder <- cylinder %>%
    select(
      id = {{ id }},
      parent = {{ parent }},
      branch = {{ branch }},
      branch_alt = {{ branch_alt }},
      radius = {{ radius }},
      raw_radius = {{ raw_radius }},
      length = {{ length }},
      segment = {{ segment }},
      branch_position = {{ branch_position }},
      growth_length = {{ growth_length }},
      branch_order = {{ branch_order }},
      reverse_order = {{ reverse_order }},
      total_children = {{ total_children }},
      base_distance = {{ base_distance }},
      twig_distance = {{ twig_distance }},
      vessel_volume = {{ vessel_volume }},
      pipe_area = {{ pipe_area }},
      pipe_radius = {{ pipe_radius }},
      start_x = {{ start_x }},
      start_y = {{ start_y }},
      start_z = {{ start_z }},
      axis_x = {{ axis_x }},
      axis_y = {{ axis_y }},
      axis_z = {{ axis_z }},
      end_x = {{ end_x }},
      end_y = {{ end_y }},
      end_z = {{ end_z }},
      modified = any_of("modified")
    )

  # Verify connectivity --------------------------------------------------------
  if (verify == TRUE) {
    qsm_g <- verify_network(cylinder, graph = TRUE)
    qsm_connectivity <- igraph::is_connected(qsm_g)
  } else {
    qsm_connectivity <- TRUE # bypass connectivity checks
    message <- paste(
      "`verify` is not TRUE!",
      "QSM topology will not be verified!",
      "Please set `verify = TRUE` unless you have a specific reason not to.",
      sep = "\n"
    )
    warn(message)
  }

  if (qsm_connectivity == FALSE) {
    message <- paste(
      "The QSM is not a connected structure!",
      "x Tree metrics can not be calculated!",
      "i Please use `summarise_qsm()` to calculate metrics for disconnected structures.",
      sep = "\n"
    )
    abort(message)
  } else {
    if (verify == TRUE) {
      base <- as.numeric(igraph::ends(qsm_g, 1)[2])
    } else {
      base <- cylinder$id[1]
    }
  }

  stem_info <- cylinder %>%
    filter(.data$id == !!base) %>%
    select("branch", "branch_order")

  # Check for missing values ---------------------------------------------------
  if (anyNA(cylinder)) {
    message <- paste(
      "NA values detected!",
      "Removing NA values to ensure compatibility.",
      "Please verify the QSM data is correct.",
      sep = "\n"
    )
    warn(message)

    cylinder <- drop_na(cylinder)
  }

  # Update variables for individual branches -----------------------------------
  if (base != 1 & stem_info$branch != 1) {
    message <- paste(
      "Branch detected!",
      "The branch will be treated like an individual tree.",
      sep = " "
    )
    warn(message)

    # Re-scale branch order and base distance
    cylinder <- cylinder %>%
      mutate(
        branch_order = .data$branch_order - min(.data$branch_order),
        base_distance = .data$base_distance - min(.data$base_distance),
        index = row_number()
      )

    network <- list()
    network$child_df <- build_network(
      cylinder, "id", "parent",
      pruning = TRUE, cache = FALSE
    ) %>%
      rename(id2 = "id") %>%
      suppressMessages()

    cyl_id <- select(cylinder, "id", "index")

    network$child_df <- network$child_df %>%
      left_join(cyl_id, by = "index") %>%
      select(index2 = "id", index = "id2") %>%
      left_join(cyl_id, by = "index") %>%
      select(index = "index2", "id")

    cylinder <- branch_alt(
      network = network, cylinder = select(cylinder, -"index", -"branch_alt"),
      "id", "parent", "branch", "branch_order"
    ) %>%
      suppressMessages()
  }

  # List to store metrics ------------------------------------------------------
  metrics <- list()

  # Extract required matrix variables ------------------------------------------
  axis <- cbind(cylinder$axis_x, cylinder$axis_y, cylinder$axis_z)
  start <- cbind(cylinder$start_x, cylinder$start_y, cylinder$start_z)
  tips <- cbind(cylinder$end_x, cylinder$end_y, cylinder$end_z)

  # Calculate branch metrics ---------------------------------------------------
  inform("Calculating Branch Metrics")
  metrics$branch <- branch_metrics(cylinder, base)

  if (length(unique(metrics$branch$branch)) == 1) {
    branch_check <- FALSE
    message <- paste(
      "No branches are present!",
      "Branch metrics cannot be calculated!",
      sep = " "
    )
    warn(message)
  } else {
    branch_check <- TRUE
  }

  # Calculate segment metrics --------------------------------------------------
  inform("Calculating Segment Metrics")
  metrics$segment <- segment_metrics(cylinder, base)

  if (length(unique(metrics$segment$segment)) == 1) {
    segment_check <- FALSE
    message <- paste(
      "Only one segment is present!",
      "Segment metrics cannot be calculated!",
      sep = " "
    )
    warn(message)
  } else {
    segment_check <- TRUE
  }

  # List to store tree metrics -------------------------------------------------
  tree <- tidytable()

  # Extract Cylinder Variables -------------------------------------------------
  inform("Calculating Tree Metrics")
  trunk_cyl <- filter(cylinder, branch == !!stem_info$branch)
  branch_cyl <- filter(cylinder, !branch == !!stem_info$branch)
  twig_cyl <- filter(cylinder, reverse_order == 1)

  # Tree Attributes from Cylinders ---------------------------------------------
  tree$tree_volume_m3 <- sum(pi * cylinder$radius^2 * cylinder$length)
  tree$stem_volume_m3 <- sum(pi * trunk_cyl$radius^2 * trunk_cyl$length)
  tree$branch_volume_m3 <- sum(pi * branch_cyl$radius^2 * branch_cyl$length)

  tree$tree_height_m <- max(cylinder$start_z) - min(cylinder$start_z)

  tree$tree_length_m <- sum(cylinder$length)
  tree$stem_length_m <- sum(trunk_cyl$length)
  tree$branch_length_m <- sum(branch_cyl$length)

  tree$branches <- length(unique(cylinder$branch))
  tree$branches_alt <- length(unique(cylinder$branch_alt))
  tree$max_branch_order <- max(cylinder$branch_order)
  tree$max_reverse_order <- max(cylinder$reverse_order)

  tree$twigs <- length(unique(twig_cyl$branch))
  tree$twig_length_m <- sum(twig_cyl$length)
  tree$twig_distance_m <- cylinder$twig_distance[base]

  tree$path_fraction <- cylinder %>%
    filter(.data$twig_distance == .data$length) %>%
    summarise(
      path_fraction = mean(.data$base_distance) / max(.data$base_distance)
    )

  tree$tree_area_m2 <- 2 * pi * sum(cylinder$radius * cylinder$length)
  tree$stem_area_m2 <- 2 * pi * sum(trunk_cyl$radius * trunk_cyl$length)
  tree$branch_area_m2 <- 2 * pi * sum(branch_cyl$radius * branch_cyl$length)

  # Diameter at Breast Height --------------------------------------------------
  tree$dbh_qsm_cm <- suppressWarnings(dbh_cylinder(trunk_cyl, "radius") * 100)
  tree$dbh_raw_cm <- dbh_cylinder(trunk_cyl, "raw_radius") * 100

  # Diameter at Base (Tree or Branch) ------------------------------------------
  tree$d_base_qsm_cm <- cylinder$radius[base] * 200
  tree$d_base_raw_cm <- cylinder$raw_radius[base] * 200

  # Generate Point Cloud -------------------------------------------------------
  inform("Generating Point Cloud")
  metrics$cloud <- generate_cloud(
    start = start,
    axis = axis,
    tips = tips,
    length = cylinder$length,
    radius = cylinder$radius,
    branch = cylinder$branch,
    metrics = TRUE,
    spacing = 0
  )

  # Crown Diameters ------------------------------------------------------------
  inform("Calculating Crown Metrics")
  diameters <- crown_diameters(metrics$cloud, tips)
  tree$crown_diameter_mean_m <- mean(diameters)
  tree$crown_diameter_max_m <- max(diameters)
  tree$crown_diameter_min_m <- min(diameters)

  # Crown Areas ----------------------------------------------------------------
  tree$crown_projected_area_m2 <- convex_hull_area(metrics$cloud)

  # Crown Base Height ----------------------------------------------------------
  tree$crown_base_height_m <- crown_base_height(
    cylinder = cylinder, branch = metrics$branch, base = base,
    dbh = tree$dbh_qsm_cm, d_base = tree$d_base_qsm_cm,
    start = start, axis = axis, tips = tips
  )

  # Crown Length and Crown Ratio -----------------------------------------------
  tree$crown_length_m <- tree$tree_height_m - tree$crown_base_height_m
  tree$crown_ratio <- tree$crown_length_m / tree$tree_height_m

  # Crown 3D Convex Hull -------------------------------------------------------
  if (is.na(tree$crown_base_height_m)) {
    tree$crown_area_m2 <- NA
    tree$crown_volume_m3 <- NA
  } else {
    base_height <- min(metrics$cloud[, 3]) + tree$crown_base_height_m
    crown_cloud <- metrics$cloud[metrics$cloud[, 3] >= base_height, ]
    crown_hull_3d <- geometry::convhulln(crown_cloud, "FA")
    tree$crown_area_m2 <- crown_hull_3d$area
    tree$crown_volume_m3 <- crown_hull_3d$vol / 1000
  }

  # Allometric Variables -------------------------------------------------------
  tree$vessel_volume <- cylinder$vessel_volume[base]
  tree$pipe_area <- cylinder$pipe_area[base]
  tree$pipe_radius <- cylinder$pipe_radius[base]

  # Tree Location --------------------------------------------------------------
  tree$start_x <- start[1, 1]
  tree$start_y <- start[1, 2]
  tree$start_z <- start[1, 3]

  # Modification Indices -------------------------------------------------------
  if ("modified" %in% colnames(cylinder)) {
    tree$modified <- mean(cylinder$modified)
  } else {
    tree$modified <- NA
  }

  volume_old <- sum(pi * cylinder$raw_radius^2 * cylinder$length)
  area_old <- 2 * pi * sum(cylinder$raw_radius * cylinder$length)

  tree$volume_change_pct <- (tree$tree_volume_m3 - volume_old) / volume_old * 100
  tree$area_change_pct <- (tree$tree_area_m2 - area_old) / area_old * 100

  # Save Tree Metrics ----------------------------------------------------------
  metrics$tree <- tree

  # Tree Distribution Metrics --------------------------------------------------
  inform("Calculating Tree Distributions")
  metrics$tree_height_dist <- tree_distributions(cylinder, "height")
  metrics$tree_diameter_dist <- tree_distributions(cylinder, "diameter")
  metrics$tree_zenith_dist <- tree_distributions(cylinder, "zenith")
  metrics$tree_azimuth_dist <- tree_distributions(cylinder, "azimuth")

  # Branch Distribution Metrics ------------------------------------------------
  if (branch_check == TRUE) {
    inform("Calculating Branch Distributions")
    metrics$branch_diameter_dist <- branch_distributions(metrics$branch, tree, "diameter")
    metrics$branch_height_dist <- branch_distributions(metrics$branch, tree, "height")
    metrics$branch_angle_dist <- branch_distributions(metrics$branch, tree, "angle")
    metrics$branch_zenith_dist <- branch_distributions(metrics$branch, tree, "zenith")
    metrics$branch_azimuth_dist <- branch_distributions(metrics$branch, tree, "azimuth")
    metrics$branch_order_dist <- branch_order_distributions(metrics$branch)
  }

  # Segment Distribution Metrics -----------------------------------------------
  if (segment_check == TRUE) {
    inform("Calculating Segment Distributions")
    metrics$segment_diameter_dist <- segment_distributions(metrics$segment, tree, "diameter")
    metrics$segment_height_dist <- segment_distributions(metrics$segment, tree, "height")
    metrics$segment_angle_dist <- segment_distributions(metrics$segment, tree, "angle")
    metrics$segment_zenith_dist <- segment_distributions(metrics$segment, tree, "zenith")
    metrics$segment_azimuth_dist <- segment_distributions(metrics$segment, tree, "azimuth")
    metrics$segment_order_dist <- segment_order_distributions(metrics$segment)
  }

  # Stem Taper -----------------------------------------------------------------
  inform("Calculating Stem Taper")
  n <- nrow(trunk_cyl)

  if (n == 1) {
    message <- paste(
      "Only one main stem cylinder is present!",
      "Stem taper cannot be calculated!",
      sep = " "
    )
    warn(message)

    stem_taper <- tidytable(height_m = na_dbl, diameter_cm = na_dbl)
  } else {
    stem_taper <- tidytable(height_m = as.double(0:n), diameter_cm = na_dbl)
    stem_taper[1, 1] <- 0
    stem_taper[1, 2] <- 2 * trunk_cyl$radius[1]
    stem_taper[2:(n + 1), 1] <- cumsum(trunk_cyl$length)
    stem_taper[2:(n + 1), 2] <- c(2 * trunk_cyl$radius[2:n], 2 * trunk_cyl$radius[n])
  }

  metrics$stem_taper <- stem_taper
  metrics$stem_taper$diameter_cm <- metrics$stem_taper$diameter_cm * 100

  # Calculate Spreads ----------------------------------------------------------
  inform("Calculating Spreads")
  metrics$spreads <- calculate_spreads(metrics$cloud)
  metrics$spreads <- as_tidytable(t(metrics$spreads)) %>%
    mutate(azimuth_deg = seq(20, 360, by = 20)) %>%
    pivot_longer(cols = 1:20, names_to = "height_class", values_to = "spread_m")
  metrics$spreads$height_class <- gsub("V", "", metrics$spreads$height_class)
  metrics$spreads$height_class <- as.integer(metrics$spreads$height_class)

  # Vertical Profile -----------------------------------------------------------
  metrics$vertical_profile <- metrics$spreads %>%
    group_by("height_class") %>%
    summarise(avg_spread_m = mean(.data$spread_m))

  # Alternate Branch Metrics ---------------------------------------------------
  if (branch_check == TRUE) {
    inform("Calculating Alternate Branch Metrics")
    metrics$branch_alt <- branch_alt_metrics(cylinder, base)
  }

  # rTwig Version & Run Date ---------------------------------------------------
  metrics$version <- paste0("rTwig v", utils::packageVersion("rTwig"))
  metrics$run_date <- Sys.time()

  return(metrics)
}

#' Calculates the dbh cylinder from a QSM
#' @param trunk QSM cylinder data frame containing only the main stem
#' @param radius column name of trunk radii
#' @returns dbh of the tree in meters
#' @noRd
dbh_cylinder <- function(trunk, radius) {
  dbh <- trunk %>%
    arrange(.data$branch_position) %>%
    select(length = "length", radius = !!rlang::sym(radius))

  if (sum(dbh$length) < 1.37) {
    message <- paste(
      "The main stem is shorter than 1.37 meters.",
      "DBH cannot be calculated!",
      sep = " "
    )
    warn(message)

    return(NA)
  } else {
    # Finds the DBH cylinder
    for (i in 1:nrow(dbh)) {
      dbh_cyl <- sum(dbh$length[1:i])
      if (dbh_cyl >= 1.37) {
        break
      }
    }
    return(dbh[i, radius] * 2)
  }
}

#' Calculates branch metrics
#' @param cylinder QSM cylinder data frame
#' @param base cylinder id of the stem base
#' @returns data frame with per branch summary metrics
#' @noRd
branch_metrics <- function(cylinder, base) {
  # Get Parent Axis IDs
  parent_axis <- cylinder %>%
    select(
      parent = "id",
      parent_branch = "branch",
      p_axis_x = "axis_x",
      p_axis_y = "axis_y",
      p_axis_z = "axis_z"
    )

  # Base Cylinder
  base <- cylinder %>%
    filter(.data$id == !!base) %>%
    select(start_z = "start_z") %>%
    slice_head(1) %>%
    pull()

  # Calculate Branch Metrics
  cylinder %>%
    group_by("branch") %>%
    summarise(
      parent = first(.data$parent),
      axis_x = first(.data$axis_x),
      axis_y = first(.data$axis_y),
      axis_z = first(.data$axis_z),
      branch_order = first(.data$branch_order),
      reverse_order = first(.data$reverse_order),
      diameter_base_cm = 2 * first(.data$radius) * 100,
      diameter_mid_cm = case_when(n() > 1 ~ .data$radius[ceiling(n() / 2)] * 100, TRUE ~ .data$radius * 2),
      diameter_tip_cm = last(.data$radius) * 2 * 100,
      volume_m3 = pi * sum(.data$radius^2 * .data$length),
      area_m2 = 2 * pi * sum(.data$length * .data$radius),
      length_m = sum(.data$length),
      height_m = first(.data$start_z) - !!base,
      azimuth_deg = 180 / pi * atan2(first(.data$axis_y), first(.data$axis_x)),
      zenith_deg = 180 / pi * acos(first(.data$axis_z)),
      growth_length = first(.data$growth_length),
      cylinders = n(),
      segments = length(unique(.data$segment)),
      children = sum(.data$total_children),
      base_distance_m = first(.data$base_distance),
      twig_distance_m = first(.data$twig_distance)
    ) %>%
    left_join(parent_axis, by = "parent") %>%
    ungroup() %>%
    group_by("branch") %>%
    mutate(
      dot_product = .data$axis_x * .data$p_axis_x + .data$axis_y * .data$p_axis_y + .data$axis_z * .data$p_axis_z,
      angle_deg = acos(.data$dot_product) * 180 / pi,
      angle_deg = if_else(is.na(.data$angle_deg), 0, .data$angle_deg),
      parent_branch = if_else(is.na(.data$parent_branch), 0, .data$parent_branch)
    ) %>%
    select(-c("parent", "p_axis_x", "p_axis_y", "p_axis_z", "axis_x", "axis_y", "axis_z", "dot_product")) %>%
    relocate("angle_deg", .after = "height_m") %>%
    relocate("parent_branch", .after = "branch")
}

#' Calculates alternate branch metrics
#' @param cylinder QSM cylinder data frame
#' @param base cylinder id of the stem base
#' @returns data frame with per branch summary metrics
#' @noRd
branch_alt_metrics <- function(cylinder, base) {
  # Get Parent Axis IDs
  parent_axis <- cylinder %>%
    select(
      parent = "id",
      parent_branch = "branch",
      p_axis_x = "axis_x",
      p_axis_y = "axis_y",
      p_axis_z = "axis_z"
    )

  # Base Cylinder
  base <- cylinder %>%
    filter(.data$id == !!base) %>%
    select(start_z = "start_z") %>%
    slice_head(1) %>%
    pull()

  # Calculate Alternate Branch Metrics
  cylinder %>%
    filter(!.data$branch_alt == 0) %>%
    group_by("branch_alt") %>%
    summarise(
      parent = first(.data$parent),
      axis_x = first(.data$axis_x),
      axis_y = first(.data$axis_y),
      axis_z = first(.data$axis_z),
      branch_order = first(.data$branch_order),
      reverse_order = first(.data$reverse_order),
      diameter_base_cm = 2 * first(.data$radius) * 100,
      volume_m3 = pi * sum(.data$radius^2 * .data$length),
      area_m2 = 2 * pi * sum(.data$length * .data$radius),
      x_length = max(.data$end_x) - min(.data$start_x),
      y_length = max(.data$end_y) - min(.data$start_y),
      z_length = max(.data$end_z) - min(.data$start_z),
      height_m = first(.data$start_z) - !!base,
      azimuth_deg = 180 / pi * atan2(first(.data$axis_y), first(.data$axis_x)),
      zenith_deg = 180 / pi * acos(first(.data$axis_z)),
      growth_length = first(.data$growth_length),
      cylinders = n(),
      segments = length(unique(.data$segment)),
      base_distance_m = first(.data$base_distance),
      twig_distance_m = first(.data$twig_distance)
    ) %>%
    left_join(parent_axis, by = "parent") %>%
    ungroup() %>%
    group_by("branch_alt") %>%
    mutate(
      length_m = abs(max(c(.data$x_length, .data$y_length, .data$z_length))),
      dot_product = .data$axis_x * .data$p_axis_x + .data$axis_y * .data$p_axis_y + .data$axis_z * .data$p_axis_z,
      angle_deg = acos(.data$dot_product) * 180 / pi,
      angle_deg = if_else(is.na(.data$angle_deg), 0, .data$angle_deg),
      parent_branch = if_else(is.na(.data$parent_branch), 0, .data$parent_branch)
    ) %>%
    select(
      -c(
        "parent", "dot_product",
        "p_axis_x", "p_axis_y", "p_axis_z", "axis_x", "axis_y", "axis_z",
        "x_length", "y_length", "z_length"
      )
    ) %>%
    relocate("angle_deg", .after = "height_m") %>%
    relocate("parent_branch", .after = "branch_alt") %>%
    relocate("length_m", .after = "area_m2")
}

#' Calculates segment metrics
#' @param cylinder QSM cylinder data frame
#' @param base cylinder id of the stem base
#' @returns data frame with segment summary metrics
#' @noRd
segment_metrics <- function(cylinder, base) {
  # Get Parent Axis IDs
  parent_axis <- cylinder %>%
    select(
      parent = "id",
      parent_segment = "segment",
      p_axis_x = "axis_x",
      p_axis_y = "axis_y",
      p_axis_z = "axis_z"
    )

  # Base Cylinder
  base <- cylinder %>%
    filter(.data$id == !!base) %>%
    select(start_z = "start_z") %>%
    slice_head(1) %>%
    pull()

  # Calculate Segment Metrics
  cylinder %>%
    group_by("segment") %>%
    summarise(
      parent = first(.data$parent),
      axis_x = first(.data$axis_x),
      axis_y = first(.data$axis_y),
      axis_z = first(.data$axis_z),
      branch_order = first(.data$branch_order),
      reverse_order = first(.data$reverse_order),
      diameter_base_cm = 2 * first(.data$radius) * 100,
      diameter_mid_cm = case_when(n() > 1 ~ .data$radius[ceiling(n() / 2)] * 100, TRUE ~ .data$radius * 2),
      diameter_tip_cm = last(.data$radius) * 2 * 100,
      volume_m3 = pi * sum(.data$radius^2 * .data$length),
      area_m2 = 2 * pi * sum(.data$length * .data$radius),
      length_m = sum(.data$length),
      height_m = first(.data$start_z) - !!base,
      azimuth_deg = 180 / pi * atan2(first(.data$axis_y), first(.data$axis_x)),
      zenith_deg = 180 / pi * acos(first(.data$axis_z)),
      cylinders = n(),
      children = sum(.data$total_children),
      base_distance_m = first(.data$base_distance),
      twig_distance_m = first(.data$twig_distance)
    ) %>%
    left_join(parent_axis, by = "parent") %>%
    ungroup() %>%
    group_by("segment") %>%
    mutate(
      dot_product = .data$axis_x * .data$p_axis_x + .data$axis_y * .data$p_axis_y + .data$axis_z * .data$p_axis_z,
      angle_deg = acos(.data$dot_product) * 180 / pi,
      angle_deg = if_else(is.na(.data$angle_deg), 0, .data$angle_deg),
      parent_segment = if_else(is.na(.data$parent_segment), 0, .data$parent_segment)
    ) %>%
    select(-c("parent", "p_axis_x", "p_axis_y", "p_axis_z", "axis_x", "axis_y", "axis_z", "dot_product")) %>%
    relocate("angle_deg", .after = "height_m") %>%
    relocate("parent_segment", .after = "segment")
}

#' Calculates horizontal crown spreads across different height classes
#' @param cloud simulated point cloud matrix
#' @returns matrix of crown spreads
#' @noRd
calculate_spreads <- function(cloud) {
  bottom <- min(cloud[, 3])
  top <- max(cloud[, 3])
  height <- top - bottom
  spreads <- matrix(0, nrow = 20, ncol = 18)

  for (j in 1:20) {
    I <- cloud[, 3] >= bottom + (j - 1) * height / 20 & cloud[, 3] < bottom + j * height / 20
    if (any(I)) {
      cloud_sub <- unique(cloud[I, , drop = FALSE])
      cloud_sub <- cloud_sub[order(cloud_sub[, 1]), ]

      if (!is.matrix(cloud_sub)) {
        cloud_sub <- matrix(cloud_sub, ncol = 3)
      }

      if (nrow(cloud_sub) > 2) {
        hull_indexes <- convex_hull(cloud_sub[, 1:2]) + 1
        hull_area <- convex_hull_area(cloud_sub[, 1:2])
        n <- length(hull_indexes)
        x <- cloud_sub[hull_indexes, 1]
        y <- cloud_sub[hull_indexes, 2]
        cx <- sum((x[1:(n - 1)] + x[2:n]) * (x[1:(n - 1)] * y[2:n] - x[2:n] * y[1:n - 1])) / 6 / hull_area
        cy <- sum((y[1:(n - 1)] + y[2:n]) * (x[1:(n - 1)] * y[2:n] - x[2:n] * y[1:n - 1])) / 6 / hull_area
        V <- mat_vec_subtraction(cloud_sub[, 1:2], c(cx, cy))
        angle <- atan2(V[, 2], V[, 1]) + pi
        order <- order(angle)
        angle <- angle[order]
        length <- sqrt(rowSums(V^2))
        length <- length[order]

        for (i in 1:18) {
          I <- angle >= (i - 1) * pi / 18 & angle < i * pi / 18
          length1 <- if (any(I)) max(length[I]) else 0
          J <- angle >= (i - 1) * pi / 18 + pi & angle < i * pi / 18 + pi
          length2 <- if (any(J)) max(length[J]) else 0
          spreads[j, i] <- length1 + length2
        }
      }
    }
  }

  return(spreads)
}

#' Calculates crown diameters from the simulated point cloud
#' @param cloud simulated point cloud matrix
#' @param tips matrix of cylinder end points
#' @returns vector of crown spreads every 20 degrees
#' @noRd
crown_diameters <- function(cloud, tips) {
  cloud_sub <- unique(cloud[, 1:2])
  hull_indexes <- convex_hull(cloud_sub) + 1
  hull_area <- convex_hull_area(cloud_sub)
  n <- length(hull_indexes)
  x <- cloud_sub[hull_indexes, 1]
  y <- cloud_sub[hull_indexes, 2]
  cx <- sum((x[1:(n - 1)] + x[2:n]) * (x[1:(n - 1)] * y[2:n] - x[2:n] * y[1:(n - 1)])) / (6 * hull_area)
  cy <- sum((y[1:(n - 1)] + y[2:n]) * (x[1:(n - 1)] * y[2:n] - x[2:n] * y[1:(n - 1)])) / (6 * hull_area)

  V <- mat_vec_subtraction(tips[, 1:2], c(cx, cy))
  angle <- atan2(V[, 2], V[, 1]) + pi
  order <- order(angle)
  angle <- angle[order]
  length <- sqrt(rowSums(V^2))
  length <- length[order]
  spreads <- numeric(18)

  for (i in 1:18) {
    I <- angle >= (i - 1) * pi / 18 & angle < i * pi / 18
    length1 <- if (any(I)) max(length[I]) else 0
    J <- angle >= (i - 1) * pi / 18 + pi & angle < i * pi / 18 + pi
    length2 <- if (any(J)) max(length[J]) else 0
    spreads[i] <- length1 + length2
  }

  return(spreads)
}

#' Calculate the start of the crown
#'
#' Define first major branch as the branch whose diameter > min(0.05*dbh,5cm)
#' and whose horizontal relative reach is more than the median reach of 1st-ord.
#' branches (or at maximum 10). The reach is defined as the horizontal
#' distance from the base to the tip divided by the dbh.
#'
#' @param cylinder QSM cylinder data frame
#' @param branch branch metrics data frame
#' @param base cylinder id of the stem base
#' @param dbh diameter at breast height of the tree
#' @param d_base diameter at the base of the tree
#' @param start matrix of cylinder start points
#' @param axis matrix of cylinder axes
#' @param tips matrix of cylinder end points
#' @returns double of the base height of the tree
#' @noRd
crown_base_height <- function(
    cylinder,
    branch,
    base,
    dbh,
    d_base,
    start,
    axis,
    tips) {
  if (length(unique(cylinder$branch)) == 1) {
    message <- paste(
      "No branches are present!",
      "Crown base height cannot be calculated!",
      sep = " "
    )
    warn(message)

    return(NA)
  }

  if (is.na(dbh)) {
    message <- paste(
      "DBH is missing!",
      "Crown base height will be calculated with base diameter!",
      sep = " "
    )
    warn(message)

    dbh <- d_base
  }

  if (base != 1) {
    message <- paste(
      "Branch detected!",
      "Crown base height will use the first bifurcation!",
      sep = " "
    )
    warn(message)

    # Get base start z value
    base_z <- pull(filter(cylinder, .data$id == !!base), "start_z")
    f_branch_z <- cylinder %>%
      filter(.data$branch_order == 1 & .data$branch_position == 1) %>%
      arrange("start_z") %>%
      pull("start_z")

    base_height <- f_branch_z - base_z

    return(base_height)
  }

  dbh <- dbh / 100
  nb <- nrow(filter(branch, .data$branch_order == 1))
  if (nb > 1) {
    nc <- nrow(start)
    index1 <- 1:nc
    HL <- rep(0, nb)
    NC <- rep(0, nb)
    for (i in 2:nb) {
      C <- index1[cylinder$branch == i]
      base <- start[C[1], ]
      C <- C[length(C)]
      tip <- start[C, ] + cylinder$length[C] * axis[C]
      V <- tip[1:2] - base[1:2]
      HL[i] <- sqrt(sum(V * V)) / dbh * 2
      NC[i] <- sum(branch$parent_branch == i)
    }
    M <- min(10, stats::median(HL, na.rm = TRUE))

    # Sort the branches according to their heights
    height <- branch$height_m[1:nb]
    order <- order(height)

    d <- min(0.05, 0.05 * dbh)
    i <- 1
    while (i < nb) {
      i <- i + 1
      if (!is.na(HL[i]) && !is.na(HL[order[i]])) {
        if (branch$diameter_base_cm[order[i]] / 100 > d && HL[order[i]] > M) {
          b <- order[i]
          i <- nb + 2
        }
      }
    }
    if (i == nb && nb > 1) {
      b <- order[1]
    }
  } else {
    nc <- nrow(start)
    index1 <- 1:nc
    b <- pull(filter(branch, .data$branch_order == 1), "branch")
  }

  # Search all the children of the first major branch
  nb <- length(branch$parent_branch)
  index2 <- 1:nb
  child <- index2[branch$parent_branch == b]
  B <- b
  while (length(child) > 0) {
    B <- c(B, child)
    n <- length(child)
    C <- vector("list", n)
    for (i in 1:n) {
      C[[i]] <- index2[branch$parent_branch == child[i]]
    }
    child <- unique(unlist(C))
    child <- setdiff(child, B)
  }

  # Define crown base height from the ground
  base_height <- max(start[, 3])
  for (i in 1:length(B)) {
    C <- index1[cylinder$branch == B[i]]
    if (length(C) > 0) {
      ht <- min(tips[C, 3])
      hb <- min(start[C, 3])
      h <- min(hb, ht)
      if (h < base_height) {
        base_height <- h
      }
    }
  }

  base_height <- base_height - start[1, 3]

  return(base_height)
}

#' Calculate tree distribution metrics
#'
#' Tree volume, length, and area by diameter, zenith, and azimuth classes
#' Height: 1 meter classes
#' Diameter: 1 cm classes
#' Zenith: 10 degree classes
#' Azimuth: 10 degree classes
#'
#' @param cylinder QSM cylinder data frame
#' @param distribution string: height, diameter, zenith, or azimuth
#' @returns data frame of tree metrics by distribution type
#' @noRd
tree_distributions <- function(cylinder, distribution) {
  if (distribution == "height") {
    # Convert the tree height to the ceiling value
    max_height <- ceiling(max(cylinder$start_z) - min(cylinder$start_z))

    # Calculate bot, B, and T
    bot <- min(cylinder$start_z)
    B <- cylinder$start_z - bot
    T <- cylinder$end_z - bot

    # Data frame to store results
    results <- tidytable(
      height_class_m = 1:max_height,
      volume_m3 = na_dbl,
      area_m2 = na_dbl,
      length_m = na_dbl
    )

    # Loop through each height
    for (j in 1:max_height) {
      I1 <- B >= (j - 2) & B < (j - 1) # base below this bin
      J1 <- B >= (j - 1) & B < j # base in this bin
      K1 <- B >= j & B < (j + 1) # base above this bin
      I2 <- T >= (j - 2) & T < (j - 1) # top below this bin
      J2 <- T >= (j - 1) & T < j # top in this bin
      K2 <- T >= j & T < (j + 1) # top above this bin

      C1 <- which(J1 & J2) # base and top in this bin
      C2 <- which(J1 & K2) # base in this bin, top above
      C3 <- which(J1 & I2) # base in this bin, top below
      C4 <- which(I1 & J2) # base in bin below, top in this
      C5 <- which(K1 & J2) # base in bin above, top in this

      v1 <- pi * sum(cylinder$radius[C1]^2 * cylinder$length[C1])
      a1 <- 2 * pi * sum(cylinder$radius[C1] * cylinder$length[C1])
      l1 <- sum(cylinder$length[C1])

      r2 <- (j - B[C2]) / (T[C2] - B[C2]) # relative portion in this bin
      v2 <- pi * sum(cylinder$radius[C2]^2 * cylinder$length[C2] * r2)
      a2 <- 2 * pi * sum(cylinder$radius[C2] * cylinder$length[C2] * r2)
      l2 <- sum(cylinder$length[C2] * r2)

      r3 <- (B[C3] - j + 1) / (B[C3] - T[C3]) # relative portion in this bin
      v3 <- pi * sum(cylinder$radius[C3]^2 * cylinder$length[C3] * r3)
      a3 <- 2 * pi * sum(cylinder$radius[C3] * cylinder$length[C3] * r3)
      l3 <- sum(cylinder$length[C3] * r3)

      r4 <- (T[C4] - j + 1) / (T[C4] - B[C4]) # relative portion in this bin
      v4 <- pi * sum(cylinder$radius[C4]^2 * cylinder$length[C4] * r4)
      a4 <- 2 * pi * sum(cylinder$radius[C4] * cylinder$length[C4] * r4)
      l4 <- sum(cylinder$length[C4] * r4)

      r5 <- (j - T[C5]) / (B[C5] - T[C5]) # relative portion in this bin
      v5 <- pi * sum(cylinder$radius[C5]^2 * cylinder$length[C5] * r5)
      a5 <- 2 * pi * sum(cylinder$radius[C5] * cylinder$length[C5] * r5)
      l5 <- sum(cylinder$length[C5] * r5)

      results[j, 2] <- v1 + v2 + v3 + v4 + v5
      results[j, 3] <- a1 + a2 + a3 + a4 + a5
      results[j, 4] <- l1 + l2 + l3 + l4 + l5
    }
  } else {
    if (distribution == "diameter") {
      Par <- cylinder$radius
      n <- ceiling(max(200 * cylinder$radius))
      a <- 0.005
      name <- "diameter_class_cm"
      index <- 1:n
    } else if (distribution == "zenith") {
      Par <- 180 / pi * acos(cylinder$axis_z)
      n <- 18
      a <- 10
      name <- "zenith_class_deg"
      index <- seq(10, 180, by = 10)
    } else if (distribution == "azimuth") {
      Par <- 180 / pi * atan2(cylinder$axis_y, cylinder$axis_x) + 180
      n <- 36
      a <- 10
      name <- "azimuth_class_deg"
      index <- seq(10, 360, by = 10)
    }

    # Calculate volume, area, and length for each class
    results <- tidytable(Par, radius = cylinder$radius, length = cylinder$length) %>%
      mutate(
        class = cut(
          .data$Par,
          breaks = seq(0, n * a, by = a),
          labels = !!index, include.lowest = TRUE
        ),
        class = factor(class, levels = index)
      ) %>%
      group_by("class") %>%
      summarise(
        volume_m3 = pi * sum(.data$radius^2 * .data$length, na.rm = TRUE),
        area_m2 = 2 * pi * sum(.data$radius * .data$length, na.rm = TRUE),
        length_m = sum(.data$length, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      complete(
        class = factor(index, levels = index),
        fill = list(volume_m3 = 0, area_m2 = 0, length_m = 0)
      ) %>%
      mutate(class = as.numeric(as.character(.data$class))) %>%
      arrange(.data$class) %>%
      rename(!!name := 1)
  }

  return(results)
}

#' Calculate branch distribution metrics
#'
#' Branch diameter, height, angle, zenith and azimuth distributions
#' Diameter: 1 cm classes
#' Height: 1 meter classes
#' Angle: 10 degree classes
#' Zenith: 10 degree classes
#' Azimuth: 10 degree classes
#'
#' @param branch branch summary data frame
#' @param tree tree summary data frame
#' @param distribution string: height, angle, diameter, zenith, or azimuth
#' @returns data frame of branch metrics by distribution type
#' @noRd
branch_distributions <- function(branch, tree, distribution) {
  if (distribution == "diameter") {
    Par <- branch$diameter_base_cm[-1] / 100
    n <- ceiling(max(100 * Par))
    a <- 0.005
    index <- seq(1, n, by = 1)
    name <- "diameter_class_cm"
  } else if (distribution == "height") {
    Par <- branch$height_m[-1]
    n <- ceiling(tree$tree_height_m)
    a <- 1
    index <- seq(1, n, by = 1)
    name <- "height_class_m"
  } else if (distribution == "angle") {
    Par <- branch$angle_deg[-1]
    n <- 18
    a <- 10
    index <- seq(10, 180, by = 10)
    name <- "angle_class_deg"
  } else if (distribution == "zenith") {
    Par <- branch$zenith_deg[-1]
    n <- 18
    a <- 10
    index <- seq(10, 180, by = 10)
    name <- "zenith_class_deg"
  } else if (distribution == "azimuth") {
    Par <- branch$azimuth_deg[-1] + 180
    n <- 36
    a <- 10
    index <- seq(10, 360, by = 10)
    name <- "azimuth_class_deg"
  }

  if (is.null(n)) {
    n <- 0
  }

  # Data frame to store results
  results <- tidytable(
    !!name := index,
    volume_m3 = na_dbl,
    area_m2 = na_dbl,
    length_m = na_dbl,
    branches = na_int,
    volume_1_m3 = na_dbl,
    area_1_m2 = na_dbl,
    length_1_m = na_dbl,
    branches_1 = na_int
  )

  for (i in 1:n) {
    I <- Par >= (i - 1) * a & Par < i * a
    results[i, 2] <- sum(branch$volume_m3[-1][I]) # volume (all branches)
    results[i, 3] <- sum(branch$area_m2[-1][I]) # area (all branches)
    results[i, 4] <- sum(branch$length_m[-1][I]) # length (all branches)
    results[i, 5] <- sum(I) # number (all branches)
    results[i, 6] <- sum(branch$volume_m3[-1][I & branch$branch_order[-1] == 1]) # volume (1st-branches)
    results[i, 7] <- sum(branch$area_m2[-1][I & branch$branch_order[-1] == 1]) # area (1st-branches)
    results[i, 8] <- sum(branch$length_m[-1][I & branch$branch_order[-1] == 1]) # length (1st-branches)
    results[i, 9] <- sum(I & branch$branch_order[-1] == 1) # number (1st-branches)
  }

  return(results)
}

#' Calculate segment distribution metrics
#'
#' Segment diameter, height, angle, zenith and azimuth distributions
#' Diameter: 1 cm classes
#' Height: 1 meter classes
#' Angle: 10 degree classes
#' Zenith: 10 degree classes
#' Azimuth: 10 degree classes
#'
#' @param segment segment summary data frame
#' @param tree tree summary data frame
#' @param distribution string: height, angle, diameter, zenith, or azimuth
#' @returns data frame of segment metrics by distribution type
#' @noRd
segment_distributions <- function(segment, tree, distribution) {
  if (distribution == "diameter") {
    Par <- segment$diameter_base_cm[-1] / 100
    n <- ceiling(max(100 * Par))
    a <- 0.005
    index <- seq(1, n, by = 1)
    name <- "diameter_class_cm"
  } else if (distribution == "height") {
    Par <- segment$height_m[-1]
    n <- ceiling(tree$tree_height_m)
    a <- 1
    index <- seq(1, n, by = 1)
    name <- "height_class_m"
  } else if (distribution == "angle") {
    Par <- segment$angle_deg[-1]
    n <- 18
    a <- 10
    index <- seq(10, 180, by = 10)
    name <- "angle_class_deg"
  } else if (distribution == "zenith") {
    Par <- segment$zenith_deg[-1]
    n <- 18
    a <- 10
    index <- seq(10, 180, by = 10)
    name <- "zenith_class_deg"
  } else if (distribution == "azimuth") {
    Par <- segment$azimuth_deg[-1] + 180
    n <- 36
    a <- 10
    index <- seq(10, 360, by = 10)
    name <- "azimuth_class_deg"
  }

  if (is.null(n)) {
    n <- 0
  }

  # Data frame to store results
  results <- tidytable(
    !!name := index,
    volume_m3 = na_dbl,
    area_m2 = na_dbl,
    length_m = na_dbl,
    segments = na_int,
  )

  for (i in 1:n) {
    I <- Par >= (i - 1) * a & Par < i * a
    results[i, 2] <- sum(segment$volume_m3[-1][I]) # volume (all segments)
    results[i, 3] <- sum(segment$area_m2[-1][I]) # area (all segments)
    results[i, 4] <- sum(segment$length_m[-1][I]) # length (all segments)
    results[i, 5] <- sum(I) # number (all segments)
  }

  return(results)
}

#' Calculate branch order metrics
#'
#' @param branch branch metrics data frame
#' @returns data frame of branch metrics by branch order
#' @noRd
branch_order_distributions <- function(branch) {
  branch %>%
    filter(!.data$branch_order == 0) %>%
    group_by("branch_order") %>%
    summarise(
      branches = n(),
      volume_m3 = sum(.data$volume_m3),
      area_m2 = sum(.data$area_m2),
      length_m = sum(.data$length_m)
    )
}

#' Calculate reverse branch order metrics
#'
#' @param segment segment metrics data frame
#' @returns data frame of segment metrics by reverse branch order
#' @noRd
segment_order_distributions <- function(segment) {
  segment %>%
    group_by("reverse_order") %>%
    summarise(
      segments = n(),
      volume_m3 = sum(.data$volume_m3),
      area_m2 = sum(.data$area_m2),
      length_m = sum(.data$length_m)
    )
}
