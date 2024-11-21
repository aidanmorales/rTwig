#' Correct Radii
#'
#' @description Corrects cylinder radii
#'
#' @param cylinder QSM cylinder data frame
#' @param twig_radius Twig radius in millimeters
#' @param broken_branch Enable or disable the broken branch filter. Defaults to enabled (TRUE).
#'
#' @return Returns a data frame
#' @export
#'
#' @examples
#'
#' ## TreeQSM Processing Chain
#' file <- system.file("extdata/QSM.mat", package = "rTwig")
#' qsm <- import_qsm(file)
#' cylinder <- qsm$cylinder
#' cylinder <- update_cylinders(cylinder)
#' cylinder <- correct_radii(cylinder, twig_radius = 4.23)
#' str(cylinder)
#'
correct_radii <- function(
    cylinder,
    twig_radius,
    broken_branch = TRUE) {
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

  if (is_missing(twig_radius)) {
    message <- "argument `twig_radius` is missing, with no default."
    abort(message, class = "missing_argument")
  }

  if (!is_null(twig_radius)) {
    if (!is_scalar_double(twig_radius)) {
      message <- paste0(
        "`twig_radius` must be double, not ", class(twig_radius), "."
      )
      abort(message, class = "invalid_argument")
    }
  }

  if (!is_logical(broken_branch)) {
    message <- paste0(
      "`broken_branch` must be logical, not ", class(broken_branch), "."
    )
    abort(message, class = "invalid_argument")
  }

  # Verify cylinders
  cylinder <- verify_cylinders(cylinder)

  # Converts twig radius to meters
  twig_radius <- twig_radius / 1000

  # Gets broken branch filter
  broken_branch <- broken_branch

  # rTwig ----------------------------------------------------------------------
  if (all(c("id", "parent", "start_x", "branch_order") %in% colnames(cylinder))) {
    model_paths(
      cylinder = cylinder, id = "id", parent = "parent",
      radius = "radius", raw_radius = "raw_radius",
      growth_length = "growth_length", branch = "branch",
      branch_order = "branch_order", reverse_order = "reverse_order",
      branch_position = "branch_position",
      total_children = "total_children",
      twig_radius = twig_radius, taper = 1, broken_branch = broken_branch
    )
  }
  # TreeQSM --------------------------------------------------------------------
  else if (all(c("parent", "extension", "branch", "BranchOrder") %in% colnames(cylinder))) {
    model_paths(
      cylinder = cylinder, id = "extension", parent = "parent",
      radius = "radius", raw_radius = "UnmodRadius",
      growth_length = "growthLength", branch = "branch",
      branch_order = "BranchOrder", reverse_order = "reverseBranchOrder",
      branch_position = "PositionInBranch",
      total_children = "totalChildren",
      twig_radius = twig_radius, taper = 1, broken_branch = broken_branch
    )
  }
  # SimpleForest  --------------------------------------------------------------
  else if (all(c("ID", "parentID", "branchID", "branchOrder") %in% colnames(cylinder))) {
    model_paths(
      cylinder = cylinder, id = "ID", parent = "parentID",
      radius = "radius", raw_radius = "UnmodRadius",
      growth_length = "growthLength", branch = "branchID",
      branch_order = "branchOrder", reverse_order = "reverseBranchOrder",
      branch_position = "positionInBranch",
      total_children = "totalChildren",
      twig_radius = twig_radius, taper = 2, broken_branch = broken_branch
    )
  }
  # Treegraph ------------------------------------------------------------------
  else if (all(c("p1", "p2", "ninternode") %in% colnames(cylinder))) {
    model_paths(
      cylinder = cylinder, id = "p1", parent = "p2",
      radius = "radius", raw_radius = "UnmodRadius",
      growth_length = "growthLength", branch = "nbranch",
      branch_order = "branch_order", reverse_order = "reverseBranchOrder",
      branch_position = "positionInBranch",
      total_children = "totalChildren",
      twig_radius = twig_radius, taper = 2, broken_branch = broken_branch
    )
  }
  # aRchi ----------------------------------------------------------------------
  else if (all(c("cyl_ID", "parent_ID", "branching_order") %in% colnames(cylinder))) {
    model_paths(
      cylinder = cylinder, id = "cyl_ID", parent = "parent_ID",
      radius = "radius_cyl", raw_radius = "UnmodRadius",
      growth_length = "growthLength", branch = "branch_ID",
      branch_order = "branching_order", reverse_order = "reverseBranchOrder",
      branch_position = "positionInBranch",
      total_children = "totalChildren",
      twig_radius = twig_radius, taper = 2, broken_branch = broken_branch
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

#' Model paths
#' @param cylinder QSM cylinder data frame
#' @param id column name of cylinder ids
#' @param parent column name of parent cylinders
#' @param radius column name of the cylinder radii
#' @param raw_radius column name of the raw cylinder radii
#' @param growth_length column name of growth length
#' @param branch column name of the branches
#' @param branch_order column name of the branch orders
#' @param reverse_order column name of the reverse branch orders
#' @param branch_position column name of the branch position
#' @param total_children column name of total children
#' @param twig_radius twig radius in meters
#' @param taper taper method
#' @param broken_branch broken branch filter
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
    reverse_order,
    branch_position,
    total_children,
    twig_radius,
    taper = 1,
    broken_branch = TRUE) {
  # Build Paths ----------------------------------------------------------------
  inform("Generating Paths")

  paths <- combine_paths(
    cylinder = cylinder,
    id = id,
    parent = parent,
    radius = raw_radius,
    branch = branch,
    branch_order = branch_order,
    reverse_order = reverse_order,
    branch_position = branch_position,
    growth_length = growth_length,
    total_children = total_children
  )

  # Filter Cylinder Outliers ---------------------------------------------------
  inform("Filtering Paths")

  stem <- paths[paths$branch == 1, ]
  stem <- min(which(stem$total_children > 1))

  paths_filtered <- paths %>%
    mutate(
      index0 = .data$radius / .data$growth_length / (.data$branch_order + 1),
      index1 = log(.data$growth_length) / .data$radius^2,
      index2 = .data$radius^2 / log(.data$growth_length)
    ) %>%
    group_by("path") %>%
    mutate( # general cylinder pass
      index = iqr_filter(.data$index0, 1)
    ) %>%
    filter(.data$index == 0) %>%
    group_by("path", "branch_order") %>%
    mutate( # general cylinder pass
      index = iqr_filter(.data$index1, 1)
    ) %>%
    filter(.data$index == 0) %>%
    mutate( # general cylinder pass
      index = iqr_filter(.data$index2, 2)
    ) %>%
    filter(.data$index == 0) %>%
    ungroup() %>%
    group_by("path") %>%
    mutate(index = taper_filter(.data$radius, !!taper)) %>%
    select("path", "id", "index") %>%
    ungroup() %>%
    filter(.data$index == 0)

  paths <- paths %>%
    left_join(paths_filtered, by = c("path", "id")) %>%
    mutate(
      index = if_else(is.na(.data$index), 1, .data$index),
      index = if_else(.data$branch == 1 & .data$branch_position <= !!stem, 0, .data$index)
    )

  # Model Paths ----------------------------------------------------------------
  inform("Modeling Paths")
  paths <- paths %>%
    group_by("path") %>%
    summarise( # Broken branch filter and required variables
      data = list(broken_branch_filter(pick(), twig_radius = !!twig_radius, !!broken_branch)),
      growth_length = list(.data$growth_length),
      raw_radius = list(.data$radius),
      id = list(.data$id),
      index = list(.data$index),
      branch_order = list(.data$branch_order)
    ) %>%
    group_by("path") %>%
    mutate( # Matrix to constrain GAM with twig diameter
      matrix = list(model_matrix(.data$data[[1]]$min_gl, twig_radius = !!twig_radius))
    ) %>%
    summarise( # Model paths with gam
      radius = list(
        path_gam(
          x = .data$data[[1]]$x,
          y = .data$data[[1]]$y,
          growth_length = .data$growth_length[[1]],
          twig_radius = !!twig_radius,
          matrix = .data$matrix[[1]]
        )
      ),
      id = .data$id,
      index = .data$index,
      raw_radius = .data$raw_radius,
      branch_order = .data$branch_order,
      growth_length = .data$growth_length,
      max_rad_ord = .data$data[[1]]$max_rad_ord,
      min_rad = .data$data[[1]]$min_rad,
    )

  # Diagnostic Graph -----------------------------------------------------------
  # paths %>%
  #   filter(path == path) %>%
  #   unnest() %>%
  #   ggplot() +
  #   geom_line(
  #     aes(y = radius, x = growth_length), linewidth = 0.5, color = "black"
  #   ) +
  #   geom_point(aes(
  #     x = growth_length,
  #     y = raw_radius,
  #     color = as.factor(branch_order),
  #     shape = as.factor(index)
  #   )) +
  #   labs(
  #     x = "Growth Length (m)",
  #     y = "Radius (m)",
  #     title = "Real Twig Path Correction",
  #     color = "Branch Order",
  #     shape = "Cylinder Fit"
  #   ) +
  #   theme_classic()

  # Update Radii ---------------------------------------------------------------
  inform("Updating Radii")

  # Combines all paths and calculates weighted mean for each cylinder
  cyl_radii <- unnest(paths) %>%
    group_by("path") %>%
    summarise( # Remove tapering on good main stem cylinders and broken branches
      radius = case_when(
        .data$index == 0 & .data$branch_order == 0 ~ .data$raw_radius,
        TRUE ~ .data$radius,
        .data$radius < min_rad & !(max_rad_ord == 0) ~ min_rad,
        TRUE ~ .data$radius
      ),
      id = .data$id,
      index = .data$index
    ) %>%
    group_by("id") %>%
    summarise( # Weighted radius mean across paths and modified index
      radius = stats::weighted.mean(w = .data$radius, .data$radius, na.rm = TRUE),
      modified = mean(.data$index)
    ) %>%
    rename(
      {{ id }} := "id",
      {{ radius }} := "radius"
    )

  # Updates the QSM cylinder data with new radii
  cylinder %>%
    rename("radius" := {{ radius }}) %>%
    select(-"radius") %>%
    left_join(cyl_radii, by = id)
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
  # Extract required variables
  path_vars <- cylinder %>%
    select(
      id = {{ id }},
      parent = {{ parent }},
      radius = {{ radius }},
      branch = {{ branch }},
      branch_order = {{ branch_order }},
      reverse_order = {{ reverse_order }},
      branch_position = {{ branch_position }},
      growth_length = {{ growth_length }},
      total_children = {{ total_children }}
    )

  # Verify Network
  paths <- verify_network(path_vars, paths = TRUE)

  # Combine all path data
  left_join(paths, path_vars) %>%
    drop_na() %>%
    rename(path = "index")
}

#' Model paths with a GAM
#' @param x vector of good fit cylinder growth lengths
#' @param y vector of good fit cylinder radii
#' @param growth_length cylinder growth length vector of entire path
#' @param twig_radius twig radius in meters
#' @param matrix 1 x 3 matrix to constrain the GAM in form [0, min(growth_length), twig_radius (m)]
#'
#' @returns cylinder data frame
#' @noRd
path_gam <- function(x, y, growth_length, twig_radius, matrix) {
  if (length(x) > 3) {
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
    stats::predict(model, growth_length)[, 2]
  } else {
    rep(twig_radius, length(growth_length))
  }
}
