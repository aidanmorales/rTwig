#' Prune QSM
#'
#' @description Efficiently prune a QSM. The user can prune by cylinder, branch,
#' and segment ids, or by height or diameter classes, individually, or all at
#' the same time, and return either the pruned data, the remaining data, or a
#' binary index of which cylinders are pruned.
#'
#' @param cylinder QSM cylinder data frame
#' @param cylinder_ids A single or vector of cylinder ids. Everything connected above the cylinder is pruned.
#' @param branch_ids A single or vector of branch ids. Everything connected to the branch is pruned.
#' @param segment_ids A single or vector of segment ids. Everything connected above the segment is pruned.
#' @param height_m Height class in meters below which all cylinders are pruned. Valid inputs are numeric to one decimal.
#' @param diameter_cm Branch diameter class in centimeters below which all cylinders are pruned. Valid inputs are numeric to one decimal.
#' @param invert Return the remaining or pruned data. Defaults to TRUE (the remaining data), but can be set to FALSE.
#' @param index Returns a column index called `pruning` indicating if the cylinder is pruned (1) or un-pruned (0). Defaults to FALSE, but can be set to TRUE.
#'
#' @return a data frame
#' @export
#'
#' @examples
#'
#' ## Load QSM
#' file <- system.file("extdata/QSM.mat", package = "rTwig")
#' qsm <- import_qsm(file)
#' cylinder <- qsm$cylinder
#' cylinder <- update_cylinders(cylinder)
#'
#' ## Pruning Index
#' prune1 <- prune_qsm(cylinder, height_m = 2, index = TRUE)
#' plot_qsm(prune1, color = pruning, palette = "blue2red")
#'
#' ## Remaining
#' prune2 <- prune_qsm(cylinder, height_m = 2, invert = FALSE)
#' plot_qsm(prune2)
#'
#' ## Pruned
#' prune3 <- prune_qsm(cylinder, height_m = 2, invert = TRUE)
#' plot_qsm(prune3)
#'
prune_qsm <- function(
    cylinder,
    cylinder_ids = NULL,
    branch_ids = NULL,
    segment_ids = NULL,
    height_m = NULL,
    diameter_cm = NULL,
    invert = FALSE,
    index = FALSE) {
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

  if (!missing(cylinder) & is.null(cylinder_ids) & is.null(branch_ids) &
    is.null(segment_ids) & is.null(height_m) & is.null(diameter_cm)) {
    abort(
      "Pruning parameters empty. Please provide at least one parameter.",
      class = "missing_argument"
    )
  }

  if (!is.null(cylinder_ids)) {
    if (!is_integerish(cylinder_ids)) {
      message <- paste0(
        "`cylinder_ids` must be an integer or integer vector, not ",
        class(cylinder_ids), "."
      )
      abort(message, class = "invalid_argument")
    }
  }

  if (!is.null(branch_ids)) {
    if (!is_integerish(branch_ids)) {
      message <- paste0(
        "`branch_ids` must be an integer or integer vector, not ",
        class(branch_ids), "."
      )
      abort(message, class = "invalid_argument")
    }
  }

  if (!is.null(segment_ids)) {
    if (!is_integerish(segment_ids)) {
      message <- paste0(
        "`segment_ids` must be an integer or integer vector, not ",
        class(segment_ids), "."
      )
      abort(message, class = "invalid_argument")
    }
  }

  if (!is.null(height_m)) {
    if (!is_double(height_m)) {
      message <- paste0(
        "`height_m` must be numeric, not ", class(height_m), "."
      )
      abort(message, class = "invalid_argument")
    }
  }

  if (!is.null(diameter_cm)) {
    if (!is_double(diameter_cm)) {
      message <- paste0(
        "`diameter_cm` must be numeric, not ", class(diameter_cm), "."
      )
      abort(message, class = "invalid_argument")
    }
  }

  if (!is_logical(invert)) {
    message <- paste0(
      "`invert` must be logical, not ", class(invert), "."
    )
    abort(message, class = "invalid_argument")
  }

  if (!is_logical(index)) {
    message <- paste0(
      "`index` must be logical, not ", class(index), "."
    )
    abort(message, class = "invalid_argument")
  }

  # Verify cylinders
  cylinder <- verify_cylinders(cylinder)

  # rTwig ----------------------------------------------------------------------
  if (all(c("id", "parent", "start_x", "branch_order") %in% colnames(cylinder))) {
    prune_data(
      cylinder = cylinder, id = "id", parent = "parent",
      segment = "segment", branch = "branch", branch_position = "branch_position",
      start_z = "start_z", radius = "radius",
      cylinder_ids = cylinder_ids, branch_ids = branch_ids,
      segment_ids = segment_ids, height_m = height_m, diameter_cm = diameter_cm,
      invert = invert, index = index
    )
  }
  # TreeQSM --------------------------------------------------------------------
  else if (all(c("parent", "extension", "branch", "BranchOrder") %in% colnames(cylinder))) {
    prune_data(
      cylinder = cylinder, id = "extension", parent = "parent",
      segment = "segment", branch = "branch", branch_position = "PositionInBranch",
      start_z = "start.z", radius = "radius",
      cylinder_ids = cylinder_ids, branch_ids = branch_ids,
      segment_ids = segment_ids, height_m = height_m, diameter_cm = diameter_cm,
      invert = invert, index = index
    )
  }
  # SimpleForest ---------------------------------------------------------------
  else if (all(c("ID", "parentID", "branchID", "branchOrder") %in% colnames(cylinder))) {
    prune_data(
      cylinder = cylinder, id = "ID", parent = "parentID",
      segment = "segmentID", branch = "branchID", branch_position = "positionInBranch",
      start_z = "startZ", radius = "radius",
      cylinder_ids = cylinder_ids, branch_ids = branch_ids,
      segment_ids = segment_ids, height_m = height_m, diameter_cm = diameter_cm,
      invert = invert, index = index
    )
  }
  # Treegraph ------------------------------------------------------------------
  else if (all(c("p1", "p2", "ninternode") %in% colnames(cylinder))) {
    prune_data(
      cylinder = cylinder, id = "p1", parent = "p2",
      segment = "segment", branch = "nbranch", branch_position = "positionInBranch",
      start_z = "sz", radius = "radius",
      cylinder_ids = cylinder_ids, branch_ids = branch_ids,
      segment_ids = segment_ids, height_m = height_m, diameter_cm = diameter_cm,
      invert = invert, index = index
    )
  }
  # aRchi ----------------------------------------------------------------------
  else if (all(c("cyl_ID", "parent_ID", "branching_order") %in% colnames(cylinder))) {
    prune_data(
      cylinder = cylinder, id = "cyl_ID", parent = "parent_ID",
      segment = "segment_ID", branch = "branch_ID", branch_position = "positionInBranch",
      start_z = "startZ", radius = "radius_cyl",
      cylinder_ids = cylinder_ids, branch_ids = branch_ids,
      segment_ids = segment_ids, height_m = height_m, diameter_cm = diameter_cm,
      invert = invert, index = index
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

#' Prune data
#' @param cylinderQSM cylinder data frame
#' @param id cylinder id
#' @param parent parent id
#' @param segment segment id
#' @param branch branch id
#' @param branch_position cylinder branch position
#' @param start_z cylinder start z
#' @param radius cylinder radii
#' @param cylinder_ids a vector of cylinder ids to prune
#' @param branch_ids a vector of branch ids to prune
#' @param segment_ids a vector of segment ids to prune
#' @param height_m a double
#' @param diameter_cm a double
#' @param invert a boolean
#' @param index a boolean
#' @return a data frame
#' @noRd
prune_data <- function(
    cylinder,
    id,
    parent,
    segment,
    branch,
    branch_position,
    start_z,
    radius,
    cylinder_ids,
    branch_ids,
    segment_ids,
    height_m,
    diameter_cm,
    invert,
    index) {
  # Select required variables --------------------------------------------------
  cyl_sub <- cylinder %>%
    select(
      id = {{ id }},
      parent = {{ parent }},
      segment = {{ segment }},
      branch = {{ branch }},
      branch_position = {{ branch_position }},
      start_z = {{ start_z }},
      radius = {{ radius }}
    )

  # Verify network -------------------------------------------------------------
  network <- verify_network(cyl_sub, pruning = TRUE)

  # Prune QSM  -----------------------------------------------------------------
  inform("Pruning QSM")

  # Cylinders
  if (!is.null(cylinder_ids)) {
    cylinder_ids <- network %>%
      filter(.data$index %in% !!cylinder_ids) %>%
      pull("id")
  }

  # Branches
  if (!is.null(branch_ids)) {
    branch_ids <- cyl_sub %>%
      select("id", "branch") %>%
      filter(.data$branch %in% !!branch_ids) %>%
      group_by("branch") %>%
      filter(.data$id == min(.data$id, na.rm = FALSE)) %>%
      pull("id")

    branch_ids <- network %>%
      filter(.data$index %in% !!branch_ids) %>%
      pull("id")
  }

  # Segments
  if (!is.null(segment_ids)) {
    segment_ids <- cyl_sub %>%
      select("id", "segment") %>%
      filter(.data$segment %in% !!segment_ids) %>%
      group_by("segment") %>%
      filter(.data$id == min(.data$id, na.rm = FALSE)) %>%
      pull("id")

    segment_ids <- network %>%
      filter(.data$index %in% !!segment_ids) %>%
      pull("id")
  }

  # Height
  if (!is.null(height_m)) {
    base <- cyl_sub %>%
      filter(.data$id == 1) %>%
      pull("start_z")

    height_m <- cyl_sub %>%
      filter(.data$branch_position == 1) %>%
      mutate(start_z = round((.data$start_z - !!base), 1)) %>%
      filter(.data$start_z < !!height_m & !branch == 1) %>%
      pull("id")

    height_m <- network %>%
      filter(.data$index %in% !!height_m) %>%
      pull("id")
  }

  # Diameter
  if (!is.null(diameter_cm)) {
    diameter_cm <- cyl_sub %>%
      filter(.data$branch_position == 1) %>%
      mutate(radius = round((.data$radius * 200), 1)) %>%
      filter(.data$radius < !!diameter_cm) %>%
      pull("id")

    diameter_cm <- network %>%
      filter(.data$index %in% !!diameter_cm) %>%
      pull("id")
  }

  # Combine ids for pruning
  prune_ids <- c(cylinder_ids, branch_ids, segment_ids, height_m, diameter_cm)

  # Prune QSM
  if (invert == FALSE & index == FALSE) {
    cylinder %>%
      rename("id" = {{ id }}) %>%
      filter(!.data$id %in% !!prune_ids) %>%
      rename({{ id }} := "id")
  } else if (invert == TRUE & index == FALSE) {
    cylinder %>%
      rename("id" = {{ id }}) %>%
      filter(.data$id %in% !!prune_ids) %>%
      rename({{ id }} := "id")
  } else if (index == TRUE) {
    cylinder %>%
      rename("id" = {{ id }}) %>%
      mutate(pruning = if_else(.data$id %in% !!prune_ids, 1, 0)) %>%
      rename({{ id }} := "id")
  }
}
