#' Reconstruct QSM
#'
#' @description
#'  Reconstruct a QSM and all of its variables from the minimum amount of
#'  required data. The required variables are the cylinder id, parent id,
#'  3d information and the radius. The cylinder can be defined in 3d space with
#'  either a combination of the start and end points, or the start, axis, and
#'  length. Branch and branch order information are optional. If not provided,
#'  the branches and orders are defined recursively using the growth length.
#'
#' @param cylinder QSM cylinder data frame
#' @param id cylinder ids
#' @param parent cylinder parent ids
#' @param radius cylinder radius
#' @param start_x cylinder start x position
#' @param start_y cylinder start y position
#' @param start_z cylinder start z position
#' @param end_x cylinder end x position
#' @param end_y cylinder end y position
#' @param end_z cylinder end z position
#' @param axis_x cylinder end x position
#' @param axis_y cylinder end y position
#' @param axis_z cylinder end z position
#' @param length cylinder length
#' @param branch optional cylinder branch id
#' @param branch_order optional cylinder branch order
#' @param metrics Calculate tree metrics. Defaults to FALSE.
#'
#' @returns data frame
#' @export
#'
#' @examples
#' file <- system.file("extdata/QSM.csv", package = "rTwig")
#' qsm <- reconstruct_qsm(
#'   cylinder = read.csv(file),
#'   id = "ID", parent = "parentID", radius = "radius",
#'   start_x = "startX", start_y = "startY", start_z = "startZ",
#'   end_x = "endX", end_y = "endY", end_z = "endZ"
#' )
#' plot_qsm(qsm)
#'
reconstruct_qsm <- function(
    cylinder,
    id,
    parent,
    radius,
    start_x,
    start_y,
    start_z,
    end_x = NULL,
    end_y = NULL,
    end_z = NULL,
    axis_x = NULL,
    axis_y = NULL,
    axis_z = NULL,
    length = NULL,
    branch = NULL,
    branch_order = NULL,
    metrics = FALSE) {
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

  start_end <- !is_null(start_x) && !is_null(start_y) && !is_null(start_z) &&
    !is_null(end_x) && !is_null(end_y) && !is_null(end_z)

  start_axis_length <- !is_null(start_x) && !is_null(start_y) && !is_null(start_z) &&
    !is_null(axis_x) && !is_null(axis_y) && !is_null(axis_z) &&
    !is_null(length)

  if (!start_end && !start_axis_length) {
    abort(
      message = "Cylinder geometry must be defined by either start and end coordinates or start, axis, and length.",
      class = "invalid_geometry"
    )
  }

  if (!is_logical(metrics)) {
    message <- paste0(
      "`metrics` must be logical, not ", class(metrics), "."
    )
    abort(message, class = "invalid_argument")
  }

  # Reconstruct QSM ------------------------------------------------------------
  # Build cylinders from start and end point geometry
  if (!is.null(start_x) & !is.null(start_y) & !is.null(start_z) &
    !is.null(end_x) & !is.null(end_y) & !is.null(end_z)) {
    qsm <- cylinder %>%
      select(
        id = {{ id }},
        parent = {{ parent }},
        radius = {{ radius }},
        start_x = {{ start_x }},
        start_y = {{ start_y }},
        start_z = {{ start_z }},
        end_x = {{ end_x }},
        end_y = {{ end_y }},
        end_z = {{ end_z }}
      ) %>%
      mutate(
        length = sqrt(
          (.data$end_x - .data$start_x)^2 +
            (.data$end_y - .data$start_y)^2 +
            (.data$end_z - .data$start_z)^2
        ),
        axis_x = (.data$end_x - .data$start_x) / .data$length,
        axis_y = (.data$end_y - .data$start_y) / .data$length,
        axis_z = (.data$end_z - .data$start_z) / .data$length,
        raw_radius = .data$radius
      )
  }

  # Build cylinders from start, axis, and length geometry
  if (!is.null(start_x) & !is.null(start_y) & !is.null(start_z) &
    !is.null(axis_x) & !is.null(axis_y) & !is.null(axis_z) &
    !is.null(length)) {
    qsm <- cylinder %>%
      select(
        id = {{ id }},
        parent = {{ parent }},
        radius = {{ radius }},
        start_x = {{ start_x }},
        start_y = {{ start_y }},
        start_z = {{ start_z }},
        axis_x = {{ axis_x }},
        axis_y = {{ axis_y }},
        axis_z = {{ axis_z }},
        length = {{ length }}
      ) %>%
      mutate(
        end_x = .data$start_x + (.data$axis_x * .data$length),
        end_y = .data$start_y + (.data$axis_y * .data$length),
        end_z = .data$start_z + (.data$axis_z * .data$length),
        raw_radius = .data$radius
      )
  }

  # Fill TreeQSM extension gaps
  if (qsm$id[1] == 2 & nrow(filter(qsm, id == 0)) > 1) {
    qsm <- mutate(qsm, id = row_number())
  }

  # Update cylinder ordering
  qsm <- update_ordering(qsm, id = "id", parent = "parent")

  # Build QSM network
  network <- build_network(qsm, "id", "parent", cache = FALSE)

  # Calculate required variables
  qsm <- growth_length(network, qsm, "id", "length")
  qsm <- reverse_branch_order(network, qsm, "id", "parent")
  qsm <- total_children(qsm, "parent", "id")

  qsm <- qsm %>%
    rename(
      reverse_order = "reverseBranchOrder",
      total_children = "totalChildren",
      growth_length = "growthLength"
    )

  # Calculate branches and branch orders
  if (!is.null(branch)) {
    qsm$branch <- cylinder[[branch]]
    qsm$branch_order <- define_branch_order(qsm)
  } else {
    qsm$branch <- define_branches(qsm)
  }

  if (!is.null(branch_order)) {
    qsm$branch_order <- cylinder[[branch_order]]
  } else {
    qsm$branch_order <- define_branch_order(qsm)
  }

  qsm <- branch_alt(network, qsm, "id", "parent", "branch", "branch_order")
  qsm <- branch_segments(qsm, "id", "parent", "branch", "reverse_order")

  qsm <- qsm %>%
    group_by("branch") %>%
    mutate(
      branch_position = 1:n(),
    ) %>%
    ungroup()

  # Calculate path metrics
  qsm <- path_metrics(network, qsm, "id", "length")

  # Arrange variables
  qsm <- select(qsm,
    start_x = "start_x", start_y = "start_y", start_z = "start_z",
    axis_x = "axis_x", axis_y = "axis_y", axis_z = "axis_z",
    end_x = "end_x", end_y = "end_y", end_z = "end_z",
    id = "id", parent = "parent", radius = "radius",
    raw_radius = "raw_radius", length = "length", branch = "branch",
    branch_position = "branch_position",
    branch_order = "branch_order", reverse_order = "reverse_order",
    branch_alt = "branch_alt",
    segment = "segment", parent_segment = "parentSegment",
    total_children = "total_children", growth_length = "growth_length",
    base_distance = "distanceFromBase", twig_distance = "distanceToTwig",
    vessel_volume = "vesselVolume",
    pipe_area = "reversePipeAreaBranchorder",
    pipe_radius = "reversePipeRadiusBranchorder"
  )

  if (metrics == TRUE) {
    metrics <- tree_metrics(qsm)
    return(list(cylinder = qsm, metrics = metrics))
  } else {
    return(qsm)
  }
}
