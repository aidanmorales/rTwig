#' Summarise QSM
#'
#' @description Generates a simple QSM summary. The summary includes basic tree
#'  metrics, such as DBH, total height, and also volume and surface area
#'  arranged by totals and branching order.
#'
#' @details
#' `summarise_qsm()` and `summarize_qsm()` are synonyms.
#'
#'
#' @param cylinder QSM cylinder data frame
#' @param radius Radius column name either quoted or unquoted.
#' @param triangulation Optional QSM triangulation list. Only supports TreeQSM.
#'
#' @return Returns a list
#' @export
#'
#' @examples
#'
#' ## SimpleForest Processing Chain
#' file <- system.file("extdata/QSM.csv", package = "rTwig")
#' cylinder <- read.csv(file)
#' cylinder <- update_cylinders(cylinder)
#' summarise_qsm(cylinder, radius)
#'
#' # TreeQSM Triangulation
#' file <- system.file("extdata/QSM.mat", package = "rTwig")
#' qsm <- import_treeqsm(file)
#' cylinder <- qsm$cylinder
#' cylinder <- update_cylinders(cylinder)
#' summarise_qsm(cylinder, radius, triangulation = qsm$triangulation)
#'
summarise_qsm <- function(cylinder, radius, triangulation = NULL) {
  # Check inputs ---------------------------------------------------------------
  if (is_missing(cylinder)) {
    message <- "argument `cylinder` is missing, with no default."
    abort(message, class = "missing_argument")
  }

  if (!is.data.frame(cylinder)) {
    message <- paste(
      "`cylinder` must be a data frame.",
      "i Did you accidentally pass the QSM list instead of the cylinder data frame?",
      sep = "\n"
    )
    abort(message, class = "data_format_error")
  }

  if (missing(radius)) { # Use base::missing to support unquoted columns
    message <- "argument `radius` is missing, with no default."
    abort(message, class = "missing_argument")
  } else {
    radius <- select_column(rlang::enquo(radius))

    if (!any(radius %in% colnames(cylinder))) {
      abort(paste(
        "Can't select columns that don't exist.",
        paste0("X Column `", radius, "' doesn't exist."),
        "i Did you mistype your `radius` column name?`.",
        sep = "\n"
      ))
    } else {
      radius <- pull(cylinder, !!radius)
    }
  }

  inform("Creating QSM Summary")

  # rTwig ----------------------------------------------------------------------
  if (all(c("id", "parent", "start_x", "branch_order") %in% colnames(cylinder))) {
    if (!is.null(triangulation)) {
      inform("Main stem triangulation not supported.")
    }

    data_summary(
      cylinder = cylinder,
      radius = radius, length = "length", branch = "branch",
      branch_order = "branch_order", branch_position = "branch_position",
      start_z = "start_z", id = "id", parent = "parent",
      triangulation = NULL
    )
  }
  # TreeQSM --------------------------------------------------------------------
  else if (all(c("parent", "extension", "branch", "BranchOrder") %in% colnames(cylinder))) {
    data_summary(
      cylinder = cylinder,
      radius = radius, length = "length", branch = "branch",
      branch_order = "BranchOrder", branch_position = "PositionInBranch",
      start_z = "start.z", id = "extension", parent = "parent",
      triangulation = triangulation
    )
  }
  # SimpleForest ---------------------------------------------------------------
  else if (all(c("ID", "parentID", "branchID", "branchOrder") %in% colnames(cylinder))) {
    if (!is.null(triangulation)) {
      inform("Main stem triangulation not supported.")
    }

    data_summary(
      cylinder = cylinder,
      radius = radius, length = "length", branch = "branchID",
      branch_order = "branchOrder", branch_position = "positionInBranch",
      start_z = "startZ", id = "ID", parent = "parentID",
      triangulation = NULL
    )
  }
  # Treegraph ------------------------------------------------------------------
  else if (all(c("p1", "p2", "ninternode") %in% colnames(cylinder))) {
    if (!is.null(triangulation)) {
      inform("Main stem triangulation not supported.")
    }

    data_summary(
      cylinder = cylinder,
      radius = radius, length = "length", branch = "nbranch",
      branch_order = "branch_order", branch_position = "positionInBranch",
      start_z = "sz", id = "p1", parent = "p2",
      triangulation = NULL
    )
  }
  # aRchi ----------------------------------------------------------------------
  else if (all(c("cyl_ID", "parent_ID", "branching_order") %in% colnames(cylinder))) {
    if (!is.null(triangulation)) {
      inform("Main stem triangulation not supported.")
    }

    data_summary(
      cylinder = cylinder,
      radius = radius, length = "length", branch = "branch_ID",
      branch_order = "branching_order", branch_position = "positionInBranch",
      start_z = "startZ", id = "cyl_ID", parent = "parent_ID",
      triangulation = NULL
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

#' Data summary
#' @param cylinder QSM cylinder data frame
#' @param radius cylinder radii
#' @param length cylinder length
#' @param branch cylinder branch id
#' @param branch_order cylinder branch order
#' @param branch_position cylinder branch position
#' @param start_z column name of start_z
#' @param id cylinder id
#' @param parent cylinder parent id
#' @param triangulation TreeQSM triangulation
#'
#' @return a list
#' @noRd
#'
data_summary <- function(
    cylinder,
    radius,
    length,
    branch,
    branch_order,
    branch_position,
    start_z,
    id,
    parent,
    triangulation) {
  # Subset cylinder data -------------------------------------------------------
  cylinder <- cylinder %>%
    select(
      all_of(
        c(length, branch, branch_order, branch_position, start_z, id, parent)
      )
    ) %>%
    rename(
      "length" = {{ length }},
      "branch" = {{ branch }},
      "branch_order" = {{ branch_order }},
      "branch_position" = {{ branch_position }},
      "start_z" = {{ start_z }},
      "id" = {{ id }}
    ) %>%
    mutate(radius := {{ radius }})

  # Verify connectivity --------------------------------------------------------
  qsm_g <- verify_network(cylinder, graph = TRUE)
  qsm_connectivity <- igraph::is_connected(qsm_g)

  # Diameter at breast height (DBH) --------------------------------------------
  if (qsm_connectivity == TRUE) {
    base <- as.numeric(igraph::ends(qsm_g, 1)[2])

    stem_info <- cylinder %>%
      filter(.data$id == !!base) %>%
      select("branch", "branch_order")

    # Diameter at breast height (DBH)
    dbh <- cylinder %>%
      filter(.data$branch == !!stem_info$branch) %>%
      arrange(.data$branch_position) %>%
      select("length", "radius")


    if (sum(dbh$length) < 1.37) {
      message <- paste(
        "The main stem is shorter than 1.37 meters.",
        "DBH cannot be calculated!",
        sep = " "
      )
      warn(message)

      dbh_qsm_cm <- NA
    } else {
      for (i in 1:nrow(dbh)) {
        DBHCyl <- sum(dbh$length[1:i])
        if (DBHCyl >= 1.37) {
          break
        }
      }

      dbh_qsm_cm <- dbh$radius[as.numeric(i)] * 200
    }
  } else {
    message <- paste(
      "The QSM is not a connected structure!",
      "Main stem, branch, and DBH metrics cannot be calculated!",
      sep = " "
    )
    warn(message)

    dbh_qsm_cm <- NA
  }
  # Tree height ----------------------------------------------------------------
  tree_height_m <- max(cylinder$start_z) - min(cylinder$start_z)

  # Triangulation volumes ------------------------------------------------------
  if (!is.null(triangulation)) {
    if (!is_list(triangulation)) {
      message <- paste(
        paste0("`triangulation` must be a list, not ", class(triangulation), "."),
        "i `triangulation` must be created by `import_treeqsm()`.",
        sep = "\n"
      )
      abort(message, class = "data_format_error")
    }

    # Finds the triangulation end cylinder
    cyl_end <- pull(triangulation$cylind - 1)

    # Gets the QSM volumes
    qsm_vol_sa <- cylinder %>%
      filter(.data$id %in% c(1:cyl_end)) %>%
      mutate(
        volume = pi * .data$radius^2 * .data$length * 1e3,
        surface_area = 2 * pi * .data$radius * .data$length
      ) %>%
      summarise(
        cyl_volume = sum(.data$volume),
        cyl_surface_area = sum(.data$surface_area)
      )

    # Triangulation Volume and Surface Area
    tri_volume <- pull(triangulation$volume)
    tri_surface_area <- pull(triangulation$SideArea)
  }

  # Branch order summary -------------------------------------------------------
  summary <- cylinder %>%
    mutate(
      volume = pi * .data$radius^2 * .data$length,
      surface_area = 2 * pi * .data$radius * .data$length
    ) %>%
    group_by("branch_order") %>%
    summarise(
      tree_volume_L = sum(.data$volume, na.rm = TRUE) * 1e3,
      tree_area_m2 = sum(.data$surface_area, na.rm = TRUE)
    )

  # Update with triangulation volumes
  if (!is.null(triangulation)) {
    summary$tree_volume_L[1] <- summary$tree_volume_L[1] - qsm_vol_sa$cyl_volume + tri_volume
    summary$tree_area_m2[1] <- summary$tree_area_m2[1] - qsm_vol_sa$cyl_surface_area + tri_surface_area
  }



  # Tree summary ---------------------------------------------------------------
  if (qsm_connectivity == TRUE) {
    stem_volume_L <- summary %>%
      filter(.data$branch_order == !!stem_info$branch_order) %>%
      summarise(stem_volume_L = sum(.data$tree_volume_L))

    branch_volume_L <- summary %>%
      filter(.data$branch_order != !!stem_info$branch_order) %>%
      summarise(branch_volume_L = sum(.data$tree_volume_L))

    stem_area_m2 <- summary %>%
      filter(.data$branch_order == !!stem_info$branch_order) %>%
      summarise(stem_area_m2 = sum(.data$tree_area_m2))

    branch_area_m2 <- summary %>%
      filter(.data$branch_order != !!stem_info$branch_order) %>%
      summarise(branch_area_m2 = sum(.data$tree_area_m2))
  }

  tree_volume_L <- summary %>%
    summarise(tree_volume_L = sum(.data$tree_volume_L))

  tree_area_m2 <- summary %>%
    summarise(tree_area_m2 = sum(.data$tree_area_m2))

  if (qsm_connectivity == TRUE) {
    summary2 <- bind_cols(
      "dbh_qsm_cm" = dbh_qsm_cm,
      "tree_height_m" = tree_height_m,
      stem_volume_L,
      branch_volume_L,
      tree_volume_L,
      stem_area_m2,
      branch_area_m2,
      tree_area_m2
    )
  } else {
    summary2 <- bind_cols(
      "dbh_qsm_cm" = dbh_qsm_cm,
      "tree_height_m" = tree_height_m,
      stem_volume_L = NA,
      branch_volume_L = NA,
      tree_volume_L,
      stem_area_m2 = NA,
      branch_area_m2 = NA,
      tree_area_m2
    )
  }

  return(list(summary, summary2))
}

#' @rdname summarise_qsm
#' @export
summarize_qsm <- summarise_qsm


#' @title QSM Summary
#'
#' @description `qsm_summary` is deprecated and will be removed in a future version. Use `summarise_qsm()` instead.
#' @param ... function inputs
#' @return Returns a list
#' @export
qsm_summary <- function(...) {
  warn(paste(
    "`qsm_summary()` was deprecated in rTwig 1.4.0.",
    "i Please use `summarise_qsm()` instead.",
    sep = "\n"
  ))
  suppressMessages(summarise_qsm(...))
}
