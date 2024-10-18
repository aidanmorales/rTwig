#' Box Dimension
#'
#' @description R port of Dominik Seidel's fractal analysis "box-dimension" metric.
#'
#' @param cloud A point cloud matrix size n x 3. Non-matrices are automatically converted to a matrix.
#' @param lowercutoff The smallest box size determined by the point spacing of the cloud in meters. Defaults to 1 cm.
#' @param rm_int_box Remove the initial box as TRUE or FALSE. Defaults to FALSE.
#' @param plot Plot the results. The user can specify "2D", "3D", or "ALL" plots. FALSE disables plotting. Defaults to FALSE.
#'
#' @return Returns a list
#' @export
#'
#' @references
#' \insertRef{box_dimension1}{rTwig}
#'
#' \insertRef{box_dimension2}{rTwig}
#'
#' \insertRef{box_dimension3}{rTwig}
#'
#' \insertRef{box_dimension4}{rTwig}
#'
#' \insertRef{box_dimension5}{rTwig}
#'
#' @examples
#' ## Calculate Box Dimension
#' file <- system.file("extdata/cloud.txt", package = "rTwig")
#' cloud <- read.table(file, header = FALSE)
#' output <- box_dimension(cloud, plot = "ALL")
#' output
#'
box_dimension <- function(cloud, lowercutoff = 0.01, rm_int_box = FALSE, plot = FALSE) {
  # Check inputs ---------------------------------------------------------------
  if (is_missing(cloud)) {
    message <- "argument `cloud` is missing, with no default."
    abort(message, class = "missing_argument")
  }

  if (!is_scalar_double(lowercutoff)) {
    message <- paste0(
      "`lowercutoff` must be numeric, not ", class(lowercutoff), "."
    )
    abort(message, class = "invalid_argument")
  }

  if (!is_logical(rm_int_box)) {
    message <- paste0(
      "`lowercutoff` must be logical, not ", class(rm_int_box), "."
    )
    abort(message, class = "invalid_argument")
  }


  if (!any(plot %in% c(FALSE, "2D", "3D", "ALL"))) {
    message <- paste0(
      "`plot` must be `FALSE`, `2D`, `3D`, or `ALL`, not ", plot, "."
    )
    abort(message, class = "invalid_argument")
  }

  if (all(!is.data.frame(cloud), !is.matrix(cloud))) {
    message <- paste0(
      "`cloud` must be a data frame or matrix, not a ", class(cloud), "."
    )
    abort(message, class = "data_format_error")
  }

  # Calculates Box Dimension ---------------------------------------------------
  # Ensure cloud is a matrix with x, y, z as the first columns
  if (!is.matrix(cloud)) {
    cloud <- as.matrix(cloud[, 1:3])
  } else {
    cloud <- cloud[, 1:3]
  }

  if (!is.numeric(cloud)) {
    message <- paste0(
      "The first three columns of `cloud` must be x, y, z."
    )
    abort(message, class = "data_format_error")
  }

  # Calculate box sizes and count
  results <- box_counting(cloud, lowercutoff)
  voxelnumber <- results$voxelnumber
  size <- results$size
  ruler <- results$ruler


  if (rm_int_box == TRUE) {
    data <- tidytable(log(1 / ruler), log(voxelnumber)) %>%
      rename(
        log.box.size = 1,
        log.voxels = 2
      ) %>%
      slice(-1) # Removes the initial box
  } else {
    data <- tidytable(log(1 / ruler), log(voxelnumber)) %>%
      rename(
        log.box.size = 1,
        log.voxels = 2
      )
  }

  results <- lm(data$log.voxels ~ data$log.box.size)

  # Creates the summary files from the linear model
  summary <- tidytable(
    r.squared = summary(results)$r.squared,
    adj.r.squared = summary(results)$adj.r.squared,
    intercept = as.double(results$coefficients[1]),
    slope = as.double(results$coefficients[2])
  )

  # 2D Plot --------------------------------------------------------------------
  if (plot == "ALL" | plot == "2D") {
    plot(data$log.box.size,
      data$log.voxels,
      pch = 19,
      xlab = "Log(Inverse Box Size)",
      ylab = "log(Box Count)"
    )

    # Model Line
    abline(lm(data$log.voxels ~ data$log.box.size))

    # Statistics Labels
    label_step <- (max(data$log.voxels) * 0.25) / 3

    x <- max(data$log.box.size) / 2
    y1 <- max(data$log.voxels)
    y2 <- max(data$log.voxels) - label_step
    y3 <- max(data$log.voxels) - label_step * 2

    text(x, y1, paste0("y = ", round(summary$slope, 3), "x + ", round(summary$intercept, 4)))
    text(x, y2, (bquote("R"^2 ~ .(paste0(" = ", round(summary$adj.r.squared, 4))))))
    text(x, y3, (bquote("D"[b] ~ .(paste0(" = ", round(summary$slope, 2))))))
  }

  # 3D Plot --------------------------------------------------------------------
  if (plot == "ALL" | plot == "3D") {
    # Convert point cloud to a local coordinate system
    cloud[, 1] <- cloud[, 1] - min(cloud[, 1])
    cloud[, 2] <- cloud[, 2] - min(cloud[, 2])
    cloud[, 3] <- cloud[, 3] - min(cloud[, 3])

    # Plots Point Cloud
    open3d()
    plot3d(cloud[, 1], cloud[, 2], cloud[, 3], aspect = FALSE, decorate = FALSE)

    # Plot Voxels
    for (i in 1:length(size)) {
      cube <- cube3d()
      cube$vb[cube$vb == -1] <- 0
      cube$vb[cube$vb == 1] <- size[i]
      cube[["vb"]][4, ] <- 1
      wire3d(cube)
    }

    # Plot Labels & Tick Marks
    title3d(xlab = "X", ylab = "Y", zlab = "Z")
    axes3d(labels = FALSE, tick = TRUE, box = FALSE)

    # Pan plot
    pan_plot(button = 2)
  }

  return(list(data, summary))
}
