#' Box Dimension
#'
#' @description R port of Dominik Seidel's fractal analysis "box-dimension" metric.
#'
#'
#' @param cloud A point cloud object
#' @param x The column name or position of the x coordinates. Defaults to the first column.
#' @param y The column name or position of the y coordinates. Defaults to the second column.
#' @param z The column name or position of the x coordinates. Defaults to the third column.
#' @param lowercutoff The smallest box size determined by the point spacing of the cloud in meters. Defaults to 1 cm.
#' @param rm_int_box Remove the initial box as TRUE or FALSE. Defaults to FALSE.
#'
#' @return Returns a list
#' @export
#'
#' @import dplyr
#' @importFrom stats lm
#' @importFrom Rdpack reprompt
#' @importFrom kit uniqLen
#' @importFrom purrr map_dfr
#' @importFrom DescTools RoundTo
#'
#' @references {
#'   \insertRef{box_dimension1}{rTwig}
#'
#'   \insertRef{box_dimension2}{rTwig}
#' }
#'
#' @examples
#' \dontrun{
#' ## Calculate Box Dimension
#' file <- system.file("extdata/cloud.txt", package = "rTwig")
#' cloud <- read.table(file)
#' output <- box_dimension(cloud)
#' output
#'
#' ## Plot Results
#' data <- output[[1]]
#' plot(data$log.box.size,
#'   data$log.voxels,
#'   pch = 19,
#'   xlab = "Log(Inverse Box Size)",
#'   ylab = "log(Box Count)"
#' )
#' abline(lm(data$log.voxels ~ data$log.box.size))
#' }
box_dimension <- function(cloud, x = 1, y = 2, z = 3, lowercutoff = 0.01, rm_int_box = FALSE) {
  # Selects only the x, y, z columns from the point cloud
  cloud <- select(cloud, x = any_of(x), y = any_of(y), z = any_of(z))

  # Error message if the supplied columns are not numeric
  stopifnot("Supplied columns are not numeric!" = is.numeric(cloud$x) | is.numeric(cloud$y) | is.numeric(cloud$z))

  # Finds the largest box size edge length that can contain all points
  x_max <- round((max(cloud$x) - min(cloud$x)) * 100) / 100
  y_max <- round((max(cloud$y) - min(cloud$y)) * 100) / 100
  z_max <- round((max(cloud$z) - min(cloud$z)) * 100) / 100
  rulerlimit <- max(x_max, y_max, z_max)

  # Halves the largest box size until it reaches the lower cutoff point
  size <- vector("double")

  for (j in 1:16) {
    if (j == 1) {
      size[1] <- rulerlimit
    } else {
      size[j] <- size[j - 1] / 2
    }
  }

  for (j in 1:length(size)) {
    if ((size[j] > lowercutoff) == TRUE) {
    } else {
      size <- size[-(j:length(size))]
      break
    }
  }

  # Ratio between all box edge lengths and the initial box
  ruler <- size / rulerlimit

  # Inverts the box sizes
  rf <- 1 / size

  # Calculates the number of boxes in each size needed to enclose all points.
  # This is done by rounding each coordinate in the point cloud to the highest
  # multiple of the box size and tallying the number of distinct points.
  # The total number of distinct points is the total number of boxes of that
  # size needed to enclose all points in the point cloud.
  voxelnumber <- vector("double")

  for (j in 1:length(size)) {
    voxelnumber[j] <- kit::uniqLen(purrr::map_dfr(.x = cloud, ~ DescTools::RoundTo(.x, multiple = size[j], FUN = "floor")))
  }

  if (rm_int_box == TRUE) {
    data <- data.frame(log(1 / ruler), log(voxelnumber)) %>%
      rename(
        log.box.size = 1,
        log.voxels = 2
      ) %>%
      slice(-1) # Removes the initial box
  } else {
    data <- data.frame(log(1 / ruler), log(voxelnumber)) %>%
      rename(
        log.box.size = 1,
        log.voxels = 2
      )
  }

  results <- lm(data$log.voxels ~ data$log.box.size)

  # Creates the summary files from the linear model
  summary <- as.data.frame(results$coefficients) %>%
    t() %>%
    as_tibble() %>%
    rename("intercept" = 1, "slope" = 2) %>%
    mutate(
      r.squared = summary(results)$r.squared,
      adj.r.squared = summary(results)$adj.r.squared
    )

  return(list(data, summary))
}
