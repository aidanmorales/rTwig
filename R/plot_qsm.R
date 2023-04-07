#' Plot QSM
#'
#' @description
#' Plots QSM cylinders using the rgl library.
#'
#' @param df QSM cylinder data frame
#' @param radius Radius type, as either "modified" or "unmodified". Defaults to modified.
#' @param color Color QSM by either "BranchOrder" or "GrowthLength". Defaults to BranchOrder.
#' @param method QSM type, as either "TreeQSM" or "SimpleForest". Defaults to TreeQSM.
#'
#' @return A rgl QSM plot
#' @export
#'
#' @import colourvalues
#' @import rgl
#'
#' @examples
#' \dontrun{
#' ## TreeQSM Processing Chain
#' file <- system.file("extdata/QSM.mat", package = "rTwig")
#' df <- import_qsm(file)
#' df <- update_cylinders(df)
#' df <- growth_length(df)
#' plot_qsm(df)
#'
#' ## SimpleForest Processing Chain
#' file <- system.file("extdata/QSM.csv", package = "rTwig")
#' df <- read.csv(file)
#' df <- update_cylinders(df, method = "SimpleForest")
#' plot_qsm(df, radius = "unmodified", color = "GrowthLength", method = "SimpleForest")
#' }
plot_qsm <- function(df, radius = "modified", color = "BranchOrder", method = "TreeQSM") {
  if (method == "TreeQSM") {
    if (color == "GrowthLength") {
      colors <- colour_values(df$GrowthLength, palette = "viridis")
    } else if (color == "path") {
      colors <- as.vector(df$colors)
    } else {
      colors <- color_values(df$BranchOrder, palette = "rainbow")
    }

    if (radius == "modified") {
      radius <- df$radius
    } else if (radius == "unmodified") {
      radius <- df$UnmodRadius
    }

    plot_data <- lapply(1:nrow(df), function(i) {
      cyl <- cylinder3d(
        center = cbind(
          c(df$start.x[i], df$end.x[i]),
          c(df$start.y[i], df$end.y[i]),
          c(df$start.z[i], df$end.z[i])
        ),
        radius = radius[i],
        sides = 100,
        closed = -1
      )
      cyl$material$color <- colors[i]
      cyl
    })

    open3d()
    shade3d(shapelist3d(plot_data, plot = FALSE))
    axes3d(edges = c("x", "y", "z"))
  } else if (method == "SimpleForest") {
    if (color == "GrowthLength") {
      colors <- colour_values(df$growthLength, palette = "viridis")
    } else {
      colors <- color_values(df$branchOrder, palette = "rainbow")
    }

    if (radius == "modified") {
      radius <- df$radius
    } else if (radius == "unmodified") {
      radius <- df$UnmodRadius
    }

    plot_data <- lapply(1:nrow(df), function(i) {
      cyl <- cylinder3d(
        center = cbind(
          c(df$startX[i], df$endX[i]),
          c(df$startY[i], df$endY[i]),
          c(df$startZ[i], df$endZ[i])
        ),
        radius = radius[i],
        sides = 100,
        closed = -1
      )
      cyl$material$color <- colors[i]
      cyl
    })

    open3d()
    shade3d(shapelist3d(plot_data, plot = FALSE))
    axes3d(edges = c("x", "y", "z"))
  } else {
    message("Invalid Method Entered!!!\nValid Methods = TreeQSM or SimpleForest")
  }
}
