#' Plot QSM
#'
#' @description Plots QSM cylinders using the rgl library
#'
#' @param cylinder QSM cylinder data frame
#' @param radius Radius type, as either "modified" or "unmodified". Defaults to "modified".
#' @param color Color QSM by "BranchOrder", "GrowthLength", or a user supplied color. Defaults to BranchOrder. The user supplied color must be a vector or column with the same length as the cylinder data frame.
#' @param hover Show cylinder ID and branch on mouse hover. Defaults to FALSE.
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
#' qsm <- import_qsm(file)
#' cylinder <- qsm$cylinder
#' cylinder <- update_cylinders(cylinder)
#' cylinder <- growth_length(cylinder)
#' cylinder <- correct_radii(cylinder, twigRad = 4.23)
#' plot_qsm(cylinder)
#'
#' ## SimpleForest Processing Chain
#' file <- system.file("extdata/QSM.csv", package = "rTwig")
#' cylinder <- read.csv(file)
#' cylinder <- update_cylinders(cylinder)
#' cylinder <- correct_radii(cylinder, twigRad = 4.23)
#' plot_qsm(cylinder, radius = "modified", color = "GrowthLength")
#' }
plot_qsm <- function(cylinder, radius = "modified", color = "BranchOrder", hover = FALSE) {
  message("Plotting QSM")

  # TreeQSM --------------------------------------------------------------------
  if (all(c("parent", "extension", "branch", "BranchOrder") %in% colnames(cylinder))) {

    # Error message if cylinders have not been updated
    stopifnot("Cylinder indexes have not been updated! Please run update_cylinders() before proceeding." = pull(slice_head(cylinder, n = 1),.data$extension) == 1)

    if (color == "GrowthLength") {
      colors <- colour_values(cylinder$GrowthLength, palette = "viridis")
    } else if (color == "custom") {
      colors <- as.vector(cylinder$colors)
    } else {
      colors <- color_values(cylinder$BranchOrder, palette = "rainbow")
    }

    if (radius == "modified") {
      radius <- cylinder$radius
    } else if (radius == "unmodified") {
      radius <- cylinder$UnmodRadius
    }

    plot_data <- lapply(1:nrow(cylinder), function(i) {
      cyl <- cylinder3d(
        center = cbind(
          c(cylinder$start.x[i], cylinder$end.x[i]),
          c(cylinder$start.y[i], cylinder$end.y[i]),
          c(cylinder$start.z[i], cylinder$end.z[i])
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

    if (hover == TRUE){
      hover3d(
        cylinder$start.x,
        cylinder$start.y,
        cylinder$start.z,
        labels = paste0("ID:", cylinder$extension, " - Branch:", cylinder$branch)
      )
    }

  # SimpleForest ---------------------------------------------------------------
  } else if (all(c("ID", "parentID", "branchID", "branchOrder") %in% colnames(cylinder))) {
    if (color == "GrowthLength") {
      colors <- colour_values(cylinder$growthLength, palette = "viridis")
    } else if (color == "custom") {
      colors <- as.vector(cylinder$colors)
    } else {
      colors <- color_values(cylinder$branchOrder, palette = "rainbow")
    }

    if (radius == "modified") {
      radius <- cylinder$radius
    } else if (radius == "unmodified") {
      radius <- cylinder$UnmodRadius
    }

    plot_data <- lapply(1:nrow(cylinder), function(i) {
      cyl <- cylinder3d(
        center = cbind(
          c(cylinder$startX[i], cylinder$endX[i]),
          c(cylinder$startY[i], cylinder$endY[i]),
          c(cylinder$startZ[i], cylinder$endZ[i])
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

    if (hover == TRUE){
      hover3d(
        cylinder$startX,
        cylinder$startY,
        cylinder$startZ,
        labels = paste0("ID:", cylinder$ID, " - Branch:", cylinder$branchID)
      )
    }

  } else {
    message(
      "Invalid QSM!!!
      \nMake sure the cylinder data frame and not the QSM list is supplied."
    )
  }
}