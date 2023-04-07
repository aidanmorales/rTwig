#' Import QSM
#'
#' @description Imports a QSM stored as a MATLAB file created by TreeQSM.
#'
#' @param file a .mat file
#'
#' @return Returns a data.frame
#' @export
#'
#' @importFrom R.matlab readMat
#'
#' @examples
#' \dontrun{
#' ## Read a TreeQSM MATLAB file
#' file <- system.file("extdata/QSM.mat", package = "rTwig")
#' df <- import_qsm(file)
#' str(df)
#' }
import_qsm <- function(file) {
  list <- R.matlab::readMat(file)[[1]]

  # Extracts the variable names
  all_names <- dimnames(list)
  all_names <- all_names[[1]]

  # Adds list names
  for (j in 1:length(list)) {
    names(list)[j] <- all_names[j]
  }

  # List to store cylinder data
  cylinder <- list()

  # Names of each element of the cylinder data
  cylinder_names <- dimnames(list$cylinder)
  cylinder_names <- cylinder_names[lengths(cylinder_names) != 0]
  cylinder_names <- cylinder_names[[1]]

  # Assigns names to each element in the cylinder list
  for (j in 1:length(list$cylinder)) {
    temp <- as.data.frame(list$cylinder[j])
    if (min(dim(temp)) == 1) {
      colnames(temp) <- cylinder_names[j]
    } else if (min(dim(temp)) == 3) {
      colnames(temp) <- c(paste0(cylinder_names[j], ".x"), paste0(cylinder_names[j], ".y"), paste0(cylinder_names[j], ".z"))
    }
    cylinder <- c(cylinder, list(temp))
  }

  # Combines all of the cylinder data into a single data frame
  cylinder <- do.call(cbind, cylinder)

  return(cylinder)
}
