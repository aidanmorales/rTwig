#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import rgl
#' @import rlang
#' @import tidytable
#' @import Rdpack
#' @import Rcpp
#' @importFrom cobs cobs
#' @importFrom colourvalues color_values
#' @importFrom geometry convhulln
#' @importFrom graphics abline
#' @importFrom graphics text
#' @importFrom igraph all_simple_paths
#' @importFrom igraph as_ids
#' @importFrom igraph ends
#' @importFrom igraph is_connected
#' @importFrom igraph components
#' @importFrom igraph degree
#' @importFrom igraph graph_from_data_frame
#' @importFrom igraph V
#' @importFrom igraph vcount
#' @importFrom R.matlab readMat
#' @importFrom R.matlab writeMat
#' @importFrom RcppSimdJson fload
#' @importFrom RcppSimdJson fparse
#' @importFrom Rdpack reprompt
#' @importFrom rmatio read.mat
#' @importFrom stats lm
#' @importFrom stats predict
#' @importFrom stats quantile
#' @useDynLib rTwig, .registration = TRUE
## usethis namespace: end
NULL

.datatable.aware <- TRUE

rTwigStartupMessage <- function() {
  msg <- c(
    paste0(
      "rTwig version ",
      utils::packageVersion("rTwig")
    ),
    "\nType 'citation(\"rTwig\")' for citing this R package in publications."
  )
  return(msg)
}

.onAttach <- function(lib, pkg) {
  # startup message
  msg <- rTwigStartupMessage()
  if (!interactive()) {
    msg[1] <- paste("Package 'rTwig' version", utils::packageVersion("rTwig"))
  }
  packageStartupMessage(msg)
  invisible()
}
