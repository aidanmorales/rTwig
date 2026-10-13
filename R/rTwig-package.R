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
#' @importFrom RcppSimdJson fload
#' @importFrom RcppSimdJson fparse
#' @importFrom Rdpack reprompt
#' @importFrom stats lm
#' @importFrom stats predict
#' @importFrom stats quantile
#' @useDynLib rTwig, .registration = TRUE
## usethis namespace: end
NULL

.datatable.aware <- TRUE

.rtwig_native_path_available <- FALSE

.onLoad <- function(libname, pkgname) {
  if (!is.function(SparseM::chol) || !is.function(quantreg::rq)) {
    return(invisible(NULL))
  }

  native_symbol <- function(name, package) {
    dll <- getLoadedDLLs()[[package]]
    if (is.null(dll)) {
      loadNamespace(package)
      dll <- getLoadedDLLs()[[package]]
    }
    getNativeSymbolInfo(name, dll, withRegistrationInfo = FALSE)$address
  }

  symbols <- tryCatch(list(
    basis = native_symbol("spline_basis", "cobs"),
    value = native_symbol("spline_value", "cobs"),
    transpose = native_symbol("csrcsc2", "SparseM"),
    countmul = native_symbol("amubdg", "SparseM"),
    multiply = native_symbol("amub", "SparseM"),
    plus = native_symbol("aplsb", "SparseM"),
    matvec = native_symbol("amux", "SparseM"),
    chol = native_symbol("chol", "SparseM"),
    backsolve = native_symbol("bckslv", "SparseM"),
    solver = native_symbol("srqfnc", "quantreg")
  ), error = function(e) NULL)

  if (!is.null(symbols)) {
    tryCatch({
      initialize_path_gam(symbols)
      .rtwig_native_path_available <<- TRUE
    }, error = function(e) {
      .rtwig_native_path_available <<- FALSE
    })
  }
}

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
