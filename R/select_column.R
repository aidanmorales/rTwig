#' Select Column
#'
#' @description Uses rlang to convert a variable to a string without evaluation.
#' It is used in conjunction with rlang::enquo() This allows the user to pass
#' quoted or unquoted column names to a function, making a variable selection
#' without the variable having to exist in the global environment. This makes
#' selection work with both traditional R strings and unquoted data frame
#' variables when using magrittr pipes.
#'
#' @param ... The column name either quoted or unquoted passed from rlang::enquo(...)
#'
#' @returns a string label or NULL
#' @noRd
#'
select_column <- function(...) {
  expr <- rlang::quo_get_expr(...)

  if (is.null(expr) | is.character(expr)) {
    return(expr)
  } else {
    as_label(expr)
  }
}
