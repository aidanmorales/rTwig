#' Twig Species Database
#'
#' Database of twig radii by tree species
#'
#' @format ## `twigs`
#' A data frame containing twig radii measurements
#' \describe{
#'   \item{scientific_name}{The tree's genus and species}
#'   \item{radius_mm}{The average twig radius in millimeters}
#'   \item{n}{The twig measurement sample size}
#'   \item{min}{The minimum twig radii from the samples}
#'   \item{max}{The maximum twig radii from the samples}
#'   \item{std}{The standard deviation of twig radii}
#'   \item{cv}{The coefficient of variation of twig radii}
#' }
"twigs"

#' Twig Index Database
#'
#' Database of twig radii by size index
#'
#' @format ## `twigs_index`
#' A data frame containing twig radii measurements
#' \describe{
#'   \item{size_index}{The twig size index}
#'   \item{radius_mm}{The average twig radius in millimeters}
#'   \item{n}{The twig measurement sample size}
#'   \item{min}{The minimum twig radii from the samples}
#'   \item{max}{The maximum twig radii from the samples}
#'   \item{std}{The standard deviation of twig radii}
#'   \item{cv}{The coefficient of variation of twig radii}
#' }
"twigs_index"
