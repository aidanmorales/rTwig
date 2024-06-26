% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qsm_summary.R
\name{qsm_summary}
\alias{qsm_summary}
\title{QSM Summary}
\usage{
qsm_summary(cylinder, radius = "modified", triangulation = FALSE)
}
\arguments{
\item{cylinder}{QSM cylinder data frame}

\item{radius}{Radius type as either "modified", "unmodified", or "old". Defaults to "modified".}

\item{triangulation}{QSM triangulation list. Defaults to FALSE.}
}
\value{
Returns a list
}
\description{
Generates summaries of QSM features (e.g. volume, surface area, dbh, etc.) by totals and branch order
}
\examples{

## TreeQSM Processing Chain
file <- system.file("extdata/QSM.mat", package = "rTwig")
qsm <- import_qsm(file)
cylinder <- qsm$cylinder
cylinder <- update_cylinders(cylinder)
qsm_summary(cylinder)

# TreeQSM Triangulation
file <- system.file("extdata/QSM.mat", package = "rTwig")
qsm <- import_qsm(file)
cylinder <- qsm$cylinder
cylinder <- update_cylinders(cylinder)
triangulation <- qsm$triangulation
qsm_summary(cylinder = cylinder, triangulation = triangulation)

## SimpleForest Processing Chain
file <- system.file("extdata/QSM.csv", package = "rTwig")
cylinder <- read.csv(file)
cylinder <- update_cylinders(cylinder)
qsm_summary(cylinder)

}
