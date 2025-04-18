% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/smooth_qsm.R
\name{smooth_qsm}
\alias{smooth_qsm}
\title{Smooth QSM}
\usage{
smooth_qsm(cylinder)
}
\arguments{
\item{cylinder}{QSM cylinder data frame}
}
\value{
Returns a data frame
}
\description{
Visually smooth a QSM by ensuring the midpoints of all cylinders
are connected. Only TreeQSM is supported.
}
\examples{

file <- system.file("extdata/QSM.mat", package = "rTwig")
qsm <- import_treeqsm(file)
cylinder <- qsm$cylinder
cylinder <- update_cylinders(cylinder)

## Before Smoothing
plot_qsm(cylinder)

## After Smoothing
cylinder <- smooth_qsm(cylinder)
plot_qsm(cylinder)

}
