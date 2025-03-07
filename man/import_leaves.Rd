% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_leaves.R
\name{import_leaves}
\alias{import_leaves}
\title{Import Leaves}
\usage{
import_leaves(filename, format = "obj", simplify = FALSE)
}
\arguments{
\item{filename}{a QSM-FaNNI .obj file}

\item{format}{.obj file format. Valid formats include \code{obj} and \code{obj_ext}.
Defaults to \code{obj}.}

\item{simplify}{simplify the mesh by removing duplicate vertices. This can
reduce memory usage at the cost of import speed. Defaults to FALSE.}
}
\value{
rgl::mesh3d object
}
\description{
Imports leaves created by the QSM foliage and needles naive
insertion (QSM-FaNNI) algorithm. The leaves are stored as a triangular mesh,
compatible with both rTwig and RGL functions.
}
\examples{
\dontrun{

## Import FaNNI leaves from the standard obj format
file <- "path_to_leaves.obj"
qsm <- import_leaves(file, format = "obj")

## Import FaNNI leaves from the extended obj format
file <- "path_to_leaves_extended.obj"
qsm <- import_leaves(file, format = "obj_ext")
}

}
\references{
\insertRef{FaNNI}{rTwig}
}
