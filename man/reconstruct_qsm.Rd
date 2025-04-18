% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reconstruct_qsm().R
\name{reconstruct_qsm}
\alias{reconstruct_qsm}
\title{Reconstruct QSM}
\usage{
reconstruct_qsm(
  cylinder,
  id,
  parent,
  radius,
  start_x,
  start_y,
  start_z,
  end_x,
  end_y,
  end_z,
  branch = NULL,
  branch_order = NULL
)
}
\arguments{
\item{cylinder}{QSM cylinder data frame}

\item{id}{cylinder ids}

\item{parent}{cylinder parent ids}

\item{radius}{cylinder radius}

\item{start_x}{cylinder start x position}

\item{start_y}{cylinder start y position}

\item{start_z}{cylinder start z position}

\item{end_x}{cylinder end x position}

\item{end_y}{cylinder end y position}

\item{end_z}{cylinder end z position}

\item{branch}{optional cylinder branch id}

\item{branch_order}{optional cylinder branch order}
}
\value{
data frame
}
\description{
Reconstruct a QSM and all of its variables from the minimum amount of
required data. The required variables are the cylinder id, parent id,
3d information (e.g. a combination of start and end points, or start, axis,
and length), and the radius. Branch and branch order information are
optional. If not provided, the branches and orders are defined recursively
using the growth length.
}
\examples{
file <- system.file("extdata/QSM.csv", package = "rTwig")
qsm <- reconstruct_qsm(
  cylinder = read.csv(file),
  id = "ID", parent = "parentID", radius = "radius",
  start_x = "startX", start_y = "startY", start_z = "startZ",
  end_x = "endX", end_y = "endY", end_z = "endZ"
)
plot_qsm(qsm)

}
