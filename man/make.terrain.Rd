% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MakeTerrain.R
\name{make.terrain}
\alias{make.terrain}
\title{Make an elevational grid with optional lakes}
\usage{
make.terrain(n, lakes = TRUE)
}
\arguments{
\item{n}{Size of grid will be a 2^n +1 grid (default = 4; a 17X17 grid)}

\item{lakes}{logical whether to make all terrain lower than -3
height underwater (default: TRUE)}
}
\value{
a terrain matrix; numeric elements indicate heights, and
NAs indicate cells filled with water
}
\description{
A light wrapper around \code{land.fill}
}
\examples{
big.terrain <- make.terrain(4)
image(big.terrain)
}
