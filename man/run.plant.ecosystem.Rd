% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Plants.R
\name{run.plant.ecosystem}
\alias{run.plant.ecosystem}
\title{Makes a matrix of character values with different plant species that have the ability
to survive, reproduce(spread), and compete with each other through time.}
\usage{
run.plant.ecosystem(terrain, timesteps = 50, seed.fracs = c(0.1, 0.1, 0.1,
  0.1), repro = c(0.4, 0.6), survive = c(0.6, 0.4),
  comp.mat = matrix(c(0.75, 0.25, 0.25, 0.75), nrow = 2, ncol = 2),
  names = NULL)
}
\arguments{
\item{terrain}{an elevational grid generated from \code{make.terrain} on which plant.matrix will be placed}

\item{timesteps}{number of times to loop the plant population to simulate time (default = 50). The third value in the plant.matrix array}

\item{seed.fracs}{random values to start off the first plant.matrix}

\item{repro}{a vector of probability values (0-1) equal to the length of the number of species}

\item{survive}{a vector of probability values (0-1) equal to the length of the number of species}

\item{comp.mat}{a matrix that has dimensions the same length as the number of species. Contains the probabilities of each species}

\item{names}{assigns names to the species (default "a" and "b")}
}
\value{
plants
}
\description{
A wrapper around \code{run.plant.ecosystem}
}
\examples{
plant.pop <- run.plant.ecosystem(terrain, timesteps=50, seed.fracs=c(0.1, 0.1, 0.1, 0.1),
repro=c(0.75,0.25), survive=c(0.25, .75), comp.mat=matrix(c(0.75,0.25,0.25,0.75), 2))
}
