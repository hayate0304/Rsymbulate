% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/random_variables.R
\name{draw.RV}
\alias{draw.RV}
\title{A function that takes no arguments and returns a single
  realization of the random variable.}
\usage{
\method{draw}{RV}(self)
}
\description{
A function that takes no arguments and returns a single
  realization of the random variable.
}
\examples{
  X = RV(Normal(0, 1))
  X \%>\% draw() might return -0.9, for example.
}
