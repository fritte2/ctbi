% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hidd.median.R
\name{hidd.median}
\alias{hidd.median}
\title{hidd.median}
\usage{
hidd.median(x, N.min.NA)
}
\arguments{
\item{x}{a numeric vector}

\item{N.min.NA}{a numeric threshold}
}
\value{
a numeric (either NA or the median of x)
}
\description{
Calculate the median of a vector if the number of its non-NA values is above a threshold. Otherwise, return NA.
}
