% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hidd.replace.R
\name{hidd.replace}
\alias{hidd.replace}
\title{hidd.replace}
\usage{
hidd.replace(x, N.min.NA)
}
\arguments{
\item{x}{a numeric vector}

\item{N.min.NA}{a numeric threshold}
}
\value{
the vector x
}
\description{
Replace all values within a vector with NA values if the sum of its non-NA values is below a threshold.
}
