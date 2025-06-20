% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/weighted_var.R
\name{weighted_var}
\alias{weighted_var}
\title{Weighted Variance}
\usage{
weighted_var(x, w, na.rm = FALSE)
}
\arguments{
\item{x}{A numeric vector.}

\item{w}{A numeric vector of weights.}

\item{na.rm}{A logical value indicating whether NA values in both \code{x} and \code{w} should be removed.}
}
\value{
The weighted variance.
}
\description{
This function computes the weighted variance of a numeric vector.
It uses the weighted mean to do so.
The implementation does not apply a sample correction factor,
i.e., it does not divide by \code{sum(weights) - 1}.
}
\examples{

x <- c(1, 2, 3, 4, 5)
w <- c(0.1, 0.2, 0.3, 0.2, 0.2)
weighted_var(x, w)

}
\seealso{
\code{\link{var}}, \code{\link{mean}}, \code{\link{weighted.mean}}.
}
