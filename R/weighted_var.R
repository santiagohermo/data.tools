#' @title Weighted Variance
#'
#' @description
#' This function computes the weighted variance of a numeric vector.
#' It uses the weighted mean to do so.
#'
#' @param x A numeric vector.
#' @param w A numeric vector of weights.
#' @param na.rm A logical value indicating whether NA values in both `x` and `w` should be removed.
#'
#' @return The weighted variance.
#'
#' @examples
#'
#' x <- c(1, 2, 3, 4, 5)
#' w <- c(0.1, 0.2, 0.3, 0.2, 0.2)
#' weighted_var(x, w)
#'
#' @seealso \code{\link{var}}, \code{\link{mean}}, \code{\link{weighted.mean}}.
#'
#' @export
#'
weighted_var <- function(x, w, na.rm = FALSE) {

  if (!is.numeric(x) || !is.numeric(w)) {
    stop("TypeError: x and w must be numeric vectors")
  }

  if (!na.rm) {
    if (length(x) != length(w)) {
      stop("ValueError: x and w must have the same length")
    }
    if (any(w < 0)) {
      stop("ValueError: w must be non-negative")
    }
  } else {

    x <- x[!is.na(x)]
    w <- w[!is.na(w)]

    if (length(x) != length(w)) {
      stop("ValueError: x and w must have the same valid values after removing NAs")
    }
    if (any(w < 0)) {
      stop("ValueError: w must be non-negative")
    }
  }

  weighted_mean     <- sum(x * w) / sum(w)
  weighted_variance <- sum(w * (x - weighted_mean)^2) / sum(w)

  return(weighted_variance)
}
