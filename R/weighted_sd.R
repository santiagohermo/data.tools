#' @title Weighted Standard Deviation
#'
#' @description
#' This function computes the weighted standard deviation of a numeric vector.
#' It uses the weighted mean to do so.
#'
#' @param x A numeric vector.
#' @param w A numeric vector of weights.
#' @param na.rm A logical value indicating whether NA values in both `x` and `w` should be removed.
#'
#' @return The weighted standard deviation.
#'
#' @examples
#'
#' x <- c(1, 2, 3, 4, 5)
#' w <- c(0.1, 0.2, 0.3, 0.2, 0.2)
#' weighted_sd(x, w)
#'
#' @seealso \code{\link{sd}}, \code{\link{mean}}, \code{\link{weighted.mean}}.
#' 
#' @export
#'
weighted_sd <- function(x, w, na.rm = FALSE) {
  
  if (length(x) != length(w)) {
      stop("ValueError: x and w must have the same length")
  }
  if (any(w < 0)) {
      stop("ValueError: w must be non-negative")
  }

  if (na.rm) {
    x <- x[!is.na(x) & !is.na(w)]
    w <- w[!is.na(x) & !is.na(w)]

    # Error if x is NA and w is not and vice versa
    if (any(is.na(x)) | any(is.na(w))) {
      stop("ValueError: x and w must have the same valid values after removing NAs")
    }
  }
  
  if (!is.numeric(x) || !is.numeric(w)) {
      stop("TypeError: x and w must be numeric vectors")
  }

  weighted_mean     <- sum(x * w) / sum(w)
  weighted_variance <- sum(w * (x - weighted_mean)^2) / sum(w)

  sqrt(weighted_variance)
}
