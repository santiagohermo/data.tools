#' @title Weighted Median
#'
#' @description
#' This function computes the weighted median of a numeric vector.
#'
#' @param x A numeric vector of values.
#' @param w A numeric vector of weights, corresponding to the values in x.
#' @param na.rm A logical value indicating whether NA values in `x` should be removed before computation. Defaults to FALSE.
#'
#' @return The weighted median value.
#'
#' @examples
#'
#' x <- c(1, 2, 3, 4, 5)
#' w <- c(0.1, 0.2, 0.3, 0.2, 0.2)
#' weighted_median(x, w)
#'
#' # Example with a heavily weighted value
#' values <- c(10, 20, 30, 40, 50)
#' weights <- c(1, 2, 1, 5, 1) 
#' weighted_median(values, weights)
#'
#' # Example with NA values
#' x_na <- c(1, 2, NA, 4, 5)
#' w_na <- c(0.1, 0.2, 0.3, 0.2, 0.2)
#' weighted_median(x_na, w_na, na.rm = TRUE)
#'
#' @seealso \code{\link{median}}, \code{\link{weighted.mean}}.
#'
#' @export
weighted_median <- function(x, w, na.rm = FALSE) {

  if (!is.numeric(x) || !is.numeric(w)) {
    stop("TypeError: x and w must be numeric vectors")
  }

  if (na.rm) {
    complete_cases <- !is.na(x) & !is.na(w)
    x <- x[complete_cases]
    w <- w[complete_cases]
  } else if (any(is.na(x)) || any(is.na(w))) {
    return(NA_real_)
  }
  
  if (length(x) != length(w)) {
    stop("ValueError: x and w must have the same length after handling NAs.")
  }

  if (length(x) == 0) {
    return(NA_real_)
  }

  if (any(w < 0)) {
    stop("ValueError: weights w must be non-negative")
  }
  
  if (sum(w) == 0) {
      return(NA_real_)
  }

  order_x <- order(x)
  x_sorted <- x[order_x]
  w_sorted <- w[order_x]
  
  w_normalized <- w_sorted / sum(w_sorted)
  cumulative_weights <- cumsum(w_normalized)
  median_value <- x_sorted[which.max(cumulative_weights >= 0.5)]
  
  return(median_value)
}
