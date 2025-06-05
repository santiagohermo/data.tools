#' @title Weighted Variance
#'
#' @description
#' This function computes the weighted variance of a numeric vector.
#' It uses the weighted mean to do so.
#' The formula implemented is:
#' \[
#' \text{Weighted Variance} = \frac{\sum w_i (x_i - \bar{x}_w)^2}{\sum w_i}
#' \]
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

  if (na.rm) {
    complete_cases <- !is.na(x) & !is.na(w)
    x <- x[complete_cases]
    w <- w[complete_cases]
  }

  if (length(x) != length(w)) {
    stop("ValueError: x and w must have the same length after handling NAs (if na.rm = TRUE)")
  }

  if (length(x) == 0) {
    if (na.rm && sum(!is.na(x_original) & !is.na(w_original)) == 0 && (length(x_original) > 0 || length(w_original) > 0)) {
        return(NA_real_)
    } else if (length(x_original) == 0 && length(w_original) == 0) {
        return(NA_real_)
    } else {
        return(NA_real_)
    }
  }


  if (any(w < 0, na.rm = TRUE)) {
    stop("ValueError: weights w must be non-negative")
  }

  if (sum(w) == 0) {
      if (all(x == x[1])) return(0)
      return(NaN) # Or NA_real_ depending on desired behavior for sum(w) = 0
  }

  weighted_mean   <- sum(x * w) / sum(w)

  if (length(x) == 1) {
      return(0)
  }

  weighted_variance <- sum(w * (x - weighted_mean)^2) / sum(w)

  return(weighted_variance)
}
