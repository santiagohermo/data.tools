#' @title Weighted Median
#'
#' @description
#' This function computes the weighted median of a numeric vector.
#' When the cumulative weight lands exactly on 0.5 the two straddling values are
#' averaged, so that with equal weights the result matches \code{\link{median}}.
#'
#' @param x A numeric vector of values.
#' @param w A numeric vector of weights, corresponding to the values in x.
#' @param na.rm A logical value indicating whether observations with an NA in either `x` or `w` should be
#' removed before computation. Defaults to FALSE, in which case any NA leads to `NA_real_`.
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
#' # With equal weights it reduces to the unweighted median
#' weighted_median(c(1, 2, 3, 4), rep(1, 4))
#' median(c(1, 2, 3, 4))
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

  # Validate the inputs before touching NAs
  if (!is.numeric(x) || !is.numeric(w)) {
    stop("TypeError: x and w must be numeric vectors")
  }
  if (length(x) != length(w)) {
    stop("ValueError: x and w must have the same length")
  }
  if (any(w < 0, na.rm = TRUE)) {
    stop("ValueError: weights w must be non-negative")
  }

  if (na.rm) {
    complete_cases <- !is.na(x) & !is.na(w)
    x <- x[complete_cases]
    w <- w[complete_cases]
  } else if (any(is.na(x)) || any(is.na(w))) {
    return(NA_real_)
  }
  
  if (length(x) == 0) {
    return(NA_real_)
  }

  if (sum(w) == 0) {
      return(NA_real_)
  }

  order_x  <- order(x)
  x_sorted <- x[order_x]
  w_sorted <- w[order_x]
  
  cumulative_weights <- cumsum(w_sorted) / sum(w_sorted)
  idx                <- which.max(cumulative_weights >= 0.5)

  # An exact crossing at 0.5 leaves two values straddling the median; average
  # them, mirroring how median() averages the two middle values for even n.
  if (idx < length(x_sorted) && isTRUE(all.equal(cumulative_weights[idx], 0.5))) {
    return(mean(x_sorted[c(idx, idx + 1)]))
  }

  median_value <- x_sorted[idx]
  
  return(median_value)
}
