#' @title Weighted Variance
#'
#' @description
#' This function computes the weighted variance of a numeric vector.
#' It uses the weighted mean to do so.
#' By default no sample correction is applied, i.e., the weighted sum of squared
#' deviations is divided by `sum(w)`. Use `correction` to obtain a sample variance
#' under frequency or reliability (analytic) weights.
#'
#' @param x A numeric vector.
#' @param w A numeric vector of weights.
#' @param na.rm A logical value indicating whether observations with an NA in either `x` or `w` should be removed.
#' If FALSE (the default) and any NA is present, `NA_real_` is returned.
#' @param correction The sample correction applied to the denominator. One of `"none"` (the default,
#' divides by `sum(w)`), `"frequency"` (divides by `sum(w) - 1`, for weights that count repeated
#' observations), or `"reliability"` (divides by `sum(w) - sum(w^2) / sum(w)`, for analytic weights).
#' `NA_real_` is returned when the resulting denominator is not positive.
#'
#' @return The weighted variance.
#'
#' @examples
#'
#' x <- c(1, 2, 3, 4, 5)
#' w <- c(0.1, 0.2, 0.3, 0.2, 0.2)
#' weighted_var(x, w)
#'
#' # With frequency weights, matches the sample variance of the expanded vector
#' weighted_var(c(1, 2, 3), c(2, 3, 5), correction = "frequency")
#' var(rep(c(1, 2, 3), c(2, 3, 5)))
#'
#' @seealso \code{\link{var}}, \code{\link{mean}}, \code{\link{weighted.mean}}, \code{\link{weighted_sd}}.
#'
#' @export
#'
weighted_var <- function(x, w, na.rm = FALSE,
                         correction = c("none", "frequency", "reliability")) {

  correction <- match.arg(correction)

  # Validate the inputs before touching NAs, so that a length mismatch is
  # reported as such instead of being masked by recycling in the NA filter.
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

  weighted_mean <- sum(x * w) / sum(w)
  sum_squares   <- sum(w * (x - weighted_mean)^2)

  denominator <- switch(correction,
                        none        = sum(w),
                        frequency   = sum(w) - 1,
                        reliability = sum(w) - sum(w^2) / sum(w))

  if (denominator <= 0) {
    return(NA_real_)
  }

  weighted_variance <- sum_squares / denominator

  return(weighted_variance)
}
