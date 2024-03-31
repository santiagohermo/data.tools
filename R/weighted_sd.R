#' @title Weighted Standard Deviation
#'
#' @description
#' This function computes the weighted standard deviation of a numeric vector.
#' It leverages the `weighted_var` function to compute the variance and then
#' takes the square root.
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
#' @seealso \code{\link{sd}}, \code{\link{var}}, \code{\link{mean}}, \code{\link{weighted.mean}}, \code{\link{weighted_var}}.
#'
#' @export
#'
weighted_sd <- function(x, w, na.rm = FALSE) {
  
  sqrt(weighted_var(x, w, na.rm))
  
}
