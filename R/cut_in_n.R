#' @title Cut a numeric vector into n equal-sized groups
#'
#' @description
#' This function applies base R's `cut` and `quantile` functions to
#' cut a numeric vector into n equal-sized groups. It behaves similarly
#' to the `cut` function, with added functionality.
#'
#' @param x A numeric vector.
#' @param n An integer specifying the number of groups.
#' @param na.rm A logical value indicating whether NA values should be removed.
#'
#' @examples
#'
#' data(iris)
#' cut_in_n(iris$Sepal.Length, n = 3)
#'
#' @seealso \code{\link{cut}}, \code{\link{quantile}}.
#'
#' @export
#'
cut_in_n <- function(x, n = 4, na.rm = FALSE) {
  if (!is.numeric(x)) {
    stop("TypeError: x must be a numeric vector")
  }

  if (!is.numeric(n) || n != as.integer(n) || n <= 0) {
    stop("TypeError: n must be a positive integer")
  }

  n <- as.integer(n)

  breaks <- quantile(x, probs = 0:n/n, na.rm = na.rm)
  cut(x, breaks = breaks, labels = 1:n, include.lowest = TRUE)
}
