test_that("returns the correct result for a basic example", {
  x <- c(1, 2, 3, 4, 5)
  w <- c(0.1, 0.2, 0.3, 0.2, 0.2)

  wgt_mean     <- sum(x * w) / sum(w)
  wgt_variance <- sum(w * (x - wgt_mean)^2) / sum(w)
  expected_result <- wgt_variance

  result <- weighted_var(x, w)

  expect_equal(result, expected_result, tolerance = 1e-6)
})

test_that("returns an error when vectors have different lengths", {
  x <- c(1, 2, 3, 4, 5)
  w <- c(0.1, 0.2, 0.3, 0.2)

  expect_error(weighted_var(x, w))
})

test_that("returns an error when any value of the weights is negative", {
  x <- c(1, 2, 3, 4, 5)
  w <- c(0.1, 0.2, 0.3, -0.2, 0.2)

  expect_error(weighted_var(x, w))
})

test_that("returns the correct result with NAs common to both x and w", {
  x <- c(1, 2, NA, 4, 5)
  w <- c(0.1, 0.2, NA, 0.2, 0.2)

  x_noNA <- x[!is.na(x) & !is.na(w)]
  w_noNA <- w[!is.na(x) & !is.na(w)]

  wgt_mean     <- sum(x_noNA * w_noNA) / sum(w_noNA)
  wgt_variance <- sum(w_noNA * (x_noNA - wgt_mean)^2) / sum(w_noNA)
  expected_result <- wgt_variance

  result <- weighted_var(x, w, na.rm = TRUE)

  expect_equal(result, expected_result, tolerance = 1e-6)
})

test_that("returns an error when any value of number of NAs differs", {
  x <- c(1, 2, NA, 4, 5)
  w <- c(0.1, 0.2, 0.3, 0.2, 0.2)

  expect_error(weighted_var(x, w, na.rm = TRUE))
})
