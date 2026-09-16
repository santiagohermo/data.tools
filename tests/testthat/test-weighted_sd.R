test_that("is the square root of the weighted variance", {
  x <- c(1, 2, 3, 4, 5)
  w <- c(0.1, 0.2, 0.3, 0.2, 0.2)

  expect_equal(weighted_sd(x, w), sqrt(weighted_var(x, w)))
})

test_that("matches the population standard deviation with equal weights", {
  x <- c(1, 2, 3, 4, 5)

  expect_equal(weighted_sd(x, rep(1, length(x))), sd(x) * sqrt(4 / 5))
})

test_that("forwards the correction to weighted_var", {
  x <- c(1, 2, 3)
  w <- c(2, 3, 5)

  expect_equal(weighted_sd(x, w, correction = "frequency"), sd(rep(x, w)))
})

test_that("forwards na.rm to weighted_var", {
  x <- c(1, 2, NA, 4, 5)
  w <- c(0.1, 0.2, 0.3, 0.2, 0.2)

  expect_equal(weighted_sd(x, w), NA_real_)
  expect_equal(weighted_sd(x, w, na.rm = TRUE), sqrt(weighted_var(x, w, na.rm = TRUE)))
})

test_that("propagates the input validation of weighted_var", {
  expect_error(weighted_sd(c(1, 2, 3), c(1, 2)),
               "ValueError: x and w must have the same length")
  expect_error(weighted_sd(c(1, 2, 3), c(1, 1, -1)),
               "ValueError: weights w must be non-negative")
})

test_that("returns NA_real_ if all weights are zero", {
  expect_equal(weighted_sd(c(1, 2, 3), c(0, 0, 0)), NA_real_)
})
