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

test_that("correct computation when missing in one vector is present", {
  x <- c(1, 2, NA, 4, 5)
  w <- c(0.1, 0.2, 0.3, 0.2, 0.2)

  x_noNA <- x[!is.na(x)]
  w_noNA <- w[!is.na(x)]

  wgt_mean     <- sum(x_noNA * w_noNA) / sum(w_noNA)
  wgt_variance <- sum(w_noNA * (x_noNA - wgt_mean)^2) / sum(w_noNA)
  expected_result <- wgt_variance

  result <- weighted_var(x, w, na.rm = TRUE)

  expect_equal(result, expected_result, tolerance = 1e-6)
})

test_that("returns NA when na.rm = FALSE and NAs are present in w", {
  expect_equal(weighted_var(c(1, 2, 3), c(1, 1, NA)), NA_real_)
  expect_equal(weighted_var(c(1, 2, NA), c(1, 1, 1)), NA_real_)
})

test_that("reports mismatched lengths even when na.rm = TRUE", {
  x <- c(1, 2, 3, 4)
  w <- c(1, 2)

  expect_error(weighted_var(x, w, na.rm = TRUE),
               "ValueError: x and w must have the same length")
})

test_that("returns NA_real_ if all weights are zero", {
  expect_equal(weighted_var(c(1, 2, 3), c(0, 0, 0)), NA_real_)
  expect_equal(weighted_var(c(2, 2, 2), c(0, 0, 0)), NA_real_)
})

test_that("returns NA_real_ for empty vectors", {
  expect_equal(weighted_var(numeric(0), numeric(0)), NA_real_)
})

test_that("frequency correction matches the sample variance of the expanded vector", {
  x <- c(1, 2, 3)
  w <- c(2, 3, 5)

  expect_equal(weighted_var(x, w, correction = "frequency"), var(rep(x, w)))
})

test_that("reliability correction divides by the effective sample size", {
  x <- c(1, 2, 3, 4)
  w <- c(0.1, 0.2, 0.3, 0.4)

  wgt_mean <- sum(x * w) / sum(w)
  expected <- sum(w * (x - wgt_mean)^2) / (sum(w) - sum(w^2) / sum(w))

  expect_equal(weighted_var(x, w, correction = "reliability"), expected)
})

test_that("returns NA_real_ when a correction leaves a non-positive denominator", {
  expect_equal(weighted_var(42, 1, correction = "frequency"), NA_real_)
  expect_equal(weighted_var(42, 1, correction = "reliability"), NA_real_)
})

test_that("a single observation has zero population variance", {
  expect_equal(weighted_var(42, 10), 0)
})

test_that("rejects an unknown correction", {
  expect_error(weighted_var(c(1, 2, 3), c(1, 1, 1), correction = "bogus"))
})
