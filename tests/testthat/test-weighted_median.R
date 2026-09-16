
test_that("returns the correct result for a basic example", {
  x <- c(1, 2, 3, 4, 5)
  w <- c(0.1, 0.2, 0.3, 0.2, 0.2)
  # Cumulative weights: 0.1, 0.3, 0.6, 0.8, 1.0
  # The first value where cumulative weight >= 0.5 is 3.
  expected_result <- 3
  
  result <- weighted_median(x, w)
  
  expect_equal(result, expected_result)
})

test_that("returns the correct result with a heavily weighted value", {
  x <- c(10, 20, 30, 40, 50)
  w <- c(1, 2, 1, 5, 1) # Total weight is 10.
  # Sorted x: 10, 20, 30, 40, 50
  # Sorted w: 1,  2,  1,  5,  1
  # Cum. norm. w: 0.1, 0.3, 0.4, 0.9, 1.0
  # The first value where cumulative weight >= 0.5 is 40.
  expected_result <- 40
  
  result <- weighted_median(x, w)
  
  expect_equal(result, expected_result)
})


test_that("returns an error when vectors have different lengths", {
  x <- c(1, 2, 3, 4, 5)
  w <- c(0.1, 0.2, 0.3, 0.2)
  
  expect_error(weighted_median(x, w), "ValueError: x and w must have the same length")
})

test_that("returns an error when any weight is negative", {
  x <- c(1, 2, 3, 4, 5)
  w <- c(0.1, 0.2, 0.3, -0.2, 0.2)
  
  expect_error(weighted_median(x, w), "ValueError: weights w must be non-negative")
})

test_that("returns NA when na.rm = FALSE and NAs are present", {
  x <- c(1, 2, NA, 4, 5)
  w <- c(0.1, 0.2, 0.3, 0.2, 0.2)
  
  result <- weighted_median(x, w, na.rm = FALSE)
  
  expect_equal(result, NA_real_)
})

test_that("correctly computes when NAs are in x and na.rm = TRUE", {
  x <- c(1, 2, NA, 4, 5)
  w <- c(0.1, 0.2, 0.3, 0.2, 0.2)
  
  # After removing NA, x is c(1,2,4,5) and w is c(0.1,0.2,0.2,0.2)
  # Sorted x: 1, 2, 4, 5
  # Sorted w: 0.1, 0.2, 0.2, 0.2
  # Cum. norm. w: 0.142, 0.428, 0.714, 1.0
  # The first value where cumulative weight >= 0.5 is 4.
  expected_result <- 4
  
  result <- weighted_median(x, w, na.rm = TRUE)
  
  expect_equal(result, expected_result)
})

test_that("correctly computes when NAs are in w and na.rm = TRUE", {
  x <- c(1, 2, 3, 4, 5)
  w <- c(0.1, 0.2, NA, 0.2, 0.2)
  
  # After removing NA, x is c(1,2,4,5) and w is c(0.1,0.2,0.2,0.2)
  # Same expectation as the previous test.
  expected_result <- 4

  result <- weighted_median(x, w, na.rm = TRUE)
  
  expect_equal(result, expected_result)
})

test_that("returns NA_real_ for empty vectors", {
  x <- numeric(0)
  w <- numeric(0)
  
  expect_equal(weighted_median(x, w), NA_real_)
})

test_that("returns NA_real_ if all weights are zero", {
    x <- c(1, 2, 3)
    w <- c(0, 0, 0)
    
    expect_equal(weighted_median(x, w), NA_real_)
})

test_that("handles a single element vector", {
    x <- c(42)
    w <- c(10)
    
    expect_equal(weighted_median(x, w), 42)
})

test_that("reduces to the unweighted median with equal weights", {
  for (n in 2:8) {
    x <- seq_len(n)
    expect_equal(weighted_median(x, rep(1, n)), median(x))
  }
})

test_that("averages the straddling values when the weights cross exactly at 0.5", {
  # Cumulative normalised weights: 0.25, 0.5, 1.0 -> straddles 2 and 3.
  expect_equal(weighted_median(c(1, 2, 3), c(1, 1, 2)), 2.5)

  # Cumulative normalised weights: 0.25, 0.5, 0.75, 1.0 -> straddles 20 and 30.
  expect_equal(weighted_median(c(10, 20, 30, 40), c(2, 2, 2, 2)), 25)
})

test_that("does not average when the crossing is strictly above 0.5", {
  # Cumulative normalised weights: 0.25, 0.75, 1.0 -> the median is 2.
  expect_equal(weighted_median(c(1, 2, 3), c(1, 2, 1)), 2)
})

test_that("reports mismatched lengths even when na.rm = TRUE", {
  expect_error(weighted_median(c(1, 2, 3, 4), c(1, 2), na.rm = TRUE),
               "ValueError: x and w must have the same length")
})

test_that("returns NA when na.rm = FALSE and NAs are present in w", {
  expect_equal(weighted_median(c(1, 2, 3), c(1, 1, NA)), NA_real_)
})
