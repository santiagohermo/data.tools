
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
  
  expect_error(weighted_median(x, w), "ValueError: x and w must have the same length after handling NAs.")
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
