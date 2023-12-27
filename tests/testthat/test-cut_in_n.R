test_that("Function errors on non-numeric input", {
  expect_error(cut_in_n("not numeric", n = 3))
})

test_that("Function errors if breaks are not unique", {
  expect_error(cut_in_n(c(1,2,1,1,1,1,1,2), n = 5))
})

test_that("Function errors on non-integer or non-positive 'n'", {
  expect_error(cut_in_n(c(1, 2, 3), n = "not integer"))
  expect_error(cut_in_n(c(1, 2, 3), n = -1))
  expect_error(cut_in_n(c(1, 2, 3), n = 0))
})

test_that("Function handles NA values based on na.rm parameter", {
  expect_length(cut_in_n(c(1, NA, 3), n = 2, na.rm = TRUE), 3)
  expect_error(cut_in_n(c(1, NA, 3), n = 2, na.rm = FALSE))
})

test_that("Function divides data into 'n' groups", {
  result <- cut_in_n(c(1, 2, 3, 4), n = 2)
  expect_equal(length(unique(result)), 2)
})

test_that("Function works with a standard dataset", {
  data(iris)
  result <- cut_in_n(iris$Sepal.Length, n = 3)
  expect_equal(length(unique(result)), 3)
})
