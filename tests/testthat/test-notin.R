test_that("Numeric vectors are correctly identified as not in", {
  expect_equal((1:5) %notin% (4:6), c(TRUE, TRUE, TRUE, FALSE, FALSE))
})

test_that("Character vectors are correctly identified as not in", {
  expect_equal(letters[1:5] %notin% letters[3:7], c(TRUE, TRUE, FALSE, FALSE, FALSE))
})
