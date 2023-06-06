library(testthat)
library(data.table)

test_that("check_key handles non-existing keys correctly", {

  dt <- data.table(A = c(1,2,3), B = c(4,5,6), C = c(7,8,9))

  expect_error(check_key(dt, c("D", "E")),
               "KeyError: Key variables D and E are not in the data table.")
})

test_that("check_key handles non-unique keys correctly", {

  dt_non_unique <- data.table(A = c(1,2,3,1), B = c(4,5,6,4), C = c(7,8,9,7))

  expect_error(check_key(dt_non_unique, c("A", "B")),
               "KeyError: Key variables do not uniquely identify observations.")
})

test_that("check_key handles valid keys correctly", {

  dt <- data.table(A = c(1,2,3), B = c(4,5,6), C = c(7,8,9))

  dt_reordered <- check_key(dt, c("A", "C"))

  expect_equal(names(dt_reordered), c("A", "C", "B"))
  expect_equal(nrow(unique(dt_reordered[, .SD, .SDcols = c("A", "B")])),
               nrow(dt_reordered))
})
