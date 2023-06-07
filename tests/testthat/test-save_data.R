library(data.table)
library(haven)
library(fst)
library(arrow)

dt <- data.table(id = c(1, 2, 3, 4),
                 y  = c(3.2, 3.8, 4.5, 2.9),
                 x  = c(5.1, 6.3, 7.0, 5.4))

test_that("save_data correctly saves data table in CSV format", {
  save_data(dt, key = "id", outfile = "iris.csv")
  saved_data <- fread("iris.csv")
  for (rr in names(dt)) {
    expect_equal(saved_data[[rr]], dt[[rr]])
  }
  file.remove("iris.csv")
})

test_that("save_data correctly saves data table in CSV format using uppercase", {
  save_data(dt, key = "id", outfile = "iris.CSV")
  saved_data <- fread("iris.csv")
  expect_true(file.exists("iris.CSV"))
  file.remove("iris.csv")
})

test_that("save_data correctly saves data table in DTA format", {
  save_data(dt, key = "id", outfile = "iris.dta")
  saved_data <- as.data.frame(read_dta("iris.dta"))
  for (rr in names(dt)) {
    expect_equal(as.vector(saved_data[[rr]]), dt[[rr]])
  }
  file.remove("iris.dta")
})

test_that("save_data correctly saves data table in FST format", {
  save_data(dt, key = "id", outfile = "iris.fst")
  saved_data <- read_fst("iris.fst")
  for (rr in names(dt)) {
    expect_equal(saved_data[[rr]], dt[[rr]])
  }
  file.remove("iris.fst")
})

test_that("save_data correctly saves data table in RDS format", {
  save_data(dt, key = "id", outfile = "iris.rds")
  saved_data <- readRDS("iris.rds")
  for (rr in names(dt)) {
    expect_equal(saved_data[[rr]], dt[[rr]])
  }
  file.remove("iris.rds")
})

test_that("save_data correctly saves data table in Feather format", {
  save_data(dt, key = "id", outfile = "iris.feather")
  saved_data <- read_feather("iris.feather")
  for (rr in names(dt)) {
    expect_equal(saved_data[[rr]], dt[[rr]])
  }
  file.remove("iris.feather")
})

test_that("save_data correctly stops execution when directory does not exist", {
  non_existent_dir <- "nonexistentdir"
  expect_error(save_data(dt, key = "id", outfile = file.path(non_existent_dir, "iris.csv")),
               paste0("Directory ", non_existent_dir, " does not exist in the current working directory."))
})

test_that("save_data correctly stops execution when incorrect format is provided", {
  expect_error(save_data(dt, key = "id", outfile = "iris.xyz"),
               "Incorrect format. Only .csv, .dta, .fst, .feather, and .rds are allowed.")
})

test_that("save_data correctly stops when invalid key is provided", {
  invalid_key <- "NonexistentKey"
  expect_error(save_data(dt, key = invalid_key, outfile = "iris.csv"),
               paste0("KeyError: Key variable ", invalid_key, " is not present in the data table."))
})
