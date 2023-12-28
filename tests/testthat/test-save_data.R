library(data.table)
library(haven)
library(fst)
library(arrow)

test_that("save_data correctly saves data table in CSV format", {
  
  dt <- data.table(id = c(1, 2, 3, 4),
                    y  = c(3.2, 3.8, 4.5, 2.9),
                    x  = c(5.1, 6.3, 7.0, 5.4))

  save_data(dt, key = "id", outfile = "dt_test.csv")
  saved_data <- fread("dt_test.csv")
  for (rr in names(dt)) {
    expect_equal(saved_data[[rr]], dt[[rr]])
  }
  file.remove("dt_test.csv")
})

test_that("save_data correctly saves data table in CSV format using uppercase", {
  
  dt <- data.table(id = c(1, 2, 3, 4),
                    y  = c(3.2, 3.8, 4.5, 2.9),
                    x  = c(5.1, 6.3, 7.0, 5.4))
  
  save_data(dt, key = "id", outfile = "dt_test.CSV")
  saved_data <- fread("dt_test.csv")
  expect_true(file.exists("dt_test.CSV"))
  file.remove("dt_test.csv")
})

test_that("save_data correctly saves data table in DTA format", {
  
  dt <- data.table(id = c(1, 2, 3, 4),
                    y  = c(3.2, 3.8, 4.5, 2.9),
                    x  = c(5.1, 6.3, 7.0, 5.4))
  
  save_data(dt, key = "id", outfile = "dt_test.dta")
  saved_data <- as.data.frame(read_dta("dt_test.dta"))
  for (rr in names(dt)) {
    expect_equal(as.vector(saved_data[[rr]]), dt[[rr]])
  }
  file.remove("dt_test.dta")
})

test_that("save_data correctly saves data table in FST format", {
  
  dt <- data.table(id = c(1, 2, 3, 4),
                    y  = c(3.2, 3.8, 4.5, 2.9),
                    x  = c(5.1, 6.3, 7.0, 5.4))
  
  save_data(dt, key = "id", outfile = "dt_test.fst")
  saved_data <- read_fst("dt_test.fst")
  for (rr in names(dt)) {
    expect_equal(saved_data[[rr]], dt[[rr]])
  }
  file.remove("dt_test.fst")
})

test_that("save_data correctly saves data table in RDS format", {
  
  dt <- data.table(id = c(1, 2, 3, 4),
                    y  = c(3.2, 3.8, 4.5, 2.9),
                    x  = c(5.1, 6.3, 7.0, 5.4))
  
  save_data(dt, key = "id", outfile = "dt_test.rds")
  saved_data <- readRDS("dt_test.rds")
  for (rr in names(dt)) {
    expect_equal(saved_data[[rr]], dt[[rr]])
  }
  file.remove("dt_test.rds")
})

test_that("save_data correctly saves data table in Feather format", {
  
  dt <- data.table(id = c(1, 2, 3, 4),
                    y  = c(3.2, 3.8, 4.5, 2.9),
                    x  = c(5.1, 6.3, 7.0, 5.4))
  
  save_data(dt, key = "id", outfile = "dt_test.feather")
  saved_data <- read_feather("dt_test.feather")
  for (rr in names(dt)) {
    expect_equal(saved_data[[rr]], dt[[rr]])
  }
  file.remove("dt_test.feather")
})

test_that("save_data correctly stops execution when directory does not exist", {
  
  dt <- data.table(id = c(1, 2, 3, 4),
                    y  = c(3.2, 3.8, 4.5, 2.9),
                    x  = c(5.1, 6.3, 7.0, 5.4))
  
  non_existent_dir <- "nonexistentdir"
  expect_error(save_data(dt, key = "id", outfile = file.path(non_existent_dir, "dt_test.csv")),
               paste0("Directory ", non_existent_dir, " does not exist in the current working directory."))
})

test_that("save_data correctly stops execution when incorrect format is provided", {
  
  dt <- data.table(id = c(1, 2, 3, 4),
                    y  = c(3.2, 3.8, 4.5, 2.9),
                    x  = c(5.1, 6.3, 7.0, 5.4))
  
  expect_error(save_data(dt, key = "id", outfile = "dt_test.xyz"),
               "Incorrect format. Only .csv, .dta, .fst, .feather, and .rds are allowed.")
})

test_that("save_data correctly stops when invalid key is provided", {
  
  dt <- data.table(id = c(1, 2, 3, 4),
                    y  = c(3.2, 3.8, 4.5, 2.9),
                    x  = c(5.1, 6.3, 7.0, 5.4))
  
  invalid_key <- "NonexistentKey"
  expect_error(save_data(dt, key = invalid_key, outfile = "dt_test.csv"),
               paste0("KeyError: Key variable ", invalid_key, " is not present in the data table."))
})
