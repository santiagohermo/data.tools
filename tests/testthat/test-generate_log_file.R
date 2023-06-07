library(data.table)

test_that("generate_log_file creates log file", {
  dt      <- data.table(iris)
  key     <- "123"
  outfile <- "test_iris.csv"
  logname <- "test_iris.log"

  generate_log_file(dt, key, outfile, logname)

  expect_true(file.exists(logname))

  unlink(logname)
})

test_that("generate_log_file creates log file with replace = FALSE", {
  dt      <- data.table(iris)
  key     <- "123"
  outfile <- "test_iris.csv"
  logname <- "test_iris.log"

  generate_log_file(dt, key, outfile, logname, replace = TRUE)
  first_md5 <- digest::digest(dt, algo = "md5")

  dt[, Sepal.Length := Sepal.Length * 2]
  generate_log_file(dt, key, outfile, logname, replace = FALSE)
  second_md5 <- digest::digest(dt, algo = "md5")

  log_contents <- readLines(logname)
  md5_lines <- grep("MD5", log_contents, value = TRUE)

  expect_true(length(md5_lines) == 2)
  expect_true(grepl(first_md5, md5_lines[1]))
  expect_true(grepl(second_md5, md5_lines[2]))

  unlink(logname)
})

test_that("generate_log_file masks variables correctly", {
  dt      <- data.table(iris)
  key     <- "123"
  outfile <- "test_iris.csv"
  logname <- "test_iris.log"
  mask_vars <- c("Sepal.Length", "Sepal.Width")

  generate_log_file(dt, key, outfile, logname, replace = TRUE, mask_vars = mask_vars)
  log_contents <- readLines(logname)

  expect_false(any(grepl("Sepal.Length\\s{1,4}\\d{1,4}", log_contents)))
  expect_false(any(grepl("Sepal.Width\\s{1,4}\\d{1,4}", log_contents)))
  expect_true(any(grepl("Petal.Length\\s{1,4}\\d{1,4}", log_contents)))
  expect_true(any(grepl("Petal.Width\\s{1,4}\\d{1,4}", log_contents)))

  unlink(logname)
})
