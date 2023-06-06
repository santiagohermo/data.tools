#' Generate a Log File with Summary Stats of Data Table
#'
#' This function generates a log file containing the summary of a given data table.
#' The summary includes mean, standard deviation, min and max values of numeric variables,
#' number of non-NA values, and class of all variables.
#' It also includes an MD5 hash of the data table and the given key.
#'
#' @param dt A data table.
#' @param key A key that identifies the unique combination of rows.
#' @param outfile The name of the file the data is being saved to.
#' @param logname The name of the log file.
#' @param replace Whether the function should append or replace an existing logfile. Defaults to TRUE.
#' @param mask_vars A vector of names of numeric variables to be excluded from the summary. Defaults to NULL.
#'
#' @return A message indicating successful log file generation.
#'
#' @examples
#' data(iris)
#' generate_log_file(iris, key = "123", outfile = "iris.csv", logname = "iris.log")
#'
#' @importFrom stats sd
#' @importFrom utils capture.output
#' @importFrom digest digest
#' @importFrom base file.exists
#' @importFrom stargazer stargazer
#' @export
generate_log_file <- function(dt, key, outfile, logname,
                              replace = TRUE, mask_vars = NULL) {

  for (col in names(dt)) {
    if (!is.null(attr(dt[[col]], "label"))) attr(dt[[col]], "label") <- NULL
  }

  numeric_vars <- names(dt)[sapply(dt, is.numeric)]
  if (!is.null(mask_vars)) {
    numeric_vars <- setdiff(numeric_vars, mask_vars)
  }

  all_sum <- t(rbind(dt[, lapply(.SD, function(vv) sum(!is.na(vv)))],
                     dt[, lapply(.SD, function(vv) class(vv))]))

  if (length(numeric_vars) > 0) {
    numeric_sum <- t(rbind(dt[, lapply(.SD, mean, na.rm = T), .SDcols = numeric_vars],
                           dt[, lapply(.SD, sd,   na.rm = T), .SDcols = numeric_vars],
                           dt[, lapply(.SD, min,  na.rm = T), .SDcols = numeric_vars],
                           dt[, lapply(.SD, max, na.rm = T), .SDcols = numeric_vars]))

    summary_table <- merge(numeric_sum, all_sum, by = "row.names", all = T)
    names(summary_table) <- c("variable", "mean", "sd", "min", "max", "N", "type")

  } else {

    summary_table <- all_sum
    names(summary_table) <- c("variable", "N", "type")
  }


  hash <- digest::digest(dt, algo = "md5")

  if (replace | !file.exists(logname)) {
    cat("=======================================================", "\n", file = logname, append = F)
  } else {
    cat("\n=======================================================", "\n", file = logname, append = T)
  }

  cat("File:", outfile, '\n', file = logname, append = T)
  cat("MD5: ", hash,     '\n', file = logname, append = T)
  cat("Key: ", key,      '\n', file = logname, append = T)

  s = capture.output(stargazer::stargazer(summary_table, summary = F, type = 'text'))
  cat(paste(s,"\n"), file = logname, append = T)

  return("Log file generated successfully.")
}