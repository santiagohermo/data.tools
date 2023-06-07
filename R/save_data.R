#' Save Data Table to Various Formats with Logging
#'
#' This function saves a data table to various formats including .csv, .dta, .fst, .feather, and .RDS.
#' It also generates a log file containing the summary of a given data table by calling the `generate_log_file` function.
#'
#' @param dt A data table.
#' @param key A key that identifies the unique combination of rows.
#' @param outfile The name of the output file.
#' @param logfile The name of the log file. If NULL, a log file with name `data_file_manifest.log` is created in the same directory as `outfile`.
#' Deafults to NULL. Set to FALSE to not create a log file.
#' @param replacelog A boolean flag to control whether the generated log file replaces an existing logfile or not. Deafults to TRUE.
#' @param mask_vars A vector of names of numeric variables to be excluded from the summary in the log file. Defaults to NULL.
#' @param compress_opt The compression level for fst files. Defaults to 50.
#'
#' @return A message indicating successful saving of the data file.
#'
#' @examples
#' data(iris)
#' save_data(iris, key = "Species", outfile = "iris.csv")
#'
#' @importFrom data.table fwrite setDT
#' @importFrom haven write_dta
#' @importFrom fst write_fst
#' @importFrom arrow write_feather
#' @export
#'
save_data <- function(dt, key, outfile,
                      logfile = NULL, replacelog = TRUE,
                      mask_vars = NULL, compress_opt = 50) {

  filename <- base::basename(outfile)
  dir      <- base::dirname(outfile)

  if (!dir.exists(dir)) {
    stop(paste0("Directory ", dir, " does not exist in the current working directory."))
  }
  if (!is.null(logfile)) {
    log_dir <- base::dirname(logfile)

    if (!dir.exists(log_dir)) {
      stop(paste0("Log file directory ", dir, " does not exist in the current working directory."))
    }
  }

  data.table::setDT(dt)

  dt <- check_key(dt, key)

  name_parts <- strsplit(filename, ".", fixed = TRUE)[[1]]
  filetype   <- name_parts[length(name_parts)]

  if (tolower(filetype) == "csv") {

    data.table::fwrite(dt, file = outfile)
  } else if (tolower(filetype) == "dta") {

    haven::write_dta(dt, outfile)
  } else if (tolower(filetype) == "rds") {

    base::saveRDS(dt, outfile)
  } else if (tolower(filetype) == "fst") {

    fst::write_fst(dt, outfile, compress = compress_opt)
  } else if (tolower(filetype) == "feather") {

    arrow::write_feather(dt, outfile)
  } else {
    stop("Incorrect format. Only .csv, .dta, .fst, .feather, and .rds are allowed.")
  }

  message(paste0("File '", outfile, "' saved successfully."))

  create_log = TRUE
  if (!is.null(logfile)) {
    if (logfile == FALSE) create_log = FALSE
  }

  if (create_log) {
    if (is.null(logfile)) {
      logfile <- file.path(dir, "data_file_manifest.log")
    }

    generate_log_file(dt, key, outfile, logfile, replacelog, mask_vars)
  }
}
