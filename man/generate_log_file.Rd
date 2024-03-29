% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_log_file.R
\name{generate_log_file}
\alias{generate_log_file}
\title{Generate a log file with summary stats of data table}
\usage{
generate_log_file(dt, key, outfile, logname, replace = TRUE, mask_vars = NULL)
}
\arguments{
\item{dt}{A data table.}

\item{key}{A key that identifies the unique combination of rows.}

\item{outfile}{The name of the file the data is being saved to.}

\item{logname}{The name of the log file.}

\item{replace}{Whether the function should append or replace an existing logfile. Defaults to TRUE.}

\item{mask_vars}{A vector of names of numeric variables to be excluded from the summary. Defaults to NULL.}
}
\value{
A message indicating successful log file generation.
}
\description{
This function generates a log file containing the summary of a given data table,
including mean, standard deviation, min and max, number of non-NA values, and class of all variables.
It also includes an MD5 hash of the data table and the given key.
}
\examples{

dt <- data.table::data.table(unit = c("A", "A", "B", "C", "B", "C"), 
                             time = c(1, 2, 1, 1, 2, 2),
                             y    = c(3, 2, 3, 2, 3, 4))

generate_log_file(dt, key = c("unit", "time"), 
                  outfile = "data.csv", 
                  logname = "data.log")

}
