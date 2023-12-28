#' @title Check Key in Data Table
#'
#' @description
#' This function checks if a given key (or keys) is present in the data object and uniquely identifies each observation.
#' If the conditions are met, it reorders the data table with the key columns first, otherwise, it stops with an error message.
#'
#' @param dt A data object.
#' @param key A character vector specifying the key(s).
#'
#' @return A reordered data.table object if the key is valid, otherwise an error is thrown.
#'
#' @examples
#'
#' dt <- data.table::data.table(unit = c("A", "A", "B", "C", "B", "C"), 
#'                              time = c(1, 2, 1, 1, 2, 2),
#'                              y    = c(3, 2, 3, 2, 3, 4))
#' 
#' dt <- check_key(dt, c("unit", "time"))
#' dt
#'
#' @importFrom data.table setorderv setDT is.data.table
#' @importFrom and and
#' @importFrom methods as
#' @importFrom stats quantile
#' 
#' @export
#'
check_key <- function(dt, key) {

  # Check inputs
  if (!is.data.table(dt)) {
    dt_format <- class(dt)
    data.table::setDT(dt)
  }
  if (!is.character(key)) {
    stop("TypeError: `key` must be a character vector.")
  }

  if (!any(key %in% names(dt))) {
    missing_keys <- key[!(key %in% names(dt))]
    if (length(missing_keys) == 1) {
      var = "variable"
      is  = "is"
    } else {
      var = "variables"
      is  = "are"
    }

    stop(paste0("KeyError: Key ", var, " ",
                and(missing_keys), " ", is, " not present in the data table."))

  } else if (nrow(dt) != nrow(unique(dt[, .SD, .SDcols = key]))) {
    stop("KeyError: Key variables do not uniquely identify observations.")

  } else {
    data.table::setorderv(dt, key)
    reordered_colnames <- c(key, setdiff(names(dt), key))
    dt <- dt[, reordered_colnames, with=FALSE]
    
    # Change back to original format
    if (exists("dt_format")) {
      dt <- as(dt, dt_format)
    }

    return(dt)
  }
}
