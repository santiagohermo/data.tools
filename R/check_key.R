#' Check Key in Data Table
#'
#' This function checks if a given key (or keys) is present in the data.table object and uniquely identifies each observation.
#' If the conditions are met, it reorders the data table with the key columns first, otherwise, it stops with an error message.
#'
#' @param dt A data.table object.
#' @param key A character vector specifying the key(s).
#'
#' @return A reordered data.table object if the key is valid, otherwise an error is thrown.
#'
#' @examples
#'
#' dt <- data.table::data.table(A = c(1,2,3), B = c(4,5,6), C = c(7,8,9))
#' check_key(dt, c("A", "B"))
#'
#' @importFrom data.table setorderv
#' @importFrom and and
#' @export
#'
check_key <- function(dt, key) {

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
    dt <- dt[, ..reordered_colnames]
    return(dt)
  }
}
