#' @title Negation of `%in%`
#' 
#' @description
#' This function is a negation of the %in% operator.
#'
#' @examples
#' 
#' c(1,2,5) %notin% c(2,4,5)
#' 
'%notin%' <- Negate(`%in%`)
