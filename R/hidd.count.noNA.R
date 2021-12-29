#' @title hidd.count.noNA
#'
#' @description Calculate the number of non-NA values in a vector
#'
#' @param x a numeric vector
#' @return the number of non-NA values in a vector
#' @export
hidd.count.noNA <- function(x)
{
  return(sum(!is.na(x)))
}
