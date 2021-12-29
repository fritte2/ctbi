#' @title hidd.count.NA
#'
#' @description Calculate the number of NA values in a vector
#'
#' @param x a numeric vector
#' @return the number of NA values in a vector
#' @export
hidd.count.NA <- function(x)
{
  return(sum(is.na(x)))
}
