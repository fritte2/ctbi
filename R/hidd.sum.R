#' @title hidd.sum
#'
#' @description Calculate the sum of a vector if the number of its non-NA values is above a threshold. Otherwise, return NA.
#'
#' @param x a numeric vector
#' @param N.min.NA a numeric threshold
#' @return a numeric (either NA or the sum of x)
#' @export
hidd.sum <- function(x,N.min.NA)
{
  if(sum(!is.na(x)) >= N.min.NA)
  {
    y <- sum(x,na.rm=TRUE)+sum(is.na(x))*mean(x,na.rm=TRUE)
  }else
  {
    y <- NA_real_
  }

  return(y)
}
