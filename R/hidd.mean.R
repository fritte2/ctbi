#' @title hidd.mean
#'
#' @description Calculate the mean of a vector if the number of its non-NA values is above a threshold. Otherwise, return NA.
#'
#' @param x a numeric vector
#' @param N.min.NA a numeric threshold
#' @return y a numeric (either NA or the mean of x)
#' @export
hidd.mean <- function(x,N.min.NA)
{
  if(sum(!is.na(x)) >= N.min.NA)
  {
    y <- mean(x,na.rm=TRUE)
  }else
  {
    y <- NA_real_
  }

  return(y)
}
