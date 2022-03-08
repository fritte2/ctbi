#' @title hidd.sd
#'
#' @description Calculate the standard deviation of a vector if the number of its non-NA values is above a threshold. Otherwise, return NA.
#'
#' @param x a numeric vector
#' @param N.min.NA a numeric threshold
#' @return a numeric (either NA or the standard deviation of x)
#' @export
hidd.sd <- function(x,N.min.NA)
{
  if(sum(!is.na(x)) >= N.min.NA)
  {
    y <- sd(x,na.rm=TRUE)
  }else
  {
    y <- NA_real_
  }

  return(y)
}
