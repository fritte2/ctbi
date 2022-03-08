#' @title hidd.median
#'
#' @description Calculate the median of a vector if the number of its non-NA values is above a threshold. Otherwise, return NA.
#'
#' @param x a numeric vector
#' @param N.min.NA a numeric threshold
#' @return a numeric (either NA or the median of x)
#' @export
hidd.median <- function(x,N.min.NA)
{
  if(sum(!is.na(x)) >= N.min.NA)
  {
    y <- median(x,na.rm=TRUE)
  }else
  {
    y <- NA_real_
  }

  return(y)
}
