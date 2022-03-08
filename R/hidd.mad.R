#' @title hidd.mad
#'
#' @description Calculate the mean absolute deviation (MAD) of a vector if the number of its non-NA values is above a threshold. Otherwise, return NA.
#'
#' @param x a numeric vector
#' @param N.min.NA a numeric threshold
#' @return a numeric (either NA or the MAD of x)
#' @export
hidd.mad <- function(x,N.min.NA)
{
  if(sum(!is.na(x)) >= N.min.NA)
  {
    y <- mad(x,na.rm=TRUE)
  }else
  {
    y <- NA_real_
  }

  return(y)
}
