#' @title hidd.replace
#'
#' @description Replace all values within a vector with NA values if the sum of its non-NA values is below a threshold.
#'
#' @param x a numeric vector
#' @param N.min.NA a numeric threshold
#' @return the vector x
#' @export
hidd.replace <- function(x,N.min.NA)
{
  if(sum(!is.na(x)) < N.min.NA)
  {
    x <- rep(NA_real_,length(x))
  }

  return(x)
}
