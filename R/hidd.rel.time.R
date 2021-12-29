#' @title hidd.rel.time
#'
#' @description calculate the relative position of each timestep of a vector with respect to the bin boundaries.
#'
#' @param x a vector (numeric, POSIXct or Date)
#' @param seq.bin.side the sequence of bin sides
#' @return the relative position of each value of x with respect to the bins in seq.bin.side (between 0 and 1)
#' @export
hidd.rel.time <- function(x,seq.bin.side)
{
  x1 <- as.numeric(max(seq.bin.side[seq.bin.side <= x[1]]))
  x2 <- as.numeric(min(seq.bin.side[x[length(x)] < seq.bin.side]))

  r.out <- (as.numeric(x)-x1)/(x2-x1)
  return(r.out)
}
