## global variables
globalVariables(c(":=","side.index","index"))

#' @title ctbi.long.term
#'
#' @description Calculate the long-term trend with a linear interpolation between the mean (or median) of bins defined between two consecutive centers. Bins defined between two consecutive sides are calculated as well to complete for missing values if they have neighbors. Bins without sufficient data are discarded.
#'
#' @param data0 data.table with the columns x (time series), y (values), side.index (index associated with each bin defined between two consecutive centers) and index.bin (index associated with each bin defined between two consecutive sides)
#' @param n.bin.min minimum number of points for a bin to be accepted
#' @param seq.bin.side sequence of the sides of the bins
#' @param outliers.checked boolean to indicate if the median (outliers.checked = FALSE) or the mean (outliers.checked = TRUE) should be used to calculate the long-term trend
#'
#' @return data0 with the long-term trend added (column long.term)
#' @examples
#' library(data.table)
#' x <- seq(from=as.Date('2001-01-01'),to=as.Date('2010-12-01'),by='1 month')
#' y <- 3*cos(2*pi*(0:(length(x)-1))/12)+runif(length(x))
#' outliers.checked <- TRUE
#' seq.bin.side <- seq(from=as.Date('2001-01-01'),to=as.Date('2011-01-01'),by='1 year')
#' seq.bin.center <- seq(from=as.Date('2001-06-01'),to=as.Date('2010-06-01'),by='1 year')
#' index.bin <- findInterval(x,seq.bin.side)
#' side.index <- findInterval(x,seq.bin.center)+0.5
#' n.bin.min <- 10 # minimum of 10 months of data for a bin to be accepted
#' data0 <- data.table(x=x,y=y,index.bin=index.bin,side.index=side.index)
#' data0.with.long.term <- ctbi.long.term(data0,n.bin.min,seq.bin.side,outliers.checked)
#' @export


ctbi.long.term <- function(data0,n.bin.min,seq.bin.side,outliers.checked)
{
  N.bin.tot <- max(data0[,index.bin])
  if(outliers.checked == FALSE)
  {
    side.dt <- merge(data0[,lapply(.SD,hidd.median,n.bin.min),by=side.index,.SDcols='y'],data.table(side.index=c(1:N.bin.tot-0.5,N.bin.tot+0.5)),all=TRUE)
    center.dt <- merge(data0[,lapply(.SD,hidd.median,n.bin.min),by=index.bin,.SDcols='y'],data.table(index.bin=1:N.bin.tot),all=TRUE)
  }else
  {
    side.dt <- merge(data0[,lapply(.SD,hidd.mean,n.bin.min),by=side.index,.SDcols='y'],data.table(side.index=c(1:N.bin.tot-0.5,N.bin.tot+0.5)),all=TRUE)
    center.dt <- merge(data0[,lapply(.SD,hidd.mean,n.bin.min),by=index.bin,.SDcols='y'],data.table(index.bin=1:N.bin.tot),all=TRUE)
  }
  setnames(side.dt,c('index','y'))
  setnames(center.dt,c('index','y'))
  both.dt <- rbind(side.dt,center.dt)
  both.dt <- both.dt[order(index)]

  # some of the sides might be missing. If there is a valid value for the center around, complete it linearly.
  r1 <- c(rep(c(TRUE,FALSE),(nrow(both.dt)-1)/2),FALSE)
  r2 <- c(rep(c(FALSE,TRUE),(nrow(both.dt)-1)/2),FALSE)
  r3 <- c(FALSE,rep(c(FALSE,TRUE),(nrow(both.dt)-1)/2))
  y.int <- as.numeric(both.dt[[2]])
  y.save <- y.int

  # case 1 : (center OK) (side NA) (center OK)
  if(nrow(both.dt) >= 5)
  {
    r4 <- c(FALSE,FALSE,rep(c(TRUE,FALSE),(nrow(both.dt)-3)/2),FALSE)
    r5 <- c(rep(c(FALSE,TRUE),(nrow(both.dt)-3)/2),FALSE,FALSE,FALSE)
    r6 <- c(FALSE,FALSE,FALSE,rep(c(TRUE,FALSE),(nrow(both.dt)-3)/2))
    y.int[r4] <- (y.int[r5]+y.int[r6])/2
    y.int[!is.na(y.save)] <- y.save[!is.na(y.save)]
    y.save <- y.int
  }

  # case 2 : (side OK) (center OK) (side NA)
  y.int[r3] <- 2*y.int[r2]-y.int[r1]
  y.int[!is.na(y.save)] <- y.save[!is.na(y.save)]
  y.save <- y.int

  # case 3 : (side NA) (center OK) (side OK)
  y.int[r1] <- 2*y.int[r2]-y.int[r3]
  y.int[!is.na(y.save)] <- y.save[!is.na(y.save)]
  y.save <- y.int

  # case 4 : (side NA) (center OK) (side NA)
  y.int[r1] <- y.int[r2]
  y.int[!is.na(y.save)] <- y.save[!is.na(y.save)]
  y.save <- y.int
  y.int[r3] <- y.int[r2]
  y.int[!is.na(y.save)] <- y.save[!is.na(y.save)]
  y.save <- y.int

  # get the interpolation
  y.int <- y.int[c(rep(c(TRUE,FALSE),(nrow(both.dt)-1)/2),TRUE)]

  # interpolation
  if(sum(!is.na(y.int)) >= 2)
  {
    data0[,long.term := approx(x=seq.bin.side,y=y.int,xout=data0[[1]])$y]
  }else
  {
    data0[,long.term := NA]
  }

  return(data0)
}
