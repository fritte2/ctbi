## global variables
globalVariables(c(":=","detrended"))

#' @title ctbi.cycle
#'
#' @description Calculate the mean (or median) stack of the detrended data for all bins, and add the cyclic component column to data0.
#'
#' @param data0 data.table with the columns x (time series), y (values), time.bin (position of x between 0 and 1 with respect to the bin boundaries), cycle.index (index between 1 and bin.size attached to time.bin), and long.term (the long-term trend)
#' @param bin.size median number of points within all non-empty bins
#' @param outliers.checked boolean to indicate if the median (outliers.checked = FALSE) or the mean (outliers.checked = TRUE) should be used to calculate the stack
#'
#' @return A list that contains data0 (data0.l) and a data.table that contains the mean (or median) stack of all accepted bins (FUN.cycle.l)
#' @examples
#' library(data.table)
#' x <- seq(from=as.Date('2001-01-01'),to=as.Date('2010-12-01'),by='1 month')
#' y <- 3*cos(2*pi*(0:(length(x)-1))/12)+runif(length(x))
#' bin.size <- 12
#' outliers.checked <- TRUE
#' time.bin <- rep(((1:bin.size)/bin.size)-(1/(2*bin.size)),10)
#' cycle.index <- findInterval(time.bin,(1:(bin.size-1))/bin.size)+1
#' long.term <- rep(0,length(x))
#' data0 <- data.table(x=x,y=y,cycle.index=cycle.index,long.term=long.term,time.bin=time.bin)
#' list.cycle <- ctbi.cycle(data0,bin.size,outliers.checked)
#' data0.with.cyclic.component <- list.cycle$data0.l
#' mean.cycle <- list.cycle$FUN.cycle.l
#' @export

ctbi.cycle <- function(data0,bin.size,outliers.checked)
{
  data0[,detrended := y-long.term]

  if(sum(!is.na(data0[,detrended])) != 0)
  {
    if(outliers.checked == FALSE)
    {
      FUN.cycle <- data0[,lapply(.SD,median,na.rm=TRUE),by=cycle.index,.SDcols='detrended']
    }else
    {
      FUN.cycle <- data0[,lapply(.SD,mean,na.rm=TRUE),by=cycle.index,.SDcols='detrended']
    }

    FUN.cycle <- merge(FUN.cycle,data0[,lapply(.SD,sd,na.rm=TRUE),by=cycle.index,.SDcols='detrended'],by="cycle.index")
    if(outliers.checked == FALSE)
    {
      setnames(FUN.cycle,c('cycle.index','median','sd'))
    }else
    {
      setnames(FUN.cycle,c('cycle.index','mean','sd'))
    }

    # some NA values in the mean.cycle can be linearly interpolated.
    y.int <- c(FUN.cycle[[2]],FUN.cycle[[2]],FUN.cycle[[2]])
    if(sum(is.na(FUN.cycle[[2]])) != 0)
    {
      yapp <- approx(x=1:length(y.int),y=y.int,xout=1:length(y.int))
      y.int <- yapp[[2]]
      FUN.cycle[[2]] <- y.int[(bin.size+1):(2*bin.size)]
    }
    y.int <- y.int[(bin.size):(2*bin.size+1)]
    x.int <- c(-1/(2*bin.size),((1:bin.size)/bin.size)-(1/(2*bin.size)),1+1/(2*bin.size))

    # interpolation across the whole time series
    data0[,cycle := approx(x=x.int,y=y.int,xout=unlist(data0[,time.bin]))$y]
  }else
  {
    data0[,cycle := NA]

    if(outliers.checked == FALSE)
    {
      FUN.cycle <- data.table(cycle.index=1:bin.size,median=rep(NA_real_,bin.size),sd=rep(NA_real_,bin.size))
    }else
    {
      FUN.cycle <- data.table(cycle.index=1:bin.size,mean=rep(NA_real_,bin.size),sd=rep(NA_real_,bin.size))
    }
  }

  data0[,detrended := NULL]

  DTl <- list()
  DTl$data0.l <- data0
  DTl$FUN.cycle.l <- setDT(FUN.cycle)

  return(DTl)
}
