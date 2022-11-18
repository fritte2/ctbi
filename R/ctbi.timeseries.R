#' @title ctbi.timeseries
#'
#' @description Calculate the sequence of bin sides that encompasses the original time series based on a bin period and a bin side (or a bin center). The sequence of bin centers is calculated as well.
#'
#' @param x.t original time series (date, POSIXct or numeric)
#' @param bin.period time interval between two sides of a bin. If x.t is numeric, bin.period is numeric. If x.t is POSIXct or Date, bin.period = 'k units', with k an integer and units = (seconds, minutes, hours, days, weeks, half-months, months, years, decades, centuries, millenaries)
#' @param bin.side one side of a bin (same class as x.t)
#' @param bin.center if bin.side is not specified, one center of a bin (same class as x.t)
#'
#' @return A list that contains:
#' @return seq.bin.side, the sequence of bin sides (same class as bin.side)
#' @return seq.bin.center, the sequence of bin centers (same class as bin.side)
#' @return time.step.median, the median time step (numeric)
#' @examples
#' x.t <- seq(from=as.Date('2001-01-01'),to=as.Date('2010-12-01'),by='1 month')
#' bin.side <- as.Date('2003-10-01')
#' bin.period <- '4 months'
#' list.ts <- ctbi.timeseries(x.t,bin.period,bin.side)
#' seq.bin.side.Date <- list.ts$seq.bin.side
#' seq.bin.center.Date <- list.ts$seq.bin.center
#'
#' x.t <- seq(from=as.POSIXct('2001-01-01 12:45:23'),to=as.POSIXct('2001-01-01 13:34:21'),by='18 s')
#' bin.side <- as.POSIXct('2001-01-01 13:00:00')
#' bin.period <- '1 minute' # '60 s', '60 sec', '60 seconds', '1 min' are also possible
#' list.ts <- ctbi.timeseries(x.t,bin.period,bin.side)
#' seq.bin.side.POSIXct <- list.ts$seq.bin.side
#' seq.bin.center.POSIXct <- list.ts$seq.bin.center
#'
#' x.t <- seq(from= - 50000,to= 2000 ,by=1000)
#' bin.side <- 0
#' bin.period <- 10000
#' list.ts <- ctbi.timeseries(x.t,bin.period,bin.side)
#' seq.bin.side.numeric <- list.ts$seq.bin.side
#' seq.bin.center.numeric <- list.ts$seq.bin.center
#' @export

ctbi.timeseries <- function(x.t,bin.period,bin.side,bin.center=NULL)
{
  if(missing(bin.side))
  {
    bin.side <- NULL
  }

# class of the time series
class.t <- class(x.t)
class.t <- class.t[1]

log.numeric.t <- class.t == 'numeric' | class.t == 'integer'
log.POSIXct.t <- class.t == "POSIXct"
log.Date.t <- class.t == "Date" | class.t == "IDate"
log.none <- log.numeric.t | log.POSIXct.t | log.Date.t
if(!log.none)
{
  stop('The class of the first column (time series) needs to be numeric, POSIXct or Date.')
}
if(class.t == 'integer')
{
  x.t <- as.numeric(x.t)
  class.t <- 'numeric'
}

if(class.t == 'IDate')
{
  x.t <- as.Date(x.t)
  class.t <- 'Date'
}

# class of the by
log.character.by <- is.character(bin.period)
log.numeric.by <- is.numeric(bin.period)
if(log.numeric.t)
{
  if(!log.numeric.by)
  {
    stop('When the first column (time series) is numeric, argument bin.period must be numeric as well.')
  }
}else
{
  if(!log.character.by)
  {
    stop('When the first column (time series) is Date or POSIXct, argument bin.period is a character string (ex: bin.period = \'1 month\'). Format: bin.period=\'k units\', with k an integer and units = {seconds, minutes, hours, days, weeks, half-months, months, years, decades, centuries, millenaries}.')
  }
}

# extract information from by
if(log.character.by)
{
  list.return <- hidd.check.bin.period(bin.period)

  bin.period.number <- list.return$number
  bin.period.units <- list.return$units
  bin.period.value.seconds <- list.return$bin.period.value.seconds
  bin.period.value.days <- list.return$bin.period.value.days

  if((bin.period.units == 'half-months') & ((bin.period.number %% 2) == 0))
  {
    bin.period <- paste0(bin.period.number/2,' months')
    list.return <- hidd.check.bin.period(bin.period)

    bin.period.number <- list.return$number
    bin.period.units <- list.return$units
    bin.period.value.seconds <- list.return$bin.period.value.seconds
    bin.period.value.days <- list.return$bin.period.value.days
  }

  by.plus <- paste0(bin.period.number,' ',bin.period.units)
  by.minus <- paste0('-',bin.period.number,' ',bin.period.units)
}else
{
  bin.period.units <- 'unit'
  bin.period.value <- bin.period
  by.plus <- bin.period
  by.minus <- -bin.period
}

log.bin.side <- !is.null(bin.side)
log.bin.center <- !is.null(bin.center)
if(log.bin.side & log.bin.center)
{
  stop('Specify either bin.center (time centered around a bin) or bin.side (time boundary of a bin), but not both. They have the same class than the time series.')
}
if(!log.bin.side & !log.bin.center)
{
  stop('Specify bin.center (time centered around a bin) or bin.side (time boundary of a bin). They have the same class than the time series.')
}

# class of the bin.side
if(log.bin.side)
{
  class.bin.side <- class(bin.side)
  class.bin.side <- class.bin.side[1]
  if(class.bin.side != class.t)
  {
    stop('The bin.side needs to have the same class than the time series (POSIXct, Date or numeric)')
  }
}
# class of the bin.center
if(log.bin.center)
{
  class.bin.center <- class(bin.center)
  class.bin.center <- class.bin.center[1]
  if(class.bin.center != class.t)
  {
    stop('The bin.center needs to have the same class than the time series (POSIXct, Date or numeric)')
  }
}

# check there are no NA in x.t
if(sum(is.na(x.t))!=0)
{
  stop('The time series contains NA timesteps.')
}

# check the timezone of bin center or bin side
if(log.POSIXct.t)
{
  if(log.bin.center)
  {
    if(attr(bin.center,'tzone') != attr(x.t[1],'tzone'))
    {
      warning('Check that the timezone of bin.center is the same than the time series.')
    }
  }
  if(log.bin.side)
  {
    if(attr(bin.side,'tzone') != attr(x.t[1],'tzone'))
    {
      warning('Check that the timezone of bin.side is the same than the time series.')
    }
  }
}


# timestep (= sampling period) of the time series
if(log.POSIXct.t)
{
  # diff expressed in seconds
  diff.consecutive <- round(as.numeric(difftime(x.t[-1],x.t[-length(x.t)],units='secs')),digits=4)
  length.timeseries <- round(as.numeric(difftime(x.t[length(x.t)],x.t[1],units='secs')),digits=4)
}else
{
  # diff expressed as default units or days (if date).
  diff.consecutive <- round(as.numeric(diff(x.t)),digits=4)
  length.timeseries <- round(as.numeric(x.t[length(x.t)]-x.t[1]),digits=4)
}

time.step.median <- median(diff.consecutive)
if((length(diff.consecutive) %% 2) == 0) # the median has to be calculated on an odd time series to avoid approximation.
{
  time.step.median <- median(diff.consecutive[-1])
}
time.step <- unique(diff.consecutive)

if(sum(time.step <= 0) != 0)
{
  stop('The time series contains backward or redundant timesteps.')
}

if(log.POSIXct.t)
{
  log.bin.min <- (time.step.median*0.95 <= bin.period.value.seconds)
  log.bin.max <- (bin.period.value.seconds < length.timeseries)
}

if(log.Date.t)
{
  log.bin.min <- (time.step.median*0.95 <= bin.period.value.days)
  log.bin.max <- (bin.period.value.days < length.timeseries)
}

if(log.numeric.t)
{
  log.bin.min <- (time.step.median*0.95 <= bin.period)
  log.bin.max <- (bin.period < length.timeseries)
}

if(!log.bin.min)
{
  stop('The bin.period is too small. It needs to be at least equal to the sampling period of the time series.')
}
if(!log.bin.max)
{
  stop('The bin.period is too big. Its maximum size is half the length of the time series.')
}

log.halfmonth <- (bin.period.units == 'half-months')
if(!log.halfmonth)
{
  # if !log.bin.side, find the exact bin.side so that mean(bin.side,bin.side+period) = bin.center
  if(!log.bin.side)
  {
    # approximate bin.side
    if(log.Date.t)
    {
      bin.side <- bin.center - round(bin.period.value.days/2,digits=0)
    }
    if(log.POSIXct.t)
    {
      bin.side <- bin.center - round(bin.period.value.seconds/2,digits=0)
    }
    if(log.numeric.t)
    {
      bin.side <- bin.center - bin.period/2
    }

    # calculate the next consecutive bin.side and the mean(bin.side,bin.side+period)
    bin.side.next <- hidd.seq(from=bin.side,length.out=2,by=by.plus)
    bin.side.next <- bin.side.next[2]
    bin.center.compare <- mean(c(bin.side,bin.side.next))

    # calculate the shift
    if(log.Date.t)
    {
      shift.bin.side <- round(as.numeric(difftime(bin.center,bin.center.compare,units='days')),digits=0)
    }
    if(log.POSIXct.t)
    {
      shift.bin.side <- round(as.numeric(difftime(bin.center,bin.center.compare,units='secs')),digits=0)
    }
    if(log.numeric.t)
    {
      shift.bin.side <- bin.center-bin.center.compare
    }

    bin.side <- bin.side+shift.bin.side
  }

  # if log.bin.side, find the exact bin.center to that mean(bin.side,bin.side+period) = bin.center
  if(log.bin.side)
  {
    # calculate the next consecutive bin.side and the mean(bin.side,bin.side+period)
    bin.side.next <- hidd.seq(from=bin.side,length.out=2,by=by.plus)
    bin.side.next <- bin.side.next[2]
    bin.center <- mean(c(bin.side,bin.side.next))
  }

  # we now have two consistent bin.side and bin.center. They could be outside the time series.
  # find seq.bin.side
  seq.bin.side <- bin.side
  if(bin.side <= x.t[length(x.t)])
  {
    seq.temp <-  hidd.seq(from=bin.side,by=by.plus,to=x.t[length(x.t)])

    seq.bin.side <- c(seq.bin.side,seq.temp[-1])
  }
  if(bin.side >= x.t[1])
  {
    seq.temp <-  hidd.seq(from=bin.side,by=by.minus,to=x.t[1])
    seq.temp <- seq.temp[-1]
    seq.temp <- rev(seq.temp)

    seq.bin.side <- c(seq.temp,seq.bin.side)
  }

  # complete forward
  while(seq.bin.side[length(seq.bin.side)] <= x.t[length(x.t)])
  {
    x.add <- hidd.seq(from=seq.bin.side[length(seq.bin.side)],length.out=2,by=by.plus)
    x.add <- x.add[2]
    seq.bin.side <- c(seq.bin.side,x.add)
  }

  # complete backward
  while(seq.bin.side[1] > x.t[1])
  {
    x.add <- hidd.seq(from=seq.bin.side[1],length.out=2,by=by.minus)
    x.add <- x.add[2]
    seq.bin.side <- c(x.add,seq.bin.side)
  }

  #cut seq.bin.side
  index.side <- 1:length(seq.bin.side)
  beg.side <- max(index.side[seq.bin.side <= x.t[1]])
  end.side <- min(index.side[seq.bin.side > x.t[length(x.t)]])
  seq.bin.side <- seq.bin.side[beg.side:end.side]

  # find seq.bin.center
  seq.bin.center <- bin.center
  if(bin.center < seq.bin.side[length(seq.bin.side)])
  {
    seq.temp <-  hidd.seq(from=bin.center,by=by.plus,to=seq.bin.side[length(seq.bin.side)])

    seq.bin.center <- c(seq.bin.center,seq.temp[-1])
  }
  if(bin.center > seq.bin.side[1])
  {
    seq.temp <-  hidd.seq(from=bin.center,by=by.minus,to=seq.bin.side[1])
    seq.temp <- seq.temp[-1]
    seq.temp <- rev(seq.temp)

    seq.bin.center <- c(seq.temp,seq.bin.center)
  }

  seq.bin.center <- seq.bin.center[seq.bin.side[1] < seq.bin.center & seq.bin.center < seq.bin.side[length(seq.bin.side)]]
}

if(log.halfmonth)
{
  # approximate bin.side from bin.center
  if(!log.bin.side)
  {
    log.up <- bin.center > x.t[length(x.t)]

    if(!log.up)
    {
    seq.temp <- hidd.seq(from=bin.center,length.out=2,by=paste0('-',bin.period.number,' week'))
    bin.side <- seq.temp[2]
    }else
    {
      seq.temp <- hidd.seq(from=bin.center,length.out=2,by=paste0(bin.period.number,' week'))
      bin.side <- seq.temp[2]
    }
  }

  # extend the boundaries of the time series
  if(log.Date.t)
  {
    ts.min <- x.t[1]-round((bin.period.number+5)*bin.period.value.days,digits=0)
    ts.max <- x.t[length(x.t)]+round((bin.period.number+5)*bin.period.value.days,digits=0)
  }else
  {
    ts.min <- x.t[1]-round((bin.period.number+5)*bin.period.value.seconds,digits=0)
    ts.max <- x.t[length(x.t)]+round((bin.period.number+5)*bin.period.value.seconds,digits=0)
  }

  seq.bin.side <- bin.side
  if(bin.side < ts.max)
  {
    seq.temp <-  hidd.seq(from=bin.side,by='1 month',to=ts.max)

    seq.bin.side <- c(seq.bin.side,seq.temp[-1])
  }
  if(ts.min < bin.side)
  {
    seq.temp <-  hidd.seq(from=bin.side,by='-1 month',to=ts.min)
    seq.temp <- seq.temp[-1]
    seq.temp <-  rev(seq.temp)

    seq.bin.side <- c(seq.temp,seq.bin.side)
  }


  # Convention (1 month, 15 days)
  bin.side.DD <- as.numeric(substr(as.character(bin.side),9,10))
  log.shift.forward <- TRUE
  if(8 <= bin.side.DD & bin.side.DD <= 24) # bin.side is close to 16, month start at seq - 15 days
  {
    log.shift.forward <- FALSE
  }

  seq.bin.temp <- rep(seq.bin.side[1],2*length(seq.bin.side))
  if(log.shift.forward)
  {
    seq.bin.temp[seq(from=1,to=2*length(seq.bin.side),by=2)] <- seq.bin.side
    if(log.Date.t)
    {
      seq.bin.temp[seq(from=2,to=2*length(seq.bin.side),by=2)] <- seq.bin.side+15
    }else
    {
      seq.bin.temp[seq(from=2,to=2*length(seq.bin.side),by=2)] <- seq.bin.side+15*24*60*60
    }
  }else
  {
    seq.bin.temp[seq(from=2,to=2*length(seq.bin.side),by=2)] <- seq.bin.side
    if(log.Date.t)
    {
      seq.bin.temp[seq(from=1,to=2*length(seq.bin.side),by=2)] <- seq.bin.side-15
    }else
    {
      seq.bin.temp[seq(from=1,to=2*length(seq.bin.side),by=2)] <- seq.bin.side-15*24*60*60
    }
  }

  seq.bin.side <- seq.bin.temp
  rm(seq.bin.temp)

  # bin.side is now a sequence of every 1 half-month. We need to adapt it to every k half-months.
  bin.side.up <- hidd.seq(from=bin.side,length.out=2,by='1 week')
  bin.side.down <- hidd.seq(from=bin.side,length.out=2,by='-1 week')
  bin.side.up <- bin.side.up[2]
  bin.side.down <- bin.side.down[2]

  read.forward <- bin.side.down < seq.bin.side
  read.backward <- seq.bin.side < bin.side.up

  log.forward <- FALSE
  seq.bin.temp <- seq.bin.side
  if(sum(read.forward)!=0)
  {
    seq.bin.temp <- seq.bin.side[read.forward] # the first element is bin.side
    seq.bin.temp <- seq.bin.temp[seq(from=1,to=length(seq.bin.temp),by=bin.period.number)]
    log.forward <- TRUE
  }
  if(sum(read.backward)!=0)
  {
    seq.backward <- seq.bin.side[read.backward]
    seq.backward <- rev(seq.backward)
    seq.backward <- seq.backward[seq(from=1,to=length(seq.backward),by=bin.period.number)]
    seq.backward <- rev(seq.backward)
    if(log.forward)
    {
      seq.bin.temp <- c(seq.backward,seq.bin.temp[-1])
    }else
    {
      seq.bin.temp <- seq.backward
    }
  }
  seq.bin.side <- seq.bin.temp

  # calculate the shift with respect to bin.center
  if(!log.bin.side) # bin.center exists. Does the mean of [bin.side,bin.side+1] corresponds to its value? if not, shift seq.bin.side.
  {
    read.up <- bin.center < seq.bin.side
    read.down <- bin.center > seq.bin.side

    bin.side1 <- max(seq.bin.side[read.down])
    bin.side2 <- min(seq.bin.side[read.up])

    bin.center.compare <- mean(c(bin.side1,bin.side2))

    # calculate the shift.seq.bin.center
    if(log.Date.t)
    {
      shift.seq.bin.side <- round(as.numeric(difftime(bin.center,bin.center.compare,units='days')),digits=0)
    }
    if(log.POSIXct.t)
    {
      shift.seq.bin.side <- round(as.numeric(difftime(bin.center,bin.center.compare,units='secs')),digits=0)
    }
    seq.bin.side <- seq.bin.side+shift.seq.bin.side # now the time series has been shifted to respect bin.center

    # calculate the future shift to create seq.bin.center
    if(log.Date.t)
    {
      shift.seq.bin.center <- round(as.numeric(difftime(bin.center.compare,bin.side1,units='days')),digits=0)
    }
    if(log.POSIXct.t)
    {
      shift.seq.bin.center <- round(as.numeric(difftime(bin.center.compare,bin.side1,units='secs')),digits=0)
    }
  }else # bin.center does not exist
  {
    log.up <- (sum(bin.side.up > seq.bin.side)!=0) & (sum(bin.side.up < seq.bin.side)!=0)

    if(log.up)
    {
      bin.side1 <- max(seq.bin.side[bin.side.up > seq.bin.side])
      bin.side2 <- min(seq.bin.side[bin.side.up < seq.bin.side])
    }else
    {
      bin.side1 <- seq.bin.side[length(seq.bin.side)-1]
      bin.side2 <- seq.bin.side[length(seq.bin.side)]
    }

    bin.center.compare <- mean(c(bin.side1,bin.side2))

    # calculate the future shift to create seq.bin.center
    if(log.Date.t)
    {
      shift.seq.bin.center <- round(as.numeric(difftime(bin.center.compare,bin.side1,units='days')),digits=0)
    }
    if(log.POSIXct.t)
    {
      shift.seq.bin.center <- round(as.numeric(difftime(bin.center.compare,bin.side1,units='secs')),digits=0)
    }
  }

  #cut seq.bin.side
  index.side <- 1:length(seq.bin.side)
  beg.side <- max(index.side[seq.bin.side <= x.t[1]])
  end.side <- min(index.side[seq.bin.side > x.t[length(x.t)]])
  seq.bin.side <- seq.bin.side[beg.side:end.side]

  seq.bin.center <- seq.bin.side[-length(seq.bin.side)]
  seq.bin.center <- seq.bin.center+shift.seq.bin.center
}

list.all <- list(seq.bin.side=seq.bin.side,seq.bin.center=seq.bin.center,time.step.median=time.step.median)

return(list.all)
}




