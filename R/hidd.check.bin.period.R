#' @title hidd.check.bin.period
#'
#' @description interpret the string character bin.period used in ctbi.timeseries or ctbi.main
#'
#' @param bin.period a character string or a numeric
#' @return A list that contains:
#' @return number, a numeric that indicates the value of bin.period
#' @return units, a character that indicates the unit of bin.period
#' @return bin.period.value.seconds, a numeric that indicates the value in seconds of bin.period
#' @return bin.period.value.days, a numeric that indicates the value in days of bin.period
#' @export
hidd.check.bin.period <- function(bin.period)
{
  char.stop <- 'When the first column (time series) is Date or POSIXct, argument \'bin.period\' is a character string. Example: bin.period=\'k units\', with k an integer and units = {seconds, minutes, hours, days, weeks, half-months, months, years, decades, centuries, millenaries}.'

  input <- c('s','sec','secs','second','seconds','m','min','mins','minute','minutes','h','hr','hour','hours','d','day','days','w','week','weeks','hm','h-m','half-month','half-months','halfmonth','halfmonths','half month','half months','month','months','y','yr','yrs','year','years','decade','decades','century','centuries','millenary', 'millenaries', 'millennium')
  output.standard <- c('secs','secs','secs','secs','secs','mins','mins','mins','mins','mins','hours','hours','hours','hours','days','days','days','weeks','weeks','weeks','half-months','half-months','half-months','half-months','half-months','half-months','half-months','half-months','months','months','years','years','years','years','years','decades','decades','centuries','centuries','millenaries','millenaries','millenaries')
  output.value.POSIXct <- c(1,1,1,1,1,60,60,60,60,60,60*60,60*60,60*60,60*60,60*60*24,60*60*24,60*60*24,60*60*24*7,60*60*24*7,60*60*24*7,60*60*24*15,60*60*24*15,60*60*24*15,60*60*24*15,60*60*24*15,60*60*24*15,60*60*24*15,60*60*24*15,60*60*24*30.5,60*60*24*30.5,60*60*24*365,60*60*24*365,60*60*24*365,60*60*24*365,60*60*24*365,60*60*24*365*10,60*60*24*365*10,60*60*24*365*100,60*60*24*365*100,60*60*24*365*1000,60*60*24*365*1000,60*60*24*365*1000)
  output.value.Date <- c(1/(60*60*24),1/(60*60*24),1/(60*60*24),1/(60*60*24),1/(60*60*24),1/(60*24),1/(60*24),1/(60*24),1/(60*24),1/(60*24),1/24,1/24,1/24,1/24,1,1,1,7,7,7,15,15,15,15,15,15,15,15,30.4167,30.4167,365,365,365,365,365,365*10,365*10,365*100,365*100,365*1000,365*1000,365*1000)

  data.units <- data.frame(input=input,output.standard=output.standard,output.value.POSIXct=output.value.POSIXct,output.value.Date=output.value.Date,stringsAsFactors = F)

  # Check for special characters
  #!#$%&()*+,-/:;<=>?@[]_`{|}~
  special.letters <- c('s','m','h','d','w','y','c','S','M','H','D','W','Y','C')
  special.characters <- c('!','#','$','%','&','*',',',':',';','<','=','>','?','@','[',']','_','`','{','|','}','~')
  special.power <- '^'
  special.bracket.beg <- '('
  special.bracket.end <- ')'

  n <- nchar(bin.period)
  n.power <- NA
  n.beforepower.beg <- NA
  n.beforepower.end <- NA
  n.afterpower.beg <- NA
  n.afterpower.end <- NA
  n.firstletter <- NA
  log.first <- FALSE
  for(i in 1:n)
  {
    char.1 <- substr(bin.period,i,i)
    check.special <- sum(char.1 == special.characters) != 0

    if(check.special)
    {
      stop(char.stop)
    }

    if(sum(char.1 == special.power)==1)
    {
      n.power <- i
    }
    if(sum(char.1 == special.bracket.beg)==1)
    {
      if(is.na(n.power))
      {
        n.beforepower.beg <- i+1
      }else
      {
        n.afterpower.beg <- i+1
      }
    }
    if(sum(char.1 == special.bracket.end)==1)
    {
      if(is.na(n.power))
      {
        n.beforepower.end <- i-1
      }else
      {
        n.afterpower.end <- i-1
      }
    }
    if(sum(char.1 == special.letters)==1 & log.first==FALSE)
    {
      n.firstletter <- i
      log.first <- TRUE
    }
  }

  if(is.na(n.beforepower.beg) & !is.na(n.beforepower.end))
  {
    stop(char.stop)
  }
  if(!is.na(n.beforepower.beg) & is.na(n.beforepower.end))
  {
    stop(char.stop)
  }
  if(is.na(n.afterpower.beg) & !is.na(n.afterpower.end))
  {
    stop(char.stop)
  }
  if(!is.na(n.afterpower.beg) & is.na(n.afterpower.end))
  {
    stop(char.stop)
  }

  if(!is.na(n.firstletter))
  {
    if(n.firstletter > 1)
    {
      bin.period.number <- substr(bin.period,1,n.firstletter-1)
      bin.period.unit <- substr(bin.period,n.firstletter,n)
      bin.period.unit <- tolower(bin.period.unit)
    }else
    {
      stop(char.stop)
    }
  }
  else
  {
    stop(char.stop)
  }

  # Number
  if(is.na(n.power))
  {
    if(is.na(n.beforepower.beg))
    {
      bin.period.number <- suppressWarnings(as.numeric(bin.period.number))

      if(is.na(bin.period.number))
      {
        stop(char.stop)
      }

    }else
    {
      bin.period.number <- suppressWarnings(as.numeric(substr(bin.period.number,n.beforepower.beg,n.beforepower.end)))

      if(is.na(bin.period.number))
      {
        stop(char.stop)
      }
    }

  }else
  {
    if(is.na(n.beforepower.beg))
    {
      a <- suppressWarnings(as.numeric(substr(bin.period.number,1,n.power-1)))
    }else
    {
      a <- suppressWarnings(as.numeric(substr(bin.period.number,n.beforepower.beg,n.beforepower.end)))
    }

    if(is.na(n.afterpower.beg))
    {
      b <- suppressWarnings(as.numeric(substr(bin.period.number,n.power+1,n.firstletter-1)))
    }else
    {
      b <- suppressWarnings(as.numeric(substr(bin.period.number,n.afterpower.beg,n.afterpower.end)))
    }

    if(is.na(a) | is.na(b))
    {
      stop(char.stop)
    }
    else
    {
      bin.period.number <- a^b
    }
  }

  # no negative numbers
  if(bin.period.number <= 0)
  {
    stop(char.stop)
  }

  # only integers
  if(round(bin.period.number,digits=0) != bin.period.number)
  {
    stop(char.stop)
  }


  # check that there are no spaces at the end.
  log.space <- 1
  n.end <- nchar(bin.period.unit)
  while(log.space)
  {
    if(substr(bin.period.unit,n.end,n.end)==' ')
    {
      n.end <- n.end-1
    }
    else
    {
      log.space <- 0
    }
  }
  bin.period.unit <- substr(bin.period.unit,1,n.end)

  n.units <- length(data.units[,1])
  index <- NA
  for(i in 1:n.units)
  {
    if(data.units[i,1]==bin.period.unit)
    {
      index <- i
    }
  }

  if(is.na(index))
  {
    stop(char.stop)
  }else
  {
    bin.period.unit <- data.units[index,2]
    bin.period.value.seconds <- bin.period.number*data.units[index,3]
    bin.period.value.days <- bin.period.number*data.units[index,4]
  }



  list.return <- list(number=bin.period.number,units=bin.period.unit,bin.period.value.seconds=bin.period.value.seconds,bin.period.value.days=bin.period.value.days)

  return(list.return)
}
