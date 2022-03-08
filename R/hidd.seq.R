#' @title hidd.seq
#'
#' @description similar to seq, except that when 'from' starts the 29, 30 or 31 of a month, seq2 adds 5 days to 'from', run seq, and then subtracts 5 days to the output. This is done because the function seq is not consistent when time series start at the end of the months.
#'
#' @param from,to the starting and (maximal) end values of the sequence. Of length 1 unless just from is supplied as an unnamed argument.
#' @param by number: increment of the sequence.
#' @param length.out desired length of the sequence. A non-negative number, which for seq and seq.int will be rounded up if fractional.
#'
#' @return a vector of same class than from
#' @export
hidd.seq <- function(from = 1, to = 1, by = ((to - from)/(length.out - 1)),length.out = NULL)
{
  class.from <- class(from)
  class.from <- class.from[1]

  if(class.from == 'POSIXct' | class.from == 'Date')
  {
    DD <- as.numeric(substr(as.character(from),9,10))

    if(DD > 28) # shift the 'from' by 5 days, and then shift the sequence by 5 days.
    {
      if(class.from == 'POSIXct')
      {
        from <- from+5*24*60*60
      }
      if(class.from == 'Date')
      {
        from <- from+5
      }

      if(!is.null(length.out))
      {
        seq.all <- seq(from=from,by=by,length.out=length.out)
      }else
      {
        seq.all <- seq(to=to,from=from,by=by)
      }

      if(class.from == 'POSIXct')
      {
        seq.all <- seq.all-5*24*60*60
      }
      if(class.from == 'Date')
      {
        seq.all <- seq.all-5
      }

    }else
    {
      if(!is.null(length.out))
      {
        seq.all <- seq(from=from,by=by,length.out=length.out)
      }else
      {
        seq.all <- seq(from=from,to=to,by=by)
      }
    }

  }else
  {
    if(!is.null(length.out))
    {
      seq.all <- seq(from=from,by=by,length.out=length.out)
    }else
    {
      seq.all <- seq(to=to,from=from,by=by)
    }
  }

  return(seq.all)
}
