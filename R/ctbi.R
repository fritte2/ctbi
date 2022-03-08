## global variables
globalVariables(c(":=","x", "y", "index.bin","n.points","n.NA","time.bin","cycle.index","outliers","bin.accepted","long.term","imputed","n.outliers","n.imputed"))

#' @title ctbi
#'
#' @description Clean, decompose and aggregate univariate time series following the procedure "Cyclic/trend decomposition using bin interpolation" and the Logbox method for flagging outliers, both detailed in Ritter, F.: Technical note: A procedure to clean, decompose and aggregate time series, Hydrol. Earth Syst. Sci. Discuss. [preprint], <https://doi.org/10.5194/hess-2021-609>, in review, 2021.
#'
#' @param data.input Two columns data.table (or data.frame) with the first column being the time component (POSIXct, Date or numeric) and the second column the value (numeric)
#' @param bin.side one side of a bin (same class as the time component)
#' @param bin.center if bin.side is not specified, one center of a bin (same class as the time component)
#' @param bin.period time interval between two sides of a bin. If the time component x.t of data0 is numeric, bin.period is numeric. If x.t is POSIXct or Date, bin.period = 'k units', with k an integer and units = (seconds, minutes, hours, days, weeks, half-months, months, years, decades, centuries, millenaries)
#' @param bin.FUN character ('mean', 'median' or 'sum') that defines the aggregating operator
#' @param bin.max.f.NA numeric between 0 and 1 that specifies the maximum fraction of missing values for a bin to be accepted. The minimum number of non-NA points for a bin to be accepted is bin.size*(1-bin.max.f.NA) with bin.size the number of points per bin
#' @param SCI.min numeric between 0 and 1 that is compared to the Stacked Cycles Index (SCI). If SCI > SCI.min, missing values are imputed in accepted bins with the sum of the long-term and cyclic components. SCI.min = Inf means that no values are imputed
#' @param k.outliers positive numeric that defines the outlier level in the Logbox method used to flag outliers, with k.outliers = 0.16 corresponding to a Gaussian distribution and k.outliers = 0.8 to an Exponential distribution. The default value of k.outliers = 0.6 has been calculated based on a set of distributions with moderate skewness and kurtosis (the Pearson family). k.outliers = Inf means that no outliers are flagged
#' @param ylim numeric vector of length 2 that defines the range of possible values. Values below ylim[1] or above ylim[2] are set to NA
#' @return A list that contains:
#' @return data0, the raw dataset (same class as data.input), with 8 columns: (i) time; (ii) outlier-free and imputed data; (iii) index.bin: index of the bins associated with each data points (the index is negative if the bin is rejected); (iv) long.term: long-term trend; (v) cycle: cyclic component; (vi) outliers: quarantined outliers; (vii) imputed: value of the imputed data points; (viii) time.bin: relative position of the data points in their bins, between 0 and 1
#' @return data1, the aggregated dataset (same class as data.input), with 10 columns: (i) aggregated time (center of the bins); (ii) aggregated data; (iii) index.bin: index of the bin (negative value if the bin is rejected); (iv) bin.start: start of the bin; (v) bin.end: end of the bin; (vi) n.points: number of points per bin (including NA values); (vii) n.NA: number of NA values per bin, originally; (viii) n.outliers: number of outliers per bin; (ix) n.imputed: number of imputed points per bin; (x) variability associated with the aggregation (standard deviation for the mean, MAD for the median and nothing for the sum)
#' @return SCI (Stacked Cycle Index), a numeric between 0 and 1 related to the strength of the cyclic pattern within each bin. SCI is defined as SCI = 1 - SS.res/SS.tot - 1/N.bin with SS.tot the sum of the squared detrended data, SS.res the sum of the squared detrended & deseasonalized data, and N.bin the number of accepted bins
#' @return mean.cycle, a dataset (same class as data.input) with bin.size rows and 4 columns: (i) generic.time.bin1: time of the first bin; (ii) mean: the mean stack of detrended data; (iii) sd: the standard deviation on the mean; (iv) time.bin: relative position of the data points in the bin, between 0 and 1
#' @return bin.size, the median number of points in non-empty bins
#' @return n.bin.min, the minimum number of points for a bin to be accepted
#' @examples
#' # example of contaminated sunspot data
#' example1 <- data.frame(year = 1700:1988,sunspot = as.numeric(sunspot.year))
#' example1[sample(1:289,30),'sunspot'] <- NA
#' example1[c(5,30,50),'sunspot'] <- c(-50,300,400)
#' example1 <- example1[-(70:100),]
#' bin.period <- 11 # aggregation performed every 11 years (the year is numeric here)
#' bin.side <- 1989 # to capture the last year, 1988, in a complete bin
#' bin.FUN <- 'mean'
#' bin.max.f.NA <- 0.2 # maximum of 20% of missing data per bin
#' ylim <- c(0,Inf) # negative values are impossible
#'
#' list.main <- ctbi(example1,bin.period=bin.period,
#'                        bin.side=bin.side,bin.FUN=bin.FUN,
#'                        ylim=ylim,bin.max.f.NA=bin.max.f.NA)
#' data0.example1 <- list.main$data0 # cleaned raw dataset
#' data1.example1 <- list.main$data1 # aggregated dataset.
#' SCI.example1 <- list.main$SCI # this data set shows a moderate seasonality
#' mean.cycle.example1 <- list.main$mean.cycle # this data set shows a moderate seasonality
#' bin.size.example1 <- list.main$bin.size # 12 data points per bin on average (12 months per year)
#'
#' plot(mean.cycle.example1[,'generic.time.bin1'],
#'      mean.cycle.example1[,'mean'],type='l',ylim=c(-80,80),
#'      ylab='sunspot cycle',
#'      xlab='11 years window')
#' lines(mean.cycle.example1[,'generic.time.bin1'],
#'       mean.cycle.example1[,'mean']+mean.cycle.example1[,'sd'],type='l',lty=2)
#' lines(mean.cycle.example1[,'generic.time.bin1'],
#'       mean.cycle.example1[,'mean']-mean.cycle.example1[,'sd'],type='l',lty=2)
#' title(paste0('mean cycle (weak cyclicity: SCI = ',SCI.example1,')'))
#' # the SCI is much higher on the raw dataset without contamination (SCI = 0.34)
#' ctbi.plot(list.main,show.n.bin=10)
#'
#' # example of beaver data
#' temp.beaver <- beaver1[,'temp']
#' t.char <- as.character(beaver1[,'time'])
#' minutes <- substr(t.char,nchar(t.char)-1,nchar(t.char))
#' hours <- substr(t.char,nchar(t.char)-3,nchar(t.char)-2)
#' hours[hours==""] <- '0'
#' days <- c(rep(12,91),rep(13,23))
#' time.beaver <- as.POSIXct(paste0('2000-12-',days,' ',hours,':',minutes,':00'),tz='UTC')
#' example2 <- data.frame(time=time.beaver,temp=temp.beaver)
#'
#' bin.period <- '1 hour' # aggregation performed every hour
#' bin.side <- as.POSIXct('2000-12-12 00:00:00',tz='UTC') # start of a bin
#' bin.FUN <- 'mean' # aggregation operator
#' bin.max.f.NA <- 0.2 # maximum of 20% of missing data per bin
#' ylim <- c(-Inf,Inf)
#' list.main <- ctbi(example2,bin.period=bin.period,
#'                  bin.side=bin.side,bin.FUN=bin.FUN,
#'                  ylim=ylim,bin.max.f.NA=bin.max.f.NA)
#' data0.example2 <- list.main$data0 # cleaned raw dataset
#' data1.example2 <- list.main$data1 # aggregated dataset. 1 outlier flagged.
#' SCI.example2 <- list.main$SCI # this data set shows no seasonality every hour
#' ctbi.plot(list.main,show.n.bin = 50)
#' @export
#' @import data.table
#' @import stats
#' @import graphics
#' @import utils

ctbi <- function(data.input,bin.side=NULL,bin.period,bin.center=NULL,bin.FUN = 'mean',bin.max.f.NA = 0.2,SCI.min = 0.6,k.outliers=0.6,ylim=c(-Inf,+Inf))
{
  # check the format of the inputs
  if(1)
  {
    # data
    char.format <- 'data in ctbi is a two columns data.frame (or data.table), with the first column being the time series (numeric, Date or POSIXct) and the second column the variable to be temporally aggregated (numeric).'
    if(length(data.input[1,]) == 2)
    {
      if(!is.numeric(data.input[[2]]) & !is.integer(data.input[[2]]))
      {
        stop(char.format)
      }
    }else
    {
      stop(char.format)
    }

    # is the input data a data.table ?
    log.data.table <- length(grep('data.table',paste(class(data.input),collapse = ' '))) == 1
    # Convert the input to a data.table
    if(!log.data.table)
    {
      data0 <- as.data.table(data.input)
    }else
    {
      data0 <- copy(data.input)
    }

    # bin.max.f.NA
    char.frac.NA <- 'bin.max.f.NA is a numeric between 0 and 1 and corresponds to the maximum fraction of NA values accepted in a bin (otherwise the bin is rejected).'
    if(length(bin.max.f.NA)==1)
    {
      if(bin.max.f.NA < 0 | bin.max.f.NA > 1)
      {
        stop(char.frac.NA)
      }
    }else
    {
      stop(char.frac.NA)
    }

    # ylim
    char.ylim <- 'ylim is the range of possible values with the default being ylim=c(-Inf,Inf). Example: ylim=c(0,Inf) for a precipitation dataset (important for the imputation process).'
    if(length(ylim) == 2)
    {
      if(ylim[1] > ylim[2])
      {
        stop(char.ylim)
      }
    }else
    {
      stop(char.ylim)
    }

    # bin.FUN
    char.bin.FUN <- 'bin.FUN must be the character \'mean\',\'median\' or \'sum\' to perform the temporal aggregation.'
    if(length(bin.FUN)==1)
    {
      if(!is.character(bin.FUN))
      {
        stop(char.bin.FUN)
      }
      if(sum(bin.FUN==c('mean','sum','median'))==0)
      {
        stop(char.bin.FUN)
      }
    }else
    {
      stop(char.bin.FUN)
    }

    # SCI.min
    char.cycle <- 'SCI.min is a value between 0 and 1 that is compared to the Stacked Cycles Index (impute if SCI > SCI.min). SCI = Inf means that no imputation is performed.'
    if(length(SCI.min)==1)
    {
      if(is.numeric(SCI.min))
      {
        if(!is.infinite(SCI.min))
        {
          if(SCI.min < 0 | SCI.min > 1)
          {
            stop(char.cycle)
          }
        }
      }else
      {
        stop(char.cycle)
      }

    }else
    {
      stop(char.cycle)
    }

    # k.outliers
    char.k.coeff <- 'k.outliers is a positive number. Outliers are flagged based on the boxplot method, with the original 1.5 constant replaced by k.outliers*log(n)+1 to account for sample size and non-gaussian distributions. The Gaussian value is k.outliers=0.16, the default value is k.outliers=0.6 and k.outliers=Inf means that no outliers are flagged.'
    if(length(k.outliers) == 1)
    {
      if(!is.na(k.outliers))
      {
        if(k.outliers <= 0)
        {
          stop(char.k.coeff)
        }
      }else
      {
        stop(char.k.coeff)
      }
    }else
    {
      stop(char.k.coeff)
    }
  }

  # change colnames
  colnames.raw <- copy(colnames(data0))
  setnames(data0,colnames.raw,c('x','y'))

  # force the numeric class of y
  data0[,y := as.numeric(y)]

  # calculate the sequence of bin.side and bin.center
  out <- ctbi.timeseries(data0[[1]],bin.period=bin.period,bin.side=bin.side,bin.center=bin.center)
  seq.bin.side <- out$seq.bin.side
  seq.bin.center <- out$seq.bin.center
  time.step.median <- out$time.step.median
  rm(out)

  # calculate the number of bins, N.bin.tot
  N.bin.tot <- length(seq.bin.center)

  # find the intervals for the seq.bin.side,seq.bin.center, and add extra columns outliers & imputed
  data0[,':=' (index.bin = findInterval(x,seq.bin.side),side.index = (findInterval(x,seq.bin.center))+0.5,outliers = rep(NA_real_,length(y)),imputed = rep(NA_real_,length(y)))]

  # create the aggregated dataset, data1
  data1 <- merge(data0[,lapply(.SD,length),by=index.bin,.SDcols='y'],data.table(index.bin=1:N.bin.tot),all=TRUE)
  names(data1) <- c('index.bin','n.points')
  data1[is.na(n.points),n.points := 0]

  # calculate the median number of points per bin, bin.size
  bin.size <- unlist(data1[,n.points],use.names = FALSE)
  bin.size <- bin.size[bin.size!=0]
  if(length(bin.size) <= 4)
  {
    bin.size <- round(max(bin.size),digits=0)
  }else
  {
    bin.size <- round(median(bin.size),digits=0)
  }

  # calculate the minimum number of points for a bin to be accepted, n.bin.min
  n.bin.min <- ceiling(bin.size*(1-bin.max.f.NA))
  if(n.bin.min < 1)
  {
    n.bin.min <- 1
  }

  # add columns to data1: bin.center, bin.start, bin.end and the number of NA values per bin
  data1 <- merge(data1,data0[,lapply(.SD,hidd.count.NA),by=index.bin,.SDcols='y'],all=TRUE)
  names(data1) <- c('index.bin','n.points','n.NA')
  data1[is.na(n.NA),n.NA := 0]
  data1[,':=' (bin.center = seq.bin.center,bin.start = seq.bin.side[-length(seq.bin.side)], bin.end = seq.bin.side[-1])]

  # calculate the relative position of the values with respect to their bin.
  data0[,time.bin := hidd.rel.time(x,seq.bin.side),by=index.bin]
  all.mins <- data0[,lapply(.SD,min),by=index.bin,.SDcols='time.bin']
  if(nrow(all.mins) > 4)
  {
    shift.min <- (1/(2*bin.size))-median(all.mins[[2]])
  }else
  {
    shift.min <- (1/(2*bin.size))-min(all.mins[[2]])
  }
  data0[,time.bin := (time.bin+shift.min)] # the position of the first value needs to be 1/(2*bin.size) on average
  data0[,cycle.index := (findInterval(time.bin,(1:(bin.size-1))/bin.size)+1)]

  # step 0 : remove errors
  data0[is.infinite(y) | y < ylim[1] | y > ylim[2],outliers:=y]
  data0[is.infinite(y) | y < ylim[1] | y > ylim[2],y:=NA]

  # step 1 : replace bins with insufficient data with NA values
  data0[,y := hidd.replace(y,N.min.NA=n.bin.min),by=index.bin]

  # step 2 : calculate the long.term trend with the median
  data0 <- ctbi.long.term(data0,n.bin.min,seq.bin.side,outliers.checked=FALSE)

  # step 3 : calculate the median cycle
  list.cycle <- ctbi.cycle(data0,bin.size,outliers.checked=FALSE)
  data0 <- list.cycle$data0.l
  rm(list.cycle)

  # step 4 : calculate the outliers
  data0 <- ctbi.outliers(data0,k.outliers)

  # repeat steps 1,2,3 with the mean
  data0[,y := hidd.replace(y,N.min.NA=n.bin.min),by=index.bin]
  data0 <- ctbi.long.term(data0,n.bin.min,seq.bin.side,outliers.checked=T)
  list.cycle <- ctbi.cycle(data0,bin.size,outliers.checked=T)
  data0 <- list.cycle$data0.l
  mean.cycle <- list.cycle$FUN.cycle.l
  rm(list.cycle)

  # change bin sign for rejected bins
  data0[,bin.accepted := ifelse(sum(!is.na(y)) >= n.bin.min,index.bin,-index.bin),by=index.bin]

  # calculate SCI
  N.bin.accepted <- unique(unlist(data0[,bin.accepted]))
  N.bin.accepted <- length(N.bin.accepted[N.bin.accepted > 0])
  if(N.bin.accepted > 2)
  {
    SS.tot <- sum((data0[bin.accepted > 0,y]-data0[bin.accepted > 0,long.term])^2,na.rm=TRUE)
    SS.res <- sum((data0[bin.accepted > 0,y]-data0[bin.accepted > 0,long.term]-data0[bin.accepted > 0,cycle])^2,na.rm=TRUE)
    SCI <- round(1-(SS.res/SS.tot)-(1/N.bin.accepted),digits=3)

    # impute data
    if(SCI >= SCI.min)
    {
      data0[bin.accepted > 0 & is.na(y),imputed := long.term+cycle]
      # some imputed values might be impossible
      data0[imputed < ylim[1],imputed := ylim[1]]
      data0[imputed > ylim[2],imputed := ylim[2]]
      data0[!is.na(imputed),y := imputed]

      for(i in 1:2) # repeat twice
      {
        # the cycle + long.term need to be recalculated.
        data0 <- ctbi.long.term(data0,n.bin.min,seq.bin.side,outliers.checked=T)
        list.cycle <- ctbi.cycle(data0,bin.size,outliers.checked=T)
        data0 <- list.cycle$data0.l
        mean.cycle <- list.cycle$FUN.cycle.l
        rm(list.cycle)
        data0[!is.na(imputed),imputed := long.term+cycle]
        # some imputed values might be impossible
        data0[imputed < ylim[1],imputed := ylim[1]]
        data0[imputed > ylim[2],imputed := ylim[2]]
        data0[!is.na(imputed),y := imputed]
      }

      # recalculate SCI
      SS.tot <- sum((data0[bin.accepted > 0,y]-data0[bin.accepted > 0,long.term])^2,na.rm=TRUE)
      SS.res <- sum((data0[bin.accepted > 0,y]-data0[bin.accepted > 0,long.term]-data0[bin.accepted > 0,cycle])^2,na.rm=TRUE)
      SCI <- round(1-(SS.res/SS.tot)-(1/N.bin.accepted),digits=3)
    }
  }else
  {
    SCI <- NA
  }

  # add columns to data1: n.imputed and n.outliers
  data1 <- merge(data1,data0[,lapply(.SD,hidd.count.noNA),by=index.bin,.SDcols='imputed'],all=TRUE)
  setnames(data1,'imputed','n.imputed')
  data1 <- merge(data1,data0[,lapply(.SD,hidd.count.noNA),by=index.bin,.SDcols='outliers'],all=TRUE)
  setnames(data1,'outliers','n.outliers')
  data1[is.na(n.imputed),n.imputed := 0]
  data1[is.na(n.outliers),n.outliers := 0]

  # perform the aggregation
  if(1)
  {
    if(bin.FUN == 'mean')
    {
      data1 <- merge(data1,data0[,lapply(.SD,hidd.sd,n.bin.min),by=index.bin,.SDcols='y'],all=TRUE)
      add.char <- paste0('sd.',colnames.raw[2])
      setnames(data1,'y',add.char)
      data1 <- merge(data1,data0[,lapply(.SD,hidd.mean,n.bin.min),by=index.bin,.SDcols='y'],all=TRUE)
    }

    if(bin.FUN == 'median')
    {
      data1 <- merge(data1,data0[,lapply(.SD,hidd.mad,n.bin.min),by=index.bin,.SDcols='y'],all=TRUE)
      add.char <- paste0('mad.',colnames.raw[2])
      setnames(data1,'y',add.char)
      data1 <- merge(data1,data0[,lapply(.SD,hidd.median,n.bin.min),by=index.bin,.SDcols='y'],all=TRUE)
    }

    if(bin.FUN == 'sum')
    {
      data1 <- merge(data1,data0[,lapply(.SD,hidd.sum,n.bin.min),by=index.bin,.SDcols='y'],all=TRUE)
      add.char <- NULL
    }
  }

  # change sign of the bins that have been rejected
  data1[is.na(y),index.bin := -index.bin]

  # put back the raw data, remove the outliers, add the imputed data and remove useless columns
  data0[,y := data.input[[2]]]
  data0[!is.na(outliers),y := NA]
  data0[!is.na(imputed),y := imputed]
  data0[,index.bin := bin.accepted]
  data0[, ':=' (bin.accepted = NULL, cycle.index = NULL,side.index = NULL)]

  # the long.term and cycle components are NA for rejected bins
  data0[index.bin < 0, ':=' (long.term = NA, cycle = NA)]

  # rearrange columns and column names.
  setnames(data0,'x',colnames.raw[1])
  setnames(data1,'bin.center',colnames.raw[1])
  setnames(data0,'y',colnames.raw[2])
  setnames(data1,'y',colnames.raw[2])
  setcolorder(data0,c(colnames.raw,'index.bin','long.term','cycle','outliers','imputed','time.bin'))
  setcolorder(data1,c(colnames.raw,'bin.start','bin.end','index.bin','n.points','n.NA','n.imputed','n.outliers',add.char))

  # add the time step of the mean.cycle based on the first bin.
  epsilon <- ((1/(2*bin.size))-shift.min)*(as.numeric(seq.bin.side[2])-as.numeric(seq.bin.side[1]))
  t.cycle <- seq(from=(seq.bin.side[1]+epsilon),length.out=bin.size,to=(seq.bin.side[2]+epsilon-time.step.median))
  t.bin <- ((1:bin.size)/bin.size)-(1/(2*bin.size))
  mean.cycle[,':=' (generic.time.bin1 = t.cycle, time.bin = t.bin, cycle.index = NULL)]
  setcolorder(mean.cycle,c('generic.time.bin1','mean','sd','time.bin'))

  # convert the outputs to the original format of the input
  if(!log.data.table)
  {
    setDF(data0)
    setDF(data1)
    setDF(mean.cycle)
  }else
  {
    setDT(data0)
    setDT(data1)
    setDT(mean.cycle)
  }

  list.main <- list(data0=data0,data1=data1,SCI=SCI,mean.cycle=mean.cycle,bin.size=bin.size,n.bin.min=n.bin.min)

  return(list.main)
}
