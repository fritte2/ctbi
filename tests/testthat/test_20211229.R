set.seed(1)
# example 1, airpassenger data
example1 <- data.table(month = seq(from=as.Date('1949-01-01'),to=as.Date('1960-12-01'),
                                   by='1 month'),airp = as.numeric(AirPassengers))
example1[sample(1:144,30),airp := NA]
example1[c(5,30,50,130),airp := c(-5,850,0,100)]
example1 <- example1[-(70:100),]
bin.period <- '1 year' # aggregation performed every year
bin.side <- as.Date('2000-01-01') # side of a bin
bin.FUN <- 'sum' # calculate the number of passengers for each year
bin.max.f.NA <- 0.2 # maximum of 20% of missing values per bin to accept the bin
ylim <- c(0,Inf) # a negative number of passengers is impossible
list.main <- ctbi(example1,bin.period=bin.period,
                       bin.side=bin.side,bin.FUN=bin.FUN,
                       ylim=ylim,bin.max.f.NA=bin.max.f.NA)
data0.example1 <- list.main$data0 # cleaned raw dataset
data1.example1 <- list.main$data1 # aggregated dataset.
SCI.example1 <- list.main$SCI # this data set shows a moderate seasonality
mean.cycle.example1 <- list.main$mean.cycle # this data set shows a moderate seasonality
bin.size.example1 <- list.main$bin.size # 12 data points per bin on average (12 months per year)
plot(example1[,month],example1[,airp]) # contaminated data
lines(data0.example1[,month],data0.example1[,airp],col='red') # cleaned data
#ctbi.plot(list.main,show.n.bin=15) # run to see the long.term + cyclic component together

# example 2, sunspot data
example2 <- data.frame(year = 1700:1988,sunspot = as.numeric(sunspot.year))
example2[sample(1:289,30),'sunspot'] <- NA
example2[c(5,30,50),'sunspot'] <- c(-50,300,400)
example2 <- example2[-(100:200),]
bin.period <- 11 # aggregation performed every 11 years (the year is numeric here)
bin.side <- 1989 # to capture the last year, 1988, in a complete bin
bin.FUN <- 'mean'
bin.max.f.NA <- 0.2
ylim <- c(0,Inf)

list.main <- ctbi(example2,bin.period=bin.period,
                       bin.side=bin.side,bin.FUN=bin.FUN,
                       ylim=ylim,bin.max.f.NA=bin.max.f.NA)
data0.example2 <- list.main$data0 # cleaned raw dataset
data1.example2 <- list.main$data1 # aggregated dataset.
SCI.example2 <- list.main$SCI # this data set shows a moderate seasonality
mean.cycle.example2 <- list.main$mean.cycle # this data set shows a moderate seasonality
bin.size.example2 <- list.main$bin.size # 12 data points per bin on average (12 months per year)

plot(example2[,'year'],example2[,'sunspot']) # contaminated data
lines(data0.example2[,'year'],
      data0.example2[,'sunspot'],col='red') # cleaned data
lines(data1.example2[,'year'],
      data1.example2[,'sunspot'],col='blue',lwd=2) # aggregated data

plot(mean.cycle.example2[,'generic.time.bin1'],
     mean.cycle.example2[,'mean'],type='l',ylim=c(-80,80))
lines(mean.cycle.example2[,'generic.time.bin1'],
      mean.cycle.example2[,'mean']+mean.cycle.example2[,'sd'],type='l',lty=2)
lines(mean.cycle.example2[,'generic.time.bin1'],
      mean.cycle.example2[,'mean']-mean.cycle.example2[,'sd'],type='l',lty=2)
title(paste0('mean cycle (weak cyclicity: SCI = ',SCI.example2,')'))
# the SCI is much higher on the raw dataset without contamination (SCI = 0.34)
# ctbi.plot(list.main,show.n.bin=10)
