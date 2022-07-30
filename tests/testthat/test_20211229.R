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
