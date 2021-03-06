
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ctbi

<!-- badges: start -->
<!-- badges: end -->

The goal of ctbi is to clean, decompose, impute and aggregate univariate
time series. Ctbi stands for *Cyclic/trend decomposition using bin
interpolation* : the time series is divided into a sequence of
non-overlapping bins. The long-term trend is a linear interpolation of
the mean values between successive bins and the cyclic component is the
mean stack of detrended data within all bins. Outliers present in the
residuals are flagged using an enhanced Boxplot rule called Logbox,
which replaces the original 1.5 constant with
![A \\times \\log(n)+B+C/n](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;A%20%5Ctimes%20%5Clog%28n%29%2BB%2BC%2Fn "A \times \log(n)+B+C/n")
(![n](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;n "n")
being the sample size, C = 36 and A,B being automatically calculated on
a predictor of the kurtosis-excess). The strength of the cyclic pattern
within each bin is quantified by a new metric, the Stacked Cycles Index
(SCI), with SCI \<= 0 associated with no cyclicity and SCI = 1 a
perfectly cyclic signal.

## Installation

You can install the development version of ctbi from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("fritte2/ctbi")
```

## Example

``` r
library(ctbi)
example1 <- data.frame(year = 1700:1988,sunspot = as.numeric(sunspot.year))
example1[sample(1:289,30),'sunspot'] <- NA # contaminate data with missing values
example1[c(5,30,50),'sunspot'] <- c(-50,300,400) # contaminate data with outliers
example1 <- example1[-(70:100),]
bin.period <- 11 # aggregation performed every 11 years (the year is numeric here)
bin.side <- 1989 # to capture the last year, 1988, in a complete bin
bin.FUN <- 'mean'
bin.max.f.NA <- 0.2 # maximum of 20% of missing data per bin
ylim <- c(0,Inf) # negative values are impossible

list.main <- ctbi(example1,bin.period=bin.period,
                       bin.side=bin.side,bin.FUN=bin.FUN,
                       ylim=ylim,bin.max.f.NA=bin.max.f.NA)
data0.example1 <- list.main$data0 # cleaned raw dataset
data1.example1 <- list.main$data1 # aggregated dataset.
mean.cycle.example1 <- list.main$mean.cycle # this data set shows a moderate seasonality
summary.bin.example1 <- list.main$summary.bin # confirmed with SCI = 0.50
summary.outlier.example1 <- list.main$summary.outlier
```
