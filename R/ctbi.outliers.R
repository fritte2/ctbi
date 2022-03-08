#' @title ctbi.outliers
#'
#' @description Flag the outliers with the Logbox method, which replaces the original constant 1.5 of the Boxplot rule with k.outliers*log(n)+1, with n being the sample size of the residuals (detrended & deseasonalized).
#'
#' @param data0 data.table with the columns x (time series), y (values), long.term (the long-term trend) and cycle (the cyclic component)
#' @param k.outliers positive numeric that defines the outlier level in the Logbox method used to flag outliers, with k.outliers = 0.16 corresponding to a Gaussian distribution and k.outliers = 0.8 to an Exponential distribution. The default value of k.outliers = 0.6 has been calculated based on a set of distributions with moderate skewness and kurtosis (the Pearson family). k.outliers = Inf means that no outliers are flagged
#'
#' @return data0 with the outliers flagged (column outliers), and the corresponding y values set to NA
#' @examples
#' library(data.table)
#' x <- seq(from=as.Date('2001-01-01'),to=as.Date('2010-12-01'),by='1 month')
#' y <- 3*cos(2*pi*(0:(length(x)-1))/12)+runif(length(x))
#' y[c(5,15,20)] <- c(-30,20,40) # add 3 outliers
#' long.term <- rep(0,length(x))
#' cycle <- 3*cos(2*pi*(0:(length(x)-1))/12)
#' k.outliers <- 0.6
#' data0 <- data.table(x=x,y=y,cycle=cycle,long.term=long.term)
#' data0.with.outliers.flagged <- ctbi.outliers(data0,k.outliers)
#' @export

ctbi.outliers <- function(data0,k.outliers)
{
  # residuals
  data0[,residuals := y-long.term-cycle]

  if(sum(!is.na(data0[,residuals])) != 0)
  {
    res.non0 <- unlist(data0[,residuals],use.names = FALSE)
    res.non0 <- res.non0[!is.na(res.non0)]
    res.non0 <- res.non0[res.non0 != 0]

    if(length(res.non0) > 4 & !is.infinite(k.outliers))
    {
      q.25 <- quantile(res.non0,0.25)
      q.75 <- quantile(res.non0,0.75)
      alpha <- k.outliers*log(sum(!is.na(data0[,residuals])))+1
      lower.boundary <- q.25 - alpha*(q.75-q.25)
      upper.boundary <- q.75 + alpha*(q.75-q.25)

      data0[residuals < lower.boundary | residuals > upper.boundary,outliers := y]
      data0[residuals < lower.boundary | residuals > upper.boundary,y := NA]
    }
  }

  data0[,residuals := NULL]

  return(data0)
}
