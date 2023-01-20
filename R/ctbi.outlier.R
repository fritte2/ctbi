#' @title ctbi.outlier
#'
#' @description \bold{Please cite} the following companion paper if you're using the \code{ctbi} package: Ritter, F.: Technical note: A procedure to clean, decompose, and aggregate time series, Hydrol. Earth Syst. Sci., 27, 349–361, https://doi.org/10.5194/hess-27-349-2023, 2023.
#'
#' Outliers in an univariate dataset \code{y} are flagged using an enhanced box plot rule (called \bold{Logbox}, input: \code{coeff.outlier}) that is adapted to non-Gaussian data and keeps the type I error at \eqn{\frac{0.1}{\sqrt{n}}} \% (percentage of erroneously flagged outliers).
#'
#' The box plot rule flags data points as outliers if they are below \eqn{L} or above \eqn{U} using the sample quantile \eqn{q}:
#'
#' \eqn{L = q(0.25)-\alpha \times (q(0.75)- q(0.25))}
#'
#' \eqn{U = q(0.75)+\alpha \times (q(0.75)- q(0.25))}
#'
#' \bold{Logbox} replaces the original \eqn{\alpha = 1.5} constant of the box plot rule with \eqn{\alpha = A \times \log(n)+B+\frac{C}{n}}. The variable \eqn{n \geq 9} is the sample size, \eqn{C = 36} corrects biases emerging in small samples, and \eqn{A} and \eqn{B} are automatically calculated on a predictor of the maximum tail weight defined as \eqn{m_{*} = \max(m_{-},m_{+})-0.6165}.
#'
#' The two functions (\eqn{m_{-}},\eqn{m_{+}}) are defined as:
#'
#' \eqn{m_{-} = \frac{q(0.875)- q(0.625)}{q(0.75)- q(0.25)}}
#'
#' \eqn{m_{+} = \frac{q(0.375)- q(0.125)}{q(0.75)- q(0.25)}}
#'
#' And finally, \eqn{A = f_{A}(}\eqn{m_{*}}\eqn{)} and \eqn{B = f_{B}(}\eqn{m_{*}}\eqn{)} with \eqn{m_{*}} restricted to [0,2]. The functions \eqn{(f_{A},f_{B})} are defined as:
#'
#' \eqn{f_{A}(x) = 0.2294\exp(2.9416x-0.0512x^{2}-0.0684x^{3})}
#'
#' \eqn{f_{B}(x) = 1.0585+15.6960x-17.3618x^{2}+28.3511x^{3}-11.4726x^{4}}
#'
#' Both functions have been calibrated on the Generalized Extreme Value and Pearson families.
#'
#' @param y univariate data (numeric vector)
#' @param coeff.outlier one of \code{coeff.outlier} = 'auto' (default value), \code{coeff.outlier} = 'gaussian', \code{coeff.outlier} = c(A,B,C) or \code{coeff.outlier} = \code{NA}. If \code{coeff.outlier} = 'auto', C = 36 and the coefficients A and B are calculated on \eqn{m_{*}}. If \code{coeff.outlier} = 'gaussian', \code{coeff.outlier} = c(0.08,2,36), adapted to the Gaussian distribution. If \code{coeff.outlier} = \code{NA}, no outliers are flagged
#'
#' @return A list that contains:
#' @return \bold{xy}, a two columns data frame that contains the clean data (first column) and the outliers (second column)
#' @return \bold{summary.outlier}, a vector that contains A, B, C, \eqn{m_{*}}, the size of the residuals (n), and the lower and upper outlier threshold
#' @examples
#' x <- runif(30)
#' x[c(5,10,20)] <- c(-10,15,30)
#' example1 <- ctbi.outlier(x)
#' @export

ctbi.outlier <- function(y,coeff.outlier='auto')
{
  # type of outlier checking
  log.user.coeff <- FALSE
  log.no.outlier.checking <- FALSE
  if(length(coeff.outlier) == 1)
  {
    if(is.na(coeff.outlier))
    {
      log.no.outlier.checking <- TRUE
    }else
    {
      if(coeff.outlier == 'gaussian')
      {
        log.user.coeff <- TRUE
        coeff.outlier <- c(0.08,2,36) # gaussian values
      }
    }
  }else
  {
    if(length(coeff.outlier) == 3)
    {
      coeff.outlier[is.na(coeff.outlier)] <- 0
      log.user.coeff <- TRUE
    }else
    {
      log.no.outlier.checking <- TRUE
    }
  }

  xy <- data.frame(y.clean=y,outliers=rep(NA,length(y)),stringsAsFactors = F)

  if(log.no.outlier.checking)
  {
    A.coeff <- NA
    B.coeff <- NA
    C.coeff <- NA
    m.star <- NA
    n.residuals <- NA
    lower.outlier.threshold <- NA
    upper.outlier.threshold <- NA
  }else
  {
    res.nonylim <- as.numeric(xy[,'y.clean'])
    res.nonylim <- res.nonylim[!is.na(res.nonylim)]
    n.residuals <- length(res.nonylim)

    if(n.residuals > 8)
    {
      if(log.user.coeff)
      {
        q0.25 <- as.numeric(quantile(res.nonylim,0.25))
        q0.75 <- as.numeric(quantile(res.nonylim,0.75))
        A.coeff <- coeff.outlier[1]
        B.coeff <- coeff.outlier[2]
        C.coeff <- coeff.outlier[3]
        m.star <- NA
      }else
      {
        q0.125 <- as.numeric(quantile(res.nonylim,0.125))
        q0.25 <- as.numeric(quantile(res.nonylim,0.25))
        q0.375 <- as.numeric(quantile(res.nonylim,0.375))
        q0.625 <- as.numeric(quantile(res.nonylim,0.625))
        q0.75 <- as.numeric(quantile(res.nonylim,0.75))
        q0.875 <- as.numeric(quantile(res.nonylim,0.875))

        m.plus <- (q0.875-q0.625)/(q0.75-q0.25)
        m.minus <- (q0.375-q0.125)/(q0.75-q0.25)

        if(!is.na(m.minus) & !is.na(m.plus))
        {
          m.star <- max(c(m.plus,m.minus))-0.6165

          if(m.star < 0){m.star <- 0}
          if(m.star > 2){m.star <- 2}

          # version 13
          a1 <- 0.2294
          a2 <- 2.9416
          a3 <- -0.0512
          a4 <- -0.0684
          A.coeff <- round(a1*exp(a2*m.star+a3*m.star^2+a4*m.star^3),digits=2)

          # version 13
          b1 <- 1.0585
          b2 <- 15.6960
          b3 <- -17.3618
          b4 <- 28.3511
          b5 <- -11.4726
          B.coeff <- round(b1+b2*m.star+b3*(m.star^2)+b4*(m.star^3)+b5*(m.star^4),digits=2)

          C.coeff <- 36

        }else
        {
          m.star <- NA
          A.coeff <- NA
          B.coeff <- NA
          C.coeff <- NA
          lower.outlier.threshold <- NA
          upper.outlier.threshold <- NA
        }
      }


      if(!is.na(A.coeff))
      {
        alpha <- A.coeff*log(n.residuals)+B.coeff+C.coeff/n.residuals
        lower.outlier.threshold <- q0.25 - alpha*(q0.75-q0.25)
        upper.outlier.threshold <- q0.75 + alpha*(q0.75-q0.25)

        read_ <- (xy[,'y.clean'] < lower.outlier.threshold) | (xy[,'y.clean'] > upper.outlier.threshold)
        read_[is.na(read_)] <- FALSE
        read_ <- as.logical(read_)
        xy[read_,'outliers'] <- xy[read_,'y.clean']
        xy[read_,'y.clean'] <- NA_real_
      }
    }
    else
    {
      A.coeff <- NA
      B.coeff <- NA
      C.coeff <- NA
      m.star <- NA
      lower.outlier.threshold <- NA
      upper.outlier.threshold <- NA
    }

  }

  summary.xy <- c(A.coeff,B.coeff,C.coeff,m.star,n.residuals,lower.outlier.threshold,upper.outlier.threshold)
  names(summary.xy) <- c('A','B','C','m.star','n','lower.outlier.threshold','upper.outlier.threshold')
  list.out <- list(xy=xy,summary.outlier=summary.xy)
  return(list.out)
}
