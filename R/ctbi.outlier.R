#' @title ctbi.outlier
#'
#' @description Flag outliers in univariate datasets with the Logbox method, which replaces the original constant 1.5 of the Boxplot rule with Alog(n)+B+C/n, with n being the sample size (n > 8). For 2 < n < 9, the lower/upper outlier threshold are defined as median(y) +/- 12.5MAD with MAD the median absolute deviation. Details are given in Ritter, F.: Technical note: A procedure to clean, decompose and aggregate time series, Hydrol. Earth Syst. Sci. Discuss., in review, 2022. Temporary DOI linked to the latest version of the manuscript until the review is finished: <https://doi.org/10.31223/X5107C>.
#'
#' @param y univariate data (numeric vector)
#' @param coeff.outlier One of coeff.outlier = 'auto' (default value), coeff.outlier = 'gaussian', coeff.outlier = c(A,B,C) or coeff.outlier = NA. If coeff.outlier = 'auto', C = 36 and the coefficients A and B are calculated on m.star, a predictor of the kurtosis excess. If coeff.outlier = 'gaussian', coeff.outlier = c(0.08,2,36), adapted to the Gaussian distribution. If coeff.outlier = NA, no outliers are flagged.
#'
#' @return A list that contains:
#' @return xy, a two columns data frame that contains the clean data (first column) and the outliers (second column).
#' @return summary.outlier = c(A,B,C,m.star,n,lower.outlier.threshold,upper.outlier.threshold)
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

      if(n.residuals > 2)
      {
        beta <- 12.5

        MAD0 <- mad(res.nonylim)

        if(MAD0 != 0)
        {
          lower.outlier.threshold <- median(res.nonylim)-beta*MAD0
          upper.outlier.threshold <- median(res.nonylim)+beta*MAD0

          read_ <- (xy[,'y.clean'] < lower.outlier.threshold) | (xy[,'y.clean'] > upper.outlier.threshold)
          read_[is.na(read_)] <- FALSE
          read_ <- as.logical(read_)
          xy[read_,'outliers'] <- xy[read_,'y.clean']
          xy[read_,'y.clean'] <- NA_real_
        }else
        {
          lower.outlier.threshold <- NA
          upper.outlier.threshold <- NA
        }

      }else
      {
        lower.outlier.threshold <- NA
        upper.outlier.threshold <- NA
      }
    }

  }

  summary.xy <- c(A.coeff,B.coeff,C.coeff,m.star,n.residuals,lower.outlier.threshold,upper.outlier.threshold)
  names(summary.xy) <- c('A','B','C','m.star','n','lower.outlier.threshold','upper.outlier.threshold')
  list.out <- list(xy=xy,summary.outlier=summary.xy)
  return(list.out)
}
