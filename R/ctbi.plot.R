#' @title ctbi.plot
#'
#' @description Plot the raw data with the bins, long-term trend and cyclic component shown.
#'
#' @param list.main the list output from the function ctbi.main
#' @param show.n.bin number of bins shown within one graphic
#' @param show.outliers boolean to show or hide flagged outliers
#'
#' @return No return value
#' @export

ctbi.plot <- function(list.main,show.outliers=TRUE,show.n.bin=10)
{
  # Add legend function (From Jan van der Laan, stackoverflow)
  # https://stackoverflow.com/questions/3932038/plot-a-legend-outside-of-the-plotting-area-in-base-graphics/3932558
  add_legend <- function(...) {
    opar <- par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0),
                mar=c(0, 0, 0, 0), new=TRUE)
    on.exit(par(opar))
    plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
    legend(...)
  }

  data1 <- as.data.frame(list.main$data1)
  data0 <- as.data.frame(list.main$data0)
  SCI <- list.main$SCI
  rm(list.main)

  # add NA values to data0 if entire bins are missing
  read_ <- data1[,'n.points'] == 0
  if(sum(read_)!=0)
  {
    missing.bins <- data1[read_,]

    for(i in 1:length(missing.bins[,1]))
    {
      add.bin <- data0[1,]
      add.bin[1,1] <- missing.bins[i,1]
      add.bin[1,-1] <- NA
      add.bin[1,'index.bin'] <- missing.bins[i,'index.bin']
      data0 <- rbind(data0,add.bin)
    }

    data0 <- data0[order(data0[,1]),]
  }


  seq.bin.side <- c(data1[1,'bin.start'],data1[,'bin.end'])

  # number of variables
  n.var <- length(data0[1,])-1
  col.var <- colnames(data0)
  col.var <- col.var[-1]

  if(show.n.bin > length(seq.bin.side))
  {
    loop.k <- c(1,length(seq.bin.side))
  }else
  {
    loop.k <- seq(from=1,by=show.n.bin,to=length(seq.bin.side))
    if(loop.k[length(loop.k)] < length(seq.bin.side))
    {
      loop.k <- c(loop.k,length(seq.bin.side))
    }
  }

  data0.save <- data0
  for(k in 1:(length(loop.k)-1))
  {
    beg_ <- seq.bin.side[loop.k[k]]
    end_ <- seq.bin.side[loop.k[k+1]]

    data0 <- data0.save[beg_ <= data0.save[,1] & data0.save[,1] < end_,]

    read.good <- data0[,'index.bin'] > 0
    if(sum(read.good)!=0)
    {
      if(show.outliers)
      {
        y.lim <- c(min(c(data0[,2],data0[,'outliers'],data0[,'long.term']+data0[,'cycle']),na.rm=TRUE),max(c(data0[,2],data0[,'outliers'],data0[,'long.term']+data0[,'cycle']),na.rm=TRUE))
      }else
      {
        y.lim <- c(min(c(data0[read.good,2],data0[read.good,'long.term']+data0[read.good,'cycle']),na.rm=TRUE),max(c(data0[read.good,2],data0[read.good,'long.term']+data0[read.good,'cycle']),na.rm=TRUE))
      }

      par(mar = c(5, 4, 1.4, 0.6))
      plot(data0[read.good,1],data0[read.good,2],ylim=y.lim,xlim=c(min(data0[,1]),max(data0[,1])),ylab=colnames(data0)[2],xlab='time')
      points(data0[!read.good,1],data0[!read.good,2],col='grey')
      lines(data0[,1],data0[,'long.term'],col='red',lwd=2)
      points(data0[,1],data0[,'imputed'],col='tan2',pch=16)
      if(show.outliers)
      {
      points(data0[,1],data0[,'outliers'],col='red',pch=16)
      }
      abline(v=seq.bin.side[beg_ <= seq.bin.side & seq.bin.side <= end_])


        if(sum(data0[,'cycle'],na.rm=TRUE)!=0)
        {
        lines(data0[,1],data0[,'long.term']+data0[,'cycle'],lwd=2,col='blue')
          if(sum(data0[,'imputed'],na.rm=TRUE)!=0)
          {
            if(show.outliers)
            {
            add_legend("topright", legend=c("data","imputed","outliers","long-term","long-term + cycles"), pch=c(1,16,16,NA,NA), col=c('black','tan2','red','red','blue'),lty=c(NA,NA,NA,1,1),lwd=c(NA,NA,NA,2,2),horiz=TRUE, bty='n', cex=0.8)
            }else
            {
            add_legend("topright", legend=c("data","imputed","long-term","long-term + cycles"), pch=c(1,16,NA,NA), col=c('black','tan2','red','blue'),lty=c(NA,NA,1,1),lwd=c(NA,NA,2,2),horiz=TRUE, bty='n', cex=0.8)
            }
          }else
          {
            if(show.outliers)
            {
            add_legend("topright", legend=c("data","outliers","long-term","long-term + cycles"), pch=c(1,16,NA,NA), col=c('black','red','red','blue'),lty=c(NA,NA,1,1),lwd=c(NA,NA,2,2),horiz=TRUE, bty='n', cex=0.8)
            }
            else
            {
            add_legend("topright", legend=c("data","long-term","long-term + cycles"), pch=c(1,NA,NA), col=c('black','red','blue'),lty=c(NA,1,1),lwd=c(NA,2,2),horiz=TRUE, bty='n', cex=0.8)
            }
          }
        }else
        {
          if(show.outliers)
          {
          add_legend("topright", legend=c("data","outliers","long-term"), pch=c(1,16,NA), col=c('black','red','red'),lty=c(NA,NA,1),lwd=c(NA,NA,2),horiz=TRUE, bty='n', cex=0.8)
          }
          else
          {
          add_legend("topright", legend=c("data","long-term"), pch=c(1,NA), col=c('black','red'),lty=c(NA,1),lwd=c(NA,2),horiz=TRUE, bty='n', cex=0.8)
          }
        }

      message('Press Enter to see next plot')
      para.scan <- readline()
    }
  }

  return()
}
