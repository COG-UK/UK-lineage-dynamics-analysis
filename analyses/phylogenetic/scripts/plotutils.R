require(lubridate)

capitalise <- function(str) {
    return( paste0( toupper(substr(str, 1, 1)), substr(str, 2, nchar(str))))
}


#' Given a set of dates and frequencies on those dates, get a histogram
#'
#' Slow and inefficient way to do this, but it only needs to be done a few times
getHistogram <- function(date, freq, breaks="weeks", maxDate=as.Date("2020-06-28")) {
  result <- c()
  for (i in 1:length(date)) {
    if (!is.na(freq[i]) && date[i] <= maxDate) {
      result <- c(result, rep(format.Date(date[i]), freq[i]))
    }
  }
  result <- as.Date(result)
  
  return(hist(result, breaks=breaks, plot=FALSE, right=FALSE))
}


plotShadedAxes <- function(xlim = c("2020-01-01", "2020-04-16"), ylim=c(0,1), ylab="", xlab="", 
                           bigBreaks="weeks", smallBreaks="days", shadeCol="#EDEDED", side=2,
                           thinXLabel=1, thinYLabel=1, las=2, srt=NA,
                           label=NA, line=1, decimal=FALSE, dateFormat="%b %d", 
                           axes = FALSE, scientific=FALSE, ...) {
  
      startDate  <- as.Date(xlim[1])
      endDate    <- as.Date(xlim[2])

      bigTicks   <- seq.Date(startDate, endDate, by=bigBreaks)
      smallTicks <- seq.Date(startDate, endDate, by=smallBreaks)
      
      if (decimal) {
          startDate  <- lubridate::decimal_date(startDate)
          endDate    <- lubridate::decimal_date(endDate)
          
          bigTicks   <- lubridate::decimal_date(bigTicks)
          smallTicks <- lubridate::decimal_date(smallTicks)
      }
      
      plot(1, type='n', xlim=c(startDate, endDate), ylim=ylim, axes=FALSE, xaxs='i', ylab=ylab, xlab=xlab, ...)
      rect(xlim[1], ylim[1], xlim[2], ylim[2], border = NA, col = "#FFFFFF")
      
      # Shaded bars
      yheight <- abs(diff(ylim))
      bigTickWidth <- bigTicks[2]-bigTicks[1]
      for (i in 1:(length(bigTicks))) {
          if (i %% 2 == 0) {
              rect(bigTicks[i], ylim[1]-0.1*yheight, bigTicks[i]+bigTickWidth, ylim[2]+0.1*yheight, col=shadeCol, border=NA)
          }
      }
      
      if (axes) {
          rect(xlim[1], ylim[1], xlim[2], ylim[2], xpd=TRUE)
      }
      
      # Time axis
      if (decimal) {
          xlabels <- format.Date(as.Date(lubridate::round_date(lubridate::date_decimal(bigTicks), unit = "day")), format=dateFormat)
      } else {
          xlabels <- format.Date(bigTicks, format=dateFormat)
      }
      #axis(1, at=bigTicks, labels=NA, lwd=0, lwd.ticks=1, las=las)
      if (!is.na(srt)) {
          axis(1, at=bigTicks[seq(1,length(xlabels), by=thinXLabel)], labels=NA, lwd=0, lwd.ticks=1, las=las)  
          text(x = bigTicks[seq(1,length(xlabels), by=thinXLabel)], y = (ylim[1]-0.1*(ylim[2]-ylim[1])), 
               labels = xlabels[seq(1,length(xlabels), by=thinXLabel)], srt=45, pos = 2, offset = -1,
               cex = par("cex.axis"), xpd=TRUE)  
      } else {
          axis(1, at=bigTicks[seq(1,length(xlabels), by=thinXLabel)], labels=xlabels[seq(1,length(xlabels), by=thinXLabel)], lwd=0, lwd.ticks=1, las=las)  
      }
      
      if (length(smallTicks) != length(bigTicks)) {
          axis(1, at=smallTicks, labels=NA, tcl=-0.25, lwd=0, lwd.ticks=1)
      }
      axis(1, at=c(startDate, endDate), labels=NA, tcl=-0.25, lwd=1, lwd.ticks=NA)
      
      if (side == 2 || side == 4) {
          #axis(side, las=1)
          yticks <- axTicks(side)
          yticks <- yticks[seq(1,length(yticks), by=thinYLabel)]
          axis(side, las=las, at=yticks, labels=format(yticks, scientific=scientific))
      }
      
      x <- startDate - 0.15*(endDate - startDate)
      mtext(label, side=3, line=line, at=x, cex = par("cex.main"))
}



sciNotation <- function(x, digits = 1) {
  if (length(x) > 1) {
    return(append(sciNotation(x[1]), sciNotation(x[-1])))
  }
  if (!x) return(0)
  exponent <- floor(log10(x))
  base <- round(x / 10^exponent, digits)
  if (base == 1) {
    as.expression(substitute(10^exponent, list(exponent = exponent))) 
  } else {
    as.expression(substitute(base %*% 10^exponent, list(base = base, exponent = exponent)))
  }
}

plotLogAxis <- function(lims, side=2, smallticks=TRUE, cex.axis=NA, las=2) {
  
  if (is.na(cex.axis)) {
      cex.axis <- par("cex.axis")
  }
  
  explims <- floor(log10(lims))
  ticks  <- 10^c(explims[1]:explims[2])
  axis(side, at=ticks, labels=sciNotation(ticks), las=las, cex.axis=cex.axis)
  
  if (smallticks == TRUE) {
      smallticks <- unlist(lapply(c((explims[1]-1):explims[2]), function(x) (2:9)*10^x))
      axis(side, at=smallticks, labels=NA, tcl=-0.25, lwd=1)
  }  
}

plotLogAxisReal <- function(lims, side=2, smallticks=TRUE, cex.axis=NA, las=2) {
  
  if (is.na(cex.axis)) {
    cex.axis <- par("cex.axis")
  }
  
  explims <- floor(log10(lims))
  ticks   <- 10^c(explims[1]:explims[2])
  axis(side, at=log(ticks), labels=sciNotation(ticks), las=las, cex.axis=cex.axis)
  
  if (smallticks == TRUE) {
      smallticks <- unlist(lapply(c((explims[1]-1):explims[2]), function(x) (2:9)*10^x))
      axis(side, at=log(smallticks), labels=NA, tcl=-0.25, lwd=1)
  }  
}


#' Plots a frequency distribution above a date axis. By default expects the frequencies in `heights` and the 
#' dates in `dates`. The frequency in slot `i` is assumed to be the count between `dates[i]` and `dates[i+1]`, 
#' that is: 
#' 
#'     `dates[i] <= X < dates[i+1]`  OR `sum(x > dates[i-1] && x <= dates[i])`
#'     
#' This is equivalent to running `heights = hist(data, breaks=dates, right=FALSE, plot=FALSE)$counts`
#' The reason for not using the standard behaviour of `hist()` is for the behaviour to mimic epi-weeks, 
#' e.g. 2020 epi-week 2 starts on 5 January and ends on 11 January. Using Sundays as breaks, `right=FALSE`
#'      should be used to get the correct limits. This week should then be plotted between Jan 05 and Jan 12
#'      on the x-axis when using a barplot. 
#' Using `right=FALSE` for daily breaks will mimic the same behaviour, where the count for e.g. Jan 05 will
#' be plot between Jan 05 and Jan 06. More detail below:
#' 
#' If `heights` is a one-dimensional vector:   
#' - If `barplot` is TRUE a barplot is drawn, with the bar starting on `dates[i]` indicating `heights[i]`.
#' - If `barplot` is FALSE a polygon is drawn, with the height on `dates[i]` equal to `heights[i]`.
#' 
#' If `heights` is a matrix:
#' - It is assumed that each column represents a date and rows represent frequencies of different classes 
#'   on that date.
#' - If `barplot` is TRUE a stacked barplot is drawn, with the bar starting on `dates[i]` indicating the 
#'   rowSum of `heights[, i]`.
#' - If `barplot` is FALSE a stacked polygon is drawn, with the height on `dates[i]` equal to the 
#'   rowSum of `heights[, i]`.
#' - In both cases, if `col` is a vector colors are recycled.
#' 
#' If `heights` is a matrix with exactly 3 rows and `plot.ci` is TRUE:
#' - It is assumed that the second row represents central (mean/median) frequency values and the other 
#'   2 rows lower and upper limits.
#' - If `barplot` is TRUE a barplot is drawn and CIs are plotted using `ci.color`.
#' - If `barplot` is FALSE a line is drawn for the central values in `col` and a polygon is drawn along 
#'   the upper and lower limits, with fill color `coi.color` and border color `border`.
#' 
dateFreqDistribution <- function(heights, dates, startDate="2020-01-01", endDate="2020-06-26", 
                                    col=dark$blue, border=NA, ci.color=mPal(dark$red, 0.5), lty=1, lwd=1, 
                                    ylab="Frequency of TMRCAs\n(per day)", ymax=NA, plot.ci=FALSE, barplot=TRUE, 
                                    add = FALSE, ...) {
  
  
  startDate <- as.Date(startDate)
  endDate   <- as.Date(endDate)
  dates     <- as.Date(dates)
  
  start <- which(dates == startDate)
  end   <- which(dates == endDate) 
  
  # print(paste(start, end))
  if (length(start) == 0 || length(end) == 0) {
    stop("Error: startDate and endDate need to be included in dates.")
  }
  
  
  if (is.na(ymax)) {
    ymax <- max(heights)*1.25
  }
  
  if (!add) {
    plotShadedAxes(xlim=c(startDate, endDate), ylim=c(0, ymax), ylab=ylab, yaxs='i', xaxs='i', ...)
  }
  
  if (!is.null(dim(heights)) && nrow(heights) > 1) {
    # Dealing with a matrix
    n <- ncol(heights)
    if (length(dates) != (n+1)) {
      stop("Error: Need one more date than number of elements in heights to delimit all intervals.")
    }
    
    if (plot.ci && nrow(heights) == 3) {
      # Have central, upper and lower values
      
      if (barplot) {
        par(new=TRUE)
        barplot2(heights[2, start:end], xlim=c(0,end-start), lty=lty, lwd=lwd, 
                 col=col, border=border, ci.color=ci.color, ci.lwd=0.5, ci.width = 0,  
                 plot.ci=TRUE, ci.l=heights[1, (start+1):end], ci.u=heights[3, (start+1):end], 
                 las=2, ylim=c(0,ymax), width=1, space=0, xaxs='i', ylab="", xpd=FALSE)
      } else {
        polygon(c(startDate:endDate, rev(startDate:endDate)), c(heights[1, start:end], rev(heights[3, start:end])),
                border=border, col=ci.color, lty=lty, lwd=lwd) 
        lines(startDate:endDate, heights[2, start:end], col=col, type='l')
        axis(2, las=2)
      }
      
    } else {
      
      if (barplot) {
        # Stacked barplot, set all 0's below the last nonzero value in a column to NA to prevent extra lines being drawn
        # on top of bars
        for (i in 1:ncol(heights)) {
            j <- max(c(0, which(heights[,i] != 0)))
            if (j < nrow(heights)) {
                heights[(j+1):nrow(heights), i] <- NA
            }
        }
        #heights[heights == 0] <- NA
        
        par(new=TRUE)
        barplot2(heights[, start:end], xlim=c(0,end-start), names.arg = NULL,
                 col=col, border=border, lty=lty, lwd=lwd,
                 las=2, ylim=c(0,ymax), width=1, space=0, xaxs='i', ylab="", xpd=FALSE)
      } else {  
        col  <- rep(col, ceiling(nrow(heights)/length(col)))
        prev <- rep(0, ncol(heights[, start:end]))
        i <- 1
        for (i in 1:nrow(heights)) {
          curr <- prev + heights[i, start:end]
          polygon(c(dates[start:end], rev(dates[start:end])), c(curr, rev(prev)), col=col[i], border=border, lty=lty, lwd=lwd)
          prev <- curr
          i <- i + 1
        }
        #rect(min(plotDates), 0, max(plotDates), 1, xpd=TRUE)
        axis(2, las=2)
      }
      
    }
  } else {
    # One-dimensional vector
    n <- length(heights)
    if (length(dates) != (n+1)) {
      stop("Error: Need one more date than number of elements in heights to delimit all intervals.")
    }
    
    if (barplot) {
      par(new=TRUE)
      barplot2(heights[start:end], xlim=c(0,end-start), col=col, border=border, lty=lty, lwd=lwd, 
               las=2, ylim=c(0,ymax), width=1, space=0, xaxs='i', ylab="", xpd=FALSE, axes = FALSE)
    } else {
      
      polygon(c(dates[start], dates[start:end], dates[end]), c(0, heights[start:end], 0),
              border=border, col=col, lty=lty, lwd=lwd) 
      axis(2, las=2)
    } 
  }
  
  
  # Draw x-axis line again (may have been plot over) 
  axis(1, at=0:n, labels=NA, las=2, lwd.ticks=NA)
  
}



plotDateGradient <- function(dateBreakdown, dateBreaks, startDate="2020-03-01", endDate="2020-05-17", 
                             normalise=TRUE, ymax=NA, ylab="",
                             palfn = viridis, direction=1, alpha=0.75,
                             plotLegend = TRUE, legend="Weeks", legendTicks=NULL, legendPos=c(0.83, 0.85, 0.3, 0.6), ...) {
  
  
    startDate <- as.Date(startDate)
    endDate   <- as.Date(endDate)
    
    gradPal <- palfn(n = ncol(dateBreakdown), alpha=alpha, direction=direction)
    
    if (normalise) {
      dateBreakdown <- dateBreakdown / rowSums(dateBreakdown) 
      ymax <- 1
    } else 
      if (is.na(ymax)) {
        ymax   <- max(rowSums(dateBreakdown))*1.25
      }
    n <- nrow(dateBreakdown)
    
    dateFreqDistribution(t(dateBreakdown), dateBreaks,  
                         startDate = startDate, endDate = endDate, 
                         col=gradPal,
                         ymax = ymax, ylab = ylab, ...)
    

    # Legend 
    if (plotLegend) {
      
      oldpar <- par(no.readonly = TRUE)
      par(mar=rep(0.05, 4), new=TRUE, fig=legendPos, mgp=c(3,0.25,0))
      
      plot(1, type='n', xlim=c(0,1), ylim=c(0, length(gradPal)), xaxs='i', yaxs='i', axes=FALSE, bty='n', xlab="", ylab="")
      
      for (i in 1:(length(gradPal))) {
        rect(0, i-1, 1, i, col=gradPal[i], border = NA)
      }
      rect(0, 0, 1, length(gradPal), xpd=TRUE)
      
      yticks <- pretty(1:length(gradPal))
      if (is.null(legendTicks)) {
          legendTicks <- yticks 
      } else {
          legendTicks <- c(legendTicks[1], legendTicks[yticks])
      }
      
      axis(4, at=yticks, labels = legendTicks, lwd=0, lwd.ticks = 1, las=1, tcl=-0.1, cex=par("cex.axis"))
      mtext(side=2, line=0.1, legend, cex=par("cex.lab"))
      
      par(oldpar)
    }
  
} 



#' Lag between tmrca and oldest sequence
plotLagScatter <- function(clusterSummary, metadata, stat="tmrca_median", 
                           startDate=as.Date("2020-01-01"), endDate=as.Date("2020-06-26"), ymax=50, method="pearson", addLine=FALSE, cex.lab=0.8, ...) {
  
      plotShadedAxes(xlim=c(startDate, endDate), ylim=c(0, ymax), ylab="Detection lag (days)", side=2, decimal=TRUE, yaxs='i', ...)
      mtext(side=1, line=4, text="Transmission lineage TMRCA", cex=cex.lab)
      
      lag <- (clusterSummary$oldest - clusterSummary[[stat]])*366
      points(clusterSummary[[stat]], lag, col=mPal(dark$blue, 0.25), pch=20, xpd=FALSE)
    
      corAll     <- round(cor(clusterSummary[[stat]], lag, method = method) ,3)
      corNonZero <- round(cor(clusterSummary[[stat]][lag >= 1], lag[lag >= 1], method = method) ,3)
      
      
      if (addLine == TRUE) {
          abline(linmodAll     <- lm(lag ~ clusterSummary[[stat]]), lwd=2, lty=2)
          abline(linmodNonZero <- lm(lag[lag >= 1] ~ clusterSummary[[stat]][lag >= 1]), lwd=2, lty=1)
          
          mtext(side=3, line=0.5, text=paste0("r = ", corNonZero, " (lineages with lag > 0)    r = ", corAll, " (all lineages)"), cex=0.8)
          mtext(side=3, line=1.5, text=paste0("slope = ", round(linmodNonZero$coefficients[2],2), " (lineages with lag > 0)    slope = ", 
                                              round(linmodAll$coefficients[2],2), " (all lineages)"), cex=0.8)    
      }
      
      
      par(new=TRUE)

      ukseqhist  <- hist(metadata$sample_date[metadata$country == "UK"], breaks="days", plot=FALSE)
      ukseqs     <- c(0, cumsum(ukseqhist$counts))
      ukseqdates <- lubridate::decimal_date(as.Date(ukseqhist$breaks, origin=as.Date("1970-01-01")))

      plot(ukseqdates, ukseqs, type='l', lwd=2, col=mPal(dark$red), ylim=c(0,30000),
           bty='n', axes=FALSE, xlim=c(lubridate::decimal_date(startDate), lubridate::decimal_date(endDate)),
           xlab="", ylab="", xaxs='i', yaxs='i')
      axis(4, at=c(0, axTicks(4)), las=2)
      mtext(side=4, text = "UK virus genome sequences", line=3.5, cex=cex.lab)
      
      return(list(corAll = corAll, corNonZero = corNonZero))
}




#' Scatterplot between two variables, stat1 (x-axis) has to be a date, stat2 (y-axis) has to be a real or integer number
plotLineageScatter <- function(clusterSummary, stat1="tmrca_median", stat2="seqs", 
                            startDate=as.Date("2020-01-01"), endDate=as.Date("2020-06-26"), ymax=500, method="pearson", addLine=FALSE, log=FALSE, 
                            xlab="Transmission lineage TMRCA", ylab="Transmission lineage size", cex.lab=0.8, ...) {
  
  
  if (log) { 
    plotShadedAxes(xlim=c(startDate, endDate), ylim=c(1, log(ymax)), ylab=ylab, side=0, decimal=TRUE, yaxs='i',  ...)
    plotLogAxisReal(lims=c(1, ymax), side = 2)
    y <- log(clusterSummary[[stat2]])
  } else {
    plotShadedAxes(xlim=c(startDate, endDate), ylim=c(0, ymax), ylab=ylab, side=2, decimal=TRUE, yaxs='i', ...)
    y <- clusterSummary[[stat2]]
  }
  mtext(side=1, line=4, text=xlab, cex=cex.lab)
  
  points(clusterSummary[[stat1]], y, col=mPal(dark$blue, 0.25), pch=20, xpd=FALSE)
  
  
  cor     <- cor.test(clusterSummary[[stat1]], y, method = method)
  if (addLine == TRUE) {
    abline(linmodAll     <- lm(y ~ clusterSummary[[stat1]]), lwd=2, lty=2)
    
    r <- round(cor$estimate, 3)
    p <- round(cor$p.value, 4)
    if ("conf.int" %in% names(cor)) {
      c <- paste0(" (", round(cor$conf.int[1],2), ", ", round(cor$conf.int[2],2), ")")
    } else {
      c <- ""
    }
    cortext <- paste0("r = ", r, c, "\np ", ifelse(p == 0, "< 0.0001", paste0("= ",p)))    
    #mtext(side=3, line=0.5, text=paste0("r = ", corAll, " (all lineages)"), cex=0.8)
    #mtext(side=3, line=1.5, text=paste0("slope = ", round(linmodAll$coefficients[2],2), " (all lineages)"), cex=0.8)    
    legend("bottomright", inset=c(0,0.8), title=cortext, cex=par("cex.lab"), legend=NA, bty='n', xpd=TRUE)
  }
  
}



#' Scatterplot between two variables, stat1 (x-axis) and stat2 (y-axis) have to be real or integer numbers
plotScatter <- function(clusterSummary, stat1="duration", stat2="seqs", 
                        xmax=200, ymax=500, method="pearson", addLine=FALSE, log=FALSE, 
                        plotCI=TRUE, plotPredict=FALSE,
                        xlab="Transmission lineage TMRCA", ylab="Transmission lineage size", cex.lab=0.8, label="", line=1, ...) {
  
  
  if (log) { 
    plot(1, type='n', xlim=c(0, xmax), ylim=c(1,log(ymax)), ylab=ylab, xlab="", yaxs='i', xaxs='i', yaxt='n', bty='n')
    plotLogAxisReal(lims=c(1, ymax), side = 2)
    y <- log(clusterSummary[[stat2]])
  } else {
    plot(1, type='n', xlim=c(0, xmax), ylim=c(0,ymax), ylab=ylab, xlab="", yaxs='i', xaxs='i', bty='n', las=1)
    y <- clusterSummary[[stat2]]
  }
  mtext(side=1, line=3, text=xlab, cex=cex.lab)
  
  clusterdf  <- data.frame(x = clusterSummary[[stat1]], y = y)
  linmodAll  <- lm(y ~ x, data = clusterdf)
  
  # Plot CI
  if (plotCI) {
    clusterdfNew <- data.frame(x = seq(from = min(clusterdf$x), to = max(clusterdf$x), length.out=100))
    ci_95 <- predict(linmodAll,
                     newdata  = clusterdfNew,
                     interval = "confidence",
                     level = 0.95)
    clusterdfNew <- cbind(clusterdfNew, ci_95)
    polygon(c(clusterdfNew$x, rev(clusterdfNew$x)), c(clusterdfNew$lwr, rev(clusterdfNew$upr)), col="#00000077", border=NA)
  }
  
  if (plotPredict) {
    clusterdfNew <- data.frame(x = seq(from = min(clusterdf$x), to = max(clusterdf$x), length.out=100))
    ci_95 <- predict(linmodAll,
                     newdata  = clusterdfNew,
                     interval = "predict",
                     level = 0.95)
    clusterdfNew <- cbind(clusterdfNew, ci_95)
    polygon(c(clusterdfNew$x, rev(clusterdfNew$x)), c(clusterdfNew$lwr, rev(clusterdfNew$upr)), col="#00000077", border=NA)
  }
  
  # Plot scatter
  points(clusterdf$x, clusterdf$y, col=mPal(dark$blue, 0.25), pch=20, xpd=FALSE)
  
  corAll  <- round(cor(clusterdf$x, clusterdf$y, method = method) ,3)
  
  cor     <- cor.test(clusterdf$x, clusterdf$y, method = method)
  if (addLine == TRUE) {
    abline(linmodAll, lwd=2, lty=2)
    
    r <- round(cor$estimate, 2)
    p <- round(cor$p.value, 4)
    if ("conf.int" %in% names(cor)) {
      c <- paste0(" (", round(cor$conf.int[1],2), ", ", round(cor$conf.int[2],2), ")")
    } else {
      c <- ""
    }
    cortext <- paste0("r = ", r, c, "\np ", ifelse(p == 0, "< 0.0001", paste0("= ",p)))
    
    #mtext(side=3, line=0.5, text=paste0("r = ", corAll, " (all lineages)"), cex=0.8)
    #mtext(side=3, line=1.5, text=paste0("slope = ", round(linmodAll$coefficients[2],2), " (all lineages)"), cex=0.8)    
    legend("bottomright", inset=c(0,0.8), title=cortext, cex=par("cex.lab"), legend=NA, bty='n', xpd=TRUE)
  }
  
  x <- -0.15*(xmax)
  mtext(label, side=3, line=line, at=x, cex = par("cex.main"))
  
}


plotArrivalsInfections <- function(arrivals, infections, location="all",
                                   smoothArrivals=FALSE, smoothInfections=TRUE, 
                                   startDate=as.Date("2020-01-01"), endDate=as.Date("2020-06-26"), ymax=500, ...) {
 
      arrivals <- arrivals[arrivals$date <= endDate, ]
      infections <- infections[infections$date <= endDate, ]
  
      if (smoothArrivals) {
         estimatedArrivals <- arrivals$estimate_smoothed[arrivals$location == location]
      } else {
         estimatedArrivals <- arrivals$estimate[arrivals$location == location]
      }
  
      if (smoothInfections) {
          estimatedInfections <- infections$num_infs_smoothed[infections$location == location]
      } else {
          estimatedInfections <- infections$num_infs[infections$location == location]
      }
  
      # Set ylim for arrivals
      ymax   <- max(estimatedArrivals, na.rm = TRUE)
      if (ymax < 1E3) {
          ymax <- 1E3
      } else 
      if (ymax > 1E5) {
          ymax <- max(1E5, ceiling(ymax/1E5)*1E5)
      } else {
          ymax <- max(10000, ceiling(ymax/10000)*10000)
      } 
  
      # Plot arrivals
      plotShadedAxes(xlim=c(startDate, endDate), ylim=c(0,ymax), yaxs='i', ylab="Estimated travellers per day\n(solid line)", ...)
      lines(arrivals$date[arrivals$location == location], estimatedArrivals, lwd=2)
      
      
      # Set ylim for infections
      ymax   <- max(estimatedInfections, na.rm = TRUE)
      i <- 1
      ymaxlevels <- c(1E3, 5E3, 1E4, 5E4, 1E5, 5E5, 1E6, 5E6, 1E7)
      ylim <- c(0, 0)
      while (ymax > ylim[2]) {
          ylim[2] <- ymaxlevels[i]
          i <- i + 1
      }      

      # Plot infections
      par(new=TRUE)
      plot(1, type='n', xlim=c(startDate, endDate), ylim=ylim, axes=FALSE, 
           xaxs='i', yaxs='i', xlab="", ylab="")
      lines(infections$date[infections$location == location], estimatedInfections, 
            lty=2, lwd=2, col=ukPal$eng)
      axis(4, las=1, at=axTicks(4), labels=gsub(" ", "", format(axTicks(4), scientific=TRUE)))
      
      mtext(side=4, text="Estimated daily infections\n(dashed line)", line=4, cex=par("cex.lab"))
    
    
}




# Don't show individual samples before the cutoff date
plotLineageDurations <- function(clusterSummary, clusterSamples, orderby="mostrecent",
                                       startDate="2020-01-01", endDate="2020-06-26", cutoff="2019-01-01",
                                       plotNames=TRUE, plotDurations=TRUE, ...) {
  
  plotBarStart <- function(x1, y, x2, h, n=100, col="#00000011") {
    
    x <- seq(0, 5, length.out=n)
    
    # Hill function
    f <- (x^5)/(10 + x^5)
    
    xx <- seq(x1, x2, length.out=n)
  
    p <- ceiling(seq(n/2, n, length.out=20))
    for (i in p) {
        polygon(c(xx[1:i], rev(xx[1:i])), c(y + f[1:i]*h/2, y - rev(f[1:i])*h/2), col=col, border=NA)
    }
    lines(xx, y + f*h/2)
    lines(xx, y - f*h/2)
    
    
  }
  
  startDate <- as.Date(startDate)
  endDate   <- as.Date(endDate)
  cutoff    <- lubridate::decimal_date(as.Date(cutoff))
  
  n <- nrow(clusterSummary)
  plotShadedAxes(xlim=c(startDate, endDate), ylim=c(0.5, n+0.5), side=0, yaxs='i', decimal=TRUE, ...)
  
  colMap  <- list("UK-ENG" = mPal(ukPal$eng), 
                  "UK-SCT" = mPal(ukPal$sct),
                  "UK-WLS" = mPal(ukPal$wls),
                  "UK-NIR" = mPal(ukPal$nir))
  
  fillMap <- list("UK-ENG" = mPal(ukPal$eng, 0.5), 
                  "UK-SCT" = mPal(ukPal$sct, 0.5),
                  "UK-WLS" = mPal(ukPal$wls, 0.5),
                  "UK-NIR" = mPal(ukPal$nir, 0.5))
  
  labelMap <- c("England", "Scotland", "Wales", "Northern Ireland")
  
  j <- n
  abline(h=(0.5+1:(n-1)), lty=3, lwd=0.5)
  names <- sizes <- c()
  
  if (orderby %in% names(clusterSummary)) {
    plotOrder <- order(clusterSummary[[orderby]], decreasing = TRUE)
  } else {
    plotOrder <- 1:n
  }
  
  for (i in plotOrder) {
    
    if (clusterSummary$oldest[i] < cutoff) {
        plotBarStart(clusterSummary$tmrca[i], j, min(cutoff, clusterSummary$mostrecent[i]), 0.5)
        if (cutoff < clusterSummary$mostrecent[i]) {
            lines(c(max(clusterSummary$oldest[i],cutoff), clusterSummary$mostrecent[i]), rep(j + 0.25, 2), col=dark$black)
            lines(c(max(clusterSummary$oldest[i],cutoff), clusterSummary$mostrecent[i]), rep(j - 0.25, 2), col=dark$black)
        }
        lines(rep(clusterSummary$mostrecent[i],2), j + c(0.25, -0.25), col=dark$black)
    } else {
        rect(max(clusterSummary$oldest[i],cutoff), j+0.25, clusterSummary$mostrecent[i], j-0.25, col=NA, border=dark$black)
        lines(c(clusterSummary$tmrca[i], clusterSummary$oldest[i]), rep(j,2))
    }
    lines(c(clusterSummary$tmrca_HPD_lower[i], clusterSummary$tmrca_HPD_upper[i]), rep(j,2), lwd=4, col=mPal(ukPal$wls, 0.5))
    points(clusterSummary$tmrca[i], j, pch=8, col=dark$black, cex=0.5)

    clustSamples <- clusterSamples[clusterSamples$cluster == as.character(clusterSummary$cluster[i]), ]
    clustSamples$adm1 <- as.character(clustSamples$adm1)        
    clustSamples$adm1[clustSamples$country != "UK"] <- "Not UK"
    
    # Plot country by country, so last country plotted is on top and most visible
    # for (country in names(colMap)) {        
    #   samplesX <- clustSamples$decimal_date[clustSamples$adm1 == country]
    #   samplesY <- jitter(rep(j, length(samplesX)), amount=0.15)
    #   
    #   points(samplesX, samplesY, col=fillMap[[country]], pch=16, cex=0.5)
    #   points(samplesX, samplesY, col= colMap[[country]], pch=1, cex=0.5)
    # }
    
    # Plot all at the same time, no border
    samplesX <- clustSamples$decimal_date[clustSamples$decimal_date >= cutoff]
    samplesY <- jitter(rep(j, length(samplesX)), amount=0.15)
    fills    <- sapply(clustSamples$adm1, function(x) fillMap[[x]])
    points(samplesX, samplesY, col=fills, pch=16, cex=0.5)
    
    
    
    # Change duration from median TMRCA to most recent to be sampling (detection) duration
    #duration <- lubridate::round_date(lubridate::date_decimal(clusterSummary$mostrecent[i]), unit="day") -
    #            lubridate::round_date(lubridate::date_decimal(clusterSummary$tmrca[i]), unit="day")
    duration <- round(366*(clusterSummary$mostrecent[i] - clusterSummary$oldest[i]))
    
    names <- c(names, as.character(clusterSummary$cluster[i]))
    sizes <- c(sizes, paste0("n = ", clusterSummary$seqs[i], ", ", duration, " days"))
    
    j <- j-1
    
  }
  legend("top", horiz=TRUE, legend=labelMap, bty='n', inset=c(0,-0.08), xpd=TRUE,
         col=unlist(fillMap), pch=16, text.col = "#FFFFFFFF", cex=par("cex.lab"))
  legend("top", horiz=TRUE, legend=labelMap, bty='n', inset=c(0,-0.08), xpd=TRUE,
         col=unlist(colMap), pch=1, cex=par("cex.lab"))
  legend("top", horiz=TRUE, legend=c("TMRCA"), bty='n', inset=c(0,-0.05), xpd=TRUE,
         pch=c(8), pt.cex=0.8, cex=par("cex.lab"))
  
  if (plotNames) {
    # Remove _MCC from the names to fit in with other plots...
    #axis(2, at = n:1, labels = gsub("_MCC", "", names), las=1, lwd=NA)  
    axis(2, at = n:1, labels = names, las=1, lwd=NA)  
  }
  
  if (plotDurations) {
    axis(4, at = n:1, labels = sizes, las=1, lwd=NA)
  }
  
}





#' Plot small 2 class piechart
plotPieProp <- function(count, total, title, line=-2, col=dark$blue, cex=0.8) {
  pie(c(count, total-count), labels=c(paste0(round(100*count/total,2), "%\nsequenced","")), col=c(mPal(col),mPal(col,0.5)), border=mPal(col), cex=cex)
  mtext(side=3, text = title, line=line, cex=cex)
}


plotStats <- function(stats, ylab="", xlab="", ylim=NULL, names=c(), plotGrid=TRUE, plotStrip=TRUE, plotN=TRUE, las=1, ny=NULL, boxMin=10) {
  
  if (is.null(ylim)) {
    ymin <- min(c(0, sapply(stats, min)))
    ymax <- max(sapply(stats, max))
    ylim <- c(ymin, ymax)  
  }

  
  #boxStats <- stats
  #boxStats[which(sapply(boxStats, length) < boxMin)] <- numeric(0)
  boxplot(stats, ylab = ylab, outline=ifelse(plotStrip, FALSE, TRUE), col=mPal(ukPal$nir,0.75),
          ylim=ylim, las = las, lwd = 1, xaxs='i', yaxs='i', xaxt='n')
  
  axis(1, at = 1:length(names), labels = names, las = las, tick = FALSE)
  mtext(side=1, text=xlab, line=4, cex=par("cex.lab"))
  
  if (plotGrid) {
    grid(nx=0, ny=ny, lwd=0.5, col="#000000")
  }
  
  if (plotStrip) {
    stripchart(stats, vertical = TRUE, method = "jitter", jitter = 0.2, add = TRUE, pch = 16, col = mPal(ukPal$eng, 0.5), cex=1)
  }
  
}

