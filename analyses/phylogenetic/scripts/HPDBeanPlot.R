HPDBeanPlot <- function(mcmc.trace, names=NULL, bw = "sj", kernel = "gaussian", fill = "blue", border = "black", medcol = "red", 
                        side="both", horiz=FALSE, maxwidth = 0.5, medwidth=0.5, lwd = c(1,1,NA), add = FALSE, axes = TRUE, ...) {

    if (is.null(names)) {
        names <- colnames(mcmc.trace)
    }
    n <- length(names)
    
    # Actually calculate the statistics
    hpdmed      <- getHPDMedian(mcmc.trace)
    par_density <- lapply(1:ncol(mcmc.trace), function(i) density(mcmc.trace[,i], from=hpdmed[i,1], to=hpdmed[i,3], bw=bw, kernel=kernel))
    
    maxdens <- max(sapply(par_density, function(x) max(x$y)))
    scaling <- maxwidth/maxdens
    
    # Draw plot axes
    if (add==FALSE) {
        if (horiz) {
            plot(1,type='n', ylim=c(1-maxwidth,n+maxwidth), xlim=range(hpdmed), xaxt='n', yaxt='n', ...)
        } else {
            plot(1,type='n', xlim=c(1-maxwidth,n+maxwidth), ylim=range(hpdmed), xaxt='n', yaxt='n', ...)
        }
    }
    
    if (axes) {
        if (horiz) {
            axis(2, at = 1:n, labels = names)
            axis(1) 
        } else {
            axis(1, at = 1:n, labels = names)
            axis(2)
        }
    }
    

    # Plot beans
    left  <- side == "both" | side == "left"
    right <- side == "both" | side == "right"
    
    if (horiz) {
      
    } else {
        
    }
    
    for (i in 1:n) {
      
            polyY <- c(par_density[[i]]$x[1], par_density[[i]]$x, par_density[[i]]$x[length(par_density[[i]]$x)])
            lineY <- par_density[[i]]$x    
            
            # Left bean
            if (left) {
                polyX <- c(i, i-par_density[[i]]$y*scaling, i)
                lineX <- i-par_density[[i]]$y*scaling
                
                if (horiz) {
                    polygon(x = polyY, y = polyX, col=fill, border=NA)
                    lines(x = lineY,   y = lineX, col=border, lwd=lwd[1])
                } else {
                    polygon(x = polyX, y = polyY, col=fill, border=NA)
                    lines(x = lineX,   y = lineY, col=border, lwd=lwd[1])
                }
            }
          
            # Right bean
            if (right) {
                  polyX <- c(i, i+par_density[[i]]$y*scaling, i)
                  lineX <- i+par_density[[i]]$y*scaling
              
                  if (horiz) {
                      polygon(x = polyY, y = polyX, col=fill, border=NA)
                      lines(x = lineY,   y = lineX, col=border, lwd=lwd[1])
                  } else {
                      #print("plotting left")
                      polygon(x = polyX, y = polyY, col=fill, border=NA)
                      lines(x = lineX,   y = lineY, col=border, lwd=lwd[1])
                  }
            }
        }
        
        if (horiz) { 
          if (left) {
              segments(y0=1:n - maxwidth*0.5, x0=hpdmed[,1], y1=1:n, x1=hpdmed[,1], col=border, lwd=lwd[3])
              segments(y0=1:n - medwidth,      x0=hpdmed[,2], y1=1:n, x1=hpdmed[,2], col=medcol, lwd=lwd[2])
              segments(y0=1:n - maxwidth*0.5, x0=hpdmed[,3], y1=1:n, x1=hpdmed[,3], col=border, lwd=lwd[3])
          }
          
          if (right) {
              segments(y0=1:n + maxwidth*0.5, x0=hpdmed[,1], y1=1:n, x1=hpdmed[,1], col=border, lwd=lwd[3])
              segments(y0=1:n + medwidth,      x0=hpdmed[,2], y1=1:n, x1=hpdmed[,2], col=medcol, lwd=lwd[2])
              segments(y0=1:n + maxwidth*0.5, x0=hpdmed[,3], y1=1:n, x1=hpdmed[,3], col=border, lwd=lwd[3])
          } 
        } else {
            if (left) {
                segments(1:n - maxwidth*0.5, hpdmed[,1], 1:n, hpdmed[,1], col=border, lwd=lwd[3])
                segments(1:n - medwidth,      hpdmed[,2], 1:n, hpdmed[,2], col=medcol, lwd=lwd[2])
                segments(1:n - maxwidth*0.5, hpdmed[,3], 1:n, hpdmed[,3], col=border, lwd=lwd[3])
            }
    
            if (right) {
                segments(1:n + maxwidth*0.5, hpdmed[,1], 1:n, hpdmed[,1], col=border, lwd=lwd[3])
                segments(1:n + medwidth,      hpdmed[,2], 1:n, hpdmed[,2], col=medcol, lwd=lwd[2])
                segments(1:n + maxwidth*0.5, hpdmed[,3], 1:n, hpdmed[,3], col=border, lwd=lwd[3])
            }
        }
}
  