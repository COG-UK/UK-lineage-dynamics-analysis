################################################################################################################################
# Palettes
################################################################################################################################

dark  <- list(blue   = RColorBrewer::brewer.pal(12,"Paired")[2], 
              green  = RColorBrewer::brewer.pal(12,"Paired")[4], 
              red    = RColorBrewer::brewer.pal(12,"Paired")[6], 
              orange = RColorBrewer::brewer.pal(12,"Paired")[8], 
              purple = RColorBrewer::brewer.pal(12,"Paired")[10], 
              gray   = "#777777",
              black  = "#000000",
              white  = "#FFFFFF")

light <- list(blue   = RColorBrewer::brewer.pal(12,"Paired")[1], 
              green  = RColorBrewer::brewer.pal(12,"Paired")[3], 
              red    = RColorBrewer::brewer.pal(12,"Paired")[5], 
              orange = RColorBrewer::brewer.pal(12,"Paired")[7], 
              purple = RColorBrewer::brewer.pal(12,"Paired")[9], 
              gray   = "#777777",
              black  = "#000000",
              white  = "#FFFFFF")


ukPal <- list(eng = "#BE0F34", 
              sct = "#191970", 
              wls = "#F5CF47",
              nir = "#9ECEEB", 
              oth = "#C7C2BC")

countryPal <- list("China"         = "#872434",
                   "Italy"         = "#33A02C",
                   "Spain"         = "#F5CF47",  
                   "France"        = "#1F78B4",
                   "Belgium"       = "#000000",
                   "Netherlands"   = "#FF7F00",
                   "Ireland"       = "#AAB300",
                   "Switzerland"   = "#BE0F34",
                   "Germany"       = "#CF7A30", 
                   "US"            = "#A6CEE3",
                   "Sweden"        = "#007770",
                   "Portugal"      = "#6A3D9A",
                   "Other"         = "#C7C2BC")

################################################################################################################################

mPal <- function(c, alpha=1.0) {
  if (is.character(c) && substr(c,1,1) == "#") {
      return(paste0(c,format(as.hexmode(round(alpha*255)), width=2)))
  } else {
      return(rgb(red=c[1], green=c[2], blue=c[3], alpha=round(alpha*255), maxColorValue=255))
  }
}


plotPalette <- function(pal, alpha=1.0) {
  
  root <- sqrt(length(pal))
  layout(matrix(1:(round(root)*ceiling(root)), nrow=round(root)))
  
  par(mar=c(2,0,0,0))
  for (col in 1:length(pal)) {
      plot(1,type='n',xlim=c(0,1),ylim=c(0,1), axes=FALSE, ylab="", xlab="")
      rect(0,0,1,1,col=mPal(pal[[col]], alpha=alpha))
      if (is.null(names(pal)[col])) {
          mtext(col,line=0,side=1)
      } else {
          mtext(names(pal)[col],line=0,side=1)
      }
  }
}


