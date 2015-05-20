library(raster)
library(maptools)
library(rasterVis)
library(RColorBrewer)
library(sp)

annualgridForecasts <- function(var, AOA, indir) {
        

  #var is text - either "ppt", "tmax", "tmean", or "tmin".  AOA is a polygon shapefile representing the area of analysis.
  #Crops annual data (the .14 rasters provided with PRISM data) by AOA polygon shapefile
  #Define location of forecast files here - hardcoded

#   workspace <- "C:/Share/LCC-VP/PACE/GRSM/climate_data/forecasts/trans_pj/p25_rcp45/" # PJ
  workspace <- indir
  
  # projection WGS84
  # need to calculate TMEAN
  
  if (cvar == "tmin" | cvar == "tmax" | cvar == "tmean") {

    #list var annual files from BeginYear to EndYear **For now just listing all - will come back to this.
    #PJ - "*.tif" pattern picks up .xml files too...
    var.list <- list.files(paste(workspace, cvar, sep=""), pattern="*.tif", full.names=T)
    
    # Check the projection of input shapefile and, if necessary, reproject
    # for consistency with climate grids
    refgrid <- raster(var.list[1])
    if (as.character(CRS(projection(AOA))) == as.character(CRS(projection(refgrid)))) {
      print("Projections match. Awesome.")
    } else {
      print("reprojecting AOA to match grids")
      AOA <- spTransform(AOA, CRS(projection(refgrid)))
    }
    
    anncrop <- function(inmonthly, AOA) {
      #stack and crop files in list and div by 100 to get proper units    
      yr.mean <- ((crop(mean(brick(inmonthly)), AOA))*1.8)+32
    #yr.mean <- crop(stack(ann.list), AOA)/100
    }
    yr.mean <- stack(lapply(var.list,FUN=anncrop,AOA=AOA))
    
    #if var == "ppt" There is an issue with the ppt annual grids (the _14 grids), so calc
    #means from monthly data.
  } else {
    
    var.list <- list.files(paste(workspace, cvar, sep=""), pattern="*.tif", full.names=T)

    # Check the projection of input shapefile and, if necessary, reproject
    # for consistency with climate grids
    refgrid <- raster(var.list[1])
    if (as.character(CRS(projection(AOA))) == as.character(CRS(projection(refgrid)))) {
      print("Projections match. Awesome.")
    } else {
      AOA <- spTransform(AOA, CRS(projection(refgrid)))
    }
    
    anncrop <- function(inmonthly, AOA) {
      #stack and crop files. Calculate annual ppt sums
      yr.mean <- ((crop(sum(inmonthly), AOA))
      #yr.mean <- crop(stack(ann.list), AOA)/100
    }
    
  }#end if
  
  return(yr.mean)
}#end function


############# Analysis Functions ###############

#calculates overall mean (single layer) from annual data - will be used to calculate anomalies
climatemean <- function(adata) {
  periodmean <- mean(adata)
  return(periodmean)
}#end function

#calculates annual mean
annMean <- function(adata) {
  ann.mean <- vector(mode="numeric",length=nlayers(adata))
  
  for (i in c(1:nlayers(adata))) {
    ann.mean[i] <- mean(as.vector(adata[[i]]))
    #make raster stack from ann.mean - with same value for every year
  }#end loop
  
  return(ann.mean)
}#end function

#least squares fit of annual data at individual cell level
lstfit <- function(adata, b.year, e.year){
  nrows <- nrow(adata)
  ncols <- ncol(adata)
  n <- nlayers(adata)
  years <- c(b.year:e.year)
  mu <- as.matrix(mean(adata))
  
  df <- matrix(data=0, nrow=nrows, ncol=ncols)
  tmu <- df
  tmu[,] <- sum(c(1:n))/n
  Sxx <- Sxy <- SST <- SSW <- df
  
  for (i in c(1:n)) {
    dt <- as.matrix(adata[[i]])
    Sxx[,] <- ((i-tmu)**2) + Sxx
    i.df <- df
    i.df[,] <- i
    Sxy <- ((dt - mu)*(i.df-tmu)) + Sxy
  }#end for
  
  beta1 <- Sxy/Sxx
  beta0 <- mu - (beta1*tmu)
  return(list(beta1,beta0))  
}#end function



#takes annual data and returns rate of change at the ind cell level
temporalgradient <- function(adata, b.year, e.year) {
  trend <- lstfit(adata, b.year, e.year)
  return(trend[[1]])
}

calcAnomaly <- function(adata) {
  #calculate overall time series mean
  data_mu <- mean(adata)
  nyears <- nlayers(adata)
  
  #take difference between each yearly average and the overall average
  anom <- adata - data_mu
  
  #average each layer(year) to get single yearly anomaly.
  yr.anom <- as.vector(c(1:nlayers(anom)))
  for (n in c(1:nlayers(adata))) {
    yr.anom[n] <- mean(as.vector(anom[[n]]))
  }#end loop
  return(yr.anom)
}#end fucntion



############# Summary Plot Functions ##############


plotAnom <- function(adata, cvar, mws=3, b.year, e.year) {
  anom <- calcAnomaly(adata)
  
  #setting up for plotting
  vec <- as.vector(rep(0, times=length(anom)))
  pos <- vec
  pos[anom >= 0] <- anom[anom >= 0]
  neg <- vec
  neg[anom < 0] <- anom[anom < 0]
  
  #for checking to make sure it all adds up properly
  n.pos <- length(pos[pos != 0])
  n.neg <- length(neg[neg != 0])
  
  if (n.pos+n.neg != length(anom)) {
    stop("problem calculating pos and neg vals. Numbers don't add up to length(anom)")
  }#end if
  
  #Calc 3-year moving average to plot over bars - consider using lag tools in zoo
  lag <- mws
  run.avg <- as.vector(rep(NA, times=mws)) #to fill in the first mws years - easier plotting
  
  
  for (j in c((lag+1):length(anom))) {
    run.avg[j] <- mean(anom[c((j-lag):(j-1))])
  }#end for
  
  
  cgrad <- lstfit(adata, b.year, e.year)
  years <- c(b.year: e.year)
  adata.mean <- annMean(adata)
  
  #beta <- lm((adata - matrix(adata.mean, nrow=nrow(adata[[1]]), ncol=ncol(adata[[1]]))) ~ years)
  trend <- lm((adata.mean-mean(adata.mean)) ~ years)
  
  #calc limits such that 0 is in the middle
  lim <- max(abs(anom))
  buff <- .1*lim
  lim <- c(-lim-buff, lim+buff)
  
  
  # Titles
  if (cvar == "ppt") {
    main <- "Historical Precipitation Anomalies"
    ylab <- paste("Precipitation Anomaly (mm)", sep="")
    leg.text <- c(paste(mws,"-year moving window", sep=""),paste("Trend ", round(trend$coefficients[2]*10, digits=2), " (mm/decade)", sep=""))
  } else if (cvar == "tmax") {
    #fix this - make pretty with italics and such from WHRC script
    main <- "Historical Maximum Temperature Anomalies"
    ylab <- expression(paste("Temperature Anomaly ",~(degree~F), sep=""))
    t10yr <- round(trend$coefficients[2]*10, digits=2)
    leg.text <-c(paste(mws,"-year moving window", sep=""), as.expression(bquote(Trend ~ .(t10yr) ~ (degree~F/decade)))) 
  } else if (cvar == "tmin") {
    main <- "Historical Minimum Temperature Anomalies"
    ylab <- expression(paste("Temperature Anomaly ",~(degree~F), sep=""))
    t10yr <- round(trend$coefficients[2]*10, digits=2)
    leg.text <-c(paste(mws,"-year moving window", sep=""), as.expression(bquote(Trend ~ .(t10yr) ~ (degree~F/decade)))) 
  } else {
    main <- "Historical Mean Temperature Anomalies"
    ylab <- expression(paste("Temperature Anomaly ",~(degree~F), sep=""))
    t10yr <- round(trend$coefficients[2]*10, digits=2)
    leg.text <-c(paste(mws,"-year moving window", sep=""), as.expression(bquote(Trend ~ .(t10yr) ~ (degree~F/decade)))) 
  }#end if
  
  #plotting - barplots - see labeling
  #plot barplot with nothing, then set background to gray, then plot for real.
  bp0 <- barplot(pos, ylim=lim, width=2, border=NA, xaxt="n", yaxt="n")
  rec <- rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "white", 
              border="black", lwd=1)
  bp1 <- barplot(pos, ylim=lim, col="firebrick3", width=2, border=NA, 
                 xaxt="n", add=TRUE, main=main, xlab="Year", ylab=ylab, 
                 cex.lab=0.80, cex.axis=0.80, cex.main=0.90)
  bp2 <- barplot(neg, col="blue", add=TRUE, width=2, border=NA, yaxt="n")    
  ab <- abline(0, 0)
  avg <- lines(bp1, run.avg, col="forestgreen", lwd=1.5)
  tline <- lines(bp1, (years*trend$coefficients[[2]]+trend$coefficients[[1]]), col="gray33", lwd=2, lty=2)
  
  
  #Labels every 20 years
  lab <- seq(b.year, e.year, 1)%%20==0
  label <- years[lab]
  at <- bp1[lab]
  ax <- axis(1, at=at, labels=label, cex.axis=0.80)
  
  #Legend  
  leg <- legend("bottomright", leg.text, lwd=c(1,1.5), lty=c(1, 2),col=c("forestgreen", "gray33"), bty="n", cex=0.8)
  
  
  
}#end function

###STARTED THIS 10/4/2013
plotAnomForecasts <- function(adata, cvar, b.year, e.year) {
  anom <- calcAnomaly(adata)
  
  
  #############
  plot_poly(tYrs[101:200], t50, t25, t75, clr="low")
  
  #from John's script - for reference
  plot_axes <- function(xval=1:10, yval=1:10, xlow, xhigh, ylow, yhigh, yLab="Degree F", xLab="Year"){
    plot(xval, yval, xlim=c(xlow, xhigh), ylim=c(ylow, yhigh), xlab=xLab,
         ylab = yLab, type = "n", col="black")}
  
  plot_obs <- function(x, y, clr="black"){
    lines(x,y,clr, type="l",  lwd=3)}
  
  # takes vectors with 1) years  2) mean  3) upper poly  4) lower poly  5) color
  # set colors - hist_sim, low, med, high
  
  plot_poly<- function(xVals, midVals, lowVals, highVals, clr="hist_sim"){
    # add check that lengths are equal; maybe deal with missing values
    if(clr=="hist_sim")clrs <- c("palegreen", "darkgreen")
    if(clr=="low")clrs <- c("lightblue1", "royalblue")
    if(clr=="med")clrs <- c("lightpink", "firebrick1")
    if(clr=="high")clrs <- c("papayawhip", "darkviolet")
    
    polypts <- data.frame(xVals, lowVals)
    
    ptop <- data.frame(xVals, highVals)
    pbot <- data.frame(rev(xVals), rev(lowVals))
    names(ptop) <- c("x", "y")
    names(pbot) <- c("x", "y")
    polypts <- rbind(ptop, pbot)
    
    polygon(polypts$x, polypts$y, col=clrs[1], border=NA)
    lines(xVals, midVals, col= clrs[2], lwd=5)}  # clr[2])}
  
}#end function

############# Map Building Functions ##############
rasExtract <- function(rasPath, AOA) {
  #RasPath <- "C:/Share/LCC-VP/RangeWide/ned/clipped/GRSM_pace_ned.tif"
  if (class(AOA) == "SpatialPolygonsDataFrame") {
    ras <- raster(rasPath)
    ras.crop <- crop(ras, AOA)
  } else if (class(AOA) == "character") {
    poly <- readShapePoly(AOA)
    ras <- raster(rasPath)
    ras.crop <- crop(ras, poly)
  } else {
    stop(paste("AOA must be a polygon or a filepath to a polygon"))
  }#endif
  
  return(ras.crop)
} #end function




plotPRISMgrad <- function(trend.ras, PACE, park, hsPath, cvar, c.points=NULL){
  
  #first crop, then plot hillshade
  hs <- rasExtract(hsPath, PACE)
  
  #trying some filtering to smooth out the hs
  hs.fil3 <- focal(hs, w=matrix(1/9,nrow=3,ncol=3))
  #hs.fil5 <- focal(hs, w=matrix(1/25,nrow=5,ncol=5))
  #hs.fil9 <- focal(hs, w=matrix(1/81, nrow=9, ncol=9))
  
  #set colors for hillshade
  cols.hs <- gray.colors(100, 0.6, 0.9)
  
  #set colors for trend ras
  cols.trend <- rev(brewer.pal(11, "Spectral"))
  pal.trend <- colorRampPalette(cols.trend)(1000)
  
  #Create plain white raster - need to trick rasterVis into plotting legend for trend ras, not hillshade.
  #There is probably a better, easier way of doing this - but it's currently escaping me :)
  white.ras <- setValues(trend.ras, 1)
  
  #legend text
  if (cvar == "tmax" | cvar == "tmin" | cvar == "tmean") {
    #deg F/decade
    leg <- expression(paste("Temperature Trend (",italic(degree),italic("F/decade"),")"))
  } else if (cvar == "ppt") {
    #mm/decade
    leg <- expression(paste("Precipitation Trend (", italic("mm/decade"), ")"))
  } else {
    stop(paste("invalid 'cvar': ", cvar, ". Must be one of 'ppt', 'tmax', 'tmean', or 'tmin.'", sep=""))
  }#end if
  
  #for plotting pretty legend and more continuous raster plot
  maxz <- max(abs(as.matrix(trend.ras)))
  
  
  #Background trickery to get a plot with the correct legend
  l0 <- levelplot(trend.ras, alpha.regions=0.6, col.regions=pal.trend, at=seq(-maxz, maxz, length=1000), colorkey=TRUE, margin=FALSE, ylab.right=leg, 
                  par.settings = list(layout.widths = list(axis.key.padding = 0,ylab.right = 2)))
  l0.1 <- levelplot(white.ras, alpha.regions=1.0, col.regions="white", colorkey=FALSE, margin=FALSE)
  
  #Now "real" plot over l0 layers
  l1 <- levelplot(hs.fil3, col.regions=cols.hs, maxpixels=500000, margin=FALSE, colorkey=FALSE, 
                  alpha.regions=0.6)
  l2 <- levelplot(trend.ras, alpha.regions=0.6, col.regions=pal.trend, at=seq(-maxz, maxz, length=1000), colorkey=T, margin=FALSE, 
                  maxpixels=500000)
  l3 <- layer(sp.polygons(PACE, lwd=0.5))
  l4 <- layer(sp.polygons(park, lwd=0.5))
  
  if (is.null(c.points) == FALSE) {
    #plot station data
    l5 <- layer(sp.points(c.points,col="black", pch=20, cex=0.3))
    #Put all layers together
    l0+l0.1+l1+l2+l3+l4+l5
  } else {
    #Put all layers together
    l0+l0.1+l1+l2+l3+l4
  }#end if
}#end function


