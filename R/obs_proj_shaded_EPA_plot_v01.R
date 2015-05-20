# plot_hist_proj_CI_vxx.R
# John Gross
# v01
#  16 Sept 2013

# create plot of data that matches EPA historical and
# projected climate data per http://www.epa.gov/climatechange/science/future.html#Temperature

#  Order of functions will generally be:
  #  plot_axes
  #  plot_poly - for all with polygon
  #  plot_obs - historical observations must be last (on top of poly)
  #  uses light colors because transparent colors only for pdf or Quartz devices


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

 ####    Test Functions   #####
 
 # test data set
test <- "Yes"
if(test == "Yes")
{
  tYrs <- 1900:2100
  t_obs <- (runif(101)-.5) + (0.015 * (tYrs[1:101]-1900))

  beginT <- mean(t_obs[97:100])

  pyr <- 1:100
  t25 <- ((runif(100)*.2)-.25) + (0.02 * pyr)+ beginT
  t50 <- ((runif(100)*.2)) + (0.02 * pyr) + beginT
  t75 <- ((runif(100)*.2)+.25) + (0.02 * pyr)+ beginT
  
  tsim25 <- ((runif(101)*.2)-.25) + (0.015 * (tYrs[1:101]-1900))
  tsim <- ((runif(101)*.2)) + (0.015 * (tYrs[1:101]-1900))
  tsim75 <- ((runif(101)*.2)+.25) + (0.015 * (tYrs[1:101]-1900))


  plot_axes(xlow=1900, xhigh=2100, ylow=-1, yhigh=6)
  
  plot_poly(tYrs[101:200], t50, t25, t75, clr="low")
  plot_poly(tYrs[101:200], t50+(pyr*.025), t25+(pyr*.025), t75+(pyr*.025), clr="med")
  plot_poly(tYrs[101:200], t50+(pyr*.045), t25+(pyr*.045), t75+(pyr*.045), clr="high")
  plot_poly(tYrs[1:101],  tsim, tsim25, tsim75, clr="hist_sim")
  plot_obs(tYrs[1:101], t_obs)
  
  legend("bottomright", lwd=c(4,4,4,4), #lty=c(1,1,2), 
  col=c("black","darkgreen", "darkviolet","firebrick1", "royalblue"),
  c("Observed", "Simulated obs", "RCP 8.5", "RCP 6.5", "FCP 4.5"), 
  cex=0.9, bty="n")

}

# EOF