# # Call packages
# require(sp)
# require(data.table)
# require(rgdal)
# require(raster)
# require(randomForest)
# require(mgcv)
# require(ncdf4)
# source("C:/Share/pjantz/Scripts/LCCVP/handy_functions.R")
# source("C:/Share/pjantz/Scripts/LCCVP/JEvans_RF_Functions.R")
# 
# # Output Directory
# outdir <- "C:/Share/LCC-VP/Rangewide/RFoutput"
# 
# # Set working directory
# setwd("C:/Share/LCC-VP/Rangewide/RFoutput")

AOAmeanDiff <- function(x,z,pp) {
  
  scode <- x
  print(scode)
  rcp <- z
  
  # Baseline
  baseline1 <- paste0(getwd(),"/","s",scode,"_pa_best_temp_19812010.nc")
  baseline2 <- paste0(getwd(),"/","s",scode,"_pa_all_temp_19812010.nc")
  
  # Forecasts
  tper <- c("2021","2030","2040","2055","2060","2070","2080")
  flist1 <- c(baseline,paste0(getwd(),"/","s",scode,"_",rcp,"_pa_best_temp_",tper,".nc"))
  flist2 <- c(baseline,paste0(getwd(),"/","s",scode,"_",rcp,"_pa_all_temp_",tper,".nc"))
  
  if (file.exists(flist1[[2]]) & file.exists(flist2[[2]])) {
    # Make list of rasters
    flist1 <- sapply(flist1, raster)
    flist2 <- sapply(flist2, raster)
    
    flist1 <- brick(flist1)
    flist2 <- brick(flist2)
    
    # SUMMARY SHAPEFILE
    # Set AOA path
    PACE.path <- pp
    
    # Coordinate system
    canomproj <- CRS(projection(flist1))
    
    # Reproject PACE shapefile to match climate anomaly coordinate system
    AOA <- readOGR(pathpop(PACE.path,1),sub("[.][^.]*$", "", basename(PACE.path), perl=TRUE))
    AOA <- spTransform(AOA,canomproj)
    
    # Extract pixels corresponding to the area of interest and calculate the spatial mean
    AOA.means1 <- extract(flist1, AOA)
    AOA.means1 <- data.frame(AOA.means1[[1]])
    
    names(AOA.means1) <- c("19812010",tper)
    AOA.means1 <- sapply(AOA.means1, mean)
    
    AOA.means2 <- extract(flist2, AOA)
    AOA.means2 <- data.frame(AOA.means2[[1]])
    
    names(AOA.means2) <- c("19812010",tper)
    AOA.means2 <- sapply(AOA.means2, mean)
    
    return(list(AOA.means1,AOA.means2))
  }
}


# Species code
species <- c(318,131,12,16,97,123,261,531,802,833)

# Rcp (either "rcp45" or "rcp85")
rcp <- "rcp45" # <----------
# Area for analysis
shp <- paste0("C:/Share/LCC-VP/ALCC_PACE_Park_boundaries/SHEN_pace_albers.shp") # <----------
# Area of analysis type
AOAtype <- "SHEN_PACE" # <----------

# Get means
datDiff <- lapply(species, function(x) AOAmeanDiff(x,z=rcp,pp=shp))



#!!!!!!! THIS MIGHT BE SLOP !!!!!!!!
# Find null list elements and subset out corresponding species codes
sset <- species[!unlist(lapply(dat,is.null))]
#conames <- c("Sugar maple","Loblolly pine","Balsam fir","Fraser fir","Red spruce",
"Table mountain pine","Eastern hemlock","American beech","White oak","Northern red oak")
conames <- glist[[1]][[2]]
conamessset <- conames[!unlist(lapply(dat,is.null))]

# Get rid of NULL elements
dat <- dat[!sapply(dat,is.null)]
# Make into a data frame with species as columns
dat <- data.frame(t(do.call("rbind",dat)))
names(dat) <- sset

# Write to file
write.csv(dat,paste0("C:/Share/LCC-VP/Rangewide/AOAmeans/",AOAtype,"_",rcp,"_pa_",vset,"_temp.csv"))


#----------------------------------------------------------------------------------
# PLOTTING
dat <- list()

# Read csv's into a list
tlist <- list("GRSM_PACE","SHEN_PACE","DEWA_PACE")
for (i in c(1:3)) {
  dat[[i]] <- read.csv(paste0("C:/Share/LCC-VP/Rangewide/AOAmeans/",tlist[[i]],"_",rcp,"_pa_",vset,"_temp.csv"))
  dat[[i]]$X <- NULL
}

# SAVE PLOT TO FILE
png(file=paste0("C:/Share/LCC-VP/Rangewide/AOAmeans/",rcp,"_pa_",vset,"_temp.png"))

# Set line symbols
plotchar <- c(1:9)

# Set par for a two by two plot
par(mfrow=c(2,2)); par(mar=c(0.5,1,0,0)); par(oma=c(3.5,3.5,4,1))

x <- c(1996,2021,2030,2040,2050,2060,2070,2080)
lets <- c("a.","b.","c.")

for (i in 1:3) {
  plot(x,dat[[i]][,1],type="n",lwd=1.5,ylim=c(0,0.5),
       xlab="",las=1,axes=F,pch=plotchar[1])
  for (j in 1:dim(dat[[i]])[2]) {
    text(1996, 0.5, lets[i], adj=0, cex=1.35)
    lines(x,dat[[i]][,j],lwd=1.5,type="b",pch=plotchar[j])
    box()
    axis(4, at=NULL, labels=F, tcl=0.5)
    if ((i == 2 | i == 3)) {axis(1, at=x, labels=T)}#, axis(1, at=NULL, labels=F, tcl=0.5))
    ifelse ((i == 1 | i == 3), axis(2, at=NULL, labels=T,las=2), axis(2, at=NULL, labels=F, tcl=0.5))
  }
}

plot(x,dat[[1]][,1],type="n",lwd=1.5,ylim=c(0,0.5),
     xlab="",las=1,axes=F,pch=plotchar[1])
legend("bottom", legend=conamessset, bty="n",pch=plotchar, lwd=1.5, cex=1.25,y.intersp=0.75)

mtext("Year", 1,2,adj=0.5, outer=T)
mtext("Probability of Presence", 2,2,adj=0.5, outer=T)

dev.off()


# par(mfrow=c())

# par(oma=c(2,2,0,0))
# par(mar=c(5.1, 4.1, 2.1, 2.1))

# SAVE PLOT TO FILE
png(file=paste0("C:/Share/LCC-VP/Rangewide/AOAmeans/",AOAtype,"_",rcp,"_pa_",vset,"_temp.png"))

# Add extra space to right of plot area; change clipping to figure
# only for one of the plots
# !!!!
# par(mar=c(5.1, 4.1, 4.1, 10.1), xpd=TRUE)

plotfun <- function(i,y=dat,x=c(1995,2021,2030,2040,2050,2060,2070,2080)) {
  if (i==1) {
    plot(x,y[,i],type="b",lwd=1.5,ylim=c(0,0.5),
         ylab="Mean Probability of Presence",xlab="Decade",pch=plotchar[i])
  } else {
    lines(x,y[,i],lwd=1.5,type="b",pch=plotchar[i])
  }
}
}
# Run plot function
sapply(1:ncol(dat), plotfun) 

# Add legend to top right, outside plot region (only for one of the plots)
#!!!!
legend("topright", inset=c(-0.525,0), legend=conamessset, pch=plotchar, lwd=1.5, cex=0.8)

dev.off()



