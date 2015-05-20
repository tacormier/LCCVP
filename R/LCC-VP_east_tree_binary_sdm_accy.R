# Calculate binary versions of species probabilities
# using thresholds calculated from sensitivity and specificity values

# Call packages
require(sp)
require(rgdal)
require(raster)
require(randomForest)
require(mgcv)
require(ncdf4)
require(PresenceAbsence)
require(rasterVis)
source("C:/Share/pjantz/Scripts/LCCVP/handy_functions.R")

classSDMbyAOA <- function(x,y,z,pp,aoaname,rdir) {
  
  scode <- x
  print(scode)
  vset <- y
  rcp <- z
  
  # Baseline
  baseline <- paste0(getwd(),"/","s",scode,"_pa_",vset,"_temp_19812010.nc")
  # Forecasts
  tper <- c("2030","2055","2080")
  # Make list of rasters
  flist <- raster(baseline) 
  
  # Coordinate system
  canomproj <- CRS(projection(flist))

#  thfile.ss <- read.csv("C:/Share/LCC-VP/RangeWide/AccyStats/modbesttemp_sensspec.csv")
#  thfile.kappa <- read.csv("C:/Share/LCC-VP/RangeWide/AccyStats/modbesttemp_maxkappa.csv")

  # Set AOA path
  PACE.path <- pp

  # SUMMARY SHAPEFILE
  # Read in summary shapefile
  AOA <- readOGR(pathpop(PACE.path,1),sub("[.][^.]*$", "", basename(PACE.path), perl=TRUE))

  # Reproject PACE shapefile to match raster coordinate system
  AOA <- spTransform(AOA,canomproj)

  # Crop brick to shapefile
  flist <- crop(flist,extent(AOA))

  # Read in points data
  ptname <- paste0("sp_",scode,"_pts_test")
  pts <- readOGR("C:/Share/LCC-VP/RangeWide/FIA/bySpecies8020",ptname)
  pts <- spTransform(pts,canomproj)
    
#     # EASTWIDE THRESHOLD
#     pts.prob <- extract(flist[[1]],pts,cellnumbers=T,df=T)
#     names(pts.prob) <- c("ID","CELLNO","PROB")
#     pts.prob$OBS <- pts$PRES
#     #adf <- data.frame(ID=pts.prob$ID,OBS=pts$PRES,PROB=pts.prob$PROB)
#     # Get rid of cells containing multiple points and NAs
#     pts.prob.sub <- pts.prob[!pts.prob$ID %in% pts.prob$ID[duplicated(pts.prob$CELLNO)],]
#     pts.prob.sub <- pts.prob.sub[complete.cases(pts.prob.sub),]
#     # Reorder and drop cell number column
#     pts.prob.sub <- pts.prob.sub[,c(1,4,3)]
#     
#     ##avec <- which(duplicated(pts.prob$CELLNO))
#     ##adf <- adf[-c(avec),]
#     ##adf <- adf[complete.cases(adf),]
#     east.thresh <- optimal.thresholds(pts.prob.sub,opt.methods=c("Sens=Spec"))

  #################
  # THRESHOLD (exract from 80/20 data)
  # Crop points to shapefile
  pts <- crop(pts,extent(AOA))
  
  #pts.prob <- extract(flist[[1]],pts,cellnumbers=T,df=T)
  pts.prob <- extract(flist,pts,cellnumbers=T,df=T)
  names(pts.prob) <- c("ID","CELLNO","PROB")
  pts.prob$OBS <- pts$PRES
  #adf <- data.frame(ID=pts.prob$ID,OBS=pts$PRES,PROB=pts.prob$PROB)
  # Get rid of cells containing multiple points and NAs
  pts.prob.sub <- pts.prob[!pts.prob$ID %in% pts.prob$ID[duplicated(pts.prob$CELLNO)],]
  pts.prob.sub <- pts.prob.sub[complete.cases(pts.prob.sub),]
  # Reorder and drop cell number column
  pts.prob.sub <- pts.prob.sub[,c(1,4,3)]
  
  local.thresh <- optimal.thresholds(pts.prob.sub,opt.methods=c("Sens=Spec"),threshold=501)
  
  # ACCURACY
  paaccy <- presence.absence.accuracy(pts.prob.sub,threshold=c(local.thresh$PROB))
        
  #############
  # BINARY MAPS (apply threshold to 100% fit data)
  # Sensitivity=Specificity
#     # EASTWIDE THRESHOLD
#     e.ss <- c(0, east.thresh$PROB, 0,  east.thresh$PROB, 1, 1)
#     rclmat.ss <- matrix(e.ss, ncol=3, byrow=TRUE)
#     rcl.east <- reclassify(flist,rclmat.ss)
  
  # Read in probability layers from full fit to which the threshold will be applied
  baseline <- paste0(rdir,"/","s",scode,"_pa_",vset,"_temp_19812010.nc")
  
  # Forecasts
  tper <- c("2030","2055","2080")
  flist <- c(baseline,paste0(rdir,"/","s",scode,"_",rcp,"_pa_",vset,"_temp_",tper,".nc"))
  # Make list of rasters
  flist <- sapply(flist, raster)
  flist <- brick(flist)
  # Crop brick to shapefile
  flist <- crop(flist,extent(AOA))
  l.ss <- c(0, local.thresh$PROB, 0,  local.thresh$PROB, 1, 1)
  rclmat.ss <- matrix(l.ss, ncol=3, byrow=TRUE)
  rcl.east <- reclassify(flist,rclmat.ss)
    
  # Write to file
  writeRaster(rcl.east, file=paste0(getwd(),"/","binary_rasters/s",scode,"_",aoaname,"_",rcp,"_pa_",vset,"_temp_eth.nc"), format="CDF", overwrite=T)
  write.csv(paaccy,file=paste0(getwd(),"/","binary_rasters/s",scode,"_",aoaname,"_pa_",vset,"_temp_accy.csv"))
}

# Output Directory
outdir <- "C:/Share/LCC-VP/Rangewide/RFoutput8020"

# Set working directory
setwd("C:/Share/LCC-VP/Rangewide/RFoutput8020")

# Raster directory (may not be the same as working directory)
rasterdir <- "C:/Share/LCC-VP/Rangewide/RFoutput"

# Species codes
slist <- list(12,16,97,110,111,121,123,126,129,131,132,261,315,316,317,318,319,332,371,403,407,408,409,412,531,541,602,611,621,746,762,802,824,832,833,835,837,951,971,972)

# Area for analysis
shp <- "C:/Share/LCC-VP/ALCC_PACE_Park_boundaries/EAST_boundary.shp" # <----------
# shp <- "C:/Share/LCC-VP/ALCC_PACE_Park_boundaries/GRSM_pace_albers.shp" # <----------
# shp <- "C:/Share/LCC-VP/ALCC_PACE_Park_boundaries/ALCC_boundary_albers.shp" # <----------

# Area of analysis type
AOAtype <- "EAST" # <----------
#AOAtype <- "GRSM_PACE" # <----------
#AOAtype <- "ALCC" # <----------

# Write classified rasters to files
# Variable set (either "best" or "all")
#vset <- "all" # <----------
# Rcp (either "rcp45" or "rcp85")
#rcp <- "rcp45" # <----------

for (vset in c("all","best")) {
  for (rcp in c("rcp45","rcp85")) {
    dattest <- lapply(slist, function(x) classSDMbyAOA(x,y=vset,z=rcp,pp=shp,aoaname=AOAtype,rdir=rasterdir))    
  }
}


#------------------------------------------------------------------------------------------
# RUN SPECIES GROUPS
# Spruce/Fir and hemlock
#spfi <- list(c(12,16,97,261),c('Balsam fir','Fraser fir','Red spruce','Eastern hemlock'))
spfi <- list(c(16,97,261),c('Fraser fir','Red spruce','Eastern hemlock'))

# Oaks
#yellowbuckeye
#white oak
#blackjack oak
#chestnut oak
#northern red oak
#post oak
#black oak
oaks <- list(c(332,802,824,832,833,835,837),c('Yellow buckeye','White oak','Blackjack oak',
                                              'Chestnut oak','Northern red oak','Post oak','Black oak'))

# Pines
#shortleaf pine
#slash pine
#longleaf pine
#table mountain pine
#pitch pine
#eastern white pine
#loblolly pine
#Virginia pine
pines <- list(c(110,111,121,123,126,129,131,132),c('Shortleaf pine','Slash pine','Longleaf pine','Table mountain pine','Pitch pine','Eastern white pine',
                                                   'Loblolly pine','Virginia pine'))

# Maples
#striped maple
#red maple
#silver maple
#sugar maple
#mountain maple
maples <- list(c(315,316,317,318,319),c('Striped maple','Red maple','Silver maple','Sugar maple','Mountain maple'))

# Hickories
#pignut hickory
#shagbark hickory
#black hickory
#mockernut hickory
#red hickory
#black walnut
#sweetgum
hickories <- list(c(403,407,408,409,412,611), c('Pignut hickory','Shagbark hickory',
                                                'Black hickory','Mockernut hickory','Red hickory','Sweetgum'))

# beeches, birches, walnut, aspen, cherry, basswood, ash, sweetgum, poplar, elm
# maple,beech,birch
mbb <- list(c(371,531,541,602,621,746,762,951,971,972),c('Yellow birch','American beech','White ash','Black walnut','Yellow-poplar',
                                                         'Quaking aspen','Black cherry','American basswood','Winged elm','American elm'))

# List species type groups
glist <- list(spfi,oaks,pines,maples,hickories,mbb)

# Species code
#species <- c(318,131,12,16,97,123,261,531,802,833)
species <- glist[[1]][[1]]

# Variable set (either "best" or "all")
vset <- "all" # <----------
# Rcp (either "rcp45" or "rcp85")
rcp <- "rcp45" # <----------
# Area for analysis
shp <- paste0("C:/Share/LCC-VP/ALCC_PACE_Park_boundaries/SHEN_pace_albers.shp") # <----------
# Area of analysis type
AOAtype <- "SHEN_PACE" # <----------

# Get means
dat <- lapply(species, function(x) AOAmean(x,y=vset,z=rcp,pp=shp))

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

# ANIMATIONS
# Write each brick to pngs
scodes <- "whatever"
for (i in scodes[1]) {
  aa <- brick(paste0("C:/Share/LCC-VP/RangeWide/RFoutput8020/binary_rasters/s",i,"_EAST_",rcp,"_pa_",vset,"_temp_lth.tif"))
  for (j in c(1:9)) {
    fname <- paste0("C:/Share/LCC-VP/RangeWide/RFoutput8020/animations/s",i,"/s",i,"_",rcp,"_",vset,"_",j,".png")
    png(file=fname)
    plot(aa[[j]],xlim=c(-100,-67),useRaster=F)
    dev.off()
  }
}

# SUITABLE PATCHES
# rcp85 all
#for (i in list(12,16,97,110,111,121,123,126,129,131,132,261,315,316,317,318,319,332,371,403,407,408,409,412,531,541,602,611,621,746,762,802,824,832,833,835,837,951,971,972)) {
for (i in list(317)) {
  print(i)
  ifile <- paste0("C:/Share/LCC-VP/RangeWide/RFoutput8020/binary_rasters/s",i,"_EAST_rcp85_pa_all_temp_lth.tif")
  aa <- brick(ifile)
  
  out.2 <- overlay(aa,fun=function(x){ 
    z <- x[[1]]
    z[x[[1]] == 0 & x[[6]] == 0 & x[[9]] == 0] <- 0 # never suitable
    z[x[[1]] == 1 & x[[6]] == 0 & x[[9]] == 0] <- 1 # suitable to unsuitable
    z[x[[1]] == 1 & x[[6]] == 1 & x[[9]] == 0] <- 1 # suitable to unsuitable
    z[x[[1]] == 1 & x[[6]] == 1 & x[[9]] == 1] <- 2 # suitable to suitable
    z[x[[1]] == 0 & x[[6]] == 1] <- 3 # unsuitable to suitable second to last time step
    z[x[[1]] == 0 & x[[6]] == 0 & x[[9]] == 1] <- 4 # unsuitable suitable last time step
    #   z[x[[1]] == 0 & x[[6]] == 1 & x[[9]] == 1] <- 4  
    return(z) 
  }) 
  
  out.3 <- as.factor(out.2)
  ofile <- paste0("C:/Share/LCC-VP/RangeWide/RFoutput8020/binary_rasters/s",i,"_EAST_rcp85_pa_all_temp_lth_4cat.tif")
  writeRaster(out.3,file=ofile,format="GTiff",overwrite=T)
}

# rcp45 best
for (i in list(12,16,97,110,111,121,123,126,129,131,132,261,315,316,317,318,319,332,371,403,407,408,409,412,531,541,602,611,621,746,762,802,824,832,833,835,837,951,971,972)) {
  print(i)
  ifile <- paste0("C:/Share/LCC-VP/RangeWide/RFoutput8020/binary_rasters/s",i,"_EAST_rcp45_pa_best_temp_lth.tif")
  aa <- brick(ifile)
  
  out.2 <- overlay(aa,fun=function(x){ 
    z <- x[[1]]
    z[x[[1]] == 0 & x[[6]] == 0 & x[[9]] == 0] <- 0 # never suitable
    z[x[[1]] == 1 & x[[6]] == 0 & x[[9]] == 0] <- 1 # suitable to unsuitable
    z[x[[1]] == 1 & x[[6]] == 1 & x[[9]] == 0] <- 1 # suitable to unsuitable
    z[x[[1]] == 1 & x[[6]] == 1 & x[[9]] == 1] <- 2 # suitable to suitable
    z[x[[1]] == 0 & x[[6]] == 1] <- 3 # unsuitable to suitable second to last time step
    z[x[[1]] == 0 & x[[6]] == 0 & x[[9]] == 1] <- 4 # unsuitable suitable last time step
    #   z[x[[1]] == 0 & x[[6]] == 1 & x[[9]] == 1] <- 4  
    return(z) 
  }) 
  
  out.3 <- as.factor(out.2)
  ofile <- paste0("C:/Share/LCC-VP/RangeWide/RFoutput8020/binary_rasters/s",i,"_EAST_rcp45_pa_best_temp_lth_4cat.tif")
  writeRaster(out.3,file=ofile,format="GTiff",overwrite=T)
}

# rcp45 all
#for (i in list(12,16,97,110,111,121,123,126,129,131,132,261,315,316,317,318,319,332,371,403,407,408,409,412,531,541,602,611,621,746,762,802,824,832,833,835,837,951,971,972)) {
for (i in list(317)) {  
  print(i)
  ifile <- paste0("C:/Share/LCC-VP/RangeWide/RFoutput8020/binary_rasters/s",i,"_EAST_rcp45_pa_all_temp_lth.tif")
  aa <- brick(ifile)
  
  out.2 <- overlay(aa,fun=function(x){ 
    z <- x[[1]]
    z[x[[1]] == 0 & x[[6]] == 0 & x[[9]] == 0] <- 0 # never suitable
    z[x[[1]] == 1 & x[[6]] == 0 & x[[9]] == 0] <- 1 # suitable to unsuitable
    z[x[[1]] == 1 & x[[6]] == 1 & x[[9]] == 0] <- 1 # suitable to unsuitable
    z[x[[1]] == 1 & x[[6]] == 1 & x[[9]] == 1] <- 2 # suitable to suitable
    z[x[[1]] == 0 & x[[6]] == 1] <- 3 # unsuitable to suitable second to last time step
    z[x[[1]] == 0 & x[[6]] == 0 & x[[9]] == 1] <- 4 # unsuitable suitable last time step
    #   z[x[[1]] == 0 & x[[6]] == 1 & x[[9]] == 1] <- 4  
    return(z) 
  }) 
  
  out.3 <- as.factor(out.2)
  ofile <- paste0("C:/Share/LCC-VP/RangeWide/RFoutput8020/binary_rasters/s",i,"_EAST_rcp45_pa_all_temp_lth_4cat.tif")
  writeRaster(out.3,file=ofile,format="GTiff",overwrite=T)
}


# PLOTTTING FIVE CATEGORY RASTERS
statelines <- readOGR("C:/Share/pjantz/NaturalEarth/Natural_Earth_quick_start/10m_cultural","ne_10m_admin_1_states_provinces_shp")
newext <- extent(c(-100,-66.47917,24.06250,49.93750))
statelines <- crop(statelines,newext)
coastline <- readOGR("C:/Share/pjantz/NaturalEarth/Natural_Earth_quick_start/10m_physical","ne_10m_coastline")
coastline <- crop(coastline,newext)
# There is no range map for 412 (red hickory, Carya ovalis) so use 403 
for (i in list(12,16,97,110,111,121,123,126,129,131,132,261,315,316,317,318,319,332,371,403,407,408,409,412,531,541,602,611,621,746,762,802,824,832,833,835,837,951,971,972)) {
#for (i in list(317,318,319,332,371,403,407,408,409,412,531,541,602,611,621,746,762,802,824,832,833,835,837,951,971,972)) {
  print(i)
  if (i == "412") {
    polys <- readOGR("C:/Share/LCC-VP/LittleRange",paste0("s","403","_little"))
  } else {
    polys <- readOGR("C:/Share/LCC-VP/LittleRange",paste0("s",i,"_little"))
  }
  ifile <- paste0("C:/Share/LCC-VP/RangeWide/RFoutput8020/binary_rasters/s",i,"_EAST_rcp85_pa_best_temp_eth_5cat.nc")
  aa <- raster(ifile)
  newext <- extent(c(-100,-66.47917,24.06250,49.93750))
  aa <- crop(aa,newext)
  aa <- as.factor(aa)
  rat <- levels(aa)[[1]]
  rat[["suitability"]] <- c("U","S-U","S-S","U-S(2055)","U-S(2080)")
  levels(aa) <- rat
  ofile <- paste0("C:/Share/LCC-VP/RangeWide/RFoutput8020/figures2/rcp85_best_temp_5cat/s",i,"_EAST_rcp85_pa_best_temp_eth_5cat.png")
  par(mar=c(5,3,2,2)+0.1)
  png(filename=ofile,width = 5, height = 4, units = 'in', res=300)
  myplot <- levelplot(aa, col.regions=c("gray","red","darkgreen","limegreen","lightgreen"), xlim=c(as.vector(newext)[1:2]),ylim=c(as.vector(newext)[3:4]),xlab="", ylab="") +
    layer(sp.polygons(statelines,col="darkgray")) + 
    layer(sp.lines(coastline,col="darkgray")) + 
    layer(sp.polygons(polys))
  print(myplot)
  dev.off()
}

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

x <- c(1996,2021,2030,2040,2055,2060,2070,2080)
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

plotfun <- function(i,y=dat,x=c(1995,2021,2030,2040,2055,2060,2070,2080)) {
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



pdf("C:/Share/LCC-VP/Rangewide/Climate/biodiff_figure.pdf")
for (i in 1:dim(aa)[3]){
  p <- levelplot(aa, layers=i,
                 margin=FALSE)
  print(p)
}
dev.off()