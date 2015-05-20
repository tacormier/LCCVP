# Call packages
require(sp)
require(data.table)
require(rgdal)
require(raster)
require(randomForest)
require(mgcv)
require(ncdf4)
source("C:/Share/pjantz/Scripts/LCCVP/handy_functions.R")
source("C:/Share/pjantz/Scripts/LCCVP/JEvans_RF_Functions.R")

# Output Directory
outdir <- "C:/Share/LCC-VP/Rangewide/RFoutput"

# Set working directory
setwd("C:/Share/LCC-VP/Rangewide/RFoutput")

AOAmean <- function(x,y,z,pp) {

  scode <- x
  print(scode)
  vset <- y
  rcp <- z
  
  # Baseline
  baseline <- paste0(getwd(),"/","s",scode,"_pa_",vset,"_temp_19812010.nc")
  
  # Forecasts
  tper <- c("2021","2030","2040","2055","2060","2070","2080")
  flist <- c(baseline,paste0(getwd(),"/","s",scode,"_",rcp,"_pa_",vset,"_temp_",tper,".nc"))
  
  if (file.exists(flist[[2]])) {
    # Make list of rasters
    flist <- sapply(flist, raster)
    
    flist <- brick(flist)
    
    # SUMMARY SHAPEFILE
    # Set AOA path
    PACE.path <- pp
    
    # Coordinate system
    canomproj <- CRS(projection(flist))
    
    # Reproject PACE shapefile to match climate anomaly coordinate system
    AOA <- readOGR(pathpop(PACE.path,1),sub("[.][^.]*$", "", basename(PACE.path), perl=TRUE))
    AOA <- spTransform(AOA,canomproj)
    
    # Extract pixels corresponding to the area of interest and calculate the spatial mean
    AOA.means <- extract(flist, AOA)
    AOA.means <- data.frame(AOA.means[[1]])
    
    names(AOA.means) <- c("19812010",tper)
    AOA.means <- sapply(AOA.means, mean)
    
    return(AOA.means)
  }
}

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




