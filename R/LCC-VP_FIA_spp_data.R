#############################################################################################
# Author: Patrick Jantz
# Date: 04/23/2014
# Name: LCC-VP_FIA_spp_data.R

# This script takes FIA data (stored by state) and for the time period from 2000-2010
# calculates presence/absence, average basal area (square meters/hectare), average stem counts and 
# average importance value for all eastern U.S. tree species for each
# FIA plot that was surveyed using the standard methodology.

# The TPA_UNADJ factor for microplots is 74.96528 and 6.018046 for standard plots
# Standard subplot radius is 24 feet, 6.8 feet for microplots
# Each subplot is 0.04154 acre. There are four subplots so their combined
# area is 0.1661669 acres. 1/0.1661669 is equal to 6.018046 which is the usual (more or less)
# adjustment factor for subplots when calculating basal area.
# Each microplot is 0.00333 acre. There are four microplots so their combined
# area is 0.0133395. 1/0.0133395 is equal to 74.96533 which is the usual (more or less)
# adjustment factor for microplots.

# Importance value = relative stem density + relative basal area
# e.g. (# of fir stems/all stems) + (fir basal area/all basal area)

# Extra Info
# In the TREE.CSV table, SPCD gives the species code (code-name crosswalk is given
# in Appendix F of the FIA 5.1.5 database documentation)

# 12 balsam fir Abies balsamea
# 16 Fraser fir Abies fraseri
# 97 red spruce Picea rubens
# 110 shortleaf pine Pinus echinata
# 111 slash pine Pinus elliottii
# 121 longleaf pine Pinus palustris
# 123 Table Mountain pine Pinus pungens
# 126 pitch pine Pinus rigida
# 129 eastern white pine Pinus strobus
# 131 loblolly pine Pinus taeda
# 132 Virginia pine Pinus virginiana
# 261 eastern hemlock Tsuga canadensis
# 318 sugar maple Acer saccharum
# 332 yellow buckeye Aesculus flava
# 371 yellow birch Betula alleghaniensis
# 407 shagbark hickory Carya ovata
# 408 black hickory Carya texana
# 409 mockernut hickory Carya alba
# 412 red hickory Carya ovalis
# 531 American beech Fagus grandifolia
# 541 white ash Fraxinus americana
# 602 black walnut Juglans nigra
# 611 sweetgum Liquidambar styraciflua
# 621 yellow-poplar Liriodendron tulipifera
# 746 quaking aspen Populus tremuloides
# 802 white oak Quercus alba
# 832 chestnut oak Quercus prinus
# 833 northern red oak Quercus rubra
# 951 American basswood Tilia americana
# 972 American elm Ulmus americana
# 762  black cherry  Prunus serotina
# 315  striped maple	Acer pensylvanicum
# 319	mountain maple	Acer spicatum
# 317	silver maple	Acer saccharinum
# 403	pignut hickory	Carya glabra
# 837	black oak	Quercus velutina
# 835	post oak	Quercus stellata
# 971	winged elm	Ulmus alata
# 824	blackjack oak	Quercus marilandica

# Start processing
# Set working directory and list directories
require(data.table) # Faster data.frame implementation
wd <- "C:/Share/LCC-VP/RangeWide/FIA/byState"
setwd("C:/Share/LCC-VP/RangeWide/FIA/byState")
dlist <- list.dirs(path=".")[-c(1)] # The -1 gets rid of the "." directory in the list

spdf <- function(x,y) {
  
  # Get state abbreviation
  x <- unlist(strsplit(x,"/"))[2]
  print(x)
  
  # Make file name
  fname <- paste0(wd,"/",x,"/",x,"_","TREE.CSV")
  
  # Read in tree table (fread is much faster than read.csv)
  fia.tree <- fread(fname)  

  # Subset the tree table
  fia.tree <- subset(fia.tree,STATUSCD == 1) # Grab live trees
  fia.tree <- subset(fia.tree, SUBP %in%  c(1,2,3,4)) # Grab only subplots 1-4. 
  fia.tree <- subset(fia.tree, DIA != '001') # Remove seedlings (not sure this is necessary)
  fia.tree <- subset(fia.tree, !is.na(DIA))
  fia.tree <- subset(fia.tree, !is.na(TPA_UNADJ))
  # Grab post 2000 plots but not those inventoried after the climate normals period (1980-2010)
  fia.tree <- subset(fia.tree, INVYR >= 2000 & INVYR <= 2010)
  
  # Read in plot table
  fname <- paste0(wd,"/",x,"/",x,"_","PLOT.CSV")
  fia.plot <- fread(fname)
  
  # Read in condition table
  fname <- paste0(wd,"/",x,"/",x,"_","COND.CSV")
  fia.cond <- fread(fname)
  
  # Set keys for join
  setkey(fia.plot,CN)
  setkey(fia.tree,PLT_CN)
  
  # Join plot and tree tables. The plot table expands to match rows in the tree table. This
  # provides plot attributes for each tree observation
  q1 <- fia.plot[fia.tree]

  # Set keys for join
  setkey(q1,CN,CONDID)
  setkey(fia.cond,PLT_CN,CONDID)

  # Join condition table to the join of tree and plot tables.
  # This table contains plot location information for each plot for each year by species.
  # I'm not sure the condition table needs to be part of these calcs but it's easy enough to change
  # later.
  q2 <- fia.cond[q1]
  rm(q1)

  # Calculate basal area of trees
  # basal area is calculated by DIA^2*0.005454*TPA_UNADJ
  # basically, square DBH, multiply by a constant to get square feet then
  # multiply by trees per acre unadjusted. This gets you basal area (square feet per acre).
  # Convert to square meters per hectare.
  q2[,BA_SPP:=(DIA^2)*0.005454*TPA_UNADJ*2.47105*0.092903]

  # Sum stems and basal area by species in each plot in each year
  # Carry through ECOSUBCD (only need to add it once)  
  q3 <- q2[,list(ECOSUBCD=`[`(ECOSUBCD,1),CONDID=`[`(CONDID,1),LAT=`[`(LAT,1),LON=`[`(LON,1),BALIVE=`[`(BALIVE,1),STEMS_SPP=as.double(.N),BA_SPP=sum(BA_SPP)),by=list(INVYR,STATECD,UNITCD,COUNTYCD,PLOT,SPCD)]
  rm(q2)

  # Sum number of stems of all species in each plot in each year
  stp <- q3[,list(STEMS_PLT=sum(STEMS_SPP)),by=list(INVYR,STATECD,UNITCD,COUNTYCD,PLOT)]

  # Set keys for join
  setkey(stp,INVYR,STATECD,UNITCD,COUNTYCD,PLOT)
  setkey(q3,INVYR,STATECD,UNITCD,COUNTYCD,PLOT)

  # Join plot stem counts to table that holds species level counts and basal areas
  q4 <- stp[q3]
  rm(q3)
  rm(stp)

  # Sum basal area of all species in each plot in each year
  # This won't match BALIVE where there is more than one condition in a plot
  bap <- q4[,list(BA_PLT=sum(BA_SPP)),by=list(INVYR,STATECD,UNITCD,COUNTYCD,PLOT)]
  
  # Set keys for join
  setkey(bap,INVYR,STATECD,UNITCD,COUNTYCD,PLOT)
  setkey(q4,INVYR,STATECD,UNITCD,COUNTYCD,PLOT)

  # Join plot basal area sums to table that holds species level counts and basal areas
  q5 <- bap[q4]
  rm(q4)
  rm(bap)

  # Calculate stem count and basal area ratios for each species relative to plot
  q5[,`:=`(ST_RATIO=STEMS_SPP/STEMS_PLT,BA_RATIO=BA_SPP/BA_PLT)]

  # Calculate importance values for each species in each plot in each year
  q5[,IV:=(ST_RATIO*100)+(BA_RATIO*100)]

  # Average across years
  q6 <- q5[,list(ECOSUBCD=`[`(ECOSUBCD,1),LAT=`[`(LAT,1),LON=`[`(LON,1),
                 BA_PLT=mean(BA_PLT),STEMS_PLT=mean(STEMS_PLT),
                 BA_SPP=mean(BA_SPP),STEMS_SPP=mean(STEMS_SPP),
                 ST_RATIO=mean(ST_RATIO),BA_RATIO=mean(BA_RATIO),
                 IV=mean(IV)),by=list(STATECD,UNITCD,COUNTYCD,PLOT,SPCD)]

  # Average plots that have the same coordinates but different plot IDs
  q6[,PID:=paste(LAT,LON,sep="_")]
  q7 <- q6[,list(ECOSUBCD=`[`(ECOSUBCD,1),LAT=`[`(LAT,1),LON=`[`(LON,1),PLOT=`[`(PLOT,1),
                 BA_PLT=mean(BA_PLT),STEMS_PLT=mean(STEMS_PLT),
                 BA_SPP=mean(BA_SPP),STEMS_SPP=mean(STEMS_SPP),
                 ST_RATIO=mean(ST_RATIO),BA_RATIO=mean(BA_RATIO),
                 IV=mean(IV)),by=list(STATECD,UNITCD,COUNTYCD,SPCD,PID)]


  q7[,PRES:=ifelse(STEMS_SPP>0,1,0)]
  rm(q6)

  return(q7)
}

# Apply function to each state
fia.stats <- lapply(dlist,spdf)

# Row bind together
fia.stats <- do.call("rbind",fia.stats)

# # Get rid of NAs (there are like 13, usually where TPA_UNADJ==0 for some reason)
# fia.stats <- fia.stats[complete.cases(fia.stats),]


#####################
# Checking for duplicates. If dups is not empty, there are duplicate plots somewhere.
dups <- list()
ids <- list()
for (i in unique(fia.stats$SPCD)) {
  aa <- subset(fia.stats,SPCD==i)
  bb <- aa[duplicated(PID),]$PID
  print(i)
  ids <- c(ids,i)
  dups <- c(dups,bb)  
}


# Write data for each species to a csv and a shapefile
# Make sure to add plots back in where a species is absent
# Where the sp. does not occur, code as 0
for (i in unique(fia.stats$SPCD)) {
  
  print(i)
  
  # Get records for a speces
  aa <- subset(fia.stats,SPCD==i)
  
  # Set the rest of the plots to zero
  xx <- fia.stats[PID %in% unique(fia.stats$PID)[!unique(fia.stats$PID) %in% unique(aa$PID)],
                  list(ECOSUBCD=`[`(ECOSUBCD,1),STATECD=`[`(STATECD,1),UNITCD=`[`(UNITCD,1),COUNTYCD=`[`(COUNTYCD,1),
                       SPCD=0,LAT=`[`(LAT,1),LON=`[`(LON,1),PLOT=`[`(PLOT,1),
                       BA_PLT=0,STEMS_PLT=0,BA_SPP=0,STEMS_SPP=0,ST_RATIO=0,BA_RATIO=0,
                       IV=0,PRES=0),by=PID]
  
  # Reorder columns to correspond with original table
  setcolorder(xx,names(fia.stats))
  
  # Append absence records to presence records
  bb <- rbind(aa,xx)
  
  # Write data to table
  write.csv(bb,paste0("C:/Share/LCC-VP/RangeWide/FIA/bySpecies/",i,".csv"))

  # Make a spatial points object
  cc <- SpatialPointsDataFrame(bb[,list(LON,LAT)],bb[,list(STATECD,UNITCD,COUNTYCD,SPCD,PID,ECOSUBCD,LAT,LON,
                                                           PLOT,BA_PLT,STEMS_PLT,BA_SPP,STEMS_SPP,ST_RATIO,
                                                           BA_RATIO,IV,PRES)],proj4string=CRS("+proj=longlat +datum=WGS84"))
  # Write to shapefile
  writeOGR(cc,"C:/Share/LCC-VP/RangeWide/FIA/bySpecies",paste0("sp_",i,"_pts"),driver="ESRI Shapefile")
}






