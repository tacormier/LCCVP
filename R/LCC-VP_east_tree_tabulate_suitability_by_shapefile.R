# Tabulate areas of classified suitability maps by AOA
# Appalachian Landscape Conservation Cooperative boundary
# GRSM PACE
# SHEN PACE
# DEWA PACE

# Do this for each species for rcp45 and rcp85
# Summarize for species groupings

# FUNCTION BODY
tareafun <- function(x) {
  vols <- raster("C:/Share/brogers/Masks/SDM_EastVul/Geo/varea.nc")
  scode <- x
  print(scode)
  vset <- c("best","all")
  rcp <- c("rcp45","rcp85")
  zones <- c("C:/Share/LCC-VP/ALCC_PACE_Park_boundaries/ALCC_boundary_albers.nc",
             "C:/Share/LCC-VP/ALCC_PACE_Park_boundaries/GRSM_pace_albers.nc",
             "C:/Share/LCC-VP/ALCC_PACE_Park_boundaries/SHEN_pace_albers.nc",
             "C:/Share/LCC-VP/ALCC_PACE_Park_boundaries/DEWA_pace_albers.nc")
  zones <- sapply(zones,raster)

  # Get projection from first raster
  canomproj <- CRS(projection(raster("C:/Share/LCC-VP/RangeWide/RFoutput8020/binary_rasters/s12_EAST_rcp45_pa_all_temp_eth_5cat.nc")))

  # Set up list for receiving area statistics
  zone.tarea <- list()
  for (i in zones) {
    
    AOA <- i
    for (j in rcp) {
      for (k in vset) {
        # Read in five category raster
        # Crop to AOA
        cat5 <- raster(paste0("s",scode,"_EAST_",j,"_pa_",k,"_temp_eth_5cat.nc"))
        cat5 <- crop(cat5,AOA)
        cat5 <- mask(cat5,AOA)
        # Crop cell area raster to AOA
        vols.2 <- crop(vols,AOA)
        vols.2 <- mask(vols.2,AOA)
        # Calculate area of each land cover type within AOA
        # Land cover types are numbered 1-6
        alist <- list()
        for (l in c(1:6)){
          tarea <- sum(vols.2[cat5==l])
          alist <- c(alist,tarea)
        }
        alist <- data.frame(unlist(alist))
        names(alist) <- paste0(rev(unlist(strsplit(AOA@file@name,"\\\\")))[1],"_",j,"_vset")
        total.area <- sum(vols.2[AOA==1])
        if (!identical(total.area,sum((alist)))) {print("problem here")}
        zone.tarea <- c(zone.tarea,alist)
        }
      }
    }
  zone.tarea <- do.call("rbind",zone.tarea)
}

# Packages
require(raster)

setwd("C:/Share/LCC-VP/RangeWide/RFoutput8020/binary_rasters")
aa <- lapply(list(12,16,97,110,111,121,123,126,129,131,132,261,315,316,317,318,319,332,371,403,407,408,409,412,531,541,602,611,621,746,762,802,824,832,833,835,837,951,971,972),
                  tareafun)
bb <- as.data.frame(do.call("rbind",aa))
bb$SCODE <- as.vector(rep(c(12,16,97,110,111,121,123,126,129,131,132,261,315,316,317,318,319,332,371,403,407,408,409,412,531,541,602,611,621,746,762,802,824,832,833,835,837,951,971,972),each=16))

# write.csv(bb, "C:/Share/LCC-VP/Rangewide/RFoutput8020/binary_rasters/AOA_tareav2.csv")

# Suitability codes corresponding to column positions
# 0 - never suitable
# 1 - suitable to unsuitable
# 2 - suitable to suitable
# 3 - unsuitable to suitable second to last time step
# 4 - unsuitable to suitable last time step

# 1 - never suitable
# 2 - suitable to unsuitable second to last time step 
# 3 - suitable to unsuitable in last time step
# 4 - suitable to suitable
# 5 - unsuitable to suitable second to last time step
# 6 - unsuitable suitable last time step

# Species group code (in order of species code)
gcodes <- c(1,7,1,8,9,9,7,7,2,8,7,2,4,3,6,3,4,5,4,5,5,10,10,10,3,3,6,10,5,4,3,5,10,5,5,10,5,3,10,6)
gnames <- unlist(strsplit("Spruce-Fir,Cent. and Mtn. Pines,Spruce-Fir,Loblolly-Shortleaf Pine,Longleaf-Slash Pine,Longleaf-Slash Pine,Cent. and Mtn. Pines,Cent. and Mtn. Pines,N. Pines,Loblolly-Shortleaf Pine,Cent. and Mtn. Pines,N. Pines,N. Hardwoods,Maple-Beech-Birch,Elm-Ash-Cottonwood,Maple-Beech-Birch,N. Hardwoods,Oak-Hickory,N. Hardwoods,Oak-Hickory,Oak-Hickory,S. Oak-Hickory,S. Oak-Hickory,S. Oak-Hickory,Maple-Beech-Birch,Maple-Beech-Birch,Elm-Ash-Cottonwood,S. Oak-Hickory,Oak-Hickory,N. Hardwoods,Maple-Beech-Birch,Oak-Hickory,S. Oak-Hickory,Oak-Hickory,Oak-Hickory,S. Oak-Hickory,Oak-Hickory,Maple-Beech-Birch,S. Oak-Hickory,Elm-Ash-Cottonwood",
                   split=","))


# Add group code to suitability area dataframe
bb$GCODE <- rep(gcodes,each=16)
bb$GNAME <- rep(gnames,each=16)

# Add AOA code
bb$AOA <- rep(rep(c("ALCC","GRSM","SHEN","DEWA"),each=4),times=40)

# Break data frame into "all" and "best" groups and then into rcp45 and rcp85 groups
best.df <- bb[rep(c(TRUE,FALSE),times=320),]
all.df <- bb[rep(c(FALSE,TRUE),times=320),]

best.df.45 <- best.df[rep(c(TRUE,FALSE),times=160),]
best.df.85 <- best.df[rep(c(FALSE,TRUE),times=160),]

all.df.45 <- all.df[rep(c(TRUE,FALSE),times=160),]
all.df.85 <- all.df[rep(c(FALSE,TRUE),times=160),]

# Use best.df.45. and best.df.85 for all species except
# 121,123,16,315,319,332,403,407,412
final.df.45 <- best.df.45
final.df.45[all.df.45$SCODE %in% c(121,123,16,315,319,332,403,407,412),] <- all.df.45[all.df.45$SCODE %in% c(121,123,16,315,319,332,403,407,412),]

final.df.85 <- best.df.85
final.df.85[all.df.85$SCODE %in% c(121,123,16,315,319,332,403,407,412),] <- all.df.85[all.df.85$SCODE %in% c(121,123,16,315,319,332,403,407,412),]

#-----------------------------------------------------------------------------------------
# FRACTION SUITABLE CLIMATE SPACE LOSS
# Calculate fraction of currently suitable habitat lost for each species group
# rel.45 <- final.df.45[,c(2)]/rowSums(final.df.45[,c(2:3)])
# rel.45 <- data.frame(fr_lost_45=rel.45,final.df.45[,c("SCODE","GCODE","GNAME","AOA")])
# #rel.45$fr_lost_45[is.na(rel.45$fr_lost_45)] <- -0.1
# 
# rel.85 <- final.df.85[,c(2)]/rowSums(final.df.85[,c(2:3)])
# rel.85 <- data.frame(fr_lost_85=rel.85,final.df.85[,c("SCODE","GCODE","GNAME","AOA")])
# #rel.85$fr_lost_85[is.na(rel.85$fr_lost_85)] <- -0.1
# 
# # Average species groups by AOA
# rel.45.AOA <- tapply(rel.45$fr_lost_45,paste0(rel.45$AOA,"_",rel.45$GNAME),mean,na.rm=T)
# rel.85.AOA <- tapply(rel.85$fr_lost_85,paste0(rel.85$AOA,"_",rel.85$GNAME),mean,na.rm=T)
# 
# rel.45.AOA <- data.frame(AOA=do.call("rbind",(strsplit(names(rel.45.AOA),split="_")))[,1],
#                          GNAME=do.call("rbind",(strsplit(names(rel.45.AOA),split="_")))[,2],
#                          RCP="rcp45",
#                          fr_lost=rel.45.AOA)
# 
# rel.85.AOA <- data.frame(AOA=do.call("rbind",(strsplit(names(rel.85.AOA),split="_")))[,1],
#                          GNAME=do.call("rbind",(strsplit(names(rel.85.AOA),split="_")))[,2],
#                          RCP="rcp85",
#                          fr_lost=rel.85.AOA)
# 
# rel.AOA <- rbind(rel.45.AOA,rel.85.AOA)
# 
# rel.AOA$AOA <- ordered(rel.AOA$AOA, levels = c("ALCC", "DEWA", "SHEN", "GRSM"))
# 
# barcolors = c('gray20', 'gray80')
# 
# ofile <- "C:/Share/LCC-VP/RangeWide/RFoutput8020/figures3/test_spp_group_responses.png"
# #png(filename=ofile,width = 5, height = 5, units = 'in', pointsize=12,res=300)
# png(filename=ofile,units="px", width=1600, height=1600, res=150)
# barchart(fr_lost~AOA | GNAME, data=rel.AOA,
#          groups=RCP,layout = c(5,2),
#          ylab=list(expression("Fraction of Suitable Climate Space Lost by 2080"),cex=1.2),
#          par.settings=list(superpose.polygon=list(col=barcolors),strip.background=list(col="gray90")),
#          auto.key=list(space='top',columns=2),scales=list(x=list(cex=c(0.8)),y=list(cex=c(1.2))))
# dev.off()

#-------------------------------------------------------------------------------------------------
# FRACTION SUITABLE CLIMATE SPACE GAIN
# Calculate fraction of currently suitable habitat gain for each species group
# rel.45.gain <- rowSums(final.df.45[,c(4:5)])/rowSums(final.df.45[,c(2:3)])
# rel.45.gain <- data.frame(fr_gain_45=rel.45.gain,final.df.45[,c("SCODE","GCODE","GNAME","AOA")])
# 
# rel.85.gain <- rowSums(final.df.85[,c(4:5)])/rowSums(final.df.85[,c(2:3)])
# rel.85.gain <- data.frame(fr_gain_85=rel.85.gain,final.df.85[,c("SCODE","GCODE","GNAME","AOA")])
# 
# # Average species groups by AOA
# rel.45.AOA.gain <- tapply(rel.45.gain$fr_gain_45,paste0(rel.45.gain$AOA,"_",rel.45.gain$GNAME),mean,na.rm=T)
# rel.85.AOA.gain <- tapply(rel.85.gain$fr_gain_85,paste0(rel.85.gain$AOA,"_",rel.85.gain$GNAME),mean,na.rm=T)
# 
# rel.85.AOA.gain <- tapply(rel.85.gain$fr_gain_85,paste0(rel.85.gain$AOA,"_",rel.85.gain$GNAME),mean,na.rm=T)
# 
# rel.45.AOA.gain <- data.frame(AOA=do.call("rbind",(strsplit(names(rel.45.AOA.gain),split="_")))[,1],
#                          GNAME=do.call("rbind",(strsplit(names(rel.45.AOA.gain),split="_")))[,2],
#                          RCP="rcp45",
#                          fr_gain=rel.45.AOA.gain)
# 
# rel.85.AOA.gain <- data.frame(AOA=do.call("rbind",(strsplit(names(rel.85.AOA.gain),split="_")))[,1],
#                          GNAME=do.call("rbind",(strsplit(names(rel.85.AOA.gain),split="_")))[,2],
#                          RCP="rcp85",
#                          fr_gain=rel.85.AOA.gain)
# 
# rel.AOA.gain <- rbind(rel.45.AOA.gain,rel.85.AOA.gain)
# 
# rel.AOA.gain$AOA <- ordered(rel.AOA.gain$AOA, levels = c("ALCC", "DEWA", "SHEN", "GRSM"))
# 
# barcolors = c('gray20', 'gray80')
# 
# ofile <- "C:/Share/LCC-VP/RangeWide/RFoutput8020/figures3/test_spp_group_responses_gain.png"
# #png(filename=ofile,width = 5, height = 5, units = 'in', pointsize=12,res=300)
# png(filename=ofile,units="px", width=1600, height=1600, res=150)
# barchart(fr_gain~AOA | GNAME, data=rel.AOA.gain,
#          groups=RCP,layout = c(5,2),
#          ylab=list(expression("Fraction of Suitable Climate Space Gain by 2080"),cex=1.2),
#          par.settings=list(superpose.polygon=list(col=barcolors),strip.background=list(col="gray90")),
#          auto.key=list(space='top',columns=2),scales=list(x=list(cex=c(0.8)),y=list(cex=c(1.2))))
# dev.off()


#-------------------------------------------------------------------------------------------------
# FRACTION SUITABLE CLIMATE SPACE GAIN OVER UNION OF OLD AND NEW
# Calculate fraction of currently suitable habitat gain for each species group
# rel.45.gainunion <- rowSums(final.df.45[,c(4:5)])/rowSums(final.df.45[,c(2:5)])
# rel.45.gainunion <- data.frame(fr_gainunion_45=rel.45.gainunion,final.df.45[,c("SCODE","GCODE","GNAME","AOA")])
# 
# rel.85.gainunion <- rowSums(final.df.85[,c(4:5)])/rowSums(final.df.85[,c(2:5)])
# rel.85.gainunion <- data.frame(fr_gainunion_85=rel.85.gainunion,final.df.85[,c("SCODE","GCODE","GNAME","AOA")])
# 
# # Average species groups by AOA
# rel.45.AOA.gainunion <- tapply(rel.45.gainunion$fr_gainunion_45,paste0(rel.45.gainunion$AOA,"_",rel.45.gainunion$GNAME),mean,na.rm=T)
# rel.85.AOA.gainunion <- tapply(rel.85.gainunion$fr_gainunion_85,paste0(rel.85.gainunion$AOA,"_",rel.85.gainunion$GNAME),mean,na.rm=T)
# 
# rel.85.AOA.gainunion <- tapply(rel.85.gainunion$fr_gainunion_85,paste0(rel.85.gainunion$AOA,"_",rel.85.gainunion$GNAME),mean,na.rm=T)
# 
# rel.45.AOA.gainunion <- data.frame(AOA=do.call("rbind",(strsplit(names(rel.45.AOA.gainunion),split="_")))[,1],
#                               GNAME=do.call("rbind",(strsplit(names(rel.45.AOA.gainunion),split="_")))[,2],
#                               RCP="rcp45",
#                               fr_gainunion=rel.45.AOA.gainunion)
# 
# rel.85.AOA.gainunion <- data.frame(AOA=do.call("rbind",(strsplit(names(rel.85.AOA.gainunion),split="_")))[,1],
#                               GNAME=do.call("rbind",(strsplit(names(rel.85.AOA.gainunion),split="_")))[,2],
#                               RCP="rcp85",
#                               fr_gainunion=rel.85.AOA.gainunion)
# 
# rel.AOA.gainunion <- rbind(rel.45.AOA.gainunion,rel.85.AOA.gainunion)
# 
# rel.AOA.gainunion$AOA <- ordered(rel.AOA.gainunion$AOA, levels = c("ALCC", "DEWA", "SHEN", "GRSM"))
# 
# barcolors = c('gray20', 'gray80')
# 
# ofile <- "C:/Share/LCC-VP/RangeWide/RFoutput8020/figures3/test_spp_group_responses_gainunion.png"
# #png(filename=ofile,width = 5, height = 5, units = 'in', pointsize=12,res=300)
# png(filename=ofile,units="px", width=1600, height=1600, res=150)
# barchart(fr_gainunion~AOA | GNAME, data=rel.AOA.gainunion,
#          groups=RCP,layout = c(5,2),
#          ylab=list(expression("Suitable Climate Space Gain by 2080"),cex=1.2),
#          par.settings=list(superpose.polygon=list(col=barcolors),strip.background=list(col="gray90")),
#          auto.key=list(space='top',columns=2),scales=list(x=list(cex=c(0.8)),y=list(cex=c(1.2))))
# dev.off()

#--------------------------------------------------------------------------
# SPECIES LEVEL
# FRACTION SUITABLE CLIMATE SPACE LOSS
# Calculate fraction of currently suitable habitat lost for each species
# RCP45
# rel.45.spploss <- tapply(rel.45$fr_lost_45,rel.45$AOA, "[") # List with four elements
# rel.45.spploss <- do.call("cbind",rel.45.spploss)
# spcodes <- tapply(rel.45$SCODE,rel.45$AOA, "[")$ALCC
# rel.45.spploss <- data.frame(scode=spcodes,rel.45.spploss)

# rel.45.spploss[order(rel.45.spploss$ALCC,decreasing=T),][c(1:10),]
# rel.45.spploss[order(rel.45.spploss$GRSM,decreasing=T),][c(1:11),]
# rel.45.spploss[order(rel.45.spploss$SHEN,decreasing=T),][c(1:10),]
# rel.45.spploss[order(rel.45.spploss$DEWA,decreasing=T),][c(1:11),]
# 
# # RCP85
# rel.85.spploss <- tapply(rel.85$fr_lost_85,rel.85$AOA, "[") # List with four elements
# rel.85.spploss <- do.call("cbind",rel.85.spploss)
# spcodes <- tapply(rel.85$SCODE,rel.85$AOA, "[")$ALCC
# rel.85.spploss <- data.frame(scode=spcodes,rel.85.spploss)
# 
# # rel.85.spploss[order(rel.85.spploss$ALCC,decreasing=T),][c(1:10),]
# # rel.85.spploss[order(rel.85.spploss$GRSM,decreasing=T),][c(1:11),]
# # rel.85.spploss[order(rel.85.spploss$SHEN,decreasing=T),][c(1:10),]
# # rel.85.spploss[order(rel.85.spploss$DEWA,decreasing=T),][c(1:11),]
# rel.85.spploss$scode <- reorder( rel.85.spploss$scode, rel.85.spploss$ALCC )
# barchart(ALCC~scode, rel.85.spploss,col="gray",
#          ylab=list(expression("Suitable Climate Space Loss by 2080"),cex=1.2))
#          #par.settings=list(superpose.polygon=list(col=barcolors),strip.background=list(col="gray90")),
#          #auto.key=list(space='top',columns=2),scales=list(x=list(cex=c(0.8)),y=list(cex=c(1.2))))
# 
# 
# 
# 
# rel.85.gainunion.spp <-  tapply(rel.85.gainunion$fr_gainunion_85,rel.85.gainunion$AOA, "[") # List with four elements
# rel.85.gainunion.spp <- do.call("cbind",rel.85.gainunion.spp)
# spcodes <- tapply(rel.85.gainunion$SCODE,rel.85.gainunion$AOA, "[")$ALCC
# rel.85.gainunion.spp <- data.frame(scode=spcodes,rel.85.gainunion.spp)
# 
# rel.85.gainunion.spp[order(rel.85.gainunion.spp$ALCC,decreasing=T),][c(1:10),]

# SPECIES LEVEL CURRENT FRACTION OF AOA
#-------------------------------------------------------------------------------------------------
# AREA OF CURRENT SUITABLE HABITAT NORMALIZED BY AOA (use RCP45 data frame, it's almost exactly the
# same as using the RCP85 data frame, these should be identical but they're not, probably
# because I didn't catch some of the pixels that shift suitability classes more than once.
# There are very few of these so not accounted for here.)
farea.now <- rowSums(final.df.45[,c(2:4)])/rowSums(final.df.45[,c(1:6)])
farea.now <- data.frame(fr_now=farea.now,final.df.45[,c("SCODE","GCODE","GNAME","AOA")])
spcodes <- tapply(farea.now$SCODE,farea.now$AOA, "[")$ALCC
gcodevec <- tapply(farea.now$GCODE,farea.now$AOA, "[")$ALCC
farea.now <- tapply(farea.now$fr_now,farea.now$AOA, "[") # List with four elements
farea.now <- do.call("cbind",farea.now)
farea.now <- data.frame(scode=spcodes,gcode=gcodevec,farea.now)
names(farea.now) <- c("scode_now","gcode_now","ALCC_now","DEWA_now","GRSM_now","SHEN_now")

# SPECIES LEVEL NET CHANGE
#-------------------------------------------------------------------------------------------------
# FRACTION SUITABLE CLIMATE SPACE FUTURE SUITABLE - CURRENT SUITABLE BY 2080 NORMALIZED BY AOA
# RCP45
rel.45.net <- (rowSums(final.df.45[,c(3:5)])-rowSums(final.df.45[,c(2:4)]))/rowSums(final.df.45[,c(1:6)])
rel.45.net <- data.frame(fr_net_45=rel.45.net,final.df.45[,c("SCODE","GCODE","GNAME","AOA")])
spcodes <- tapply(rel.45.net$SCODE,rel.45.net$AOA, "[")$ALCC
gcodevec <- tapply(rel.45.net$GCODE,rel.45.net$AOA, "[")$ALCC
rel.45.net <- tapply(rel.45.net$fr_net_45,rel.45.net$AOA, "[") # List with four elements
rel.45.net <- do.call("cbind",rel.45.net)
rel.45.net <- data.frame(scode=spcodes,gcode=gcodevec,rel.45.net)
rel.45.net$gcode <- as.numeric(as.character(rel.45.net$gcode))
rel.45.net$scode <- factor(rel.45.net$scode)
rel.45.net$scode <- reorder( rel.45.net$scode, rel.45.net$ALCC )

# RCP85
rel.85.net <- (rowSums(final.df.85[,c(3:5)])-rowSums(final.df.85[,c(2:4)]))/rowSums(final.df.85[,c(1:6)])
rel.85.net <- data.frame(fr_net_85=rel.85.net,final.df.85[,c("SCODE","GCODE","GNAME","AOA")])
spcodes <- tapply(rel.85.net$SCODE,rel.85.net$AOA, "[")$ALCC
gcodevec <- tapply(rel.85.net$GCODE,rel.85.net$AOA, "[")$ALCC
rel.85.net <- tapply(rel.85.net$fr_net_85,rel.85.net$AOA, "[") # List with four elements
rel.85.net <- do.call("cbind",rel.85.net)
rel.85.net <- data.frame(scode=spcodes,gcode=gcodevec,rel.85.net)
rel.85.net$gcode <- as.numeric(as.character(rel.85.net$gcode))
rel.85.net$scode <- factor(rel.85.net$scode)
rel.85.net$scode <- reorder( rel.85.net$scode, rel.85.net$ALCC )

# PLOT
# barchart(ALCC~scode, data=rel.85.net,origin=0, groups=as.factor(gcodes), box.ratio=4,
#          ylab=list(expression("Suitable Climate Space Loss by 2080"),cex=1.2),
#          scales=list(x=list(rot=65),cex=1.2))
# OR
# USE BASE IMPLEMENTATION FOR NOW

# Grayscale
cmat <- matrix(c(255,255,255,
                 240,240,240,
                 217,217,217,
                 189,189,189,
                 150,150,150,
                 115,115,115,
                 97,97,97,
                 82,82,82,
                 37,37,37,
                 0,0,0),nrow=10,ncol=3,byrow=T)

# Brown-blue
cmat <- matrix(c(84,48,5,
140,81,10,
191,129,45,
223,194,125,
246,232,195,
199,234,229,
128,205,193,
53,151,143,
1,102,94,
0,60,48),nrow=10,ncol=3,byrow=T)

cmat <- matrix(c(127,59,8,
179,88,6,
224,130,20,
253,184,99,
254,224,182,
216,218,235,
178,171,210,
128,115,172,
84,39,136,
45,0,75),nrow=10,ncol=3,byrow=T)

cmat <- rgb(red=cmat,max=255)

# START PLOTTING
# ALCC
alcc.net <- rel.85.net$ALCC
alcc.names <- unlist(strsplit(c("balsam fir,Fraser fir,red spruce,shortleaf pine,slash pine,longleaf pine,table mountain pine,pitch pine,eastern white pine,loblolly pine,Virginia pine,eastern hemlock,striped maple,red maple,silver maple,sugar maple,mountain maple,yellow buckeye,yellow birch,pignut hickory,shagbark hickory,black hickory,mockernut hickory,red hickory,American beech,white ash,black walnut,sweetgum ,yellow-poplar ,quaking aspen,black cherry,white oak,blackjack oak,chestnut oak,northern red oak,post oak,black oak,American basswood,winged elm,American elm"),split=","))
ss <- order(alcc.net, decreasing  = T)  # get data order
ss.alcc <- ss # for later data frame gluing
alcc.net <- alcc.net[ss] # sort data
alcc.names <- alcc.names[ss] # sort names
alcc.scode <- rel.85.net[ss, 1] # get sorted codes
alcc.scode.col <- factor(as.character(rel.85.net[ss,2])) # Get factors for colors
#levels(alcc.scode.col) <-  c("red", "orange", "blue", "dark green","gray","yellow","green","purple","pink","brown")
levels(alcc.scode.col) <-  cmat # assign color codes as factor levels
cvec <- levels(alcc.scode.col)
alcc.scode.col <- as.character(alcc.scode.col)
# mar: c(bottom, left, top, right) The default is c(5, 4, 4, 2) + 0.1.
par(mar = c(5,9,4,3)+0.1)
bp <- barplot(alcc.net,col=alcc.scode.col,names.arg=alcc.names,las=1,horiz=T,xlim=c(-0.4,1),
        #legend = levels(factor(gnames)),
        main="ALCC Net Change in Suitable Area")
points(farea.now$ALCC[ss],bp,pch=18) # Plot points showing current suitable fraction
newgnames <- gnames[ss]
adf <- data.frame(rev(newgnames)[!duplicated(rev(alcc.scode.col))],
                  rev(alcc.scode.col)[!duplicated(rev(alcc.scode.col))])

#points(alcc.net,bp,pch=17,col="black") # alcc.net here is calculated from rcp4.5 below and added to this plot for reference

legend("topright", 
       legend = adf[,1], 
       fill = as.character(adf[,2]),cex=1.2,y.intersp=0.5,x.intersp=0.5)



# GRSM
grsm.net <- rel.85.net$GRSM
grsm.names <- unlist(strsplit(c("balsam fir,Fraser fir,red spruce,shortleaf pine,slash pine,longleaf pine,table mountain pine,pitch pine,eastern white pine,loblolly pine,Virginia pine,eastern hemlock,striped maple,red maple,silver maple,sugar maple,mountain maple,yellow buckeye,yellow birch,pignut hickory,shagbark hickory,black hickory,mockernut hickory,red hickory,American beech,white ash,black walnut,sweetgum ,yellow-poplar ,quaking aspen,black cherry,white oak,blackjack oak,chestnut oak,northern red oak,post oak,black oak,American basswood,winged elm,American elm"),split=","))
ss <- order(grsm.net, decreasing  = F)  # sort our data
ss.grsm <- ss
grsm.net <- grsm.net[ss]
grsm.names <- grsm.names[ss]
grsm.scode <- rel.85.net[ss, 1]
grsm.scode.col <- factor(as.character(rel.85.net[ss,2]))
levels(grsm.scode.col) <-  c("red", "orange", "blue", "dark green","gray","yellow","green","purple","pink","brown")
grsm.scode.col <- as.character(grsm.scode.col)
# mar: c(bottom, left, top, right) The default is c(5, 4, 4, 2) + 0.1.
par(mar = c(5,9,4,3)+0.1)
bp <- barplot(grsm.net,col=grsm.scode.col,names.arg=grsm.names,las=1,horiz=T,xlim=c(-0.4,1),
        main="GRSM Net Change in Suitable Area")
points(farea.now$GRSM[ss],bp,pch=18) # Plot points showing current suitable fraction

# SHEN
shen.net <- rel.85.net$SHEN
shen.names <- unlist(strsplit(c("balsam fir,Fraser fir,red spruce,shortleaf pine,slash pine,longleaf pine,table mountain pine,pitch pine,eastern white pine,loblolly pine,Virginia pine,eastern hemlock,striped maple,red maple,silver maple,sugar maple,mountain maple,yellow buckeye,yellow birch,pignut hickory,shagbark hickory,black hickory,mockernut hickory,red hickory,American beech,white ash,black walnut,sweetgum ,yellow-poplar ,quaking aspen,black cherry,white oak,blackjack oak,chestnut oak,northern red oak,post oak,black oak,American basswood,winged elm,American elm"),split=","))
ss <- order(shen.net, decreasing  = F)  # sort our data
ss.shen <- ss
shen.net <- shen.net[ss]
shen.names <- shen.names[ss]
shen.scode <- rel.85.net[ss, 1]
shen.scode.col <- factor(as.character(rel.85.net[ss,2]))
levels(shen.scode.col) <-  c("red", "orange", "blue", "dark green","gray","yellow","green","purple","pink","brown")
shen.scode.col <- as.character(shen.scode.col)
# mar: c(bottom, left, top, right) The default is c(5, 4, 4, 2) + 0.1.
par(mar = c(5,9,4,3)+0.1)
bp <- barplot(shen.net,col=shen.scode.col,names.arg=shen.names,las=1,horiz=T,xlim=c(-0.4,1),
        main="SHEN Net Change in Suitable Area")
points(farea.now$SHEN[ss],bp,pch=18) # Plot points showing current suitable fraction

# DEWA
dewa.net <- rel.85.net$DEWA
dewa.names <- unlist(strsplit(c("balsam fir,Fraser fir,red spruce,shortleaf pine,slash pine,longleaf pine,table mountain pine,pitch pine,eastern white pine,loblolly pine,Virginia pine,eastern hemlock,striped maple,red maple,silver maple,sugar maple,mountain maple,yellow buckeye,yellow birch,pignut hickory,shagbark hickory,black hickory,mockernut hickory,red hickory,American beech,white ash,black walnut,sweetgum ,yellow-poplar ,quaking aspen,black cherry,white oak,blackjack oak,chestnut oak,northern red oak,post oak,black oak,American basswood,winged elm,American elm"),split=","))
ss <- order(dewa.net, decreasing  = F)  # sort our data
ss.dewa <- ss
dewa.net <- dewa.net[ss]
dewa.names <- dewa.names[ss]
dewa.scode <- rel.85.net[ss, 1]
dewa.scode.col <- factor(as.character(rel.85.net[ss,2]))
levels(dewa.scode.col) <-  c("red", "orange", "blue", "dark green","gray","yellow","green","purple","pink","brown")
dewa.scode.col <- as.character(dewa.scode.col)
# mar: c(bottom, left, top, right) The default is c(5, 4, 4, 2) + 0.1.
par(mar = c(5,9,4,3)+0.1)
bp <- barplot(dewa.net,col=dewa.scode.col,names.arg=dewa.names,las=1,horiz=T,xlim=c(-0.7,1),
        main="DEWA Net Change in Suitable Area")
points(farea.now$DEWA[ss],bp,pch=18) # Plot points showing current suitable fraction

# Slap them all into the same data frame
write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

write.excel(data.frame(alcc.scode,alcc.names,alcc.net,farea.now$ALCC[ss.alcc],
           grsm.scode,grsm.names,grsm.net,farea.now$GRSM[ss.grsm],
           shen.scode,shen.names,shen.net,farea.now$SHEN[ss.shen],
           dewa.scode,dewa.names,dewa.net,farea.now$DEWA[ss.dewa]))

#----------------------------------------------------------------------------------------
# RCP45 NET CHANGE AMOUNTS
# ALCC
alcc.net <- rel.45.net$ALCC
alcc.names <- unlist(strsplit(c("balsam fir,Fraser fir,red spruce,shortleaf pine,slash pine,longleaf pine,table mountain pine,pitch pine,eastern white pine,loblolly pine,Virginia pine,eastern hemlock,striped maple,red maple,silver maple,sugar maple,mountain maple,yellow buckeye,yellow birch,pignut hickory,shagbark hickory,black hickory,mockernut hickory,red hickory,American beech,white ash,black walnut,sweetgum ,yellow-poplar ,quaking aspen,black cherry,white oak,blackjack oak,chestnut oak,northern red oak,post oak,black oak,American basswood,winged elm,American elm"),split=","))
ss <- order(alcc.net, decreasing  = T)  # get data order
ss.alcc <- ss # for later data frame gluing
alcc.net <- alcc.net[ss] # sort data
alcc.names <- alcc.names[ss] # sort names
alcc.scode <- rel.45.net[ss, 1] # get sorted codes
alcc.scode.col <- factor(as.character(rel.45.net[ss,2])) # Get factors for colors
#levels(alcc.scode.col) <-  c("red", "orange", "blue", "dark green","gray","yellow","green","purple","pink","brown")
levels(alcc.scode.col) <-  cmat # assign color codes as factor levels
alcc.scode.col <- as.character(alcc.scode.col)
# mar: c(bottom, left, top, right) The default is c(5, 4, 4, 2) + 0.1.
par(mar = c(5,9,4,3)+0.1)
bp <- barplot(alcc.net,col=alcc.scode.col,names.arg=alcc.names,las=1,horiz=T,xlim=c(-0.4,1),
              #legend = levels(factor(gnames)),
              main="ALCC Net Change in Suitable Area")
points(farea.now$ALCC[ss],bp,pch=18) # Plot points showing current suitable fraction

# GRSM
grsm.net <- rel.45.net$GRSM
grsm.names <- unlist(strsplit(c("balsam fir,Fraser fir,red spruce,shortleaf pine,slash pine,longleaf pine,table mountain pine,pitch pine,eastern white pine,loblolly pine,Virginia pine,eastern hemlock,striped maple,red maple,silver maple,sugar maple,mountain maple,yellow buckeye,yellow birch,pignut hickory,shagbark hickory,black hickory,mockernut hickory,red hickory,American beech,white ash,black walnut,sweetgum ,yellow-poplar ,quaking aspen,black cherry,white oak,blackjack oak,chestnut oak,northern red oak,post oak,black oak,American basswood,winged elm,American elm"),split=","))
ss <- order(grsm.net, decreasing  = F)  # sort our data
ss.grsm <- ss
grsm.net <- grsm.net[ss]
grsm.names <- grsm.names[ss]
grsm.scode <- rel.45.net[ss, 1]
grsm.scode.col <- factor(as.character(rel.45.net[ss,2]))
levels(grsm.scode.col) <-  c("red", "orange", "blue", "dark green","gray","yellow","green","purple","pink","brown")
grsm.scode.col <- as.character(grsm.scode.col)
# mar: c(bottom, left, top, right) The default is c(5, 4, 4, 2) + 0.1.
par(mar = c(5,9,4,3)+0.1)
bp <- barplot(grsm.net,col=grsm.scode.col,names.arg=grsm.names,las=1,horiz=T,xlim=c(-0.4,1),
              main="GRSM Net Change in Suitable Area")
points(farea.now$GRSM[ss],bp,pch=18) # Plot points showing current suitable fraction

# SHEN
shen.net <- rel.45.net$SHEN
shen.names <- unlist(strsplit(c("balsam fir,Fraser fir,red spruce,shortleaf pine,slash pine,longleaf pine,table mountain pine,pitch pine,eastern white pine,loblolly pine,Virginia pine,eastern hemlock,striped maple,red maple,silver maple,sugar maple,mountain maple,yellow buckeye,yellow birch,pignut hickory,shagbark hickory,black hickory,mockernut hickory,red hickory,American beech,white ash,black walnut,sweetgum ,yellow-poplar ,quaking aspen,black cherry,white oak,blackjack oak,chestnut oak,northern red oak,post oak,black oak,American basswood,winged elm,American elm"),split=","))
ss <- order(shen.net, decreasing  = F)  # sort our data
ss.shen <- ss
shen.net <- shen.net[ss]
shen.names <- shen.names[ss]
shen.scode <- rel.45.net[ss, 1]
shen.scode.col <- factor(as.character(rel.45.net[ss,2]))
levels(shen.scode.col) <-  c("red", "orange", "blue", "dark green","gray","yellow","green","purple","pink","brown")
shen.scode.col <- as.character(shen.scode.col)
# mar: c(bottom, left, top, right) The default is c(5, 4, 4, 2) + 0.1.
par(mar = c(5,9,4,3)+0.1)
bp <- barplot(shen.net,col=shen.scode.col,names.arg=shen.names,las=1,horiz=T,xlim=c(-0.4,1),
              main="SHEN Net Change in Suitable Area")
points(farea.now$SHEN[ss],bp,pch=18) # Plot points showing current suitable fraction

# DEWA
dewa.net <- rel.45.net$DEWA
dewa.names <- unlist(strsplit(c("balsam fir,Fraser fir,red spruce,shortleaf pine,slash pine,longleaf pine,table mountain pine,pitch pine,eastern white pine,loblolly pine,Virginia pine,eastern hemlock,striped maple,red maple,silver maple,sugar maple,mountain maple,yellow buckeye,yellow birch,pignut hickory,shagbark hickory,black hickory,mockernut hickory,red hickory,American beech,white ash,black walnut,sweetgum ,yellow-poplar ,quaking aspen,black cherry,white oak,blackjack oak,chestnut oak,northern red oak,post oak,black oak,American basswood,winged elm,American elm"),split=","))
ss <- order(dewa.net, decreasing  = F)  # sort our data
ss.dewa <- ss
dewa.net <- dewa.net[ss]
dewa.names <- dewa.names[ss]
dewa.scode <- rel.45.net[ss, 1]
dewa.scode.col <- factor(as.character(rel.45.net[ss,2]))
levels(dewa.scode.col) <-  c("red", "orange", "blue", "dark green","gray","yellow","green","purple","pink","brown")
dewa.scode.col <- as.character(dewa.scode.col)
# mar: c(bottom, left, top, right) The default is c(5, 4, 4, 2) + 0.1.
par(mar = c(5,9,4,3)+0.1)
bp <- barplot(dewa.net,col=dewa.scode.col,names.arg=dewa.names,las=1,horiz=T,xlim=c(-0.7,1),
              main="DEWA Net Change in Suitable Area")
points(farea.now$DEWA[ss],bp,pch=18) # Plot points showing current suitable fraction

# Slap them all into the same data frame
write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

write.excel(data.frame(alcc.scode,alcc.names,alcc.net,farea.now$ALCC[ss.alcc],
                       grsm.scode,grsm.names,grsm.net,farea.now$GRSM[ss.grsm],
                       shen.scode,shen.names,shen.net,farea.now$SHEN[ss.shen],
                       dewa.scode,dewa.names,dewa.net,farea.now$DEWA[ss.dewa]))


#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#par.settings=list(superpose.polygon=list(col=barcolors),strip.background=list(col="gray90")),
#auto.key=list(space='top',columns=2),scales=list(x=list(cex=c(0.8)),y=list(cex=c(1.2))))
barchart(DEWA~scode, data=rel.85.net,col="gray",horizontal=F,origin=0,
         ylab=list(expression("Suitable Climate Space Loss by 2080"),cex=1.2))
barchart(GRSM~as.factor(scode), data=rel.85.net,col="gray",horizontal=F,origin=0,
         ylab=list(expression("Suitable Climate Space Loss by 2080"),cex=1.2))





# Average species groups by AOA
rel.45.AOA.gainunion <- tapply(rel.45.gainunion$fr_gainunion_45,paste0(rel.45.gainunion$AOA,"_",rel.45.gainunion$GNAME),mean,na.rm=T)
rel.85.AOA.gainunion <- tapply(rel.85.gainunion$fr_gainunion_85,paste0(rel.85.gainunion$AOA,"_",rel.85.gainunion$GNAME),mean,na.rm=T)

rel.85.AOA.gainunion <- tapply(rel.85.gainunion$fr_gainunion_85,paste0(rel.85.gainunion$AOA,"_",rel.85.gainunion$GNAME),mean,na.rm=T)

rel.45.AOA.gainunion <- data.frame(AOA=do.call("rbind",(strsplit(names(rel.45.AOA.gainunion),split="_")))[,1],
                                   GNAME=do.call("rbind",(strsplit(names(rel.45.AOA.gainunion),split="_")))[,2],
                                   RCP="rcp45",
                                   fr_gainunion=rel.45.AOA.gainunion)

rel.85.AOA.gainunion <- data.frame(AOA=do.call("rbind",(strsplit(names(rel.85.AOA.gainunion),split="_")))[,1],
                                   GNAME=do.call("rbind",(strsplit(names(rel.85.AOA.gainunion),split="_")))[,2],
                                   RCP="rcp85",
                                   fr_gainunion=rel.85.AOA.gainunion)

rel.AOA.gainunion <- rbind(rel.45.AOA.gainunion,rel.85.AOA.gainunion)

rel.AOA.gainunion$AOA <- ordered(rel.AOA.gainunion$AOA, levels = c("ALCC", "DEWA", "SHEN", "GRSM"))

barcolors = c('gray20', 'gray80')

ofile <- "C:/Share/LCC-VP/RangeWide/RFoutput8020/figures3/test_spp_group_responses_gainunion.png"
#png(filename=ofile,width = 5, height = 5, units = 'in', pointsize=12,res=300)
png(filename=ofile,units="px", width=1600, height=1600, res=150)
barchart(fr_gainunion~AOA | GNAME, data=rel.AOA.gainunion,
         groups=RCP,layout = c(5,2),
         ylab=list(expression("Suitable Climate Space Gain by 2080"),cex=1.2),
         par.settings=list(superpose.polygon=list(col=barcolors),strip.background=list(col="gray90")),
         auto.key=list(space='top',columns=2),scales=list(x=list(cex=c(0.8)),y=list(cex=c(1.2))))
dev.off()












# OVERFLOW--------------------------------------------------------------------
# Packages
require(raster)
require(geosphere)

  
  # Crop brick to shapefile
  flist <- crop(flist,extent(AOA))
  l.ss <- c(0, local.thresh$PROB, 0,  local.thresh$PROB, 1, 1)
  rclmat.ss <- matrix(l.ss, ncol=3, byrow=TRUE)
  rcl.local <- reclassify(flist,rclmat.ss)
  
  
  
  
  
  

  # Make list of rasters
  flist <- raster(baseline) 
  
    
  
  
  # Crop brick to shapefile
  flist <- crop(flist,extent(AOA))
  
  # Read in points data
  ptname <- paste0("sp_",scode,"_pts_test")
  pts <- readOGR("C:/Share/LCC-VP/RangeWide/FIA/bySpecies8020",ptname)
  pts <- spTransform(pts,canomproj)
  
  
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
  
  
  
  # Write to file
  #     writeRaster(rcl.east, file=paste0(getwd(),"/","binary_rasters/s",scode,"_",aoaname,"_",rcp,"_pa_",vset,"_temp_eth.tif"), format="GTiff")
  writeRaster(rcl.local, file=paste0(getwd(),"/","binary_rasters/s",scode,"_",aoaname,"_",rcp,"_pa_",vset,"_temp_eth.tif"), format="GTiff", overwrite=T)
  write.csv(paaccy,file=paste0(getwd(),"/","binary_rasters/s",scode,"_",aoaname,"_pa_",vset,"_temp_accy.csv"))
}


# ARGUMENTS

# Set working directory
setwd("C:/Share/LCC-VP/RangeWide/RFoutput8020/binary_rasters")

# Area of analysis type
#AOAtype <- "ALCC" # <----------
AOAtype <- "GRSM_PACE" # <----------

# AOA shapefile
# shp <- "C:/Share/LCC-VP/ALCC_PACE_Park_boundaries/ALCC_boundary_albers.shp" # <----------
shp <- "C:/Share/LCC-VP/ALCC_PACE_Park_boundaries/GRSM_pace_albers.shp" # <----------

# Read in 5 category raster
aa <- raster("s12_EAST_rcp45_pa_all_temp_eth_5cat.nc")

# Read in shapefile
AOA <- readOGR()