
#[1] "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
#points - [1] "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
#[1] "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
# * Make this the projection - [1] "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"
# Actually, not sure about this. Maybe aea should be the projection...

# Call packages
require(sp)
require(data.table)
require(rgdal)
require(raster)
require(randomForest)
require(mgcv)
source("C:/Share/pjantz/Scripts/LCCVP/handy_functions.R")
source("C:/Share/pjantz/Scripts/LCCVP/JEvans_RF_Functions.R")

#########
# Species point shapefile
# [1] "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
spp <- readOGR("C:/Share/LCC-VP/RangeWide/FIA/bySpecies",layer="sp_318_pts")
#spp <- spTransform(spp,CRS("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"))
spp <- spTransform(spp,CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))

#######
# Predictors
# Bioclimatic variables
pre <- "C:/Share/LCC-VP/RangeWide/Climate/PRISM_30yr_norm_x100_"
bclim <- paste0(pre,"bio",c(1:19),"_pro.tif")
bclim <- sapply(bclim, raster)
bclim <- brick(bclim)

# Soils
pre <- "C:/Share/LCC-VP/RangeWide/Soils/muid_800_UNID_avg_" 
soils <- c("awc150","bd","bed_depth","clay","ph","sand","silt")
soils <- paste0(pre,soils,"_pro.tif")
soils <- sapply(soils, raster)
soils <- brick(soils)

# VPD
# annual, spring, summer, fall
# Assigning projection info as with soils.
pre <- "C:/Share/LCC-VP/TOPS/EAST/NewVPD19812010/prism_vpd_east_19812010_"
vpd <- c("ann","mam","jja","son","djf")
vpd <- paste0(pre,vpd,"_pro.tif")
vpd <- sapply(vpd, raster)
vpd <- brick(vpd)

# GDD (grab nationwide gdd map)
gdd <- raster("C:/Share/LCC-VP/RangeWide/Climate/gdd_PRISM_30yr_norm_x100.tif")

# Potential relative radiation (grab nationwide srad map)
srad <- raster("C:/Share/LCC-VP/RangeWide/Solarrad/direct_irrandiance_topo_shading_800m_contus_pro.tif")

# Topographic wetness index
twi <- raster("C:/Share/LCC-VP/RangeWide/TWI/TWI_easternUS_800m.tif")

# Human modification
hmod <- raster("C:/Share/LCC-VP/RangeWide/hmnmod_pro.tif")

#########
# Extract variable values to point
bclim <- extract(bclim,spp,df=T)
bclim <- bclim[,-c(1)]
names(bclim) <- c("bio1","bio2","bio3","bio4","bio5","bio6","bio7","bio8","bio9","bio10","bio11","bio12","bio13","bio14","bio15","bio16","bio17","bio18","bio19")

vpd <- extract(vpd, spp, df=T)
vpd <- vpd[,-c(1)]
names(vpd) <- c("vpd_ann","vpd_mam","vpd_jja","vpd_son","vpd_djf")

soils <- extract(soils, spp, df=T)
soils <- soils[,-c(1)]
names(soils) <- c("awc150","bd","bed_depth","clay","ph","sand","silt")

gdd <- extract(gdd,spp)
srad <- extract(srad,spp)
twi <- extract(twi,spp)

hmod <- extract(hmod,spp)

# Write predictors to csv file
pframe <- data.frame(spp@data$PID,bclim,vpd,soils,gdd=gdd,srad=srad,twi=twi,hmod=hmod)
names(pframe)[1] <- "PID"
write.csv(pframe,file="C:/Share/LCC-VP/Rangewide/fia_35_predictors.csv")

# Read in predictors
pframe <- fread("C:/Share/LCC-VP/Rangewide/fia_35_predictors.csv")
pframe[,V1:=NULL]

# Find cells with multiple plots and delete plots that are coded as zero if there
# is a plot in the same cell with a presence
# Use gdd as the grid template raster
gdd <- raster("C:/Share/LCC-VP/RangeWide/Climate/gdd_PRISM_30yr_norm_x100.tif")
dups <- extract(gdd,spp,cellnumbers=T)

# Winnowing function to get rid of conflicting zero plots
winnow <- function(x){
  inds <- which(dups[,'cells']==x)
  if (diff(range(spp@data[inds,'PRES'])) > 0) {
    z <- inds[which(spp@data[inds,'PRES']==0)]
    return(z)
  }
}

del.dups <- sapply(dups[,'cells'][duplicated(dups[,'cells'])], winnow)
del.dups <- unlist(del.dups[sapply(del.dups,function(x) !is.null(x))])


#-------------------------------------------------------
# Combine predictors and spp data
fdf <- data.frame(spp@data,pframe)

# Remove plots with absences in the same cell as presences
fdf <- fdf[-c(del.dups),]

# Grab complete cases only (remove rows with NA values)
fdf <- fdf[complete.cases(fdf),]

#-------------------------------------------------------
# Subset and calculate spearman correlation coefficients between each predictor variable
# and the response
fdf.pres <- fdf[fdf$PRES==1,]
fdf.abs <- fdf[fdf$PRES==0,]

spear.cor.ss <- list()
for (j in c(1:10)) {
  rvec <- sample(c(1:dim(fdf.abs)[1]),size=2*dim(fdf.pres[1]))
  fdf.ss <- rbind(fdf.pres,fdf.abs[rvec,])
  spear.cor.ss[[j]] <- cor(fdf.ss[17:52],method="spearman")
}

spear.aves <- Reduce("+",spear.cor.ss)/length(spear.cor.ss)

#-----------
# Use first row of the correlation matrix. Remember the 1st column is the response variable so needs
# to be accounted for when querying correlation values.
# Best temperature variables
# best.t.var <- names(fdf.ss)[18:52][c(1:11,33)][which.max(abs(spear.aves[1,c(2:dim(spear.aves)[2])][c(1:11,33)]))]
best.t.var <- names(which.max(abs(spear.aves[1,c(2:dim(spear.aves)[2])][c(1:11,33)])))
# Best precipitation variable
# best.p.var <- names(fdf.ss)[18:52][12:19][which.max(abs(spear.aves[1,c(2:dim(spear.aves)[2])][12:19]))]
best.p.var <- names(which.max(abs(spear.aves[1,c(2:dim(spear.aves)[2])][12:19])))
# Best vpd variable
# best.v.var <- names(fdf.ss)[18:52][20:24][which.max(abs(spear.aves[1,c(2:dim(spear.aves)[2])][20:24]))]
best.v.var <- names(which.max(abs(spear.aves[1,c(2:dim(spear.aves)[2])][20:24])))

#-----------
# Test correlation between best temperature variable and other temperature variables
# Keep those that are uncorrelated at 0.7 level
t.row <- spear.aves[rownames(spear.aves) %in% best.t.var,c(2:dim(spear.aves)[2])][c(1:11,20:24,33)]
t.keep <- names(t.row)[t.row < 0.7 & t.row > -0.7]
# Test correlation between best ppt variable and other ppt variables
p.row <- spear.aves[rownames(spear.aves) %in% best.p.var,c(2:dim(spear.aves)[2])][12:19]
p.keep <- names(p.row)[p.row < 0.7 & p.row > -0.7]
# Test correlation between best vpd variable and other vpd variables
v.row <- spear.aves[rownames(spear.aves) %in% best.v.var,c(2:dim(spear.aves)[2])][c(1:11,20:24,33)]
v.keep <- names(v.row)[v.row < 0.7 & v.row > -0.7]

# Always keep these
all.keep <- c("awc", "bd", "bdepth", "clay", "ph", "sand", "silt", "srad","twi", "hmod")

# Final set
temp.set <- c(best.t.var,t.keep,p.keep,all.keep)
vpd.set <- c(best.v.var,v.keep,p.keep,all.keep)

#-------------------------------------------------------
# GAM fitting, univariate
# Used to identify variables with minimal relationships with response variable
gamlist <- list()
for (i in seq_along(names(fdf)[18:52])) {
  print(paste(i,names(fdf)[18:52][i]))
  vname <- names(fdf)[18:52][i]
  gamlist[[i]] <- gam(fdf[,"PRES"]~s(fdf[,vname],bs="cr"),family=binomial,method="REML")
}

gam.cors <- unlist(lapply(gamlist, function(x) summary(x)$dev.expl*100))
names(fdf[18:52])[which(gam.cors < 5)]
names(fdf[18:52])[which(gam.cors >= 5)]

# Subset and rerun gam function
fdf.pres <- fdf[fdf$PRES==1,]
fdf.abs <- fdf[fdf$PRES==0,]

gam.cors.ss <- list()
for (j in c(1:10)) {
  rvec <- sample(c(1:dim(fdf.abs)[1]),size=2*dim(fdf.pres[1]))
  fdf.ss <- rbind(fdf.pres,fdf.abs[rvec,])
  gamlist <- list()
  for (i in seq_along(names(fdf.ss)[18:52])) {
    print(paste(i,names(fdf.ss)[18:52][i]))
    vname <- names(fdf.ss)[18:52][i]
    gamlist[[i]] <- gam(fdf.ss[,"PRES"]~s(fdf.ss[,vname],bs="cr"),family=binomial,method="REML")
  }
  gam.cors.ss[[j]] <- unlist(lapply(gamlist, function(x) summary(x)$dev.expl*100))  
}

# Get average values of correlations across 10 runs
gam.aves <- apply(do.call("rbind",gam.cors.ss),2,FUN="mean")
# Names of variables to keep
gam.keep <- names(fdf.ss)[18:52][which(gam.aves>=5)]

# ----------------------------
# Variables that explain at least 5% of the variance in the response
# and are uncorrelated
temp.fin <- intersect(gam.keep,temp.set)
vpd.fin <- intersect(gam.keep,vpd.set)


# RF on different subsets
# Class balance on full dataset
# 20% error on presence class, 85% overall, OOB error 5.3%
system.time(temp.mod <- rfClassBalance(fdf[,17],fdf[,temp.fin]))
system.time(vpd.mod <- rfClassBalance(fdf[,17],fdf[,vpd.fin]))

ssvars <- list()
for (i in c(1:5)) {
  # Subset and run model select function
  fdf.pres <- fdf[fdf$PRES==1,]
  fdf.abs <- fdf[fdf$PRES==0,]
  rvec <- sample(c(1:dim(fdf.abs)[1]),size=2*dim(fdf.pres[1]))
  fdf.ss <- rbind(fdf.pres,fdf.abs[rvec,])
  
  system.time(ssvars[[i]] <- rf.modelSel(x=fdf.ss[,temp.set],y=as.factor(fdf.ss[,17]),final=FALSE))
}

temp.cbal <- rfClassBalance(fdf[,17],fdf[,msel$SELVARS])
"bio5"  "bio8"  "bio15" "clay"  "silt"  "srad"  "twi"
# Use gam results to clear  those that average < 5%
#fdf.col.ss <- fdf[,!names(fdf) %in% names(fdf.ss)[18:52][which(gam.aves<5)]]

# Penultimate test of random forest modeling methods
system.time(temp.mod.v1 <- rfClassBalanceMIR(fdf[,17],fdf[,temp.fin]))


# Compare regression with classification
test_regression <- randomForest(fdf.ss[,msel$SELVARS],fdf.ss[,17])
test_classification <- randomForest(fdf.ss[,msel$SELVARS],factor(fdf.ss[,17]))

# K fold sets
group <- kfold(fdf.ss,5)
pres_train <- fdf.ss[group != 1, ]
pres_test <- fdf.ss[group == 1, ]
system.time(rf.kfold <- randomForest(pres_train[,msel$SELVARS],pres_train[,17],ntree=500,importance=F))
e = evaluate(pres_test[pres_test$PRES==2,c(22,25,32,45,48,50,51)], pres_test[pres_test$PRES==1,c(22,25,32,45,48,50,51)], rf.kfold)




# -----------------
# EXTRA STUFF
# -----------------


# testing iv and ba
fia.grid <- raster("C:/Share/LCC-VP/RangeWide/FIA/griddedBasalArea/s318.img")
fia.grid <- extract(fia.grid,spp)
iver <- extract(raster("C:/Share/szolkos/ParksProject/TreeSpp/IversonData/Rasterized/iv_318rpACT.tif"),spp)
cor(fia.grid,spp@data$BA_SPP,use='complete.obs')
cor(iver,spp@data$IV,use='complete.obs')


# spear.aves <- apply(do.call("rbind",spear.cor.ss),2,FUN="mean")
# 
# best.t.var <- names(fdf.ss)[18:52][1:11][which.max(abs(spear.aves[1:11]))]
# best.p.var <- names(fdf.ss)[18:52][12:19][which.max(abs(spear.aves[12:19]))]
# best.v.var <- names(fdf.ss)[18:52][20:24][which.max(abs(spear.aves[20:24]))]



# # Get count of variables with maximum correlation
# # x is a list of bivariate correlations
# # y is a starting index
# # z is an ending index
# afun <- function(x,y,z) {
#   vnames <- names(fdf.ss)[18:52][y:z]
#   x <- x[y:z]
#   ind <- which.max(abs(unlist(x)))
#   dat <- setNames(x[ind],vnames[ind])
#   dat
# }
# 
# # Get best temperature variable
# best.t.var <- names(which.max(table(names(unlist(lapply(spear.cor.ss, afun, y=1,z=11))))))
# # Get best precipitation variable
# best.p.var <- names(which.max(table(names(unlist(lapply(spear.cor.ss, afun, y=12,z=19))))))
# # Get best vapor pressure deficit variable
# best.v.var <- names(which.max(table(names(unlist(lapply(spear.cor.ss, afun, y=20,z=24))))))


View(cor(fdf.col.ss[,c(17:dim(fdf.col.ss)[2])]))


# Subsample data.frame
# rvec <- sample(c(1:dim(fdf)[1],n=100))


# This way of formulating the model gives the predictors in the first argument and
# the response variable in the second argument
# system.time(mtryvals <- tuneRF(fdf[,20:46],fdf[,14]))


# Calculate class balanced random forest models
rfClassBalance <- function (ydata, xdata, p=0.005, cbf=3, ...)


system.time(sp318.ss.500t <- randomForest(fdf.ss[,18:52],fdf.ss[,12],ntree=500,importance=T))

system.time(sp318.ss.500t.noiv <- randomForest(fdf.ss[,18:52],fdf.ss[,12],ntree=500,importance=F))

system.time(sp621.ss.100t.noiv <- randomForest(fdf.ss[,18:52],fdf.ss[,12],ntree=100,importance=T))

system.time(sp621.ss.100t.noiv <- randomForest(fdf.ss[,18:52],fdf.ss[,12],ntree=100,importance=T))

# Class balance on full dataset
# 20% error on presence class, 85% overall, OOB error 5.3%
system.time(cbaltest2 <- rfClassBalance(fdf.atest[,17],fdf.atest[,18:52]))

# Regular random forest on full dataset
# 53% error on presence class, 90% overall, OOB error 10.4 
system.time(cbaltest3 <- randomForest(fdf.atest[,18:52],factor(fdf.atest[,17])))









# Basal Area
#system.time(sp318.1k.500t <- randomForest(fdf[,18:52],fdf[,12],sampsize=1000,ntree=500,importance=T)) # 19.59
#system.time(sp318.50k.500t <- randomForest(fdf[,18:52],fdf[,12],sampsize=50000,ntree=500))

system.time(sp318.500t <- randomForest(fdf[,18:52],fdf[,12],ntree=500,importance=T))

# Importance Value
system.time(sp318.500t.iv <- randomForest(fdf[,18:52],fdf[,16],ntree=500,importance=T))

# Presence/Absence
system.time(sp318.1k.500t.pa <- randomForest(fdf[,18:52],fdf[,17],sampsize=1000,ntree=500,importance=T))

# # Find cells with multiple plots
# vpd.ann <- raster("C:/Share/LCC-VP/TOPS/EAST/NA_set/vpd_east_19802010_13.tif")
# dups <- extract(vpd.ann,spp,cellnumbers=T)

# Subset fdf
fdf.sub <- fdf[fdf$BA_SPP > 0,]
system.time(sp318.500t.sub <- randomForest(fdf.sub[,20:53],fdf.sub[,14],ntree=500))

# Test fit of gridded fia data
sp318.gridded <- randomForest(fdf[,20:54],fdf[,55],sampsize=1000,importance=T)

#################
# Results
> system.time(sp318.500t <- randomForest(fdf[,17:51],fdf[,11],ntree=500,importance=T))
user   system  elapsed 
12277.44    32.17 12328.96

Call:
  randomForest(x = fdf[, 17:51], y = fdf[, 11], ntree = 500, importance = T) 
Type of random forest: regression
Number of trees: 500
No. of variables tried at each split: 11

Mean of squared residuals: 7.059498
% Var explained: 27.89


X.IncMSE IncNodePurity
hmod    0.3660338     27443.599
twi     0.5749498     33546.926
awc     0.6430448     10088.517
bdepth  0.7419810      9106.895
bd      0.8143172     14822.160
sand    1.2265055     15988.173
bio2    1.2558994     20579.796
ph      1.6562747     18947.240
clay    1.7539519     19588.612
silt    2.1116067     23183.750
bio3    2.1452560     22612.383
bio13   2.2899229     22956.816
bio8    2.3789718     24276.525
bio9    2.6877736     20074.576
bio16   2.9558133     23082.931
bio18   3.0762031     26701.384
bio12   3.1623604     21789.732
bio14   3.6724789     22721.743
bio4    3.8499049     26142.686
bio17   3.9037112     20311.107
srad    3.9572739     41681.196
bio15   3.9718594     24343.730
vpd_son 4.0825942     25243.059
vpd_jja 4.6628704     48301.592
bio1    4.6645390     19408.034
bio11   4.7150515     21184.613
bio7    4.7605946     25358.007
bio6    4.8726570     24897.654
bio19   5.2807022     21109.221
gdd     5.4266069     26864.006
bio5    5.5337119     40303.951
vpd_ann 5.6876297     34918.885
vpd_djf 5.9032855     33807.091
bio10   5.9987583     32035.326
vpd_mam 6.3109862     38651.074



lr.fit <- gam(BA_SPP~s(bio1,bs="cr"),family=gaussian,data=fdf.ss,method="REML")
lr.fit <- gam(BA_SPP~s(bio2,bs="cr"),family=gaussian,data=fdf.ss,method="REML")
lr.fit <- gam(BA_SPP~s(vpd_son,bs="cr"),family=gaussian,data=fdf.ss,method="REML")

lr.fit <- gam(PRES~s(bio1,bs="cr"),family=binomial,data=fdf.ss,method="REML")
lr.fit <- gam(PRES~s(vpd_son,bs="cr"),family=binomial,data=fdf.ss,method="REML")


plot(lr.fit,select=k,scale=0)
