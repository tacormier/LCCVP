# load the library
library(biomod2)

# load our species data
DataSpecies <- read.csv(system.file("external/species/mammals_table.csv",
                   package="biomod2"))
head(DataSpecies)

# the name of studied species
myRespName <- 'GuloGulo'
# the presence/absences data for our species
myResp <- as.numeric(DataSpecies[,myRespName])
# the XY coordinates of species data
myRespXY <- DataSpecies[,c("X_WGS84","Y_WGS84")]

# load the environmental raster layers (could be .img, ArcGIS rasters or any supported form
# Environmental variables extracted from Worldclim (bio_3, bio_4,
# bio_7, bio_11 & bio_12)
myExpl = stack( system.file( "external/bioclim/current/bio3.grd",
                      package="biomod2"),
                system.file( "external/bioclim/current/bio4.grd",
                      package="biomod2"),
                system.file( "external/bioclim/current/bio7.grd",
                      package="biomod2"),
                system.file( "external/bioclim/current/bio11.grd",
                      package="biomod2"),
                system.file( "external/bioclim/current/bio12.grd",
                      package="biomod2"))

#Format data
myBiomodData <- BIOMOD_FormatingData(resp.var = myResp,
                      expl.var = myExpl,resp.xy = myRespXY,
                      resp.name = myRespName)

#At this point, check whether the data are correctly formatted by printing
#and plotting the created object.
myBiomodData
plot(myBiomodData)

# 2. Defining Models Options using default options.
myBiomodOption <- BIOMOD_ModelingOptions()

# 3. Computing the models
myBiomodModelOut <- BIOMOD_Modeling(
  myBiomodData,
  models = c('SRE','CTA','RF','MARS','FDA'),
  models.options = myBiomodOption,
  NbRunEval=3,
  DataSplit=80,
  Prevalence=0.5,
  VarImport=3,
  models.eval.meth = c('TSS','ROC'),
  SaveObj = TRUE,
  rescal.all.models = TRUE,
  do.full.models = FALSE,
  modeling.id = paste(myRespName,"FirstModeling",sep=""))

# get all models evaluation
myBiomodModelEval <- getModelsEvaluations(myBiomodModelOut)
# print the dimnames of this object
dimnames(myBiomodModelEval)

# let's print the TSS scores of Random Forest
myBiomodModelEval["TSS","Testing.data","RF",,]

# let's print the ROC scores of all selected models
myBiomodModelEval["ROC","Testing.data",,,]

# print variable importances
getModelsVarImport(myBiomodModelOut)

#Here comes one of the most interesting features of biomod2. BIOMOD_EnsembleModeling
#combines individual models to build some kind of meta-model. In the following example, 
#we decide to exclude all models having a TSS score lower than 0.7.

myBiomodEM <- BIOMOD_EnsembleModeling(
  modeling.output = myBiomodModelOut,
  chosen.models = 'all',
  em.by='all',
  eval.metric = c('TSS'),
  eval.metric.quality.threshold = c(0.7),
  prob.mean = T,
  prob.cv = T,
  prob.ci = T,
  prob.ci.alpha = 0.05,
  prob.median = T,
  committee.averaging = T,
  prob.mean.weight = T,
  prob.mean.weight.decay = 'proportional' )

#We decide to evaluate all meta-models produced even the CV (Coefficient
#of Variation) one which is quite hard to interpret. You may consider it as:
#higher my score is, more the variation is localised where my species is fore-
#casted as present.

# print summary
myBiomodEM

# get evaluation scores
getEMeval(myBiomodEM)

#Once the models are calibrated and evaluated, we might want to project the
#potential distribution of the species over space and time. This is made using
#BIOMOD_Projection
#NOTE 10 :
#  All projections are stored directly on your hard drive
#First let's project the individual models on our current conditions (the
#globe) to visualize them.

# projection over the globe under current conditions
myBiomomodProj <- BIOMOD_Projection(
  modeling.output = myBiomodModelOut,
  new.env = myExpl,
  proj.name = 'current',
  selected.models = 'all',
  binary.meth = 'TSS',
  compress = 'xz',
  clamping.mask = F,
  output.format = '.grd')

# summary of crated oject
myBiomomodProj

# files created on hard drive
list.files("GuloGulo/proj_current/")

# make some plots sub-selected by str.grep argument
plot(myBiomomodProj, str.grep = 'MARS')

# if you want to make custom plots, you can also get the projected map
myCurrentProj <- getProjection(myBiomomodProj)
myCurrentProj

#Then we can project the potential distribution of the species over time, i.e.
#into the future.

# load environmental variables for the future.
myExplFuture = stack( system.file( "external/bioclim/future/bio3.grd",
                                   package="biomod2"),
                      system.file( "external/bioclim/future/bio4.grd",
                                   package="biomod2"),
                      system.file( "external/bioclim/future/bio7.grd",
                                   package="biomod2"),
                      system.file( "external/bioclim/future/bio11.grd",
                                   package="biomod2"),
                      system.file( "external/bioclim/future/bio12.grd",
                                   package="biomod2"))

myBiomomodProjFuture <- BIOMOD_Projection(
  modeling.output = myBiomodModelOut,
  new.env = myExplFuture,
  proj.name = 'future',
  selected.models = 'all',
  binary.meth = 'TSS',
  compress = 'xz',
  clamping.mask = T,
  output.format = '.grd')


# make some plots, sub-selected by str.grep argument
plot(myBiomomodProjFuture, str.grep = 'MARS')

#The last step of this vignette is to make Ensemble Forcasting, that means
#to project the meta-models you have created with BIOMOD_EnsembleModeling.
#BIOMOD_EnsembleForecasting required the output of BIOMOD_EnsembleModeling
#and BIOMOD_Projection. It will combine the projections made according
#to models ensemble rules defined at the ensemble modelling step.

myBiomodEF <- BIOMOD_EnsembleForecasting(
  projection.output = myBiomomodProj,
  EM.output = myBiomodEM,
  binary.meth = 'TSS')


#Nothing is returned but some additional files have been created in your
#projection folder (RasterStack or array depending on your projection type).
#This file contains your meta-models projections.
#
proj_current_GuloGulo_TotalConsensus_EMbyTSS <- stack("GuloGulo/proj_current/proj_current_GuloGulo_TotalConsensus_EMbyTSS.grd")
proj_current_GuloGulo_TotalConsensus_EMbyTSS

plot(proj_current_GuloGulo_TotalConsensus_EMbyTSS)