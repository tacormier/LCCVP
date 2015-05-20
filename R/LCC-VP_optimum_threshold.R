library(raster)
library(biomod2)

#Ref image used in modeling
ref.img.file <- "C:/Share/LCC-VP/parks/GRSM/analysis/Spruce_fir/inputs/clim_srad/COVER_Spruce_fir_inPRISM_800m.tif"

#Modeled output image
mod.img.file <- "C:/Share/LCC-VP/Parks/GRSM/analysis/Spruce_fir/outputs/clim_srad/Spruce_fir_current_suitability_rf.tif"

############################################

ref.img <- getValues(raster(ref.img.file))
mod.img <- getValues(raster(mod.img.file))

#
identical(is.na(ref.img), is.na(mod.img))
ref.img <- na.omit(ref.img)
mod.img <- na.omit(mod.img)

opt <- Find.Optim.Stat (Stat="ROC", mod.img, ref.img, Precision=5)

