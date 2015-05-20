library(raster)

suit.img <- "C:/Share/LCC-VP/Parks/GRSM/analysis/spruce_fir/inputs/clim_srad/COVER_spruce_fir_inPRISM_800m.tif"
biovars <- "C:/Share/LCC-VP/Parks/GRSM/analysis/spruce_fir/inputs/clim_srad/biovars_1981-2010_srad_masked.tif"
rf <- load("C:/Share/LCC-VP/Parks/GRSM/analysis/spruce_fir/outputs/clim_srad/rf_model_Spruce_fir.RData")
p.names <- "C:/Share/LCC-VP/Parks/GRSM/analysis/common_inputs/GRSM_var_names_clim_srad.txt"

s.img <- getValues(raster(suit.img))
p.img <- as.data.frame(getValues(brick(biovars)))
names(p.img) <- pred.names

cor.preds <- as.data.frame(sapply(p.img, cor, s.img, use="complete.obs"))
names(cor.preds) <- "R - Spruce Fir"

write.csv(cor.preds, file="C:/Share/LCC-VP/Parks/GRSM/analysis/spruce_fir/outputs/clim_srad/Spruce-fir_corr_coeffs.csv")


pred.names <- read.table(p.names, sep="\n")[,1]

tree <- as.data.frame(getTree(randfor,k=500))
#str(tree)

names(tree)[3] <- "split_var"

count=1
for (i in tree$split_var) {
  #print(i)
  if (i != 0) {
    tree$split_var[count] <- as.character(pred.names[i])
  } else {
    tree$split_var[count] <- "NA - Terminal Node"
  }#end if
   count <- count + 1    
} #end for loop

write.csv(tree, file="C:/Share/LCC-VP/Parks/GRSM/analysis/spruce_fir/outputs/clim_srad/spruce-fir_rf_tree.csv")