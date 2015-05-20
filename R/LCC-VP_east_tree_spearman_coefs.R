# This script takes spearman correlation matrices of predictors x FIA presence/absence data
# for individual species, grabs the 1st line of the table from each and concatenates
# them into a new table so that each row represents a species and each column is the 
# spearman correlation coefficient between that variable and the species.
# Author: Patrick Jantz
# Date: 04/01/15

# Set input directory
setwd("C:/Share/LCC-VP/RangeWide/RFoutput/SpearmanVCorrs")

# Get pairwise correlations between each species' presence/absence and all predictor variables
speardf <- list()
for (i in list(12,16,97,110,111,121,123,126,129,131,132,261,315,316,317,318,319,332,371,403,407,408,409,412,531,541,602,611,621,746,762,802,824,832,833,835,837,951,971,972)) {  
  aa <- read.csv(paste0("s",i,"spear_corr.csv"))
  aa <- aa[1,]
  aa$X <- i
  speardf <- rbind(speardf,unlist(aa))
}

speardf <- data.frame(speardf)
speardf <- data.frame(sapply(speardf,unlist))

# Read in model objects, grab the predictors used and subset the dataframe by those predictors.
# Set input directory
setwd("C:/Share/LCC-VP/RangeWide/RFoutput")
for (i in list(12,16,97,110,111,121,123,126,129,131,132,261,315,316,317,318,319,332,371,403,407,408,409,412,531,541,602,611,621,746,762,802,824,832,833,835,837,951,971,972)) {  
  print(i)
  aa <- load(paste0(getwd(),"/s",i,"_pa_modbesttemp_rfobject.RData"))
  aa <- row.names(get(aa)$importance)
  bb <- speardf[speardf$X %in% i,names(speardf) %in% aa]
  write.csv(bb,paste0("C:/Share/LCC-VP/RangeWide/RFoutput/SpearmanVCorrs/s",i,"_spear_corr_best.csv"))
}

# Not really part of this script but useful for seeing how error rates decrease w/forest size in RF
# par(mfrow=c(4,10))
# # Plot error rates
# for (i in list(12,16,97,110,111,121,123,126,129,131,132,261,315,316,317,318,319,332,371,403,407,408,409,412,531,541,602,611,621,746,762,802,824,832,833,835,837,951,971,972)) {  
#   print(i)
#   aa <- load(paste0(getwd(),"/s",i,"_pa_modalltemp_rfobject.RData"))
#   plot(get(aa)$err.rate[,"OOB"])
# }

