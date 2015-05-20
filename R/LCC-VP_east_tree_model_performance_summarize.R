
# Get model performance info and summarize
# calibration plot slope and intercept coefficients
require(PresenceAbsence)

# Variable set
# "best" or "all
vset <- "best"

# Set directory
setwd("C:/Share/LCC-VP/RangeWide/RFoutput")
#setwd("C:/Share/LCC-VP/RangeWide/AccyStats")

# Get species codes
scodes <- unlist(lapply(grep(paste0("mod",vset,"temp"),list.files(pattern="rfobject"),value=T),
                        function(x) sub("s","",unlist(strsplit(x,"_"))[1])))
rinds <- !scodes %in% c("95")
scodes <- scodes[rinds]
#rinds <- !scodes %in% c("762")
#scodes <- scodes[!scodes %in% c("95","762")]
#scodes <- scodes[!scodes %in% c("762")]

# Set directory
setwd("C:/Share/LCC-VP/RangeWide/RFoutput")
# Get calibration coefficient data
calco <- lapply(grep(paste0("mod",vset,"temp"),list.files(pattern="calcoefs"),value=T), read.csv)
calco <- calco[rinds]
alphas <- unlist(lapply(calco, function(x) x[1,2]))
betas <- unlist(lapply(calco, function(x) x[2,2]))

# Get number of presences in full set
setwd("C:/Share/LCC-VP/RangeWide/RFoutput")
fullpres <- c()
for (i in scodes) {
  print(i)
  mname <- paste0(getwd(),"/","s",i,"_pa_mod",vset,"temp_rfobject.RData")
  aa <- load(mname)
  fullpres <- c(fullpres,sum(as.numeric(as.character(get(aa)$y))))
}

#-------------
# This comes from the 80/20 training/test data
# AUC
# sensitivity
# specificity
setwd("C:/Share/LCC-VP/Rangewide/RFoutput8020/binary_rasters")

# Get AUC, sensitivity and specificity
accy <- lapply(paste0("s",scodes,"_EAST_pa_",vset,"_temp_accy.csv"), read.csv)
aucs <- unlist(lapply(accy, function(x) x$AUC))
kappa <- unlist(lapply(accy, function(x) x$Kappa))
pcc <- unlist(lapply(accy, function(x) x$PCC))
sens <- unlist(lapply(accy, function(x) x$sensitivity))
spec <- unlist(lapply(accy, function(x) x$specificity))
thold <- unlist(lapply(accy, function(x) x$threshold))

# Get number of presences in 80/20 set
trainpres <- c()
for (i in scodes) {
  print(i)
  mname <- paste0("C:/Share/LCC-VP/Rangewide/RFoutput8020","/","s",i,"_pa_mod",vset,"temp_rfobject.RData")
  aa <- load(mname)
  trainpres <- c(trainpres,sum(as.numeric(as.character(get(aa)$y))))
}

testpres <- fullpres-trainpres

# # Get AUC, sensitivity and specificity
# accy <- lapply(grep(paste0("mod",vset,"temp"),list.files(pattern="accy"),value=T), read.csv)
# aucs <- unlist(lapply(accy, function(x) x$AUC[1]))
# minsens <- unlist(lapply(accy, function(x) min(x$sensitivity)))
# maxsens <- unlist(lapply(accy, function(x) max(x$sensitivity)))
# minspec <- unlist(lapply(accy, function(x) min(x$specificity)))
# maxspec <- unlist(lapply(accy, function(x) max(x$specificity)))
# 
# # Get thresholds
# tholds <- lapply(grep(paste0("mod",vset,"temp"),list.files(pattern="accy"),value=T), read.csv)
# minthold <- unlist(lapply(accy, function(x) x$threshold[1]))
# maxthold <- unlist(lapply(accy, function(x) x$threshold[2]))

# Make a data.frame
adf <- data.frame(Code=scodes,fullpres=fullpres,trainpres=trainpres,testpres=testpres,
                  alpha=alphas,beta=betas,auc=aucs,thold=thold,
                  sens=sens,spec=spec)

# # Make a data.frame
# adf <- data.frame(Code=scodes,alpha=alphas,beta=betas,auc=aucs,minth=minthold,maxth=maxthold,
#                   minsens=minsens,maxsens=maxsens,minspec=minspec,maxspec=maxspec)

# Write to file
write.csv(adf,paste0("C:/Share/LCC-VP/Rangewide/AccyStats/mod",vset,"tempv2.csv"))


# Load model performance tables and compare
# Read model performance tables
mb <- read.csv("C:/Share/LCC-VP/RangeWide/AccyStats/modbesttempv2.csv")
ma <- read.csv("C:/Share/LCC-VP/RangeWide/AccyStats/modalltempv2.csv")

# Compare models. Negative values mean the variable selection model performs better. Positive values mean
# the unselected variable model performs better
#pccdiff <- (ma$PCC - mb$PCC)*100
sensdiff <- (ma$sens - mb$sens)
specdiff <- (ma$spec - mb$spec)
aucdiff <- (ma$auc- mb$auc)

# Alphas should be close to 0. Betas should be close to 1. Taking the absolute value and reversing the order
# makes it so negative values correspond to better performance with the variable selection model
alphadiff <- abs(mb$alpha) - abs(ma$alpha)
betadiff <- abs(1-ma$beta) - abs(1-mb$beta)

# Create and write matrix of differences between performance stats
diffmat <- data.frame(scodes,ma$fullpres,ma$trainpres,ma$testpres,aucdiff,sensdiff,specdiff,alphadiff,betadiff)
write.csv(diffmat,"C:/Share/LCC-VP/RangeWide/AccyStats/diffmatrixv2.csv")


#--------------------------------------------------------------------
# SPEARMAN COEFFICIENTS
# Get most important variable for each species for the best performing model in each case
spear <- c()
for (i in scodes) {
  print(i)
  if (!i %in% c("121","123","16","315","319","332","403","407","412")) {
    vset <- "best"
    mname <- paste0("C:/Share/LCC-VP/Rangewide/RFoutput/SpearmanVCorrs/","s",i,"_spear_corr_",vset,".csv")
  } else {
    vset <- "all"
    mname <- paste0("C:/Share/LCC-VP/Rangewide/RFoutput/SpearmanVCorrs/","s",i,"_spear_corr_",vset,".csv")
  }
  aa <- read.csv(mname)
  aa$X <- NULL
  aa <- aa[which.max(abs(aa))]
  if (aa > 0) {
    spear <- c(spear,paste0(names(aa),"(+)"))
  } else {
    spear <- c(spear,paste0(names(aa),"(-)"))
  }
}

write.csv(data.frame(scode=scodes,firstvar=spear),"C:/Share/LCC-VP/RangeWide/RFoutput/SpearmanVCorrs/first_var.csv")

# Get variable with highest importance value
setwd("C:/Share/LCC-VP/RangeWide/RFoutput")
varimp <- c()
for (i in scodes) {
  print(i)
  if (!i %in% c("121","123","16","315","319","332","403","407","412")) {
    vset <- "best"
    mname <- paste0(getwd(),"/","s",i,"_pa_mod",vset,"temp_rfobject.RData")
  } else {
    vset <- "all"
    mname <- paste0(getwd(),"/","s",i,"_pa_mod",vset,"temp_rfobject.RData")
  }
  aa <- load(mname)
  varimp <- c(varimp,row.names(get(aa)$importance)[which.max(get(aa)$importance)])
}

write.csv(data.frame(scode=scodes,varimp=varimp),"C:/Share/LCC-VP/RangeWide/RFoutput/RFVarimp/rf_varimp_full.csv")

# Get sign of most important variable (from RF models)
aa <- read.csv("C:/Share/LCC-VP/RangeWide/RFoutput/RFVarimp/rf_varimp_full.csv")
varimp <- aa$varimp
vsign <- list()
for (j in c(1:length(scodes))) {
  i <- scodes[j]
  print(i)
  if (!i %in% c("121","123","16","315","319","332","403","407","412")) {
    vset <- "best"
    mname <- paste0("C:/Share/LCC-VP/Rangewide/RFoutput/SpearmanVCorrs/","s",i,"_spear_corr_",vset,".csv")
  } else {
    vset <- "all"
    mname <- paste0("C:/Share/LCC-VP/Rangewide/RFoutput/SpearmanVCorrs/","s",i,"_spear_corr_",vset,".csv")
  }
  aa <- read.csv(mname)
  aa$X <- NULL
  tvar <- varimp[j]
  if (aa[names(aa) %in% tvar] > 0) {
    vsign <- c(vsign,paste(tvar,"(+)"))
  } else {
    vsign <- c(vsign,paste(tvar,"(-)"))
  }
}
vsign <- unlist(vsign)
write.csv(data.frame(scode=scodes,vsign=vsign),"C:/Share/LCC-VP/RangeWide/RFoutput/RFVarimp/rf_varimp_full_sign.csv")
