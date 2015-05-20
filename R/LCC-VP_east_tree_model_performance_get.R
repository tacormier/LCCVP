
# Query RF model objects for out of bag error info, number of presences and species codes.
# Run accuracy statistics for a range of threshold criteria and write out everything
# to a csv file.

require(PresenceAbsence)

# Variable set
# "best" or "all
vset <- "all"

# Set directory
setwd("C:/Share/LCC-VP/RangeWide/RFoutput")

# Define output directory
odir <- "C:/Share/LCC-VP/RangeWide/AccyStats"

# Get species codes
scodes <- unlist(lapply(grep(paste0("mod",vset,"temp"),list.files(pattern="calcoefs"),value=T),
                        function(x) sub("s","",unlist(strsplit(x,"_"))[1])))

# Load model file
modstats <- function(x,y=vset) {
  
  # Reset variable names
  scodes <- x
  vset <- y
  
  if (scodes != '762')
    # example name: "s95_pa_modbesttemp_rfobject.RData"
    mname <- paste0(getwd(),"/","s",scodes,"_pa_mod",vset,"temp_rfobject.RData")
    
    aa <- load(mname[[1]])
    
    
    ot <- optimal.thresholds(data.frame(names(get(paste0('mod.',vset,'.temp'))$predicted),
                                        as.numeric(as.character(get(paste0('mod.',vset,'.temp'))$y)),
                                        get(paste0('mod.',vset,'.temp'))$votes[,2]),threshold=201,
                             opt.methods=c("Sens=Spec","MaxSens+Spec","MaxKappa","PredPrev=Obs"))
    
    paaTSSKappa <- presence.absence.accuracy(data.frame(names(get(paste0('mod.',vset,'.temp'))$predicted),
                                                        as.numeric(as.character(get(paste0('mod.',vset,'.temp'))$y)),
                                                        get(paste0('mod.',vset,'.temp'))$votes[,2]),
                                             threshold=c(ot[1,2],ot[2,2],ot[3,2],ot[4,2]))
    paaTSSKappa$model <- paste0('mod.',vset,'.temp')
    
      # Get number of presences
    pnum <- sum(as.numeric(as.character(get(paste0('mod.',vset,'.temp'))$y)))
    
    # Get mean OOB error rate
    oobrate <- mean(get(paste0('mod.',vset,'.temp'))$err.rate[,'OOB'])
    
    # Get slope and intercept from calibration plots
    calco <- read.csv(paste0("s",scodes,"_temp_pa_","mod",vset,"temp_calcoefs.csv"))
    alphas <- calco[1,2]
    betas <- calco[2,2]
    
    adf <- data.frame(Method=ot$Method,paaTSSKappa,alphas=alphas,betas=betas,presences=pnum,oobrate=oobrate,scode=scodes)
    
    write.csv(adf,file=paste0(odir,"/s",scodes,"_mod",vset,"temp.csv"))
  
    return(adf)
  
}

adf <- lapply(scodes, modstats)

sensspec <- do.call("rbind",lapply(adf, function(x) x[1,]))
maxsensspec <- do.call("rbind",lapply(adf, function(x) x[2,]))
maxkappa <- do.call("rbind",lapply(adf, function(x) x[3,]))
preprev <- do.call("rbind",lapply(adf, function(x) x[4,]))

write.csv(sensspec,file=paste0(odir,"/mod",vset,"temp_sensspec.csv"))
write.csv(maxsensspec,file=paste0(odir,"/mod",vset,"temp_maxsensspec.csv"))
write.csv(maxkappa,file=paste0(odir,"/mod",vset,"temp_maxkappa.csv"))
write.csv(preprev,file=paste0(odir,"/mod",vset,"temp_preprev.csv"))

# Read model performance tables
mb <- read.csv("C:/Share/LCC-VP/RangeWide/AccyStats/modbesttemp_sensspec.csv")
ma <- read.csv("C:/Share/LCC-VP/RangeWide/AccyStats/modalltemp_sensspec.csv")

# Compare models. Negative values mean the variable selection model performs better. Positive values mean
# the unselected variable model performs better
pccdiff <- (ma$PCC - mb$PCC)*100
sensdiff <- (ma$sensitivity - mb$sensitivity)*100
specdiff <- (ma$specificity - mb$specificity)*100
aucdiff <- (ma$AUC- mb$AUC)*100

# Alphas should be close to 0. Betas should be close to 1. Taking the absolute value and reversing the order
# makes it so negative values correspond to better performance with the variable selection model
alphadiff <- abs(mb$alphas) - abs(ma$alphas)
betadiff <- abs(1-ma$betas) - abs(1-mb$betas)

# Create and write matrix of differences between performance stats
diffmat <- data.frame(scodes,ma$presences,sensdiff,specdiff,pccdiff,aucdiff,alphadiff,betadiff)
write.csv(diffmat,"C:/Share/LCC-VP/RangeWide/AccyStats/diffmatrix.csv")

# PLOTTING
#Create a function to generate a continuous color palette
rbPal <- colorRampPalette(c('blue','red'))

# Full model plotting
cvec <- rbPal(10)[as.numeric(cut(abs(ma$betas),breaks = 10))]
plot(ma$sensitivity,ma$betas,xlab='Sensitivity',ylab='Slopes', ylim=c(0,1),xlim=c(0,1),
     pch=16,cex=1.5,col=cvec,main='Full Models')
abline(v=0.7,h=0.7)

# Parsimonious model plotting
cvec <- rbPal(10)[as.numeric(cut(abs(ma$betas),breaks = 10))]
plot(mb$sensitivity,mb$betas,xlab='Sensitivity',ylab='Slopes', ylim=c(0,1),xlim=c(0,1),
     pch=16,cex=1.5,col=cvec,main='Parsimonious Models')
abline(v=0.7,h=0.7)

# Effects of # of presences
plot(ma$presences,ma$sensitivity,xlab="# presences",ylab="Sensitivity",main="Full Models",pch=16,cex=1.25,col="blue")
plot(ma$presences,ma$specificity,xlab="# presences",ylab="Specificity",main="Full Models",pch=16,cex=1.25,col="blue")
plot(ma$presences,ma$AUC,xlab="# presences",ylab="AUC",main="Full Models",pch=16,cex=1.25,col="blue")
plot(ma$presences,abs(ma$alphas),xlab="# presences",ylab="Intercepts",main="Full Models",pch=16,cex=1.25,col="blue")
plot(ma$presences,abs(ma$betas),xlab="# presences",ylab="Slopes",main="Full Models",pch=16,cex=1.25,col="blue")

# Barplots
barplot(pccdiff,names=scodes,las=2,main='PCC Difference',space=1)
barplot(sensdiff,names=scodes,las=2,main='Sensitivity Difference',space=1)
barplot(specdiff,names=scodes,las=2,main='Specificity Difference',space=1)
barplot(aucdiff,names=scodes,las=2,main='AUC Difference',space=1)
barplot(alphadiff,names=scodes,las=2,main='Intercept Difference',space=1)
barplot(betadiff,names=scodes,las=2,main='Slope Difference',space=1)


# Mapping 
aa <- raster::getData('GADM', country='USA', level=1, path="C:/Share/LCC-VP/RangeWide/MapFiles/")


hlock <- raster("C:/Share/LCC-VP/RangeWide/RFoutput/s261_rcp85_pa_best_temp_2021.nc")

plot(hlock,useRaster=F)
plot(aa,add=T)
