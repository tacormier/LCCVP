#This function will take an existing raster extent
#and generate a new raster with each cell having a 
#unique value.
#
#Tina Cormier
#10/25/2012


uniqueRas <- function(existingRas, outdir) {
  library(raster)
  #Read in user-provided extent/resolution raster
  param.ras <- raster(existingRas)
  
  #create unique raster
  uni.ras <- raster(param.ras)
  
  #assign unique values
  uni.ras <- setValues(uni.ras, c(1:ncell(param.ras)))
  
  #write out new raster
  out.name <- paste(outdir, unlist(strsplit(basename(existingRas), "\\."))[1], "_unique.tif", sep="")
  writeRaster(uni.ras, out.name, format="GTiff", datatype="INT4U", overwrite=T)
}

#x <- uniqueRas(existingRas, uni.outdir)

#x <- raster(x)
#dataType(x)

###################################################################

#Test for multicolinearity in predictors
#The way this is used in the modeling scripts assumes
#That the layers are already open.

# Adapted from code Written by Jeffrey Evans

# PURPOSE: IDENTIFY MULTI-COLINEAR VARIABLES USING QR MATRIX DECOMPOSITION                   
#
# ARGUMENTS: 
#       X   A DATAFRAME 
#       p   MULTI-COLINEARITY THRESHOLD (DEFAULT 1e-07)
#
# VALUE:
#       TEST STATISTIC MESSAGE
#       CHARACTER VECTOR OF POTENTIAL MULTI-COLINEAR VARIABLES
#
# NOTES:
#       COLINEARITY THRESHOLDS MUST BE ADJUSTED BASED ON NUMBER OF 
#        X-VARIABLES. FOR SMALL NUMBER OF VARIABLES (<20) USE 1e-07 
#        FOR LARGE NUMBER (>20) USE 0.05 
#
# EXAMPLES: 
#   # DUMMY DATA
#   test = data.frame(v1=seq(0.1, 5, length=100), v2=seq(0.1, 5, length=100), 
#                     v3=dnorm(runif(100)), v4=dnorm(runif(100)) ) 
#
#   # TEST FOR MULTICOLINEAR VARABLE(s)
#   MultiColinear(test[,c(1,3)])
#   cl <- MultiColinear(test)
#
#   # PCA BIPLOT OF VARIABLES 
#    pca.test <- prcomp(test[,1:ncol(test)], scale=TRUE)
#    biplot(pca.test, arrow.len=0.1, xlabs=rep(".", length(pca.test$x[,1])))        
#
#   # REMOVE IDENTIFIED VARIABLE(S)
#   test <- test[, -which(names(test)==cl)]
#
# REFERENCES:
#  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) The New S Language. 
#     Wadsworth & Brooks/Cole. 
#
#  Dongarra, J. J., Bunch, J. R., Moler, C. B. and Stewart, G. W. (1978) 
#     LINPACK Users Guide. Philadelphia: SIAM Publications. 
#
##########################################################################
multiCo <- function(df, p){
  MultiColinear <- function(x, p=1e-07) {
  if (!inherits(x, "data.frame")) stop("X MUST BE A data.frame")
  if ( (dim(x)[2] < 2) == TRUE) stop("NOT ENOUGH VARIABLES TO TEST")
  xtest <- x
  x.names <- names(xtest)
  qrx <- qr(xtest, tol=p)
  if (length(names(xtest)[qrx$pivot[1:qrx$rank]]) != length(xtest) )  
  {  
    keep <- names(xtest)[qrx$pivot[1:qrx$rank]]
    warning("MULTI-COLINEAR VARIABLES: ", paste(setdiff(x.names, keep), collapse = ","))
    return(paste(setdiff(x.names, keep)))
    remove <- setdiff(x.names, keep)
    return(remove)
  } else { print(" NO MULTICOLINEAR VARIABLES IDENTIFIED")
  }
  } #end MultiColinear function
  
  #remove variables with dependencies
  cl <- MultiColinear(df, p)
  xdata <- df  
  for(l in cl) {
    cl.test <- xdata[,-which(names(xdata)==l)]
    print(paste("REMOVE VARIABLE", l, sep=": "))
    MultiColinear(cl.test, p=p)  
  }
  for(l in cl) { df <- df[,-which(names(df)==l)] }
  return(list(df, remove))
  
} #end multiCo function

######################### Random Forest ####################################
# Run Random Forest classification algorithm 
#
# Requires randomForest package.
#
# This function takes a dataframe of predictor variables (preds), a vector
# of responses corresponding to each record in the predictor df, and an output
# directory. 

# rf.mod will compute a random forest model, save it, and save files with 
# varimp plots and model results. 
#
# pred.stk = stack of predictors - data frame
# r.img = response image.  Must have same length as pred.stk
# outDir = output directory
# mod.name = descriptive name of model ES - i.e., "Spruce_Fir"
# mod.type = descriptive name of type of model - to append to output names i.e. ks could mean kitchen sink
rf.mod <- function(pred.stk, r.img, outDir, mod.name, mod.type) {
  print(paste("computing random forest model. . ."))
  randfor <- randomForest(pred.stk, r.img)
  
  # View classification results.
  randfor
  
  # View variable importance plot.
  #varImpPlot(randfor)
  
  #save rf model for later :)
  rf.model <- paste(outDir, mod.name, "_rf_model_", mod.type, ".RData", sep="")
  save(randfor, file= rf.model)
  
  ############### Model output summary files ############################
  # Plotting parameters
  str <- 1
  dpi <- 400
  
  # Variable Importance Plot.
  pdf(file=paste(outDir,mod.name, "_rf_varImp_", mod.type, ".pdf", sep=""),family="Times")
  varImpPlot(randfor, pch=16, col="blue",main=paste("Variable Importance - ", class.name, ", ", mod.type, sep=''))
  dev.off()
  
  # redirect output to file (for selected variables)
  sink(file=paste(outDir, mod.name, "_rf_model_results_", mod.type, ".txt",sep=""))
  print(paste("***********************************************************************", sep=""))
  print(paste("************** randomForest model results - ", mod.name," *************", sep=""))
  print(paste("***********************************************************************", sep=""))
  print(randfor)
  #turn off output to file
  sink()
  
  #return random forest model object
  return(randfor)
  
}
####################### Map of Model Predictions #########################
#
# This function takes an existing model and applies it to the entire area
# covered by the predictors. This function is sort of specific to the LCC-VP
# modeling script in terms of specific inputs related to removing NAs and
# creating a difference image. 
#
# This function requies the raster package.
#
# model = model object, i.e., random forest model
# pred.stk = data frame of predictors
# pred.mask = In LCC-VP, the pred.stack is masked based on locations of NA
#             values in the response vector - this is the mask object so the
#             script knows where to put NAs back in when writing the image (NAs 
#             not accepted by randomForest function).
# res.img = Path/Name of response image - here it is used as a template for writing out 
#           predictions. 
# outDir = output directory
# mod.name = descriptive name of model ES - i.e., "Spruce_Fir"
# mod.type = descriptive name of type of model - to append to output names i.e. ks could mean kitchen sink 

pred.map <- function(model, pred.stk, pred.mask, res.img, outDir, mod.name, mod.type) {
  # output vector set up: contains same length and position of NAs as the response 
  # image/vector - because intitially, it IS the response image. Will be output 
  # prediction vector that is written to a raster.
  r.out <- getValues(raster(res.img))
  r.diff <- getValues(raster(res.img))
  
  # Detach the raster package because we want to use the randomForest version of 
  # predict, instead of the "raster" version.
  if ("package:raster" %in% search()) {
    detach("package:raster")
  }  
  
  #prediction
  pred <- predict(model, pred.stk)
  r.out[is.na(pred.mask) == F] <- pred
  
  diff <- pred - r.img
  r.diff[is.na(pred.mask) == F] <- diff
  
  # Write the output raster maps.
  # Reload the raster package.
  require(raster)
  
  # Create the output suitability map.
  # Naming outputs
  outImage <- paste(outDir, mod.name, "_rf_current_suitability_", mod.type, ".tif", sep="")
  diffImage <- paste(outDir, mod.name, "_rf_diff_image_predMinusObs_", mod.type, ".tif", sep="")

  #Writing vectors to images
  print("Writing output rasters")  
  img.out <- raster(res.img)
  img.out <- setValues(img.out, round(r.out, digits=0))
  writeRaster(img.out, filename=outImage, format="GTiff", dataType="INT1U", overwrite=T)
  
  # Create diff raster.
  img.diff <- raster(res.img)
  img.diff <- setValues(img.diff, round(r.diff, digits=0))
  writeRaster(img.diff, filename=diffImage, format="GTiff", dataType="INT1S", overwrite=T)
}

########################## Correlation Matrix Plot #################################

# Create matrix plot of correlation matrix
# Circle size represents strength of correlation
# Circle color represents direction of correlation
# From http://gallery.r-enthusiasts.com/graph/Correlation_matrix_circles_152
#
# Change - for LCC-VP colors now default to blue and red, where darker blues
# represent strong positive correlation, and darker reds show strong negative 
# correlation.  The col argument is now ignored.  To change colors or revert
# back to original, see code block starting with the comment: "assign circles' 
# fill color"
#
# Also added a new argument called "rem.var," which is a vector of variable names
# that have been removed due to multicollinearity. This will allow us to quickly
# visualize and interpret interchangeable variables, as it may be arbitrary which
# one(s) are kept/removed if they are collinear.
#
circle.corr <- function(corr, rem.var, col=c("black","white"), bg = "white",
                        cex = 1, order = FALSE, title = "", ...){
  
  if (is.null(corr))
    return(invisible())
  if ((!is.matrix(corr)) || (round(min(corr, na.rm = TRUE),
                                   6) < -1) || (round(max(corr, na.rm = TRUE), 6) > 1))
    stop("Need a correlation matrix!")
  n <- nrow(corr)
  m <- ncol(corr)
  
  ## reorder the variables using principal component analysis
  if (order) {
    if(!n==m){
      stop("The matrix must be squre if order is TRUE!")
    }
    x.eigen <- eigen(corr)$vectors[, 1:2]
    e1 <- x.eigen[, 1]
    e2 <- x.eigen[, 2]
    alpha <- ifelse(e1 > 0, atan(e2/e1), atan(e2/e1) + pi)
    corr <- corr[order(alpha), order(alpha)]
  }
  
  ## set up variable names
  rname <- rownames(corr)
  cname <- colnames(corr)
  if (is.null(rname))
    rname <- 1:n
  if (is.null(cname))
    cname <- 1:m
  rname <- as.character(rname)
  cname <- as.character(cname)
  
  ## calculate label-text width approximately
  par(mar = c(0, 0, 2, 0), bg = "white")
  plot.new()
  plot.window(c(0, m), c(0, n), asp = 1)
  xlabwidth <- max(strwidth(rname, cex = cex))
  ylabwidth <- max(strwidth(cname, cex = cex))
  
  ## set up an empty plot with the appropriate dimensions
  plot.window(c(-xlabwidth + 0.5, m + 0.5), c(0, n + 1 + ylabwidth),
              asp = 1, xlab="", ylab="")
  rect(0.5, 0.5, m + 0.5, n + 0.5, col = bg)  ##background color
  
  ## add variable names and title
  # First identify which variables have been removed due to multicollinearity.
  # and assign them a different color. Variables still IN are black.
  text.col <- rep("black", length(rname))
  text.col[rname %in% rem.var] <- "darkseagreen3"
  
  
  text(rep(-xlabwidth/2, n), n:1, rname, col = text.col, cex = 0.70)
  text(1:m, rep(n + 1 + ylabwidth/2, m), cname, srt = 90, col = text.col,
       cex = 0.70)
  title(title)
  
  ## add grid
  segments(rep(0.5, n + 1), 0.5 + 0:n, rep(m + 0.5, n + 1),
           0.5 + 0:n, col = "white")
  segments(0.5 + 0:m, rep(0.5, m + 1), 0.5 + 0:m, rep(n + 0.5,
                                                      m), col = "white")
  
  ## assign circles' fill color
  nc <- length(col)
  if(nc==1)
    bg <- rep(col, n*m)
  else{
    #Create groups for binning corrs.
    ff <- seq(-1, 1, by=0.2)
    
    #Put ff into groups/ranks - to apply to corr.
    ff.rank <- cut(ff, ff, labels=F)
    ff.rank <- na.omit(ff.rank)

    #colors
    reds <- rev(brewer.pal(5, "Reds"))
    blues <- brewer.pal(5, "Blues")
    cols <- c(reds, blues)
    #color lookup table
    cols.lut <- as.data.frame(cbind(ff.rank, cols))
    
    bg2 = rep(0, n * m)
    #assign rank to correlation values - as defined in ff.rank.
    corr.rank <- as.data.frame(cut(corr, ff, labels=F))
    names(corr.rank) <- "rank"
      
    # Match correlation rank with color using lookup table.
    corr.rank$Col <- cols.lut$cols[match(corr.rank$rank, cols.lut$ff.rank)]

    bg <- as.vector(corr.rank$Col)
  }
  
  ## plot n*m circles using vector language, suggested by Yihui Xie
  ## the area of circles denotes the absolute value of coefficient
  symbols(rep(1:m, each = n), rep(n:1, m), add = TRUE, inches = F,
          circles = as.vector(sqrt(abs(corr))/2), bg = as.vector(bg))
  
  # Create binary vector. 1 where corr stat is greater than threshold, 0 where not
  zz <- as.vector(ifelse(corr >= 0.75 | corr <= -0.75, 1, 0))
  
  #points(rep(1:m, each = n)[zz==1], rep(n:1, m)[zz==1], pch=16, col="yellow")
}


############################## Kappa Stats ############################################
  #R function to compute na\"{\i}ve statistics, overall and per-class kappa
  # for a square confusion matrix CM and a weights matrix W
  #  W has 1 on diagonals, [0..1) off; default is no partial credit
  #
  #Results are returned in a list, which can be printed with summary.kw();
  #  this takes an optional prob. of Type I error (alpha, default 0.05)
  #
  #Error checks: must be square, weights & confusion same size
  #
  #You can calculate other statistics from these, e.g. for the average
  # weighted user's and producer's accuracy something like:
  #
  #  z<-kw(x); sum(z$user.naive)/nrow(x); sum(z$prod.naive)/ncol(x)
  #
  #Author: D G Rossiter, 29-August-2001 Enschede
  #           revised 22-April-2004 Ithaca
  #
kw <- function(CM, W = diag(sqrt(length(as.matrix(CM)))) ) {
  cmx<-as.matrix(CM); wx<-as.matrix(W)
  #try to convert a vector to a square matrix
  if (ncol(cmx) == 1)
    cmx<-matrix(cmx, byrow=TRUE, nrow=sqrt(nrow(cmx)))
  if (ncol(wx) == 1)
    wx<-matrix(wx, byrow=TRUE, nrow=sqrt(nrow(wx)))
  nr<-nrow(cmx); nc<-ncol(cmx)
  if (nr != nc) { print("Error: confusion matrix is not square"); break }
  if (dim(wx) != dim(cmx))
  { print("Weight and Confusion Matrices are not the same size"); break }
  #summarize cmx
  n<-sum(cmx); rs<-apply(cmx,1,sum); cs<-apply(cmx,2,sum)
  # confusion matrix and marginals as proportions
  p <- cmx/n; cp <- cs/n; rp <- rs/n;
  if ( round((sum(rp) + sum(cp))/2, 2) != 1)
  { print("Error: Bad checksum in row proportions"); break }
  # expected proportions
  pp<- rp %o% cp
  # weighted weights
  wr <- wx%*%cp; wc <- t(t(wx)%*%rp);
  # marginal accuracy
  # rows = user's
  ua <- apply(wx*p,1,sum)/rp; uasd<-sqrt(ua*(1-ua)/rs);
  # columns = producer's
  pa <- apply(wx*p,2,sum)/cp; pasd<-sqrt(pa*(1-pa)/cs);
  thw1 <- sum(sum(p * wx)); thw1v<-((thw1*(1-thw1))/n)
  thw2 <- sum(sum(pp * wx));
  khw <- (thw1-thw2)/(1-thw2);
  thw1c <- 1 - thw1; thw2c <- 1 - thw2;
  thw4 <- 0; for (i in 1:nr) for (j in 1:nc)
    thw4 <- thw4 + (p[i,j]*((wx[i,j]*thw2c - (wr[i]+wc[j]) * thw1c)^2 ))
  khwv <- (thw4 - (thw1*thw2 - 2*thw2 + thw1)^2) / (n * thw2c^4)
  list(
    sum.n=n,
    sum.kappa=khw, sum.kvar=khwv, theta=c(thw1,thw2,thw4),
    sum.naive=thw1, sum.var=thw1v,
    user.wa=ua, prod.wa=pa,
    user.wsd=uasd, prod.wsd=pasd,
    weights.row=wr, weights.col=wc, expected=pp)
}

summary.kw <- function(kw, alpha=0.05) {
  ciw<-function(var, n) {
    qnorm(1-(alpha/2))*sqrt(var) + (1/(2*n))
  }
  print(paste("Number of observations:", kw$sum.n), quote=F)
  print(paste("Sum of weighted sum of row, column weights:",
              round(sum(kw$weights.row), 2), ",", 
              round(sum(kw$weights.col), 2) ), quote=F)
  print("Summary of weighted naive statistics", quote=F)
  print(paste(
    "Overall accuracy, stdev, CV%:",
    round(kw$sum.naive, 4), ",", round(sqrt(kw$sum.var), 4), ",", 
    round((sqrt(kw$sum.var)/kw$sum.naive)*1000,0)/10),
        quote=F)
  w<-ciw(kw$sum.var, kw$sum.n)
  print(paste(
    round((1-alpha)*100,0),"% confidence limits for accuracy:",
    round((kw$sum.naive-w),4), "...",
    round((kw$sum.naive+w),4), sep=""), quote=F)
  print("User's weighted accuracy", quote=F)
  print(round(kw$user.wa,4));
  print("Producer's weighted reliability:", quote=F)
  print(round(kw$prod.wa,4));
  print("Summary of weighted kappa statistics", quote=F)
  print(paste("Overall weighted kappa, stdev, & CV%:",
              round(kw$sum.kappa,4), ",", 
              round(sqrt(kw$sum.kvar),4), ",",
              round((sqrt(kw$sum.kvar)/kw$sum.kappa)*1000,0)/10), quote=F)
  w<-ciw(kw$sum.kvar, kw$sum.n)
  print(paste(
    round((1-alpha)*100,0),"% confidence limits for weighted kappa:",
    round((kw$sum.kappa-w),4), "...",
    round((kw$sum.kappa+w),4), sep=""), quote=F)
}


############################## Multiply Image by 100 ############################################
# Purpose: This function multiplies an input image file by 100 in order to maintain precision but avoid
# working with decimals.
#
# Currently written to accept a file path to an image, rather than a raster, brick, or stack object.
#
# Requires the raster package
#

mult100 <- function(imgpath, outdir) {
  img <- stack(imgpath)
  img100 <- round(img*100,2)
  outimg <- paste0(outdir, unlist(strsplit(basename(imgpath), "\\."))[1], "_x100.tif")
  writeRaster(img100, outimg, overwrite=T)
  
}#end function

########################## Stack Monthly Images into Annual Stack ###############################
# Purpose: This function takes a directory of monthly images (12 images per year) and stacks them into
# annual stacks.
#
# Requires the raster package
#
# Currently accepts a directory of monthly images - number must be a multiple of 12 (no partial years)
# imgext is the image extention, which will help when listing rasters. Example: imgext <- "tif"
# refimg is an image to use for comparison of extent, NA pix locations, snapping etc. Will help us
# determine if there is an issue in processing. Usually use the current biovars layer as reference.
# refimg is just the path to the reference image. 

stackMonthToAnnual <- function(imgdir, outdir, imgext, refimg) {
  pattern <- paste0("*\\.", imgext, "$")
  imgs <- list.files(imgdir, pattern=pattern, full.names=T)
  l <- length(imgs)
  seq.yrly <- seq(from=1, to=l, by=12)
  
  #grab the 12 monthly images for each year and stack
  for (i in seq.yrly) {
    #for this application (stacking up PRISM monthly grids), just removing the two-digit month works for naming
    #the output file.
    filename <- sub(pattern="_[0-9][0-9]_", replacement="_", basename(imgs[i]))
    outimg <- paste0(outdir, filename)
    
    imgs.mon <- imgs[i:(i+11)]
    #print(imgs.mon)
    
    #stack and write out raster
    yrly.img <- stack(imgs.mon)
    
    #check to make sure we are ok in terms of NA locations by comparing to refimg
    #check to make sure that NA values in stacked file are in same locations as the ones in the regimg
    #This will help to avoid and alert us of missing or wrong data.
    r.img <- as.data.frame(raster(refimg))
    
    for (j in c(1:12)) {
      s.name <- yrly.img[[j]]
      s.name <- s.name@data@names
      s.img <- as.data.frame(yrly.img[[j]])
    
      r.na <- as.vector(is.na(r.img))
      s.na <- as.vector(is.na(s.img))
    
      if (!identical(r.na, s.na)) {
        stop(paste0("NA values in stacked and ref files are not identical in ", s.name, ". CHECK."))
      } else {
          print(paste0("NA check passed for ", s.name))
        }#end NA check for band j
    }#end NA loop

    #if all checks out, write 12-band raster!
    writeRaster(yrly.img, outimg, overwrite=T)
  }#end seq.yrly for
} #end function

########################## calculate mean monthly values for 30 year blocks ###############################
# calculate mean monthly values for 30 year blocks 
#CHANGE TO ONE FUNCTION AND HAVE AN ARGUMENT FOR MONTH #....THEN LOOP
#THROUGH A LIST OF MONTHS?
jan <- function(x) {round(mean(stack(x[c(seq(1,360,12))])))}
feb <- function(x) {round(mean(stack(x[c(seq(2,360,12))])))}
mar <- function(x) {round(mean(stack(x[c(seq(3,360,12))])))}
apr <- function(x) {round(mean(stack(x[c(seq(4,360,12))])))}
may <- function(x) {round(mean(stack(x[c(seq(5,360,12))])))}
jun <- function(x) {round(mean(stack(x[c(seq(6,360,12))])))}
jul <- function(x) {round(mean(stack(x[c(seq(7,360,12))])))}
aug <- function(x) {round(mean(stack(x[c(seq(8,360,12))])))}
sep <- function(x) {round(mean(stack(x[c(seq(9,360,12))])))}
oct <- function(x) {round(mean(stack(x[c(seq(10,360,12))])))}
nov <- function(x) {round(mean(stack(x[c(seq(11,360,12))])))}
dec <- function(x) {round(mean(stack(x[c(seq(12,360,12))])))}

########################## Calculates 30-yr monthly averages from annual data. ###############################

#requires raster package
#Calculates 30-yr averages from annual data.
#img.list is a monthly of images for the whole time span - can be from diff directories, but all must be
#same proj, resolution, extent.
calc30yr <- function(img.list, outdir) {
    y <- length(img.list)
    b <- y*12
    
    print(paste0("opening ", y, "-yr stack = ", b, " bands"))
  
  # Create stacks containing all years of data.
  #print(paste("opening 95-yr stack = 1140 bands"))
  stk <- unstack(stack(img.list))
  #stk <- img.stk
  # determine 30-year groupings
  grps <- list()
  bands <- list()
  
  #iterators
  j <- 30
  k <- 1    
  for (m in c(1:(length(img.list)-29))) {
    grps[[m]] <- c(m:j)
    
    if (m == 1) {
      bands[[m]] <- c(m:360)
      
    } else {
      bands[[m]] <- c((k):(k+359))
    }
    #advance iterators
    j <- j+1
    k <- k+12
  }#end m loop
  
  #Loop through to name output 30-yr images
  for (i in c(1:length(grps))) {
    #ppt image lists and output name
    #figure out outfile name
    imgs <- img.list[grps[[i]]]
    #For restricted PRISM, names use tmax. For forecasts, uses tasmax. Want them all the same
    #for outputs. Same with tmin and ppt. CAUTION: As this is currently written, will change tmax, tmin,
    #and ppt in the paths as well, which won't exist. Doesn't matter so far, but if there are future problems,
    #check here.
    imgs <- gsub(pattern="tmax", replacement="tasmax", x=imgs)
    imgs <- gsub(pattern="tmin", replacement="tasmin", x=imgs)
    imgs <- gsub(pattern="ppt", replacement="pr", x=imgs)
    #mid <- as.numeric(unlist(strsplit(basename(imgs[1]), "_"))[4]) + 15
    mid.search <- regexpr(pattern="*_[0-9]{4}_*", text=as.vector(imgs[1]),)
    mid <- as.numeric(gsub("_","",regmatches(imgs[1], mid.search)))+15
    span <- paste(mid-15,"-", mid+14, sep="") 
    out.img <- paste(outdir, unlist(strsplit(basename(imgs[1]),"_"))[2], "_",mid, "_yearRange_", span, ".tif", sep="")
    
    #stack 30 year block.
    #quick bc it's a list :)
    print(paste("calculating 30-year averages for ", basename(out.img), sep=""))
    b <- stk[c(as.vector(bands[[i]]))]
    i.30 <- brick(jan(b), feb(b), mar(b), apr(b), may(b), jun(b), jul(b), aug(b), sep(b), oct(b), nov(b), dec(b)) 
    writeRaster(i.30, file=out.img, format="GTiff", dataType="INT4S", overwrite=T)
    
  }#end i loop.
}#end calc30yr function


########################## Rasterize a polygon using Majority Area ###############################


# # Per pixel, identify ID covering largest area
# rasterizeMajority <- function(i) {
#   
#   r_dupl <- r
#   r_dupl[i] <- 1
#   p <- rasterToPolygons(r_dupl) # Current cell -> polygon
#   sp_df_crp <- crop(sp_df, p)   # Crop initial polygons by current cell extent
#   
#   # Case 1: no polygon intersecting current cell
#   if (is.null(sp_df_crp)) {
#     return(NA)
#     # Case 2: one polygon intersecting current cell  
#   } else if (nrow(sp_df_crp@data) < 2)  {
#     return(rownames(sp_df_crp@data)) 
#     # Case 3: multiple polygons intersecting current cell
#   } else {
#     areas <- gArea(sp_df_crp, byid = TRUE)
#     index <- which.max(areas)
#     return(rownames(sp_df_crp@data)[index])
#   }
# })


########################## Matrix Plot with Corr Coefficients, bivariate plots, and histograms ########################## 
## put histograms on the diagonal
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="lavender", ...)
}
## put correlations & 95% CIs on the upper panels,
panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y,use="complete.obs")
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  prefix <- "r = "
  rc <- cor.test(x,y)
  rci <- rc$conf.int
  txt2 <- format(c(rci, 0.123456789), digits=digits)[1]
  txt3 <- format(c(rci, 0.123456789), digits=digits)[2]
  prefix2 <- "\nCI = "
  txt <- paste(prefix, txt, prefix2, txt2, ", ", txt3, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = .8)
}


########################## Stratified Random Sample ##########################   
  
# Function from https://gist.github.com/mrdwab/984691
stratified = function(df, group, size) {
  #  USE: * Specify your data frame and grouping variable (as column 
  #         number) as the first two arguments.
  #       * Decide on your sample size. For a sample proportional to the
  #         population, enter "size" as a decimal. For an equal number 
  #         of samples from each group, enter "size" as a whole number.
  #
  #  Example 1: Sample 10% of each group from a data frame named "z",
  #             where the grouping variable is the fourth variable, use:
  # 
  #                 > stratified(z, 4, .1)
  #
  #  Example 2: Sample 5 observations from each group from a data frame
  #             named "z"; grouping variable is the third variable:
  #
  #                 > stratified(z, 3, 5)
  #
  require(sampling)
  temp = df[order(df[group]),]
  if (size < 1) {
    size = ceiling(table(temp[group]) * size)
  } else if (size >= 1) {
    size = rep(size, times=length(table(temp[group])))
  }  
  strat = strata(temp, stratanames = names(temp[group]), 
                 size = size, method = "srswor")
  (dsample = getdata(temp, strat))
}#end stratified function  
  
  
########################## GGPLOT MULTIPLOT ##########################     
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}  
