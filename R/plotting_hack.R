library(raster)

modimg <- "C:/Share/LCC-VP/EcoRegions/Bailey/analysis/Cove_Forest_GAP_GRSM/outputs/allVars_BiovarsGddVpdSoilsTWIsrad/Cove_Forest_GAP_GRSM_inPRISM_800m_masked.tifcurrent_suitability_rf.tif"
refimg <- "C:/Share/LCC-VP/EcoRegions/Bailey/analysis/Cove_Forest_GAP_GRSM/inputs/allVars_BiovarsGddVpdSoilsTWIsrad/COVER_Cove_Forest_GAP_GRSM_inPRISM_800m_masked.tif"

mod <- as.data.frame(raster(modimg))
mod <- as.vector(mod[,1])
ref <- as.data.frame(raster(refimg))
ref <- as.vector(ref[,1])
fit <- lm(mod ~ ref)
fitsum <- summary(fit)
r2 <- fitsum$adj.r.squared
r2.label <- bquote(italic(R)^2 == .(format(r2, digits = 3)))


main <- "Cove Forest Level 1 Hierarchical Model"
xlab <- "% Cover"
ylab <- "Modeled % Cover"
plot(ref, mod, pch=20, main=main, xlab=xlab, ylab=ylab, xlim=c(0,100), 
     ylim=c(0,100), cex=.85, cex.axis=0.85, cex.main=.95)
abline(fit, col="blue")
legend("topleft", legend = r2.label,bty = "n", text.col = "gray16", cex = 0.85)
