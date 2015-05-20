require(OSfun)

# load general parameters
load("data/param.Rdata")



# load soil data tables
load(param$data$soilTables)



# map unit table
soilmuTable <- soilTables$mapunit["mukey"]


# available water storage
#########################
muaggattNames <- unlist(param$soil$muaggatt)
muaggatt <- soilTables$muaggatt[c("mukey", muaggattNames)]
names(muaggatt)[-1] <- names(muaggattNames)
soilmuTable <- merge(soilmuTable, muaggatt)


# pH
####
# simplify
chorizon <- soilTables$chorizon[c("cokey", "hzdept_r", "hzdepb_r", "ph1to1h2o_r")]
# factorize
chorizon$cokey <- factor(chorizon$cokey)

# maximum depth to aggregate over
maxDepths <- unlist(param$soil$pH)
# depth-weighted mean pH
pH <- data.frame(cokey = levels(chorizon$cokey), sapply(maxDepths, function(maxDepth) {
	# screen by depth and valid pH data
	chorizon <- subset(chorizon, hzdept_r < maxDepth & !is.na(ph1to1h2o_r))
	# reset bottom depth
	chorizon$hzdepb_r[chorizon$hzdepb_r > maxDepth] <- maxDepth
	# horizon height
	chorizon$height <- chorizon$hzdept_r - chorizon$hzdepb_r
	# operate by cokey
	tapply(chorizon$height * chorizon$ph1to1h2o_r, chorizon$cokey, sum) / tapply(chorizon$height, chorizon$cokey, sum)
}))

# component percentage
pH <- merge(soilTables$component[c("cokey", "mukey", "comppct_r")], pH, all.y = TRUE)
# factorize
pH$cokey <- factor(pH$cokey)
pH$mukey <- factor(pH$mukey)
# area-weighted mean
pH <- data.frame(mukey = levels(pH$mukey), sapply(pH[names(maxDepths)], function(ph) {
	# remerge
	pH <- data.frame(pH[c("mukey", "comppct_r")], ph = ph)
	# screen by valid pH data
	pH <- subset(pH, !is.na(ph))
	# operate by mukey
	tapply(pH$comppct_r * pH$ph, pH$mukey, sum) / tapply(pH$comppct_r, pH$mukey, sum)
}))

soilmuTable <- merge(soilmuTable, pH, all.x = TRUE)


# export
########
save(soilmuTable, file = param$data$soilmuTable)
