
# This script checks whether files exist for species model fits and projections
# Species codes
aa <- c(12,16,97,110,111,121,123,126,129,131,132,261,315,316,317,318,319,332,371,403,407,408,409,412,531,541,602,611,621,746,762,802,824,832,833,835,837,951,971,972)
aa <- paste0('s',aa)

# Lists of files to check for
l1 <- paste0(aa,'_pa_best_temp_19812010.nc')
l2 <- paste0(aa,'_rcp45_pa_best_temp_2021.nc')
l3 <- paste0(aa,'_rcp45_pa_best_temp_2030.nc')
l4 <- paste0(aa,'_rcp45_pa_best_temp_2040.nc')
l5 <- paste0(aa,'_rcp45_pa_best_temp_2050.nc')
l6 <- paste0(aa,'_rcp45_pa_best_temp_2055.nc')

for (i in c(1:length(l1))) {
  if (file.exists(l1[i]) & file.exists(l2[i])) {
    #print(paste0(aa[i],' all good'))
    x <- 'lame'
  } else {
    print(paste0(aa[i],' not all good'))
    print(l1[i])
    print(l2[i])
  }
}

for (i in c(1:length(l1))) {
  if (file.exists(l1[i])) {
    #print(paste0(aa[i],' all good'))
    x <- 'lame'
  } else {
    print(paste0(aa[i],' not all good'))
    print(l1[i])
  }
}

for (i in c(1:length(l2))) {
  if (file.exists(l2[i])) {
    #print(paste0(aa[i],' all good'))
    x <- 'lame'
  } else {
    print(paste0(aa[i],' not all good'))
    print(l2[i])
  }
}

for (i in c(1:length(l3))) {
  if (file.exists(l3[i])) {
    #print(paste0(aa[i],' all good'))
    x <- 'lame'
  } else {
    print(paste0(aa[i],' not all good'))
    print(l3[i])
  }
}


for (i in c(1:length(l4))) {
  if (file.exists(l4[i])) {
    #print(paste0(aa[i],' all good'))
    x <- 'lame'
  } else {
    print(paste0(aa[i],' not all good'))
    print(l4[i])
  }
}

for (i in c(1:length(l5))) {
  if (file.exists(l5[i])) {
    #print(paste0(aa[i],' all good'))
    x <- 'lame'
  } else {
    print(paste0(aa[i],' not all good'))
    print(l5[i])
  }
}


for (i in c(1:length(l6))) {
  if (file.exists(l6[i])) {
    #print(paste0(aa[i],' all good'))
    x <- 'lame'
  } else {
    print(paste0(aa[i],' not all good'))
    print(l6[i])
  }
}
