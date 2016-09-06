######################################################
# Applying regional algorithms for data visualization
# Daniel Hong
######################################################

### Make sst and chl matrices from nc files
library(ncdf4)

sst_list = list("sst_jan2014.nc", "sst_feb2014.nc", "sst_march2014.nc", "sst_apr2014.nc",
                "sst_may2014.nc", "sst_june2014.nc", "sst_july2014.nc", "sst_aug2014.nc",
                "sst_sept2014.nc", "sst_oct2014.nc", "sst_nov2014.nc", "sst_dec2014.nc")

chl_list = list("chl_jan2014.nc", "chl_feb2014.nc", "chl_march2014.nc", "chl_apr2014.nc",
                "chl_may2014.nc", "chl_june2014.nc", "chl_july2014.nc", "chl_aug2014.nc",
                "chl_sept2014.nc", "chl_oct2014.nc", "chl_nov2014.nc", "chl_dec2014.nc")

no3_list = list("no3_jan2014.nc", "no3_feb2014.nc", "no3_march2014.nc", "no3_apr2014.nc",
                "no3_may2014.nc", "no3_june2014.nc", "no3_july2014.nc", "no3_aug2014.nc",
                "no3_sept2014.nc", "no3_oct2014.nc", "no3_nov2014.nc", "no3_dec2014.nc")

rotate <- function(x) t(apply(x, 2, rev))

lon_matrix <- replicate(4320, xvals)
lat_matrix <- t(replicate(8640, yvals))

for(i in 1:12) {
  # SST data
  sst_nc <- nc_open(sst_list[[i]])
  #str(sst_nc)
  sst_matrix <- ncvar_get(sst_nc, "sst4")
  sst_matrix <- rotate(rotate(sst_matrix))
  sst_matrix <- apply(sst_matrix, 2, rev)
  
  # image(sst_matrix, axes=F, main="SST Feb 2012")
  # box(lty="solid")
  
  # Chl data
  chl_nc <- nc_open(chl_list[[i]])
  #str(chl_nc)
  chl_matrix <- ncvar_get(chl_nc, "chlor_a")
  chl_matrix <- rotate(rotate(chl_matrix))
  chl_matrix <- apply(chl_matrix, 2, rev)
  
  # image(chl_matrix, axes=F, main="Chl Feb 2012")
  # box(lty="solid")
  
  ### Check each condition to generage NO3 matrix
  
  # Create an empty matrix and append
  no3_matrix <- matrix(nrow=8640, ncol=4320)
  
  # Case ELSE
  checkE <- !is.na(sst_matrix) & !is.na(chl_matrix)
  sst_condE <- sst_matrix[checkE]
  chl_condE <- chl_matrix[checkE]
  no3_matrix[checkE] <- 25.68 - 1.97 * sst_condE + 0.04 * sst_condE^2 - 1.63 * chl_condE + 0.012 * chl_condE^2
  
  # Case 1
  check1 <- sst_matrix > 25 & (chl_matrix > 0.4) & !is.na(sst_matrix) & !is.na(chl_matrix)
  sst_cond1 <- sst_matrix[check1]
  chl_cond1 <- chl_matrix[check1]
  no3_matrix[check1] <- 38.39 - 1.32 * sst_cond1 + 4.32 * chl_cond1
  
  # Case 2
  check2 <- sst_matrix > 25 & (chl_matrix > 0.2 & chl_matrix < 0.4) & !is.na(sst_matrix) & !is.na(chl_matrix)
  sst_cond2 <- sst_matrix[check2]
  chl_cond2 <- chl_matrix[check2]
  no3_matrix[check2] <- 91.12 - 6.02 * sst_cond2 + 0.09 * sst_cond2^2 - 0.05 * chl_cond2
  
  # Clean Up
  no3_matrix[no3_matrix < 0] <- 0.001
  no3_matrix[no3_matrix > 40] <- NA
  
  #=================================
  # create new nc file
  #=================================
  
  #---------------------------------
  # make dimensions
  xvals <- seq(-180,180,length.out=8640)
  yvals <- seq(-90,90,length.out=4320)
  
  # xvals <- 1:4320
  # yvals <- 1:2160
  
  nx <- length(xvals)
  ny <- length(yvals)
  
  xdim <- ncdim_def( 'Lon', 'degreesE', as.double(xvals) )
  ydim <- ncdim_def( 'Lat', 'degreesE', as.double(yvals) )
  
  #---------------------------------
  # make variable
  var_no3 <- ncvar_def( 'NO3', 'micromoles', list(xdim,ydim), NA)
  
  #---------------------------------
  # make new output file
  no3_nc <- nc_create(no3_list[[i]], list(var_no3))
  
  #---------------------------------
  # put no3 data in file
  ncvar_put(no3_nc, var_no3, no3_matrix, start=NA, count=c(nx,ny))
  
  #---------------------------------
  # close nc file
  nc_close(no3_nc)
  
  #=================================
}
