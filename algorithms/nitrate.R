library(ncdf4)
sst <- nc_open("~/Desktop/repositories/marine-biology/data/sst_test.nc")
chl <- nc_open("~/Desktop/repositories/marine-biology/data/chl_test.nc")
sst <- ncvar_get(sst, "sst4")
rotate <- function(x) t(apply(x, 2, rev))
sst <- rotate(rotate(sst))
sst <- apply(sst, 2, rev)
chl <- ncvar_get(chl, "chlor_a")
chl <- rotate(rotate(chl))
chl <- apply(chl, 2, rev)
no3 <- matrix(nrow=4320, ncol=2160)
checkE <- !is.na(sst) & !is.na(chl)
sst_condE <- sst[checkE]
chl_condE <- chl[checkE]
no3[checkE] <- 25.68 - 1.97*sst_condE + 0.04*sst_condE^2 - 1.63*chl + 0.012*chl_condE^2
check1 <- sst > 25 & (chl > 0.4) & !is.na(sst) & !is.na(chl)
sst_cond1 <- sst[check1]
chl_cond1 <- chl[check1]
no3[check1] <- 38.39 - 1.32*sst_cond1 + 4.32*chl_cond1
check2 <- sst > 25 & (chl > 0.2 & chl < 0.4) & !is.na(sst) & !is.na(chl)
sst_cond2 <- sst[check2]
chl_cond2 <- chl[check2]
no3[check2] <- 91.12 - 6.02*sst_cond2 + 0.09*sst_cond2^2 - 0.05 * chl_cond2
no3[no3 < 0] <- 0.001
no3[no3 > 40] <- 35
xvals <- seq(-180,180,length.out=4320)
yvals <- seq(-90,90,length.out=2160)
nx <- length(xvals)
ny <- length(yvals)
xdim <- ncdim_def("lon", "degreesE", as.double(xvals))
ydim <- ncdim_def("lat", "degreesE", as.double(yvals))
var_no3 <- ncvar_def("NO3", "micromoles", list(xdim, ydim), NA)
no3_nc <- nc_create("no3_test.nc", list(var_no3))
ncvar_put(no3_nc, var_no3, no3, start=NA, count=c(nx, ny))
yvals <- seq(-89.96,89.96,length.out=2160)
nx <- length(xvals)
ny <- length(yvals)
xdim <- ncdim_def("lon", "degreesE", as.double(xvals))
ydim <- ncdim_def("lat", "degreesE", as.double(yvals))
var_no3 <- ncvar_def("NO3", "micromoles", list(xdim, ydim), NA)
no3_nc <- nc_create("no3_test.nc", list(var_no3))
ncvar_put(no3_nc, var_no3, no3, start=NA, count=c(nx, ny))
