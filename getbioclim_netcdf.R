## getbioclim.R

library(ncdf4)
library(ncdf4.helpers)
library(assertthat)
library(dismo)
library(data.table)
library(foreach)
library(doMC)
library(pryr)

args = commandArgs(trailingOnly=TRUE)
assert_that(are_equal(length(args), 4))

outdir = args[4]

datafiles = c()
datafiles$tmin = nc_open(args[1])
datafiles$tmax = nc_open(args[2])
datafiles$prec = nc_open(args[3])


climatedata = c()
climatedata$tmin = ncvar_get(datafiles$tmin, "tmn")
climatedata$tmax = ncvar_get(datafiles$tmax, "tmx")
climatedata$prec = ncvar_get(datafiles$prec, "pre")

lats  = ncvar_get(datafiles$tmin, 'lat')
lons  = ncvar_get(datafiles$tmin, 'lon')
times = ncvar_get(datafiles$tmin, 'time')

nlons = length(lons)
# bioclimVars = data.frame(lat = numeric(),
#                          lon = numeric(),
#                          year = numeric(),
#                          bio1 = numeric(),
#                          bio2 = numeric(), 
#                          bio3 = numeric(),
#                          bio4 = numeric(),
#                          bio5 = numeric(),
#                          bio6 = numeric(), 
#                          bio7 = numeric(), 
#                          bio8 = numeric(), 
#                          bio9 = numeric(), 
#                          bio10 = numeric(), 
#                          bio11 = numeric(),
#                          bio12 = numeric(),
#                          bio13 = numeric(),
#                          bio14 = numeric(),
#                          bio15 = numeric(), 
#                          bio16 = numeric(), 
#                          bio17 = numeric(), 
#                          bio18 = numeric(), 
#                          bio19 = numeric())

cluster <- makeCluster(detectCores()-1)
registerDoMC(detectCores()-1)


if(!file.exists(outdir)){
  dir.create(outdir)
}

extractBiovars.year <- function(lon, tlat, timeStart){
  this.tmin = climatedata$tmin[lon, tlat, timeStart:(timeStart+11)]
  this.tmax = climatedata$tmax[lon, tlat, timeStart:(timeStart+11)]
  this.prec = climatedata$prec[lon, tlat, timeStart:(timeStart+11)]
  
  latval = lats[tlat]
  lonval = lons[lon]
  timeval = times[timeStart]
  yearVal = as.numeric(format(as.Date(timeval, origin='1900-01-01'), '%Y'))
  
  if(any(is.null(this.tmin)) || any(is.null(this.tmax)) || any(is.null(this.prec))){
    return(c(latval, lonval, yearVal, rep(NA, 19)))
  }
  computedVars = biovars(this.prec, this.tmin, this.tmax)
  return(rbind(c(latval, lonval, yearVal, computedVars)))
}


for (lon in seq_along(lons)) {
  outfile = paste(outdir,"/","_lon_",lons[lon],".csv", sep="")
  thisLonRes = foreach(lat=seq_along(lats)) %dopar% {
    foreach(timeStart=seq(1, 108, 12), .combine=rbind) %dopar% {
    
      return(extractBiovars.year(lon, lat, timeStart))
    }
  }
  print(paste("Done ", lon, "/", nlons, sep=""))
  allrows = do.call('rbind', thisLonRes)
  write.csv(allrows[complete.cases(allrows),], outfile)
}

