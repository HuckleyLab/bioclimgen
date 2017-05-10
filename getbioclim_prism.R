## getbioclim_prism
##
##
##

library(assertthat)
library(prism)
library(raster)
library(dplyr)
library(dismo)
library(RPostgreSQL)


## usage: <year or startyear:endyear> <minlat> <maxlat> <minlon> <maxlon> [dbname] [table] [host] [user] [pass]
args = commandArgs(trailingOnly = TRUE)
usage = "usage: getbioclim_prism.R <year or startyear:endyear> <minlat> <maxlat> <minlon> <maxlon>"
assert_that(length(args) >= 5, msg=usage)
print(args)
useDB = FALSE
if (length(args) > 5){
  useDB = TRUE
  dbname = args[6]
  dbtable = args[7]
  dbhost = args[8]
  dbuser = args[9]
  if(!is.na(args[10])){
    dbpass = args[10]
    DB_CONNECTION = dbConnect(dbDriver("PostgreSQL"), db=dbname, host=dbhost, user=dbuser, pass=dbpass)
  }
  else{
    DB_CONNECTION = dbConnect(dbDriver("PostgreSQL"), db=dbname, host=dbhost, user=dbuser)
  }
  
}


if (grepl(":", args[1])){ ## check for year range
  multiYears = TRUE
  split = strsplit(args[1], ":")[[1]]
  startYear = as.integer(split[1])
  endYear = as.integer(split[2])
} else {
  multiYears = FALSE
  startYear = as.integer(args[1])
  endYear = NULL
}

cropExt = extent(as.integer(args[2]), as.integer(args[3]),as.integer(args[4]), as.integer(args[5]))

if (multiYears){
  years = startYear:endYear
} else{
  years = startYear
}

## prism.path = ~/prismtmp
prismRoot = "~/prismtmp"
options(prism.path = prismRoot)
get_prism_monthlys("tmin", years=years, mon=1:12, keepZip = F)
get_prism_monthlys("tmax", years=years, mon=1:12, keepZip = F)
get_prism_monthlys("ppt" , years=years, mon=1:12, keepZip = F)

prismTmpYearMonDir = "PRISM_%s_stable_4kmM2_%d%02d_bil"
if(startYear < 1981){
  prismPptYearMonDir = "PRISM_%s_stable_4kmM2_%d%02d_bil"
} else{
  prismPptYearMonDir = "PRISM_%s_stable_4kmM3_%d%02d_bil"
}

all_bioclim = data.frame()
for (year in years){
  YearTmin = brick(cropExt)
  YearTmax = brick(cropExt)
  YearPpt  = brick(cropExt)
  for (month in 1:12){
    TempfileTmp = paste(prismRoot, prismTmpYearMonDir, sep="/") 
    PptfileTmp = paste(prismRoot, prismPptYearMonDir, sep='/')
    tmin_f = paste(sprintf(TempfileTmp, "tmin", year, month),
                paste0(sprintf(prismTmpYearMonDir, "tmin", year, month), ".bil"),
                sep="/")
    print(tmin_f)
    tmin_mo = crop(raster(tmin_f), cropExt)
    
    tmax_f = paste(sprintf(TempfileTmp, "tmax", year, month),
                   paste0(sprintf(prismTmpYearMonDir, "tmax", year, month), ".bil"),
                   sep="/")
    print(tmax_f)
    tmax_mo = crop(raster(tmax_f), cropExt)
    
    prec_f = paste(sprintf(PptfileTmp, "ppt", year, month),
                   paste0(sprintf(prismPptYearMonDir, "ppt", year, month), ".bil"),
                   sep="/")
    print(prec_f)
    prec_mo = crop(raster(prec_f), cropExt)
    
    YearTmin = addLayer(YearTmin, tmin_mo)
    YearTmax = addLayer(YearTmax, tmax_mo)
    YearPpt  = addLayer(YearPpt , prec_mo)
  }
  year_biovars_df = as.data.frame(biovars(YearPpt, YearTmin, YearTmax), xy=T)
  year_biovars_df = year_biovars_df %>% mutate(year=rep(year, nrow(year_biovars_df))) ## add year as col.
  all_bioclim = rbind(all_bioclim, year_biovars_df[,c(2,1,22,3:21)]) ## move 'year' column to 3rd col, move to lat/lon (from lon/lat, yikes.)
  print(colnames(all_bioclim))
  print(head(all_bioclim, 1))
  if(useDB){
    dbWriteTable(DB_CONNECTION, dbtable, all_bioclim, append=T, add.id=F)
  }
}
