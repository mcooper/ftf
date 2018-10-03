library(gdalUtils)
library(raster)
library(dplyr)
library(SPEI)
library(lubridate)
library(zoo)
library(foreach)
library(doParallel)

setwd('/mnt/')

dat <- read.csv('Feed the Future/Coords&Dates.csv') %>%
  unique

sp <- SpatialPointsDataFrame(coords=dat[ c('longitude', 'latitude')], data = dat)

r <- raster('CHIRPS/Monthly/chirps-v2.0.1981.01.tif')
codes <- raster(matrix(seq(1, ncell(r)), nrow=nrow(r), ncol=ncol(r)), xmx=xmax(r), xmn=xmin(r), ymx=ymax(r), ymn=ymin(r))
codes[r==-9999] <- NA

sp@data$tmpcode <- extract(codes, sp)

#Deal with points near a coast, coming up NA
spna <- sp[is.na(sp@data$tmpcode) , ]
spna$tmpcode <- NULL
badcoords <- unique(spna@coords)
tmpcode <- apply(X = badcoords, MARGIN = 1, FUN = function(xy) codes@data@values[which.min(replace(distanceFromPoints(codes, xy), is.na(codes), NA))])
badcoords <- cbind.data.frame(badcoords, tmpcode)
spna <- merge(spna@data, badcoords)
sp <- bind_rows(spna, sp@data[!is.na(sp@data$tmpcode), ])

rll <- rasterToPoints(codes) %>% data.frame
rll <- rll[rll$layer %in% sp$tmpcode, ]

#Read in precip data
precip_in_folder <- 'CHIRPS/Monthly/'
precip_vrt_file <- extension(rasterTmpFile(), 'ivrt')
precip_files <- list.files(precip_in_folder, pattern='tif$')[1:432]
gdalbuildvrt(paste0(precip_in_folder, precip_files), precip_vrt_file, separate=TRUE, verbose=T, overwrite=TRUE)

#Read in phenology data
pheno_in_folder <- 'DHS Spatial Covars/Phenology/'
pheno_vrt_file <- extension(rasterTmpFile(), 'ivrt')
pheno_files <- list.files(pheno_in_folder, pattern='aggregate.tif$')
gdalbuildvrt(paste0(pheno_in_folder, pheno_files), pheno_vrt_file, separate=TRUE, verbose=T, overwrite=TRUE)

#Read in tmax data
tmax_in_folder <- 'temperature/'
tmax_vrt_file <- extension(rasterTmpFile(), 'ivrt')
tmax_files <- list.files(tmax_in_folder, pattern='^tmax.*tif$')
gdalbuildvrt(paste0(tmax_in_folder, tmax_files), tmax_vrt_file, separate=TRUE, verbose=T, overwrite=TRUE)

#Read in tmin data
tmin_in_folder <- 'temperature/'
tmin_vrt_file <- extension(rasterTmpFile(), 'ivrt')
tmin_files <- list.files(tmin_in_folder, pattern='^tmin.*tif$')
gdalbuildvrt(paste0(tmin_in_folder, tmin_files), tmin_vrt_file, separate=TRUE, verbose=T, overwrite=TRUE)

extract_data <- function(vrt, x, y){
  
  m <- gdallocationinfo(vrt, x, y, wgs84=TRUE, valonly=TRUE) %>%
    as.numeric
  
  m[m == -9999] <- NA
  
  return(m)
  
}

parseDates <- function(dates){

  dates <- as.numeric(dates)
  
  dates[dates < 0] <- 0
  
  #If all dates are NA, dont mask any
  if (all(dates == 0)){
    return(rep(1, 432))
  }
  
  #Get number from 0-36 (see under 'Phenology': https://mars.jrc.ec.europa.eu/asap/download.php)
  dates[dates > 36 & dates <= 72] <- dates[dates > 36 & dates <= 72] - 36
  dates[dates > 72] <- dates[dates > 72] - 72
  
  #Convert Dekad to Month
  dates <- dates/3
  dates <- ceiling(dates)
  
  year <- rep(NA, 12)
  
  #Make months of first season 1
  if (dates[1] < dates[2]){
    year[dates[1]:dates[2]] <- 1
  } else{
    year[dates[1]:12] <- 1
    year[1:dates[2]] <- 1
  }
  
  #Make months of second season 2
  if (dates[3] > dates[4]){
    year[dates[4]:12] <- 1
    year[1:dates[3]] <- 1
  } else{
    year[dates[3]:dates[4]] <- 1
  }
  
  return(rep(year, 36))
}

allprecip <- data.frame()
for(n in 1:nrow(rll)){
  
  precip <- extract_data(precip_vrt_file, rll$x[n], rll$y[n])
  
  tmax <- extract_data(tmax_vrt_file, rll$x[n], rll$y[n])
  
  tmin <- extract_data(tmin_vrt_file, rll$x[n], rll$y[n])
  
  pheno <- gdallocationinfo(pheno_vrt_file, rll$x[n], rll$y[n], wgs84=TRUE, valonly=TRUE)[c(3, 1, 4, 2)]
  
  phenoMask <- parseDates(pheno)
  
  precip_season <- precip*phenoMask
  
  PET <- hargreaves(tmin-273.15, tmax-273.15, lat=rll$y[n], Pre=precip) %>%
    as.vector
  
  s <- precip - PET
  
  s_season <- s*phenoMask
  
  interview <- data.frame(tmpcode=rll$layer[n],
                          month=month(seq(ymd('1981-01-01'), ymd('2016-12-01'), by='1 month')),
                          year=year(seq(ymd('1981-01-01'), ymd('2016-12-01'), by='1 month')),
                          spei12=as.numeric(spei(s, 12, na.rm=TRUE)$fitted),
                          spei24=as.numeric(spei(s, 24, na.rm=TRUE)$fitted),
                          spei36=as.numeric(spei(s, 36, na.rm=TRUE)$fitted),
                          spi12=as.numeric(spi(precip, 12, na.rm=TRUE)$fitted),
                          spi24=as.numeric(spi(precip, 24, na.rm=TRUE)$fitted),
                          spi36=as.numeric(spi(precip, 36, na.rm=TRUE)$fitted),
                          precip_mean=mean(precip),
                          tmin_mean=mean(tmin),
                          tmax_mean=mean(tmax),
                          spei12gs=as.numeric(spei(s_season, 12, na.rm=TRUE)$fitted),
                          spei24gs=as.numeric(spei(s_season, 24, na.rm=TRUE)$fitted),
                          spei36gs=as.numeric(spei(s_season, 36, na.rm=TRUE)$fitted),
                          spi12gs=as.numeric(spi(precip_season, 12, na.rm=TRUE)$fitted),
                          spi24gs=as.numeric(spi(precip_season, 24, na.rm=TRUE)$fitted),
                          spi36gs=as.numeric(spi(precip_season, 36, na.rm=TRUE)$fitted))
  
  sel <- sp[sp$tmpcode == rll$layer[n], ]
  sel <- merge(sel, interview, all.x=T, all.y=F) %>%
    select(-tmpcode)
  cat(n, round(n/nrow(rll)*100, 4), 'percent done\n') 
  
  allprecip <- bind_rows(sel, allprecip)
}

write.csv(allprecip, 'G://My Drive/Feed the Future/AllPrecipIndices.csv', row.names=F)





