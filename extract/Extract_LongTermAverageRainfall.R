library(gdalUtils)
#need to instal GDAL
library(ggplot2)
library(RPostgreSQL)
library(raster)
library(dplyr)
library(SPEI)
library(foreach)
library(lubridate)

setwd('D://Documents and Settings/mcooper/Google Drive/Feed the Future/')

dat <- read.csv('Coords&Dates.csv')

sp <- SpatialPointsDataFrame(coords=dat[ c('longitude', 'latitude')], data = dat)

r <- raster('../CHIRPS/chirps-v2.0.1981.01.tif')
r <- raster(matrix(seq(1, ncell(r)), nrow=nrow(r), ncol=ncol(r)), xmx=xmax(r), xmn=xmin(r), ymx=ymax(r), ymn=ymin(r))

sp@data$code <- extract(r, sp)

rll <- rasterToPoints(r) %>% data.frame
rll <- rll[rll$layer %in% sp@data$code, ]

in_folder <- 'D://Documents and Settings/mcooper/Google Drive/CHIRPS/'
tifs <- dir(in_folder, pattern='.tif$')
vrt_file <- extension(rasterTmpFile(), 'vrt')
gdalbuildvrt(paste0(in_folder, '*.tif'), vrt_file, separate=TRUE, verbose=T,
             overwrite=TRUE)

spi <- foreach(n=1:nrow(rll), .combine=rbind) %do% {
  s <- gdallocationinfo(vrt_file, rll$x[n], rll$y[n],
                        wgs84=TRUE, valonly=TRUE)
  cat(n, round(n/nrow(rll)*100, 4), 'percent done\n')

  val <- mean(as.numeric(s)[1:432])*12
  
  data.frame(code=rll$layer[n],
             mean_annual_precip=val)
}






spi_data <- merge(sp@data, spi, all.x=T, all.y=F)

write.csv(spi_data, 'RainfallLTN.csv', row.names=F)





