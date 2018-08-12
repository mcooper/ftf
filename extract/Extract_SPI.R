library(gdalUtils)
#need to instal GDAL
library(ggplot2)
library(RPostgreSQL)
library(raster)
library(dplyr)
library(SPEI)
library(foreach)
library(lubridate)

setwd('D://Documents and Settings/mcooper/Google Drive/Dissertation/')

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
  s <- as.numeric(s)
  data.frame(code=rll$layer[n],
             month=month(seq(ymd('1981-01-01'), ymd('2017-08-01'), by='1 month')),
             year=year(seq(ymd('1981-01-01'), ymd('2017-08-01'), by='1 month')),
             spi6=as.numeric(spi(s, 6, na.rm=TRUE)$fitted),
             spi12=as.numeric(spi(s, 12, na.rm=TRUE)$fitted),
             spi24=as.numeric(spi(s, 24, na.rm=TRUE)$fitted),
             spi36=as.numeric(spi(s, 36, na.rm=TRUE)$fitted))
}

spi_data <- merge(sp@data, spi, all.x=T, all.y=F)

save('spi', file='spi.Rdata')

write.csv(spi_data, 'Coords&SPI.csv', row.names=F)

spi_data$tmp <- spi_data$year
spi_data$tmp[spi_data$year==2012 & spi_data$country=='Bangladesh'] <- 2011

spi_data <- spi_data[!is.na(spi_data$year), ]

spi_data$survey <- paste0(spi_data$country, spi_data$tmp)

ggplot(spi_data) + geom_histogram(aes(x=spi36, fill=survey))

ggplot(spi_data %>% filter(survey=='Zambia2015')) + 
  geom_point(aes(x=longitude, y=latitude, color=spi6))





