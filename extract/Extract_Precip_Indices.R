library(gdalUtils)
library(raster)
library(dplyr)
library(SPEI)
library(lubridate)
library(zoo)
library(foreach)
library(doParallel)

setwd('G://My Drive/Feed the Future')

hh <- read.csv('Coords&Dates_HH.csv') %>%
  filter(!is.na(latitude))
child <- read.csv('Coords&Dates_Child.csv') %>%
  filter(!is.na(latitude) & !is.na(age))

sp_hh <- SpatialPointsDataFrame(coords=hh[ c('longitude', 'latitude')], data = hh)
sp_child <- SpatialPointsDataFrame(coords=child[ c('longitude', 'latitude')], data = child)

r <- raster('G://My Drive/CHIRPS/Monthly/chirps-v2.0.1981.01.tif')
codes <- raster(matrix(seq(1, ncell(r)), nrow=nrow(r), ncol=ncol(r)), xmx=xmax(r), xmn=xmin(r), ymx=ymax(r), ymn=ymin(r))
codes[r==-9999] <- NA

sp_hh@data$tmpcode <- extract(codes, sp_hh)
sp_child@data$tmpcode <- extract(codes, sp_child)

#Deal with points near a coast, coming up NA
spna <- sp_hh[is.na(sp_child@data$tmpcode), ]
badcoords <- unique(spna@coords)
tmpcode <- apply(X = badcoords, MARGIN = 1, FUN = function(xy) codes@data@values[which.min(replace(distanceFromPoints(codes, xy), is.na(codes), NA))])

sp_hh@data$tmpcode[is.na(sp_hh@data$tmpcode)] <- tmpcode #Normally there can be multiple errant points but in this case there is just 1, so this works
sp_child@data$tmpcode[is.na(sp_child@data$tmpcode)] <- tmpcode

rll <- rasterToPoints(codes) %>% data.frame
rll <- rll[rll$layer %in% sp_hh$tmpcode, ]

#Read in precip data
precip_in_folder <- 'C://Users/matt/Monthly/'
precip_vrt_file <- extension(rasterTmpFile(), 'ivrt')
precip_files <- list.files(precip_in_folder, pattern='tif$')[1:420]
gdalbuildvrt(paste0(precip_in_folder, precip_files), precip_vrt_file, separate=TRUE, verbose=T, overwrite=TRUE)

cl <- makeCluster(8, outfile = 'C://Users/matt/Desktop/log')
registerDoParallel(cl)

df_hh <- foreach(n=1:nrow(rll), .packages=c('raster', 'lubridate', 'gdalUtils', 'SPEI', 'dplyr', 'zoo'), .combine=bind_rows) %dopar% {
  
  precip <- gdallocationinfo(precip_vrt_file, rll$x[n], rll$y[n], wgs84=TRUE, valonly=TRUE) %>%
    as.numeric
  
  if(any(precip < 0)){
    stop('Precip is less than 0!')
  }
  
  #Get HH Vars
  interview <- data.frame(tmpcode=rll$layer[n],
                          month=month(seq(ymd('1981-01-01'), ymd('2015-12-01'), by='1 month')),
                          year=year(seq(ymd('1981-01-01'), ymd('2015-12-01'), by='1 month')),
                          spi12=as.numeric(spi(precip, 12, na.rm=TRUE)$fitted),
                          spi24=as.numeric(spi(precip, 24, na.rm=TRUE)$fitted),
                          spi36=as.numeric(spi(precip, 36, na.rm=TRUE)$fitted),
                          spi48=as.numeric(spi(precip, 48, na.rm=TRUE)$fitted),
                          spi60=as.numeric(spi(precip, 60, na.rm=TRUE)$fitted))
  
  interview_lag <- data.frame(tmpcode=rll$layer[n],
                          month=month(seq(ymd('1982-01-01'), ymd('2016-12-01'), by='1 month')),
                          year=year(seq(ymd('1982-01-01'), ymd('2016-12-01'), by='1 month')),
                          spi12_lag=as.numeric(spi(precip, 12, na.rm=TRUE)$fitted),
                          spi24_lag=as.numeric(spi(precip, 24, na.rm=TRUE)$fitted),
                          spi36_lag=as.numeric(spi(precip, 36, na.rm=TRUE)$fitted),
                          spi48_lag=as.numeric(spi(precip, 48, na.rm=TRUE)$fitted),
                          spi60_lag=as.numeric(spi(precip, 60, na.rm=TRUE)$fitted))
  
  meanannual <- data.frame(tmpcode=rll$layer[n],
                           mean_annual_precip=mean(precip, na.rm=T)*12)
  
  sel_hh <- sp_hh@data[sp_hh$tmpcode == rll$layer[n], ]
  sel_hh <- Reduce(function(x, y){merge(x,y,all.x=T,all.y=F)}, list(sel_hh, interview, interview_lag, meanannual))
  sel_hh
}

rl2 <- rll[rll$layer %in% sp_child$tmpcode, ]

#Get Child Vars
df_child <- foreach(n=1:nrow(rl2), .packages=c('raster', 'lubridate', 'gdalUtils', 'SPEI', 'dplyr', 'zoo'), 
                    .combine=bind_rows) %dopar% {
  
  precip <- gdallocationinfo(precip_vrt_file, rl2$x[n], rl2$y[n], wgs84=TRUE, valonly=TRUE) %>%
    as.numeric
  
  sel_child <- sp_child@data[sp_child$tmpcode == rl2$layer[n], ]
  
  thousandday <- data.frame(tmpcode=rl2$layer[n],
                            thousandday_month=month(seq(ymd('1981-01-01'), ymd('2015-12-01'), by='1 month')),
                            thousandday_year=year(seq(ymd('1981-01-01'), ymd('2015-12-01'), by='1 month')),
                            spi33_thousandday=as.numeric(spi(precip, 12, na.rm=TRUE)$fitted),
                            spi24_thousandday=as.numeric(spi(precip, 24, na.rm=TRUE)$fitted),
                            spi36_thousandday=as.numeric(spi(precip, 36, na.rm=TRUE)$fitted))
  
  sel_child <- merge(sel_child, thousandday, all.x=T, all.y=F)
  
  for(i in 1:nrow(sel_child)){
    ind <- seq(ymd('1981-01-01'), ymd('2015-12-01'), by='1 month') == floor_date(ymd(sel_child$survey_date[i]), unit='months')
    
    age <- sel_child$age[i]
    
    if (age < 3){
      age <- 3
    }
    
    #Get spi for age, as well as spi for age + 9 mos
    sel_child[i, 'spi_age'] <- as.numeric(spi(precip, age, na.rm=TRUE)$fitted)[ind]
    sel_child[i, 'spi_ageutero'] <- as.numeric(spi(precip, sel_child$age[i] + 9, na.rm=TRUE)$fitted)[ind]
  }
  
  cat(n, round(n/nrow(rl2)*100, 4), 'percent done\n')
  
  sel_child
}

write.csv(df_hh, 'PrecipIndices_HH.csv', row.names=F)
write.csv(df_child, 'PrecipIndices_Child.csv', row.names=F)