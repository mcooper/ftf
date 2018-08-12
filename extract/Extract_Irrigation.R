library(raster)
library(sp)
library(foreign)
library(readstata13)
library(dplyr)
library(rgeos)

setwd('G:/My Drive/Feed the Future')

aman <- raster('IRRI Bangladesh rice maps 2010/aman2010.tif')
boro <- raster('IRRI Bangladesh rice maps 2010/boro2010.tif')
aus <- raster('IRRI Bangladesh rice maps 2010/aus2010.tif')

aman_irr <- aman %in% c(2, 3, 4, 5)
boro_irr <- boro %in% c(2, 3, 4, 5)
aus_irr <- aus %in% c(2, 3, 4, 5)

aman_irr[aman==0] <- NA
boro_irr[boro==0] <- NA
aus_irr[aus==0] <- NA

combo <- aman_irr + boro_irr + aus_irr
combo <- combo > 0

bgd <- read.dta13('Bangladesh/4. BIHS_household_2011_15.dta') %>%
  mutate(hh_refno=paste0('BGD-', a01)) %>%
  filter(!is.na(bio_latitude)) %>%
  select(hh_refno,
         latitude=bio_latitude,
         longitude=bio_longitude)

bgd_sp <- SpatialPointsDataFrame(coords=bgd[ , c('longitude', 'latitude')], 
                                 proj4string = CRS(proj4string(aman)),
                                 data = bgd)

#Project to Bangladesh
bgd_sp_proj <- spTransform(bgd_sp, CRS('+proj=utm +zone=46 +ellps=WGS84 +datum=WGS84 +units=m +no_defs'))

bgd_sp <- gBuffer(bgd_sp_proj, byid=TRUE, width=7500)

combo_proj <- projectRaster(combo, crs=CRS('+proj=utm +zone=46 +ellps=WGS84 +datum=WGS84 +units=m +no_defs'))

extract <- extract(combo_proj, bgd_sp, fun=mean, na.rm=T)

bgd$perc_irrig <- extract

write.csv(bgd, 'BGD_perc_irrig.csv', row.names=F)
