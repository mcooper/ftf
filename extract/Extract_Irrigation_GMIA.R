##########################################
#Note: I decided not to do this, but this script documents why


library(raster)
library(sp)
library(foreign)
library(readstata13)
library(dplyr)
library(rgeos)

gmia <- raster('G://My Drive/DHS Spatial Covars/Irrigation/gmia_v5_aei_pct_asc/gmia_v5_aei_pct.asc',
               proj4string=CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))

bgd <- read.dta13('G://My Drive/Feed the Future/Bangladesh/4. BIHS_household_2011_15.dta') %>%
  mutate(hh_refno=paste0('BGD-', a01)) %>%
  filter(!is.na(bio_latitude)) %>%
  select(hh_refno,
         latitude=bio_latitude,
         longitude=bio_longitude)

gha <- read.csv('G://My Drive/Feed the Future/GHA-PBS-12/ghana_FtF_2012_coordinates.csv') %>%
  mutate(hh_refno = paste0('GHA-', hhserial)) %>%
  filter(!is.na(bio_latitude_offset)) %>%
  select(hh_refno, 
         latitude=bio_latitude_offset, 
         longitude=bio_longitude_offset)

coords <- bind_rows(gha, bgd) %>%
  SpatialPointsDataFrame(coords=.[ , c('longitude', 'latitude')], 
                         proj4string = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'),
                         data = .)

#Here are all the coords
coords@data$irrigation <- extract(gmia, coords)
tmp <- coords@data

hh <- read.dta13('Bangladesh/4. BIHS_household_2011_15.dta') %>%
  mutate(hh_refno=paste0('BGD-', a01),
         irrigation=bio_a_rice_i/(bio_a_rice_r + bio_a_rice_i)) %>%
  select(hh_refno,
         irrigation)

hh2 <- merge(hh %>% select(hh_refno, irrigation), tmp %>% select(hh_refno, gmia=irrigation), all.x=T, all.y=T)

#the extracted GMIA values are very different from the irrigation values for BGD, which was just rice
plot(hh2$irrigation, hh2$gmia)

#But it looks like there is not much irrigation in GHA anyway, at least compared to BGD,
#So there is no point comparing
hist(tmp$irrigation[grepl('GHA-', tmp$hh_refno)], 100)
hist(tmp$irrigation[grepl('BGD-', tmp$hh_refno)], 100)

#On average, only 0.0059% of household have irrigation
mean(tmp$irrigation[grepl('GHA-', tmp$hh_refno)])

#Only 22% of households have any irrigation
mean(tmp$irrigation[grepl('GHA-', tmp$hh_refno)] > 0)



