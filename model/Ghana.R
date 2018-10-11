library(sp)
library(spdep)
library(dplyr) 
library(broom)

setwd('G:/My Drive/Feed the Future/')

load('GHA_data.Rdata')

buffer <- 30000

#####################
#Make spatial weights
######################
wlisthh <- allhh %>%
  select(longitude, latitude) %>%
  SpatialPoints(proj4string = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')) %>%
  spTransform(CRS('+proj=eqc +lon_0=-1.0546875000000004')) %>%
  coordinates %>%
  dnearneigh(0, buffer, longlat=F) %>%
  nb2listw(style="W")

wlistchild <- allchild %>%
  select(longitude, latitude) %>%
  SpatialPoints(proj4string = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')) %>%
  spTransform(CRS('+proj=eqc +lon_0=-1.0546875000000004')) %>%
  coordinates %>%
  dnearneigh(0, buffer, longlat=F) %>%
  nb2listw(style="W")

#########################
#Regressions
#########################
############Child Nutrition############################

#Significant autocorrelation, use spatial error model
mod_child_gha <- errorsarlm(haz ~ asset_index + hh_size + spi24 + 
                          hhhead_religion + precip_mean +
                          hhhead_age + hhhead_literate + hhhead_sex + dependents + gender + age + pop + 
                          birth_order + within24,  
                        data=allchild, wlistchild)
summary(mod_child_gha)

#############Household Hunger###############################

#Significant autocorrelation, use spatial error model
mod_hh_gha <- errorsarlm(hhs~asset_index + hh_size + spi24 + 
                     precip_mean +  hhhead_religion + market + 
                     hhhead_age + hhhead_literate + pop + 
                     hhhead_sex + dependents, 
                   data=allhh, wlisthh)
summary(mod_hh_gha)


