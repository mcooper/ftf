library(sp)
library(spdep)
library(dplyr) 
library(broom)

setwd('G:/My Drive/Feed the Future/')

load('BGD_data.Rdata')

buffer <- 30000

######################
#Build spatial weights
######################
wlisthh <- allhh %>%
  select(longitude, latitude) %>%
  SpatialPoints(proj4string = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')) %>%
  spTransform(CRS('+proj=eqdc +lat_1=22.191291158578405 +lat_2=25.41960806605085 +lon_0=90.703125')) %>%
  coordinates %>%
  dnearneigh(0, buffer, longlat=F) %>%
  nb2listw(style="W")

wlistchild <- allchild %>%
  select(longitude, latitude) %>%
  SpatialPoints(proj4string = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')) %>%
  spTransform(CRS('+proj=eqdc +lat_1=22.191291158578405 +lat_2=25.41960806605085 +lon_0=90.703125')) %>%
  coordinates %>%
  dnearneigh(0, buffer, longlat=F) %>%
  nb2listw(style="W")

########################################
#Let's do a quick Chow test
#######################################
library(strucchange)

allhh <- allhh %>% arrange(year)

sctest(hhs~hhhead_education + hhhead_literate + hhhead_religion + hh_size + hhhead_sex +
         dependents + asset_index + precip_mean + 
         pop + spi24, type='Chow', data=allhh, point=table(allhh$year)[1])

#########################
#Regressions
#######################
############Child Nutrition############################

allchild$year <- as.factor(allchild$year)

mod_child_bgd <- errorsarlm(haz ~ asset_index + hh_size + spi24 + hhhead_age + 
                          hhhead_religion + precip_mean + year + 
                          hhhead_literate + hhhead_education + hhhead_sex + 
                            dependents + gender + age + pop,  
                        data=allchild, wlistchild)
summary(mod_child_bgd)

mod_child_irrig_bgd <- errorsarlm(haz ~ asset_index + hh_size + irrigation*spi24 + hhhead_age +  
                                hhhead_religion + irrigation*precip_mean + year + 
                                hhhead_literate + hhhead_education + hhhead_sex + dependents + gender + age + pop,  
                              data=allchild, wlistchild)
summary(mod_child_irrig_bgd)

#############Household Hunger###############################

mod_hh_bgd <- errorsarlm(hhs ~ asset_index + hh_size + spi24 + hhhead_age + 
                       hhhead_religion + precip_mean + year + 
                       hhhead_literate + hhhead_education + 
                       hhhead_sex + dependents + pop,  
                     data=allhh, wlisthh, tol.solve=10e-20)
summary(mod_hh_bgd)

mod_hh_irrig_bgd <- errorsarlm(hhs ~ asset_index + hh_size + irrigation*spi24 + hhhead_age + 
                             hhhead_religion + irrigation*precip_mean + year + 
                             hhhead_literate + hhhead_education + hhhead_sex + dependents + pop,  
                           data=allhh, wlisthh, tol.solve=10e-20)
summary(mod_hh_irrig_bgd)


save(list=c('mod_child_bgd', 'mod_hh_bgd', 'mod_child_irrig_bgd', 'mod_hh_irrig_bgd'), file='BGD_mods.Rdata')

