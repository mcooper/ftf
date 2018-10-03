library(sp)
library(spdep)
library(dplyr) 
library(broom)

setwd('G:/My Drive/Feed the Future/')

load('GHA_data.Rdata')

#####################
#Make spatial weights
######################
sphh <- SpatialPoints(coords = allhh[ , c('longitude', 'latitude')], proj4string = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
spmhh <- spTransform(sphh, CRS('+proj=utm +zone=30 +ellps=clrk80 +towgs84=-124.76,53,466.79,0,0,0,0 +units=m +no_defs '))
distmat <- as.matrix(dist(spmhh@coords))
diag(distmat) <- 1000000
mins <- apply(distmat, 1, min)
cat("The most remote village is", max(mins), "km away from others, so lag must be at least", max(mins))

dnnhh <- dnearneigh(coordinates(spmhh), 0, 75000, longlat=F)
wlisthh <- nb2listw(dnnhh, style="W")

spchild <- SpatialPoints(coords = allchild[ , c('longitude', 'latitude')], proj4string = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
spmchild <- spTransform(spchild, CRS('+proj=utm +zone=30 +ellps=clrk80 +towgs84=-124.76,53,466.79,0,0,0,0 +units=m +no_defs '))
distmat <- as.matrix(dist(spmchild@coords))
diag(distmat) <- 1000000
mins <- apply(distmat, 1, min)
cat("The most remote village is", max(mins), "km away from others, so lag must be at least", max(mins))

dnnchild  <- dnearneigh(coordinates(spmchild), 0, 75000, longlat=F)
wlistchild <- nb2listw(dnnchild, style="W")


#########################
#Regressions
#########################
############Child Nutrition############################

#Check for autocorrelation in the outcome variable
moran.test(allchild$haz, wlistchild)

#Significant autocorrelation, use spatial error model
mod <- errorsarlm(haz ~ asset_index + pop + market + hh_size + spi24 + hhhead_religion + precip_mean +
                  hhhead_age + hhhead_literate + hhhead_sex + dependents + gender + age + birth_order + 
                    within24,  
                data=allchild, wlistchild)
summary(mod)

#############Household Hunger###############################

#Check for autocorrelation in the outcome variable
moran.test(allhh$hhs, wlisthh)

#Significant autocorrelation, use spatial error model
mod2 <- errorsarlm(hhs~asset_index + pop + market + hh_size + spi24 + mean_annual_precip + hhhead_religion + 
                   hhhead_age + hhhead_literate + 
                   hhhead_sex + dependents, 
                 data=allhh, wlisthh)
summary(mod2)