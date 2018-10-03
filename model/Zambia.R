library(sp)
library(spdep)
library(dplyr) 
library(broom)

setwd('G:/My Drive/Feed the Future/')

load('ZAM_data.Rdata')

#####################
#Make spatial weights
######################
library(sp)
library(spdep)

sphh <- SpatialPoints(coords = allhh[ , c('longitude', 'latitude')], proj4string = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
spmhh <- spTransform(sphh, CRS('+proj=utm +zone=36 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs '))
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
#######################

############Child Nutrition############################

#Check for autocorrelation in the outcome variable
moran.test(allchild$haz, wlistchild)

#Significant autocorrelation, use spatial lag model
mod <- errorsarlm(haz ~ hh_size + asset_index + hhhead_sex + hhhead_age + hhhead_education + hhhead_literate +
                spi24 + dependents + age + gender + precip_mean, data=allchild
                , wlistchild)
summary(mod)


#############Household Hunger###############################

#Check for autocorrelation in the outcome variable
moran.test(allhh$hhs, wlisthh)

#No significant autocorrelation, perform OLS, but check outcome vars as well
mod2 <- lm(hhs ~ hh_size + asset_index + hhhead_sex + hhhead_age + hhhead_education + hhhead_literate +
                    spi24 + dependents + precip_mean, data=allhh)
summary(mod2)

moran.test(residuals(mod2), wlisthh)


