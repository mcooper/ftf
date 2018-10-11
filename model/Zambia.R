library(sp)
library(spdep)
library(dplyr) 
library(broom)

setwd('G:/My Drive/Feed the Future/')

load('ZAM_data.Rdata')

buffer <- 30000

#####################
#Make spatial weights
######################
sphh <- SpatialPoints(coords = allhh[ , c('longitude', 'latitude')], proj4string = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
spmhh <- spTransform(sphh, CRS('+proj=eqc +lon_0=30.761718749999996'))
distmat <- as.matrix(dist(spmhh@coords))
diag(distmat) <- 1000000
mins <- apply(distmat, 1, min)
cat("The most remote village is", max(mins), "km away from others, so lag must be at least", max(mins))

dnnhh <- dnearneigh(coordinates(spmhh), 0, buffer, longlat=F)
wlisthh <- nb2listw(dnnhh, style="W")

spchild <- SpatialPoints(coords = allchild[ , c('longitude', 'latitude')], proj4string = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
spmchild <- spTransform(spchild, CRS('+proj=eqc +lon_0=30.761718749999996'))
distmat <- as.matrix(dist(spmchild@coords))
diag(distmat) <- 1000000
mins <- apply(distmat, 1, min)
cat("The most remote village is", max(mins), "km away from others, so lag must be at least", max(mins))

dnnchild  <- dnearneigh(coordinates(spmchild), 0, buffer, longlat=F)
wlistchild <- nb2listw(dnnchild, style="W")

#########################
#Regressions
#######################

############Child Nutrition############################

allhh$spi24sq <- allhh$spi24^2
allchild$spi24sq <- allchild$spi24^2

#Check for autocorrelation in the outcome variable
moran.test(allchild$haz, wlistchild)

#Significant autocorrelation, use spatial lag model
mod <- errorsarlm(haz ~ hh_size + asset_index + hhhead_sex + hhhead_age + hhhead_education + hhhead_literate +
                spi24 + spi24sq + dependents + age + gender + precip_mean + pop, data=allchild
                , wlistchild)
summary(mod)

moran.test(residuals(mod), wlistchild)

#############Household Hunger###############################

#Check for autocorrelation in the outcome variable
moran.test(allhh$hhs, wlisthh)

#No significant autocorrelation, perform OLS, but check outcome vars as well
mod2 <- errorsarlm(hhs ~ hh_size + asset_index + hhhead_sex + hhhead_age + hhhead_education + hhhead_literate +
                    spi24 + spi24sq + dependents + precip_mean + pop, data=allhh, wlisthh)
summary(mod2)

moran.test(residuals(mod2), wlisthh)


