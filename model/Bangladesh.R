library(sp)
library(spdep)
library(dplyr) 
library(broom)

setwd('G:/My Drive/Feed the Future/')

load('BGD_data.Rdata')

######################
#Build spatial weights
######################
sphh <- SpatialPoints(coords = allhh[ , c('longitude', 'latitude')], proj4string = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
spmhh <- spTransform(sphh, CRS('+proj=utm +zone=30 +ellps=clrk80 +towgs84=-124.76,53,466.79,0,0,0,0 +units=m +no_defs '))
distmat <- as.matrix(dist(spmhh@coords))
diag(distmat) <- 1000000
mins <- apply(distmat, 1, min)
cat("The most remote village is", max(mins), "km away from others, so lag must be at least", max(mins))

dnnhh <- dnearneigh(coordinates(spmhh), 0, 75000, longlat=F)
wlisthh <- nb2listw(dnnhh, style="W", zero.policy=TRUE )

spchild <- SpatialPoints(coords = allchild[ , c('longitude', 'latitude')], proj4string = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
spmchild <- spTransform(spchild, CRS('+proj=utm +zone=30 +ellps=clrk80 +towgs84=-124.76,53,466.79,0,0,0,0 +units=m +no_defs '))
distmat <- as.matrix(dist(spmchild@coords))
diag(distmat) <- 1000000
mins <- apply(distmat, 1, min)
cat("The most remote village is", max(mins), "km away from others, so lag must be at least", max(mins))

dnnchild  <- dnearneigh(coordinates(spmchild), 0, 75000, longlat=F)
wlistchild <- nb2listw(dnnchild, style="W", zero.policy=TRUE )


########################################
#Let's do a quick Chow test
#######################################
library(strucchange)
library(splm)
library(plm)

allhh <- allhh %>% arrange(year)

sctest(hhs~hhhead_education + hhhead_literate + hhhead_religion + hh_size + hhhead_sex +
         irrigation + dependents + asset_score  + 
         pop + market + spi24 + year, type='Chow', data=allhh)

#Looks like we can treat both years as the same?

#########################
#Regressions
#######################
############Child Nutrition############################

#Check for autocorrelation in the outcome variable
moran.test(allchild$haz, wlistchild)

#Significant autocorrelation, use spatial error model
mod <- errorsarlm(haz ~ asset_index + pop + market + hh_size + spi24 + hhhead_religion + precip_mean + year + perc_irrig + 
                     hhhead_literate + hhhead_education + hhhead_sex + dependents + gender + age + birth_order,  
                  data=allchild, wlistchild)
summary(mod)

#############Household Hunger###############################

#Check for autocorrelation in the outcome variable
moran.test(allhh$hhs, wlisthh)

#Significant autocorrelation, use spatial error model
mod2 <- errorsarlm(haz ~ asset_index + pop + market + hh_size + spi24 + hhhead_religion + precip_mean + year + perc_irrig + 
                     hhhead_literate + hhhead_education + hhhead_sex + dependents,  
                   data=allchild, wlistchild)
summary(mod2)
