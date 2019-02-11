setwd('G://My Drive/Feed the Future/')

library(ape)
library(dplyr)

getMorans <- function(x, y, outcome, data){
  dists <- as.matrix(dist(cbind(data[ , x], data[ , y])))
  
  dists.inv <- 1/dists
  diag(dists.inv) <- 0
  
  dists.inv[is.infinite(dists.inv)] <- 0
  
  Moran.I(data[ , outcome], dists.inv)
}


moran <- data.frame()

#####################
## Ghana
######################

load('GHA_data.Rdata')

#### Ghana Child
res <- getMorans('longitude_meters', 'latitude_meters', 'haz', allchild)
moran <- bind_rows(moran, data.frame(country='Ghana', variable='haz', res))

#### Ghana Child Residual
allchild$resid <- residuals(lm(haz ~ asset_index + hh_size + hhhead_religion + mean_annual_precip + hhhead_age + hhhead_literate + hhhead_sex + dependents + gender + age + pop + birth_order + within24 + spi24, 
                               data=allchild))
res <- getMorans('longitude_meters', 'latitude_meters', 'resid', allchild)
moran <- bind_rows(moran, data.frame(country='Ghana', variable='haz_resid', res))

#### Ghana HH
res <- getMorans('longitude_meters', 'latitude_meters', 'hhs', allhh)
moran <- bind_rows(moran, data.frame(country='Ghana', variable='hhs', res))

#### Ghana HH Residual
allhh$resid <- residuals(lm(hhs ~ asset_index + hh_size + mean_annual_precip + hhhead_religion + hhhead_age + hhhead_literate + pop + hhhead_sex + dependents + spi24, 
                               data=allhh))
res <- getMorans('longitude_meters', 'latitude_meters', 'resid', allhh)
moran <- bind_rows(moran, data.frame(country='Ghana', variable='hhs_resid', res))



#####################
## Bangladesh
######################

load('BGD_data.Rdata')

#### Bangladesh Child
res <- getMorans('longitude_meters', 'latitude_meters', 'haz', allchild)
moran <- bind_rows(moran, data.frame(country='Bangladesh', variable='haz', res))

#### Bangladesh Child Residual
allchild$resid <- residuals(lm(haz ~ asset_index + hh_size + hhhead_age + interview_month + hhhead_religion + hhhead_literate + hhhead_education + hhhead_sex + dependents + gender + age + pop + mean_annual_precip + spi24, 
                               data=allchild))
res <- getMorans('longitude_meters', 'latitude_meters', 'resid', allchild)
moran <- bind_rows(moran, data.frame(country='Bangladesh', variable='haz_resid', res))

#### Bangladesh HH
res <- getMorans('longitude_meters', 'latitude_meters', 'hhs', allhh)
moran <- bind_rows(moran, data.frame(country='Bangladesh', variable='hhs', res))

#### Bangladesh HH Residual
allhh$resid <- residuals(lm(hhs ~ asset_index + hh_size + hhhead_age + survey_year + interview_month + hhhead_religion + mean_annual_precip + hhhead_literate + hhhead_education + hhhead_sex + dependents + pop + spi24, 
                               data=allhh))
res <- getMorans('longitude_meters', 'latitude_meters', 'resid', allhh)
moran <- bind_rows(moran, data.frame(country='Bangladesh', variable='hhs_resid', res))

options(scipen=100)

for (n in names(moran)){
  if (is.numeric(moran[ , n])){
    moran[ , n] <- signif(moran[ , n], 3)
  }
}

write.csv(moran, 'ESDA.csv', row.names=F)
