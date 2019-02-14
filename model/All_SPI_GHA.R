library(sp)
library(spdep)
library(dplyr) 
library(broom)
library(lubridate)

#Only run this once!
cl <- makeCluster(5, outfile = '')
registerDoParallel(cl)

setwd('~/../mattcoop/ftf')

load('GHA_data.Rdata')

allchild$birth_month <- as.factor(month(ymd('2012-07-01') - months(allchild$age)))
allchild$birth_year <- as.factor(year(ymd('2012-07-01') - months(allchild$age)))

#####################
#Make spatial weights
######################
wlisthhs <- allhh %>%
  dplyr::select(longitude, latitude) %>%
  SpatialPoints(proj4string = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')) %>%
  spTransform(CRS('+proj=eqc +lon_0=-1.0546875000000004')) %>%
  coordinates %>%
  dnearneigh(0, 125000, longlat=F) %>%
  nb2listw(style="W")

#########################
#Regressions
#########################
############Child Nutrition############################

foreach(spi=c("spi12", "spi24", "spi36", "spi48", "spi60"), .packages=c('spdep')) %dopar% {
  
  haz_gha <- lm(as.formula(paste0(
    "haz ~ asset_index + admin1 + hh_size + hhhead_religion + mean_annual_precip + hhhead_age + hhhead_literate + hhhead_sex + dependents + gender + age + pop + birth_order + within24 + ",
    spi)),
    data=allchild, weights = as.numeric(allchild$norm_hhweight))
  
  whz_gha <- lm(as.formula(paste0(
    "whz ~ asset_index + admin1 + hh_size + hhhead_religion + mean_annual_precip + hhhead_age + hhhead_literate + hhhead_sex + dependents + gender + age + pop + birth_order + within24 + ",
    spi)),
    data=allchild, weights = as.numeric(allchild$norm_hhweight))
  
  save(list=c('whz_gha'),
       file=paste0('~/gha_child_', spi, '.Rdata'))
  
  system(paste0('./telegram.sh "', spi, ' is done for children in Ghana"'))
  
}

#############Household Hunger###############################

foreach(spi=c("spi12", "spi24", "spi36", "spi48", "spi60"), .packages=c('spdep')) %dopar% {

  hhs_gha <- errorsarlm(as.formula(paste0(
    "hhs ~ asset_index + admin1 + hh_size + mean_annual_precip + hhhead_religion + hhhead_age + hhhead_literate + pop + hhhead_sex + dependents + ",
    spi)),
    data=allhh, wlisthhs, tol.solve=10e-20, weights = as.numeric(allhh$norm_hhweight))

  save(list=c('hhs_gha'),
       file=paste0('~/gha_hh_', spi, '.Rdata'))

  system(paste0('./telegram.sh "', spi, ' is done for households in Ghana"'))

}

