library(sp)
library(spdep)
library(dplyr) 
library(broom)
library(strucchange)
library(lubridate)
library(foreach)
library(doParallel)

setwd('~/../mattcoop/ftf')

load('BGD_data.Rdata')

hhw <- read.csv('bangladesh_weights.csv')

allhh <- merge(allhh, hhw, allx=F, all.y=F)
allchild <- merge(allchild, hhw, allx=F, all.y=F)


#Only run this once!
cl <- makeCluster(5, outfile = '')
registerDoParallel(cl)

allhh$interview_month <- as.factor(allhh$interview_month)
allhh$survey_year <- as.factor(allhh$survey_year)
allchild$interview_month <- as.factor(allchild$interview_month)
allchild$survey_year <- as.factor(allchild$survey_year)

######################
#Build spatial weights
######################
wlisthhs <- allhh %>%
  dplyr::select(longitude, latitude) %>%
  SpatialPoints(proj4string = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')) %>%
  spTransform(CRS('+proj=eqdc +lat_1=22.191291158578405 +lat_2=25.41960806605085 +lon_0=90.703125')) %>%
  coordinates %>%
  dnearneigh(0, 80000, longlat=F) %>%
  nb2listw(style="W")

wlisthaz <- allchild %>%
  dplyr::select(longitude, latitude) %>%
  SpatialPoints(proj4string = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')) %>%
  spTransform(CRS('+proj=eqdc +lat_1=22.191291158578405 +lat_2=25.41960806605085 +lon_0=90.703125')) %>%
  coordinates %>%
  dnearneigh(0, 30000, longlat=F) %>%
  nb2listw(style="W")

wlistwhz <- allchild %>%
  dplyr::select(longitude, latitude) %>%
  SpatialPoints(proj4string = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')) %>%
  spTransform(CRS('+proj=eqdc +lat_1=22.191291158578405 +lat_2=25.41960806605085 +lon_0=90.703125')) %>%
  coordinates %>%
  dnearneigh(0, 20000, longlat=F) %>%
  nb2listw(style="W")

#########################
#Regressions
#######################
############Child Nutrition############################


foreach(spi=c("spi12", "spi24", "spi36", "spi48", "spi60"), .packages=c('spdep')) %dopar% {

  haz_bgd <- errorsarlm(as.formula(paste0(
    "haz ~ asset_index + admin1 + hh_size + hhhead_age + interview_month + hhhead_religion + hhhead_literate + hhhead_education + hhhead_sex + dependents + gender + age + pop + mean_annual_precip + irrigation + ",
    spi)),
    data=allchild, wlisthaz, tol.solve=10e-20, weights=allchild$hhweightR1)
   
  whz_bgd <- errorsarlm(as.formula(paste0(
    "whz ~ asset_index + admin1 + hh_size + hhhead_age + interview_month + hhhead_religion + hhhead_literate + hhhead_education + hhhead_sex + dependents + gender + age + pop + mean_annual_precip + irrigation + ",
    spi)),
    data=allchild, wlistwhz, tol.solve=10e-20, weights=allchild$hhweightR1)

  save(list=c('haz_bgd', 'whz_bgd'),
       file=paste0('~/bgd_child_', spi, '.Rdata'))

  system(paste0('./telegram.sh "', spi, ' is done for children in Bangladesh"'))

 }

#############Household Hunger###############################

foreach(spi=c("spi12", "spi24", "spi36", "spi48", "spi60"), .packages=c('spdep')) %dopar% {

for (spi in c("spi12", "spi24", "spi36", "spi48", "spi60")){
  print(spi)
  
  allhh <- allhh %>% arrange(survey_year)

  chow_bgd <- sctest(as.formula(paste0(
    "hhs ~ asset_index + admin1 + hh_size + hhhead_age + survey_year + interview_month + hhhead_religion + mean_annual_precip + hhhead_literate + hhhead_education + hhhead_sex + dependents + pop + irrigation + ",
    spi)), type='Chow', data=allhh, point=table(allhh$survey_year)[1], weights=allhh$hhweightR1)
  
  hhs_bgd <- errorsarlm(as.formula(paste0(
    "hhs ~ asset_index + admin1 + hh_size + hhhead_age + survey_year + interview_month + hhhead_religion + mean_annual_precip + hhhead_literate + hhhead_education + hhhead_sex + dependents + pop + irrigation + ",
    spi)),
    data=allhh, wlisthhs, tol.solve=10e-20, weights=allhh$hhweightR1)
  
  save(list=c('chow_bgd', 'hhs_bgd'),
       file=paste0('~/bgd_hh_', spi, '.Rdata'))
  
  system(paste0('./telegram.sh "', spi, ' is done for households in Bangladesh"'))
  
}



