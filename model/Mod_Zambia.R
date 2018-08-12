setwd('G://My Drive/Feed the Future/')

library(foreign)
library(dplyr)
library(readstata13)
library(tidyr)

options(stringsAsFactors = F)

######################################
#Prep for getting asset index from PCA
######################################

PCA_assets <- function(assets, ntiles){
  #https://academic.oup.com/heapol/article/21/6/459/612115/Constructing-socio-economic-status-indices-how-to
  
  #Drop NAs
  assets_narm <- na.omit(assets)
  
  #Pivot out factor columns
  for (n in names(assets_narm)){
    
    c <- class(assets_narm[ , n])
    if (c=='factor' | c=='character'){
      
      for (f in unique(assets_narm[ , n])){
        assets_narm[ , paste0(n, '_', f)] <- assets_narm[ , n] == f
      }
      
      assets_narm[ , n] <- NULL
      
    }
    
  }
  
  #Do PCA
  res <- prcomp(assets_narm)
  sumry <- summary(res)$importance[2, 1]*100
  
  cat('PC1 explains', sumry, 'percent of the variance')
  
  #get quantiles and cut vector
  out <- quantile(res$x[ , 1], probs=seq(0, 1, 1/ntiles))
  cuts <- cut(res$x[ , 1], out)
  
  #fill back in NAs based on row.names from original df
  temp <- data.frame(cuts=as.integer(cuts), rownames=row.names(assets_narm))
  
  assets$rownames <- row.names(assets)
  
  new <- merge(temp, assets, all=T)
  
  return(new$cuts)
}

##################################
#Extract hh vars
##################################
household <- read.dta('ZMB-RALS-12/Zambia ftf/Zambia_household_PR.dta')

household$asset_index <- PCA_assets(household[ , paste0('d0', seq(1, 8))], ntiles=5)
household <- household %>%
  mutate(cluster=as.factor(a02)) %>%
  select(hhs=hungerscale,
         pbs_id,
         hh_size,
         asset_index,
         cluster)

edumap <- data.frame(zic_edu_lvl=seq(0, 17),
                     hhhead_education=c('None', 'Standard', 'Standard', 'Standard', 'Standard', 'Standard', 'Standard', 'Standard', 'Form', 'Form', 'Form', 'Form', 'Form', 'Form', 'Form', 'University', 'University', 'University'))

hhmembers <- read.dta('ZMB-RALS-12/Zambia ftf/Zambia_hhmembers_PR.dta') %>%
  filter(c03 == 1) %>%
  filter(!duplicated(pbs_id) & c02!=99) %>%
  merge(edumap, all.x=T, all.y=F) %>%
  select(pbs_id, 
         hhhead_sex=c02, 
         hhhead_age=c04,
         hhhead_education,
         hhhead_literate=zic_literacy) %>%
  mutate(hhhead_sex=factor(hhhead_sex, levels=c(1, 2), labels=c('Male', 'Female')),
         hhhead_education=factor(hhhead_education, levels=c('None', 'Standard', 'Form', 'University')),
         hhhead_literate=hhhead_literate!=1 & hhhead_literate != 99)

depend_ratio <- read.dta('ZMB-RALS-12/Zambia ftf/Zambia_hhmembers_PR.dta') %>%
  mutate(dependant = c04 < 15 | c04 > 65,
         workers = c04 > 15 & c04 < 65) %>%
  group_by(pbs_id) %>%
  summarize(dependents=sum(dependant),
            workers=sum(workers))
         
WEAI_HH_m <- read.dta('ZMB-RALS-12/Zambia ftf/Zambia_WEAI_HH_PR.dta') %>%
  filter(a04==1) %>%
  mutate(male_5de=1-ci) %>%
  select(pbs_id, male_5de)

WEAI_HH_f <- read.dta('ZMB-RALS-12/Zambia ftf/Zambia_WEAI_HH_PR.dta') %>%
  filter(a04==2) %>%
  mutate(female_5de=1-ci) %>%
  select(pbs_id, female_5de)

WEAI_HH <- merge(WEAI_HH_f, WEAI_HH_m) %>%
  mutate(combo_5de=female_5de/(male_5de + female_5de))

match <- read.dta13('ZMB-RALS-12/Zambia ftf/georeferenced WEAI/Zambia_CrossWalkFile.dta') %>%
  mutate(hh_refno=paste0('ZAM-', cluster, '-', hh)) %>%
  mutate(pbs_id=PBS_ID) %>%
  select(-PBS_ID)

hh <- Reduce(f=function(x, y) merge(x, y, all.x=T, all.y=F), list(household, hhmembers, WEAI_HH, depend_ratio, match))

hh$pbs_id <- NULL

hh$year <- 2012

hh$dependents <- hh$dependents/hh$hh_size

#Remove errant points
hh <- hh %>%
  filter(!hh_refno %in% c('ZAM-3065-5', 'ZAM-3027-247', 'ZAM-3033-314', 'ZAM-3080-9'))

########################################
#Extract Landcover Vars
##################################################
lc <- read.csv('Landcover.csv') %>%
  unique %>%
  mutate(pop=pop/5625,
       market=market/24) %>%
  select(pop, market, hh_refno, year)

######################################
#read in SPI and combine
#####################################

spi <- read.csv('Coords&SPI.csv')

ltn <- read.csv('RainfallLTN.csv') %>%
  filter(year == 2012) %>%
  select(hh_refno, mean_annual_precip) %>%
  mutate(mean_annual_precip = mean_annual_precip/100) %>%
  unique

all <- Reduce(merge, list(hh, spi, lc, ltn)) %>% 
  select(cluster, hh_refno, hhs, hh_size, asset_index, hhhead_sex, hhhead_age, hhhead_education, hhhead_literate, female_5de, male_5de, combo_5de, 
         latitude, longitude, spi6, spi12, spi24, spi36, pop, market, dependents, mean_annual_precip) %>%
  na.omit

#####################
#Make spatial weights
######################
library(sp)
library(spdep)

sphh <- SpatialPoints(coords = all[ , c('longitude', 'latitude')], proj4string = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
spmhh <- spTransform(sphh, CRS('+proj=utm +zone=36 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs '))

distmat <- as.matrix(dist(spmhh@coords))
diag(distmat) <- 1000000
mins <- apply(distmat, 1, min)
max(mins)
#The most remote village is 57009 km away from other, so lag must be at least 73000

wlisthh <- spmhh %>%
  #knearneigh(k=4) %>%
  dnearneigh(0, 60000, longlat=F) %>%
  nb2listw(style="W")

#########################
#Regressions
#######################

#############Household Hunger###############################

#LM without Female Empowerment
lm1 <- lm(hhs~asset_index + dependents + hh_size + 
            hhhead_sex + hhhead_education + hhhead_literate +
            spi24 + pop + market + mean_annual_precip, data=all)
summary(lm1)


#Check for autocorrelation in the residuals
moran.test(lm2$residuals, wlisthh)
#No significant autocorrelation

splm1 <- errorsarlm(hhs~asset_index + dependents + hh_size + 
            hhhead_sex + hhhead_education + hhhead_literate +
            spi24 + pop + market + mean_annual_precip + spi24*mean_annual_precip, data=all, wlisthh)
summary(splm1)

write.csv(summary(splm1)$Coef %>% as.data.frame,
          'G://My Drive/Papers/Feed the Future/Zambia_Effects.csv')
