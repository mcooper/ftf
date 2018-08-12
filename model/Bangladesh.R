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

############################
#Extract hh vars
############################

hh <- read.dta13('Bangladesh/4. BIHS_household_2011_15.dta') %>%
  #filter(bio_y_rice_i==0) %>%
  mutate(hh_refno=paste0('BGD-', a01),
         workers=memb_15_44 + memb_45_65,
         dependents=memb_und15 + memb_65plus,
         irrigation=bio_a_rice_i/(bio_a_rice_r + bio_a_rice_i),
         year=as.factor(survey_year)) %>%
  select(hhhead_education,
         hhhead_literate,
         hhhead_sex=hhhead_gender,
         hhhead_religion,
         hh_size=memb_total,
         year,
         hh_refno,
         irrigation,
         dependents,
         workers) %>%
  mutate(hhhead_education=as.factor(hhhead_education),
         hhhead_literate=hhhead_literate!='cannot read and write',
         hhhead_religion=factor(hhhead_religion, labels=c("Muslim", "Hindu", "Christian")))

hhs11 <- read.dta13('Bangladesh/BIHS Raw Data (2011)/051_mod_x3_female.dta') %>%
  mutate(x3_01=ifelse(x3_01=='no', 0, 1),
         x3_03=ifelse(x3_03=='no', 0, 1),
         x3_05=ifelse(x3_05=='no', 0, 1),
         x3_02=ifelse(is.na(x3_02), 0, 
                      ifelse(x3_02=="often (> 10 times)", 2, 1)),
         x3_04=ifelse(is.na(x3_04), 0, 
                      ifelse(x3_04=="often (> 10 times)", 2, 1)),
         x3_06=ifelse(is.na(x3_06), 0, 
                      ifelse(x3_06=="often (> 10 times)", 2, 1)),
         hhs=x3_01*x3_02 + x3_03*x3_04 + x3_05*x3_06,
         hh_refno=paste0('BGD-', a01),
         year=2011) %>%
  select(hh_refno, hhs, year)

hhs15 <- read.dta13('Bangladesh/BIHS Raw Data (2015)/068_r2_mod_x3_female.dta') %>%
  mutate(x3_01=ifelse(x3_01=='no', 0, 1),
         x3_03=ifelse(x3_03=='no', 0, 1),
         x3_05=ifelse(x3_05=='no', 0, 1),
         x3_02=ifelse(is.na(x3_02), 0, 
                      ifelse(x3_02=="Often (> 10 times)", 2, 1)),
         x3_04=ifelse(is.na(x3_04), 0, 
                      ifelse(x3_04=="Often (> 10 times)", 2, 1)),
         x3_06=ifelse(is.na(x3_06), 0, 
                      ifelse(x3_06=="Often (> 10 times)", 2, 1)),
         hhs=x3_01*x3_02 + x3_03*x3_04 + x3_05*x3_06,
         hh_refno=paste0('BGD-', a01),
         year=2015) %>%
  select(hh_refno, hhs, year)

hhs <- bind_rows(hhs11, hhs15)                

edumap <- data.frame(hhhead_education=c("reads in class i", "completed class i", "completed class 2", 
                           "completed class 3", "completed class 4", "completed class 5", 
                           "completed class 6", "completed class 7", "completed class 8", 
                           "completed class 9", "completed ssc/dakhil", "hsc 1st year", 
                           "completed hsc/alim", "degree 1st year", "ba/bsc pass/fazil", 
                           "ba/bsc honors/fazil", "ma/msc and above/kamil", "ssc candidate", 
                           "hsc candidate", "preschool class (general)", "preschool (mosque based)", 
                           "medical/mbbs ", "nursing", "engineer", "diploma engineer", "vocational ", 
                           "other ", "never attended school"),
                     new=c('primary', 'primary', 'primary',
                           'primary', 'primary', 'primary', 
                           'primary', 'primary', 'primary', 
                           'primary', 'secondary', 'high school',
                           'high school', 'university', 'secondary', 
                           'university',  'university',  'university', 
                           'high school', 'primary', 'primary', 
                           'high school', 'high school', 'high school', 'high school', 'high school',
                           'high school', 'never'))

hh <- merge(hh, edumap, all.x=T, all.y=F) %>%
  mutate(hhhead_education=factor(new, levels=c('never', 'primary', 'secondary', 'high school', 'university')),
         new=NULL)

hh$asset_score <- read.dta13('Bangladesh/4. BIHS_household_2011_15.dta') %>%
  select(land_total,land_rented,land_owned_rentedout,land_owned_notoperated,land_owned_operated,
         house_owned,house_rooms,house_electricity,house_watersource,house_toilet,house_roof,
         house_walls,house_floor,asset_qty_cattle,asset_qty_poultry,asset_qty_sheepgoat,
         asset_qty_otherlivestock,fertilizer) %>%
  PCA_assets(ntiles = 5)

hhcluster <- read.dta("Bangladesh/BIHS Raw Data (2011)/001_mod_a_male.dta") %>%
  mutate(hh_refno=paste0('BGD-', a01),
         cluster=as.factor(vcode_n)) %>%
  select(hh_refno, cluster)

hh <- merge(hh, hhcluster)

sperr <- c(489, 905, 908, 955, 961, 1021, 1048, 1119, 1241, 1242, 1243, 1244, 1245, 1247, 1248, 1249, 1250, 1251, 1252, 1253, 1254, 1255, 1256, 1257, 1258, 1259, 1260, 1560, 2160, 2201, 2346, 2360, 2364, 2478, 2524, 2644, 2719, 2901, 2922, 3042, 3301, 3307, 3735, 3913, 3943, 4195, 4243, 4309, 4354, 4620, 4876, 4915, 4919, 5015, 5026, 5102, 5103, 5183, 5621, 5703, 5779, 5785, 5813, 5817, 5858, 5884, 5921, 5966, 5993, 6103, 6183, 6218, 6354, 6356, 6378, 6460)
sperr <- paste0('BGD-', sperr)

hh <- hh %>%
  filter(!hh_refno %in% sperr)

###################
#Also Irrigation
###############
irrig <- read.csv('BGD_perc_irrig.csv') %>%
  select(hh_refno, perc_irrig) %>%
  unique

######################################
#read in SPI and combine
#####################################

spi <- read.csv('Coords&SPI.csv')
spi$year[spi$year==2012 & grepl('BGD', spi$hh_refno)] <- 2011

ltn <- read.csv('RainfallLTN.csv') %>%
  select(hh_refno, year, mean_annual_precip)

ltn$year[ltn$year==2012 & grepl('BGD', ltn$hh_refno)] <- 2011

allhh <- Reduce(merge, list(hh, lc, spi, hhs, ltn, hhcluster, irrig)) %>%
  select(hhs, year, hh_refno, hhhead_education, hhhead_literate, hhhead_sex, hhhead_religion, hh_size,
         irrigation, dependents, workers, asset_score, cluster, perc_irrig,
         pop, market, latitude, longitude, spi24, mean_annual_precip) %>%
  na.omit

######################
#Build spatial weights
######################
library(sp)
library(spdep)

sphh <- SpatialPoints(coords = allhh[ , c('longitude', 'latitude')], proj4string = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
spmhh <- spTransform(sphh, CRS('+proj=utm +zone=36 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs '))

distmat <- as.matrix(dist(spmhh@coords))
diag(distmat) <- 1000000
distmat[distmat == 0] <- 1000000
mins <- apply(distmat, 1, min)
max(mins)
#The most remote village is 13160.52 km away from other, so lag must be at least 13160.52
rm(distmat)

wlisthh <- spmhh %>%
  #knearneigh(k=4) %>%
  dnearneigh(0, 65000, longlat=F) %>%
  nb2listw(style="W")

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

##MORAN'S I TEST
moran.test(allhh$hhs, wlisthh)


#For both stunting and HHS:
#Spatial Autocorrelation
#Chow test says both years are the same

#########################
#Regressions
#######################

#############Household Hunger###############################

#Significant autocorrelation, use sptail lag model
mod <- lagsarlm(hhs~hhhead_education + hhhead_literate + hh_size + hhhead_sex +
                   + dependents + asset_score  + perc_irrig + 
                   pop + market + spi24 + year + mean_annual_precip, 
                 data=allhh, wlisthh)
summary(mod)

###########################
##First order differencing
###########################

hh11 <- allhh[allhh$year==2011, ] %>%
  select(hh_refno,
         spi24_11=spi24,
         hhs_11=hhs)
hh15 <- allhh[allhh$year==2015, ]

hh <- merge(hh11, hh15)

hh$spi24_dif <- hh$spi24 - hh$spi24_11
hh$hhs <- hh$hhs - hh$hhs_11

mod6 <- lm(hhs~spi24_dif, data=hh)
summary(mod6)

