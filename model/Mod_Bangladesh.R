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
         female_5de=f_weai_fivede,
         male_5de=m_weai_fivede,
         workers=memb_15_44 + memb_45_65,
         dependents=memb_und15 + memb_65plus,
         irrigation=bio_a_rice_i/(bio_a_rice_r + bio_a_rice_i),
         year=as.factor(survey_year)) %>%
  select(hhhead_education,
         hhhead_literate,
         hhhead_sex=hhhead_gender,
         hhhead_religion,
         hh_size=memb_total,
         female_5de, #Female score on five domains of empowerment
         male_5de, #Male score on five domains of empowerment
         year,
         hh_refno,
         irrigation,
         dependents,
         workers,
         fcs) %>%
  mutate(combo_5de=female_5de/(female_5de + male_5de),
         hhhead_education=as.factor(hhhead_education),
         hhhead_literate=hhhead_literate!='cannot read and write',
         hhhead_religion=factor(hhhead_religion, labels=c("Muslim", "Hindu", "Christian")),
         hhhead_sex=factor(hhhead_sex, labels=c("Male", "Female")))

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

hh$asset_index <- read.dta13('Bangladesh/4. BIHS_household_2011_15.dta') %>%
  select(land_total,land_rented,land_owned_rentedout,land_owned_notoperated,land_owned_operated,
         house_owned,house_rooms,house_electricity,house_watersource,house_toilet,house_roof,
         house_walls,house_floor,asset_qty_cattle,asset_qty_poultry,asset_qty_sheepgoat,
         asset_qty_otherlivestock,fertilizer) %>%
  PCA_assets(ntiles = 5)

hh$dependents <- hh$dependents/hh$hh_size

sperr <- c(489, 905, 908, 955, 961, 1021, 1048, 1119, 1241, 1242, 1243, 1244, 1245, 1247, 1248, 1249, 1250, 1251, 1252, 1253, 1254, 1255, 1256, 1257, 1258, 1259, 1260, 1560, 2160, 2201, 2346, 2360, 2364, 2478, 2524, 2644, 2719, 2901, 2922, 3042, 3301, 3307, 3735, 3913, 3943, 4195, 4243, 4309, 4354, 4620, 4876, 4915, 4919, 5015, 5026, 5102, 5103, 5183, 5621, 5703, 5779, 5785, 5813, 5817, 5858, 5884, 5921, 5966, 5993, 6103, 6183, 6218, 6354, 6356, 6378, 6460)
sperr <- paste0('BGD-', sperr)

hh <- hh %>%
  filter(!hh_refno %in% sperr)

########################################
#Extract Geographic Vars
#########################################
lc <- read.csv('Landcover.csv') %>%
  select(hh_refno, year, pop, market) %>%
  unique %>%
  mutate(pop=pop/5625,
         market=market/24)

lc$year[lc$year==2012 & grepl('BGD', lc$hh_refno)] <- 2011

###################
#Also Irrigation
###############
irrig <- read.csv('BGD_perc_irrig.csv') %>%
  select(hh_refno, perc_irrig) %>%
  mutate(perc_rainfed = 1-perc_irrig) %>%
  unique

######################################
#read in SPI and combine
#####################################

spi <- read.csv('Coords&SPI.csv')
spi$year[spi$year==2012 & grepl('BGD', spi$hh_refno)] <- 2011

ltn <- read.csv('RainfallLTN.csv') %>%
  mutate(mean_annual_precip = mean_annual_precip/100) %>%
  select(hh_refno, year, mean_annual_precip)

ltn$year[ltn$year==2012 & grepl('BGD', ltn$hh_refno)] <- 2011

all <- Reduce(merge, list(hh, lc, spi, hhs, ltn, irrig)) %>%
  select(hhs, year, hh_refno, hhhead_education, hhhead_literate, hhhead_sex, hhhead_religion, hh_size,
         female_5de, male_5de, irrigation, dependents, workers, combo_5de, asset_index, perc_irrig, perc_rainfed,
         pop, market, latitude, longitude, spi6, spi12, spi24, spi36, fcs, mean_annual_precip) %>%
  #filter(irrigation < 0.4) %>% 
  na.omit

############################################
# Make balanced panel - not necessary now
#############################################
# hhcount <- table(all$hh_refno)
# doubles <- names(hhcount)[hhcount==2]
# 
# all <- all %>%
#   filter(!hh_refno %in% doubles)

all <- all %>% filter(perc_rainfed > 0.75)

######################
#Build spatial weights
######################
library(sp)
library(spdep)

sphh <- SpatialPoints(coords = all[ , c('longitude', 'latitude')], proj4string = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
spmhh <- spTransform(sphh, CRS('+proj=utm +zone=36 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs '))

distmat <- as.matrix(dist(spmhh@coords))
diag(distmat) <- 1000000
distmat[distmat == 0] <- 1000000
mins <- apply(distmat, 1, min)
max(mins)

#The most remote village is 25080 km away from other, so lag must be at least 26000
rm(distmat)

wlisthh <- spmhh %>%
  #knearneigh(k=4) %>%
  dnearneigh(0, 45000, longlat=F) %>%
  nb2listw(style="W")


########################################
#Let's do a quick Chow test
#######################################
library(strucchange)
library(splm)
library(plm)

all <- all %>% arrange(year)

sctest(hhs~hhhead_education + hhhead_literate + hhhead_religion + hh_size + hhhead_sex +
         irrigation + dependents + asset_index  + 
         pop + market + spi24 + year, type='Chow', point=table(all$year)[1], data=all)

#Looks like we can treat both years as the same

#########################
#Regressions
#######################

#############Linear Regressions###############################


#LM 
lm1 <- lm(hhs~asset_index + dependents + hhhead_education + 
            hhhead_literate + hhhead_religion + hh_size + hhhead_sex +
            pop + market + perc_rainfed*spi24 + year +  + spi24*mean_annual_precip
            perc_rainfed*mean_annual_precip, 
          data=all)
summary(lm1)

#Moran's I is very significant
moran.test(residuals(lm2), wlisthh)

#SPLM 
splm1 <- errorsarlm(hhs~asset_index + dependents + hhhead_education + 
            hhhead_literate + hhhead_religion + hh_size + hhhead_sex +
            pop + market + spi24 + year + 
            mean_annual_precip + spi24*mean_annual_precip, 
          data=all, wlisthh)
summary(splm1)

write.csv(summary(splm1)$Coef %>% as.data.frame,
          'G://My Drive/Papers/Feed the Future/Bangladesh_Effects.csv')
