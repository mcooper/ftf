setwd('G://My Drive/Feed the Future/')

library(foreign)
library(dplyr)
library(readstata13)
library(tidyr)

options(stringsAsFactors = F)

source('C://Git/ftf/utils/utils.R')

############################
#Extract hh vars
############################

hh <- read.dta13('Bangladesh/4. BIHS_household_2011_15.dta') %>%
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
         hhhead_religion=factor(hhhead_religion, labels=c("Muslim", "Hindu", "Christian")),
         dependents=dependents/hh_size,
         workers=workers/hh_size,
         year=as.integer(as.character(year)))

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

hhcluster <- read.dta("Bangladesh/BIHS Raw Data (2011)/001_mod_a_male.dta") %>%
  mutate(hh_refno=paste0('BGD-', a01),
         cluster=as.factor(vcode_n)) %>%
  select(hh_refno, cluster)

child <- read.dta13('Bangladesh/5. BIHS_child_2011_15.dta') %>%
  mutate(hh_refno=paste0('BGD-', a01),
         year=as.factor(survey_year),
         gender=ifelse(gender==1, 'Male',
                       ifelse(gender==2, 'Female', gender))) %>%
  select(haz,
         waz,
         whz,
         year,
         hh_refno,
         age,
         gender)

###################
#read in Geo Vars
###############
irrig <- read.csv('BGD_perc_irrig.csv') %>%
  select(hh_refno, perc_irrig) %>%
  unique

lc <- read.csv('Landcover.csv') %>%
  select(hh_refno, year, pop, market) %>%
  unique
lc$year[lc$year==2012 & grepl('BGD', lc$hh_refno)] <- 2011

spi <- read.csv('AllFtfPrecipIndices.csv')
spi$year[spi$year==2012 & grepl('BGD', spi$hh_refno)] <- 2011

######################################
#Combine
#####################################

sperr <- c(489, 905, 908, 955, 961, 1021, 1048, 1119, 1241, 1242, 1243, 1244, 1245, 1247, 1248, 1249, 1250, 1251, 1252, 1253, 1254, 1255, 1256, 1257, 1258, 1259, 1260, 1560, 2160, 2201, 2346, 2360, 2364, 2478, 2524, 2644, 2719, 2901, 2922, 3042, 3301, 3307, 3735, 3913, 3943, 4195, 4243, 4309, 4354, 4620, 4876, 4915, 4919, 5015, 5026, 5102, 5103, 5183, 5621, 5703, 5779, 5785, 5813, 5817, 5858, 5884, 5921, 5966, 5993, 6103, 6183, 6218, 6354, 6356, 6378, 6460)
sperr <- paste0('BGD-', sperr)

allhh <- Reduce(merge, list(hh, hhs, hhcluster, irrig, lc, spi)) %>%
  filter(!hh_refno %in% sperr & market >= 8) %>%
  select(hhs, year, hh_refno, hhhead_education, hhhead_literate, hhhead_sex, 
         hhhead_religion, hh_size, irrigation, dependents, workers, asset_index, 
         cluster, perc_irrig, pop, market, latitude, longitude, spei12, spei24, 
         spei36, spi12, spi24, spi36, precip_mean,  tmin_mean, tmax_mean, spei12gs, 
         spei24gs, spei36gs, spi12gs, spi24gs, spi36gs) %>%
  mutate(spi24sq=spi24^2,
         precip_mean=(precip_mean*12)/1000) %>%
  na.omit

allchild <- merge(child, allhh, all.x=T, all.y=F) %>%
  na.omit

save(file="BGD_data.Rdata", list=c("allhh", "allchild"))
