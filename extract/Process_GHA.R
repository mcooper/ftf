setwd('G:/My Drive/Feed the Future/')

library(foreign)
library(dplyr)
library(readstata13)
library(tidyr)

options(stringsAsFactors = F)

source('C://Git/ftf/utils/utils.R')

############################
#Extract hh and child vars
############################
greater_less <- function(vect, cut){
  seq <-NULL
  for (i in 1:length(vect)){
    ct <- sum(vect < vect[i] + cut & vect > vect[i] - cut) - 1
    seq <- c(seq, ct)
  }
  seq
}

#Child Vars
child_basic <- read.csv('GHA-PBS-12/Ghana FTF datasets/MODULE 8 - CHILDREN ANTHROPOMETRY.csv') %>%
  select(hh_refno=hhserial,
         ind_refno=mid,
         age=q8_4,
         gender=q8_5,
         weight=q8_6,
         height=q8_7) %>%
  mutate(gender=as.factor(gender))

nutritionPrep()
igrowup.standard(mydf = child_basic,
                 sex = gender,
                 age = age,
                 age.month = T, 
                 weight = weight, 
                 lenhei = height)
matz$zlen[matz$flen==1] <- NA
matz$zwei[matz$fwei==1] <- NA
matz$zwfl[matz$fwfl==1] <- NA
matz <- matz[matz$age > 6, ]
matz <- matz %>%
  select(hh_refno, ind_refno, haz=zlen, waz=zwei, whz=zwfl, gender, age)

childem <- read.csv('GHA-PBS-12/Ghana FTF datasets/MODULE 2 - HOUSEHOLD DEMOGRAPHY.csv') %>%
  filter(q2_03 %in% c('Child (Son)', 'Child (Daughter)')) %>%
  mutate(q2_04=ifelse(q2_05=='Years', q2_04*12, q2_04)) %>%
  arrange(hhserial, desc(q2_04)) %>%
  group_by(hhserial) %>%
  mutate(birth_order=row_number(),
         within24=greater_less(q2_04, 24)) %>%
  select(hh_refno=hhserial,
         ind_refno=mid,
         birth_order,
         within24)

child <- Reduce(f=function(x, y){merge(x, y, all.x=T, all.y=F)}, list(child_basic, childem, matz)) %>%
  mutate(hh_refno=paste0('GHA-', hh_refno)) %>%
  na.omit

#Household Vars
assets <- read.csv('GHA-PBS-12/Ghana FTF datasets/MODULE 9 - DWELLING CHARACTERISTICS.csv')

assets$asset_index <- PCA_assets(assets[ , c("q9_01", "q9_02", "q9_03", "q9_04", 
                                             "q9_05", "q9_06", "q9_07", "q9_08", "q9_09", "q9_10", "q9_11", 
                                             "q9_12", "q9_13", "q9_14", "q9_15", "q9_16", "q9_17", "q9_18", 
                                             "q9_19")],
                                 ntiles = 5)
assets <- assets %>% 
  select(hh_refno=hhserial, asset_index)

hunger <- read.csv('GHA-PBS-12/Ghana FTF datasets/MODULE 3A - HOUSEHOLD HUNGER SCALE.csv') %>%
  mutate(q1 = ifelse(q3_01=='No', 0, ifelse(q3_02=="Often (more than 10 times)", 2, 1)),
         q2 = ifelse(q3_03=='No', 0, ifelse(q3_04=="Often (more than 10 times)", 2, 1)),
         q3 = ifelse(q3_05=='No', 0, ifelse(q3_06=="Often (more than 10 times)", 2, 1)),
         hhs = q1 + q2 + q3) %>%
  select(hh_refno=hhserial, hhs)

relmap <- data.frame(hhhead_religion=c("Ahmadi", "Catholic", "Islam", "No Religion", "Other", "Other Christian", 
                                       "Pentecostal/Charismatic", "Protestant (Anglican, Lutheran, Presbyterian, Methodist, etc)", 
                                       "Traditionalist"),
                     new_rel=c('Muslim', 'Christian', 'Muslim', 'None', 'Traditional', 'Christian',
                               'Christian', 'Christian', 'Traditional'))

hhdem <- read.csv('GHA-PBS-12/Ghana FTF datasets/MODULE 1 - HOUSEHOLD IDENTIFICATION.csv') %>%
  mutate(hh_size = as.integer(hhsize),
         cluster = as.factor(eacode)) %>%
  select(hh_refno=hhserial, hh_size, hhhead_sex=hhhsex, hhhead_religion=q1_11, cluster,
         admin1=reg, admin2=dist, hhweight, norm_hhweight) %>%
  merge(relmap, all.x=T, all.y=F) %>%
  mutate(hhhead_religion=as.factor(new_rel),
         new_rel=NULL,
         hhhead_sex=factor(hhhead_sex, levels=c('Male', 'Female')))

levels(hhdem$hhhead_sex) <- c('male', 'female')

depend_ratio <- read.csv('GHA-PBS-12/Ghana FTF datasets/MODULE 2 - HOUSEHOLD DEMOGRAPHY.csv') %>%
  mutate(dependent = ageyrs < 12 | ageyrs > 60,
         workers = ageyrs > 12 & ageyrs < 60) %>%
  group_by(hhserial) %>%
  summarize(dependents=sum(dependent),
            workers=sum(workers)) %>%
  select(hh_refno=hhserial, dependents, workers)

hhchar <- read.csv('GHA-PBS-12/Ghana FTF datasets/MODULE 2 - HOUSEHOLD DEMOGRAPHY.csv') %>%
  filter(status=='Primary Respondent') %>%
  mutate(hhhead_literate=(q2_07!='Cannot Read and Write English' | q2_07a != 'None'),
         hhhead_education=q2_10!='',
         urban_rural=as.factor(urbrur)) %>%
  select(hh_refno=hhserial, urban_rural, hhhead_age=ageyrs, hhhead_literate, hhhead_education)

#################################
#' Missing:
#' FCS - there is detailed food consumption data for the previous week,
#' but now way to get a count of days for each category.  It's too detailed!


##########################################
#read in Geo Vars
##########################################
spi_child <- read.csv('PrecipIndices_Child.csv') %>%
  select(hh_refno, ind_refno, age, latitude, longitude,
         month, survey_date, spi_age, spi_ageutero)

spi_hh <- read.csv('PrecipIndices_HH.csv') %>%
  select(hh_refno, latitude, longitude, mean_annual_precip,
         matches("spi"))

lc <- read.csv('Landcover.csv') %>%
  mutate(hh_refno=gsub("GHA-", "", hh_refno)) %>%
  select(hh_refno, year, pop) %>%
  unique

##########################################
#Combine
##########################################

sperr <- paste0('GHA-', c('1080303601', '1080505107', '1081513210', '1081915810', '1082016411', '2071400902', '2071901212', '2072001709', '2072001810', '2072002216', '2072202707', '2080203317', '2081008715', '2090317212', '2090819514', '2100321209', '2100521616', '2100722309'))

allhh <- Reduce(merge, list(hhdem, hunger, assets, depend_ratio, hhchar, lc)) %>%
  mutate(hh_refno=paste0('GHA-', hh_refno)) %>%
  merge(spi_hh) %>%
  filter(!hh_refno %in% sperr & urban_rural == 'Rural') %>%
  mutate(dependents=dependents/hh_size,
         workers=workers/hh_size) %>%
  select(hhs, asset_index, hh_size, hh_refno, cluster, hhweight, norm_hhweight,
         hhhead_religion, hhhead_age, hhhead_literate, hhhead_sex, 
         dependents, workers, admin1, admin2, latitude, pop,
         longitude, spi12, spi24, spi36, spi48, spi60, spi12_lag, 
         spi24_lag, spi36_lag, spi48_lag, spi60_lag, mean_annual_precip) %>%
  na.omit

allchild <- Reduce(function(x,y){merge(x,y, all.x=T, all.y=F)}, list(child, spi_child, allhh)) %>%
  na.omit

#Get coords in meters
library(sp)

allhh_sp <- SpatialPointsDataFrame(allhh[ , c('longitude', 'latitude')], data=allhh, proj4string = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')) %>%
  spTransform(CRS('+proj=eqc +lon_0=-1.0546875000000004'))

allhh_sp@data[ , c('longitude_meters', 'latitude_meters')] <- allhh_sp@coords


allchild_sp <- SpatialPointsDataFrame(allchild[ , c('longitude', 'latitude')], data=allchild, proj4string = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')) %>%
  spTransform(CRS('+proj=eqc +lon_0=-1.0546875000000004'))

allchild_sp@data[ , c('longitude_meters', 'latitude_meters')] <- allchild_sp@coords

allhh <- allhh_sp@data
allchild <- allchild_sp@data

save(file="GHA_data.Rdata", list=c("allhh", "allchild"))

write.csv(allhh, "GHA_hh.csv", row.names=F)
write.csv(allchild, "GHA_child.csv", row.names=F)

