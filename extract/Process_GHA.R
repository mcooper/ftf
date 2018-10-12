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
child <- read.csv('GHA-PBS-12/Ghana FTF datasets/MODULE 8 - CHILDREN ANTHROPOMETRY.csv') %>%
  select(hhserial,
         ind_refno=mid,
         age=q8_4,
         gender=q8_5,
         weight=q8_6,
         height=q8_7) %>%
  mutate(gender=as.factor(gender))

nutritionPrep()
igrowup.standard(mydf = child,
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
  select(hhserial, ind_refno, haz=zlen, waz=zwei, whz=zwfl, gender, age)

childem <- read.csv('GHA-PBS-12/Ghana FTF datasets/MODULE 2 - HOUSEHOLD DEMOGRAPHY.csv') %>%
  filter(q2_03 %in% c('Child (Son)', 'Child (Daughter)')) %>%
  mutate(q2_04=ifelse(q2_05=='Years', q2_04*12, q2_04)) %>%
  arrange(hhserial, desc(q2_04)) %>%
  group_by(hhserial) %>%
  mutate(birth_order=row_number(),
         within24=greater_less(q2_04, 24)) %>%
  select(hhserial,
         ind_refno=mid,
         birth_order,
         within24)

child <- Reduce(f=function(x, y){merge(x, y, all.x=T, all.y=F)}, list(child, childem, matz)) %>%
  mutate(hh_refno=hhserial) %>%
  na.omit

#Household Vars
assets <- read.csv('GHA-PBS-12/Ghana FTF datasets/MODULE 9 - DWELLING CHARACTERISTICS.csv')

assets$asset_index <- PCA_assets(assets[ , c("q9_01", "q9_02", "q9_03", "q9_04", 
                                             "q9_05", "q9_06", "q9_07", "q9_08", "q9_09", "q9_10", "q9_11", 
                                             "q9_12", "q9_13", "q9_14", "q9_15", "q9_16", "q9_17", "q9_18", 
                                             "q9_19")],
                                 ntiles = 5)
assets <- assets %>% 
  select(hhserial, asset_index)

hunger <- read.csv('GHA-PBS-12/Ghana FTF datasets/MODULE 3A - HOUSEHOLD HUNGER SCALE.csv') %>%
  mutate(q1 = ifelse(q3_01=='No', 0, ifelse(q3_02=="Often (more than 10 times)", 2, 1)),
         q2 = ifelse(q3_03=='No', 0, ifelse(q3_04=="Often (more than 10 times)", 2, 1)),
         q3 = ifelse(q3_05=='No', 0, ifelse(q3_06=="Often (more than 10 times)", 2, 1)),
         hhs = q1 + q2 + q3) %>%
  select(hhserial, hhs)

relmap <- data.frame(hhhead_religion=c("Ahmadi", "Catholic", "Islam", "No Religion", "Other", "Other Christian", 
                                       "Pentecostal/Charismatic", "Protestant (Anglican, Lutheran, Presbyterian, Methodist, etc)", 
                                       "Traditionalist"),
                     new_rel=c('Islam', 'Christianity', 'Islam', 'None', 'Traditional', 'Christianity',
                               'Christianity', 'Christianity', 'Traditional'))

hhdem <- read.csv('GHA-PBS-12/Ghana FTF datasets/MODULE 1 - HOUSEHOLD IDENTIFICATION.csv') %>%
  mutate(hh_size = as.integer(hhsize),
         cluster = as.factor(eacode)) %>%
  select(hhserial, hh_size, hhhead_sex=hhhsex, hhhead_religion=q1_11, cluster) %>%
  merge(relmap, all.x=T, all.y=F) %>%
  mutate(hhhead_religion=as.factor(new_rel),
         new_rel=NULL,
         hhhead_sex=factor(hhhead_sex, levels=c('Male', 'Female')))

depend_ratio <- read.csv('GHA-PBS-12/Ghana FTF datasets/MODULE 2 - HOUSEHOLD DEMOGRAPHY.csv') %>%
  mutate(dependent = ageyrs < 12 | ageyrs > 60,
         workers = ageyrs > 12 & ageyrs < 60) %>%
  group_by(hhserial) %>%
  summarize(dependents=sum(dependent),
            workers=sum(workers))

hhchar <- read.csv('GHA-PBS-12/Ghana FTF datasets/MODULE 2 - HOUSEHOLD DEMOGRAPHY.csv') %>%
  filter(status=='Primary Respondent') %>%
  mutate(hhhead_literate=(q2_07!='Cannot Read and Write English' | q2_07a != 'None'),
         hhhead_education=q2_10!='',
         urban_rural=as.factor(urbrur)) %>%
  select(hhserial, urban_rural, hhhead_age=ageyrs, hhhead_literate, hhhead_education)

#################################
#' Missing:
#' FCS - there is detailed food consumption data for the previous week,
#' but now way to get a count of days for each category.  It's too detailed!


##########################################
#read in Geo Vars
##########################################
lc <- read.csv('Landcover.csv') %>%
  mutate(hhserial=gsub("GHA-", "", hh_refno)) %>%
  select(hhserial, year, pop, market) %>%
  unique

spi <- read.csv('AllFtfPrecipIndices.csv') %>%
  filter(country=='Ghana') %>%
  mutate(hhserial=gsub("GHA-", "", hh_refno))

##########################################
#Combine
##########################################

sperr <- c('1080303601', '1080505107', '1081513210', '1081915810', '1082016411', '2071400902', '2071901212', '2072001709', '2072001810', '2072002216', '2072202707', '2080203317', '2081008715', '2090317212', '2090819514', '2100321209', '2100521616', '2100722309')

allhh <- Reduce(merge, list(hhdem, hunger, assets, depend_ratio, hhchar, spi, lc)) %>%
  filter(!hhserial %in% sperr & urban_rural == 'Rural') %>%
  mutate(dependents=dependents/hh_size,
         workers=workers/hh_size) %>%
  select(hhs, asset_index, pop, market, hh_size, latitude, longitude, hh_refno=hhserial, cluster,
         hhhead_religion, hhhead_age, hhhead_education, hhhead_literate, hhhead_sex, 
         dependents, workers, spei12, spei24, spei36, spi12, spi24, spi36, precip_mean, 
         tmin_mean, tmax_mean, spei12gs, spei24gs, spei36gs, spi12gs, spi24gs, spi36gs) %>%
  mutate(spi24sq=spi24^2,
         precip_mean=(precip_mean*12)/1000) %>%
  na.omit

allchild <- merge(child, allhh, all.x=T, all.y=F) %>%
  na.omit

save(file="GHA_data.Rdata", list=c("allhh", "allchild"))
