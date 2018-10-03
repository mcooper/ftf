setwd('G://My Drive/Feed the Future/')

library(foreign)
library(dplyr)
library(readstata13)
library(tidyr)

options(stringsAsFactors = F)

source('C://Git/ftf/utils/utils.R')

##################################
#Extract hh vars
##################################
children <- read.dta('ZMB-RALS-12/Zambia ftf/Zambia_children_file_PR.dta') %>%
  select(haz,
         age=i06,
         gender=i03,
         pbs_id) %>%
  mutate(gender=factor(gender, levels=c(1, 2), labels=c('Male', 'Female')))

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

match <- read.dta13('ZMB-RALS-12/Zambia ftf/georeferenced WEAI/Zambia_CrossWalkFile.dta') %>%
  mutate(hh_refno=paste0('ZAM-', cluster, '-', hh)) %>%
  mutate(pbs_id=PBS_ID) %>%
  select(-PBS_ID)

hh <- Reduce(f=function(x, y) merge(x, y, all.x=T, all.y=F), list(household, hhmembers, depend_ratio, match)) %>%
  mutate(dependents=dependents/hh_size,
         workers=workers/hh_size)

########################################
#Extract Landcover Vars
##################################################
lc <- read.csv('Landcover.csv') %>%
  select(hh_refno, year, pop, market) %>%
  unique

spi <- read.csv('AllFtfPrecipIndices.csv') %>%
  filter(country=='Zambia' & year==2012)

######################################
#read in SPI and combine
#####################################
allhh <- Reduce(merge, list(hh, spi, lc)) %>% 
  select(cluster, hh_refno, hhs, hh_size, asset_index, hhhead_sex, hhhead_age, hhhead_education, hhhead_literate,
         latitude, longitude, pop, market, dependents, workers, pbs_id,
         spei12, spei24, spei36, spi12, spi24, spi36, precip_mean, 
         tmin_mean, tmax_mean, spei12gs, spei24gs, spei36gs, spi12gs, spi24gs, spi36gs) %>%
  filter(!hh_refno %in% c('ZAM-3065-5', 'ZAM-3027-247', 'ZAM-3033-314', 'ZAM-3080-9')) %>%
  na.omit

allchild <- Reduce(merge, list(children, allhh)) %>%
  na.omit

save(file="ZAM_data.Rdata", list=c("allhh", "allchild"))

