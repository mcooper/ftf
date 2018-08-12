library(foreign)
library(readstata13)
library(dplyr)
library(lubridate)

setwd('D://Documents and Settings/mcooper/Google Drive/Feed the Future/')

###################################
#Ghana
###################################
#"The fieldwork itself took place between July 1 and August 17, 2012", please look at page 26 of the pdf at this link: http://www.metss-ghana.k-state.edu/PBS_Items/PBSReport22814export.pdf
gha <- read.csv('GHA-PBS-12/ghana_FtF_2012_coordinates.csv') %>%
  mutate(hh_refno = paste0('GHA-', hhserial),
         month = 7,
         year = 2012,
         country = 'Ghana') %>%
  select(hh_refno, 
         latitude=bio_latitude_offset, 
         longitude=bio_longitude_offset, 
         month,
         year,
         country)

###################################
#Zambia 2012
###################################
#No dates given, but country report says most were done between late Nov and early Dec
zam1 <- read.dta('ZMB-RALS-12/Rawdata/Data/STATA/hh_part1.dta') %>%
  mutate(hh_refno = paste0('ZAM-', cluster, '-', hh),
         month = 11,
         year = 2012,
         S_DD = -S_DD,
         country = 'Zambia') %>%
  select(hh_refno,
         latitude=S_DD,
         longitude=E_DD,
         month,
         year,
         country)

zam2 <- read.dta('ZMB-RALS-15/Rawdata/Data/STATA/household.dta') %>%
  mutate(hh_refno = paste0('ZAM-', cluster, '-', HH),
         month = month(dmy_hms(quest_date)),
         year = year(dmy_hms(quest_date)),
         s_dd_new = -s_dd_new,
         country = 'Zambia') %>%
  select(hh_refno,
         latitude=s_dd_new,
         longitude=e_dd_new,
         month,
         year,
         country)

zam <- bind_rows(zam1, zam2)

###################################
#Bangladesh
###################################
bgd1 <- read.dta13('Bangladesh/4. BIHS_household_2011_15.dta') %>%
  mutate(hh_refno = paste0(ISO, '-', a01),
         round = ifelse(survey_year==2011, 1, 2)) %>%
  select(hh_refno,
         latitude=bio_latitude,
         longitude=bio_longitude,
         round)

bgd2.1 <- read.dta('Bangladesh/BIHS Raw Data (2011)/001_mod_a_male.dta') %>%
  mutate(hh_refno = paste0('BGD-', a01),
         round = 1) %>%
  select(hh_refno,
         round,
         year=a16_yy,
         month=a16_mm)

bgd2.2 <- read.dta13('Bangladesh/BIHS Raw Data (2015)/001_r2_mod_a_male_updated.dta') %>%
  mutate(hh_refno = paste0('BGD-', a01),
         round = 2) %>%
  select(hh_refno,
         round,
         year=a16_yy,
         month=a16_mm)

bgd2 <- bind_rows(bgd2.1, bgd2.2)

bgd <- merge(bgd1, bgd2) %>%
  mutate(country = 'Bangladesh') %>%
  select(-round)

#################################
#Combine and Write
#################################

all <- Reduce(bind_rows, list(gha, zam, bgd)) %>%
  na.omit

write.csv(all, 'Coords&Dates.csv', row.names = F)
