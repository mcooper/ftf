library(foreign)
library(readstata13)
library(dplyr)
library(lubridate)

setwd('G://My Drive/Feed the Future/')

###################################
#Ghana
###################################
#"The fieldwork itself took place between July 1 and August 17, 2012", please look at page 26 of the pdf at this link: http://www.metss-ghana.k-state.edu/PBS_Items/PBSReport22814export.pdf
gha_hh <- read.csv('GHA-PBS-12/ghana_FtF_2012_coordinates.csv') %>%
  mutate(hh_refno = paste0('GHA-', hhserial),
         month = 7,
         year = 2012) %>%
  select(hh_refno, 
         latitude=bio_latitude_offset, 
         longitude=bio_longitude_offset, 
         month,
         year)

gha_child <- read.csv('GHA-PBS-12/Ghana FTF datasets/MODULE 8 - CHILDREN ANTHROPOMETRY.csv') %>%
  mutate(hh_refno = paste0("GHA-", hhserial)) %>%
  select(hh_refno,
         ind_refno=mid,
         age=q8_4) %>%
  merge(gha_hh)

###################################
#Bangladesh
###################################
bgd1 <- read.dta13('Bangladesh/4. BIHS_household_2011_15.dta') %>%
  mutate(hh_refno = paste0(ISO, '-', a01)) %>%
  select(hh_refno,
         latitude=bio_latitude,
         longitude=bio_longitude,
         survey_year)

bgd2.1 <- read.dta('Bangladesh/BIHS Raw Data (2011)/001_mod_a_male.dta') %>%
  mutate(hh_refno = paste0('BGD-', a01),
         survey_year = 2011) %>%
  select(hh_refno,
         survey_year,
         year=a16_yy,
         month=a16_mm)

bgd2.2 <- read.dta13('Bangladesh/BIHS Raw Data (2015)/001_r2_mod_a_male_updated.dta') %>%
  mutate(hh_refno = paste0('BGD-', a01),
         survey_year = 2015) %>%
  select(hh_refno,
         survey_year,
         year=a16_yy,
         month=a16_mm)

bgd2 <- bind_rows(bgd2.1, bgd2.2)

bgd_hh <- merge(bgd1, bgd2) %>%
  filter(!is.na(month))

bgd_child <- read.dta13('Bangladesh/5. BIHS_child_2011_15.dta') %>%
  mutate(hh_refno=paste0('BGD-', a01),
         survey_year=as.factor(survey_year)) %>%
  select(survey_year,
         hh_refno,
         ind_refno=mid,
         age) %>%
  merge(bgd_hh)

#################################
#Combine and Write
#################################

hh <- bind_rows(bgd_hh, gha_hh)
child <- bind_rows(bgd_child, gha_child)

child$survey_date <- ymd(paste0(child$year, '-', child$month, '-15'))

child$thousandday_month <- month((child$survey_date - months(floor(child$age))) + days(720))
child$thousandday_year <- year((child$survey_date - months(floor(child$age))) + days(720))

child$thousandday_month[child$age < 24] <- NA
child$thousandday_year[child$age < 24] <- NA

write.csv(hh, 'Coords&Dates_HH.csv', row.names = F)
write.csv(child, 'Coords&Dates_Child.csv', row.names = F)
