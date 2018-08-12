library(foreign)
library(readstata13)
library(lubridate)

setwd('D://Documents and Settings/mcooper/Google Drive/Feed the Future/')

#Bangladesh 1
out <- read.dta('Bangladesh/BIHS Raw Data (2011)/001_mod_a_male.dta')
write.csv(out[ , c('a01', 'a16_dd', 'a16_mm', 'a16_yy')], 'Bangladesh/Round1Dates.csv', row.names=F)

#Bangladesh 2
out <- read.dta13('Bangladesh/BIHS Raw Data (2015)/001_r2_mod_a_male_updated.dta')
write.csv(out[ , c('a01', 'a16_dd', 'a16_mm', 'a16_yy')], 'Bangladesh/Round2Dates.csv', row.names=F)

#Zambia 1
out <- read.dta13('ZMB-RALS-12/Rawdata/Data/STATA/hh_part1.dta')
#No dates given, but country report says most were done between late Nov and early Dec
out$country <- 'Zambia'
out$month <- 11
out$year <- 2012
out$S_DD <- - out$S_DD
write.csv(out[ , c('country', 'hh', 'cluster', 'S_DD', 'E_DD', 'month', 'year')], 'ZMB-RALS-12/Round1Dates.csv', row.names=F)

#Zambia 2
out <- read.dta13('ZMB-RALS-15/Rawdata/Data/Stata/household.dta')
out$quest_date <- dmy_hms(out$quest_date)
out$year <- year(out$quest_date)
out$month <- month(out$quest_date)
out$day <- day(out$quest_date)

write.csv(out[ , c('cluster', 'HH', 's_dd_new', 'e_dd_new', 'year', 'month', 'day')], 'ZMB-RALS-15/Round2Dates.csv', row.names=F)
