##Need:

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


# WEAI
# asset_score + 
# hhhead_gender + 
# hhsize + 
# weai_parity_gap + 
