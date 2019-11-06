setwd('G://My Drive/Feed the Future/')

library(dplyr)
library(MASS)
library(broom)
library(texreg)

load('GHA_data.Rdata')

allhh$hunger <- as.factor(allhh$hhs)

for (n in names(allhh)){
  if (is.numeric(allhh[ , n])){
    allhh[ , n] <- allhh[ , n]/max(allhh[ , n])
  }
}


for (spi in c('spi12', 'spi24', 'spi36', 'spi48', 'spi60')){
  
  print(spi)
  
  olsmod <- lm(as.formula(paste0("hhs ~ asset_index + hh_size + mean_annual_precip + hhhead_religion + hhhead_age + hhhead_literate + pop + hhhead_sex + dependents + ", spi)),
                   data=allhh)
    
  olgmod <- polr(as.formula(paste0("hunger ~ asset_index + hh_size + mean_annual_precip + hhhead_religion + hhhead_age + hhhead_literate + pop + hhhead_sex + dependents + ", spi)),
                   data=allhh)
  
  texreg(l=list(olsmod, olgmod), file=paste0('C://Users/matt/Desktop/GHA', spi),
         custom.model.names=c("OLS", "Logistic"), 
         caption=paste0(spi, ' in Ghana'))
}



load('BGD_data.Rdata')

allhh$hunger <- as.factor(allhh$hhs)

for (n in names(allhh)){
  if (is.numeric(allhh[ , n])){
    allhh[ , n] <- allhh[ , n]/max(allhh[ , n])
  }
}

for (spi in c('spi12', 'spi24', 'spi36', 'spi48', 'spi60')){
  
  print(spi)
  
  olsmod <- lm(as.formula(paste0("hhs ~ asset_index + hh_size + hhhead_age + survey_year + interview_month + hhhead_religion + mean_annual_precip + hhhead_literate + hhhead_education + hhhead_sex + dependents + pop + ", spi)),
                   data=allhh)
  
  olgmod <- polr(as.formula(paste0("hunger ~ asset_index + hh_size + hhhead_age + survey_year + interview_month + hhhead_religion + mean_annual_precip + hhhead_literate + hhhead_education + hhhead_sex + dependents + pop + ", spi)),
                     data=allhh)
       
  texreg(l=list(olsmod, olgmod), file=paste0('C://Users/matt/Desktop/BGD', spi),
         custom.model.names=c("OLS", "Logistic"), 
         caption=paste0(spi, ' in Bangladesh'))}
