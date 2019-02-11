setwd('G://My Drive/Feed the Future/')

library(dplyr)
library(MASS)
library(broom)

termmap <- read.csv('TermMap.csv')

load('GHA_data.Rdata')

allhh$hunger <- as.factor(allhh$hhs)

for (n in names(allhh)){
  if (is.numeric(allhh[ , n])){
    allhh[ , n] <- allhh[ , n]/max(allhh[ , n])
  }
}

makeTable <- function(olsmod, olgmod){
  ols <- summary(olsmod)$coef[ , c(1, 3)] %>% data.frame
  olg <- summary(olgmod)$coef[ , c(1, 3)] %>% data.frame
  
  names(ols) <- c("OLS Estimate", "OLS T-Value")
  names(olg) <- c("Ord Logit Estimate", "Ord Logit T-Value")
  
  ols$Term <- row.names(ols)
  olg$Term <- row.names(olg)
  
  comb <- merge(ols, olg)
  
  for (n in names(comb)){
    if (is.numeric(comb[ , n])){
      comb[ , n] <- signif(comb[ , n], 3)
    }
  }
  
  comb <- merge(comb, termmap %>% rename(Variable=Label))
  
  comb[ , c("Variable", "OLS Estimate", "OLS T-Value", "Ord Logit Estimate", 
            "Ord Logit T-Value")]
}

for (spi in c('spi12', 'spi24', 'spi36', 'spi48', 'spi60')){
  
  print(spi)
  
  olsmod <- lm(as.formula(paste0("hhs ~ asset_index + hh_size + mean_annual_precip + hhhead_religion + hhhead_age + hhhead_literate + pop + hhhead_sex + dependents + ", spi)),
                   data=allhh)
    
  olgmod <- polr(as.formula(paste0("hunger ~ asset_index + hh_size + mean_annual_precip + hhhead_religion + hhhead_age + hhhead_literate + pop + hhhead_sex + dependents + ", spi)),
                   data=allhh)
  
  write.csv(makeTable(olsmod, olgmod), paste0('Compare OLS Ord Logit/GHA', spi, '.csv'), row.names=F)
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
       
  write.csv(makeTable(olsmod, olgmod), paste0('Compare OLS Ord Logit/BGD', spi, '.csv'), row.names=F)
}
