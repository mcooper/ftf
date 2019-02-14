setwd('G://My Drive/Feed the Future/Cloud Model Results')

library(spdep)
library(dplyr)
library(ggplot2)

options(scipen=1000)

parse_model <- function(mod, cty){
  coefs <- as.data.frame(summary(mod)$Coef)
  
  if (nrow(coefs)==0){
    coefs <- as.data.frame(summary(mod)$coefficients)
  }
  
  coefs <- signif(coefs, 3)
  
  coefs$stars <- ifelse(coefs[, 4] > 0.1, "", 
                        ifelse(coefs[, 4] > 0.05, ".", 
                               ifelse(coefs[, 4] > 0.01, "*", "**")))
  
  coefs[spi, 'stars'] <- ifelse(coefs[spi, 4] > 0.1/30, "", 
                                ifelse(coefs[spi, 4] > 0.05/30, ".", 
                                       ifelse(coefs[spi, 4] > 0.01/30, "*", "**")))
  
  coefs <- coefs %>% select(Estimate, stars, StdError=`Std. Error`)

  coefs$Parameter <- row.names(coefs)
  
  #Rescale Mean Annual Precip
  coefs[coefs$Parameter=='mean_annual_precip', c("Estimate", "StdError")] <- coefs[coefs$Parameter=='mean_annual_precip', c("Estimate", "StdError")]*1000

  #Lambda
  if(!is.null(mod$lambda)){
    lambda <- mod$lambda
    lambdase <- mod$lambda.se
    lambdap <- pnorm(0, abs(lambda), lambdase)
    lambdastars <- ifelse(lambdap > 0.1, "", 
                         ifelse(lambdap > 0.05, ".", 
                                ifelse(lambdap > 0.01, "*", "***")))
    Lambda <- data.frame(Parameter="Lambda", stars=lambdastars, Estimate=mod$lambda, "StdError"=lambdase)
    
    coefs <- bind_rows(coefs, Lambda)
  }
  
  #AIC
  AIC <- data.frame(Parameter="AIC", Estimate=AIC(mod))
  
  coefs <- bind_rows(coefs, AIC)
  
  #Wald statistic
  if (!is.null(summary(mod)$Wald1$p.value)){
  
    waldp <- summary(mod)$Wald1$p.value
    
    waldstars <- ifelse(waldp > 0.1, "", 
                          ifelse(waldp > 0.05, ".", 
                                 ifelse(waldp > 0.01, "*", "**")))
    
    Wald <- data.frame(Parameter="Wald", Estimate=summary(mod)$Wald1$statistic, stars=waldstars)
    
    coefs <- bind_rows(coefs, Wald)
  }
  
  #n
  n <- data.frame(Parameter="n", Estimate=length(mod$residuals))
  coefs <- bind_rows(coefs, n)
  
  
  names(coefs)[names(coefs)!="Parameter"] <- paste0(names(coefs)[names(coefs)!="Parameter"], '-', cty)
  
  return(coefs)
}

labels <- read.csv('../regression_labels.csv')

for (spi in c('spi12', 'spi24', 'spi36', 'spi48', 'spi60')){
  
  #Ghana
  for (f in list.files(pattern=paste0('gha_.*_admin1.*', spi))){
    load(f)
  }
  
  #HAZ
  haz_gha <- parse_model(haz_gha, "HAZ")
  
  #WHZ
  whz_gha <- parse_model(whz_gha, "WHZ")

  #HHS
  hhs_gha <- parse_model(hhs_gha, "HHS")

  regression_res <- Reduce(function(x, y){merge(x, y, all.x=T, all.y=T)}, 
                           list(haz_gha, whz_gha, hhs_gha)) %>%
    merge(labels, all.x=T, all.y=F) %>%
    arrange(rank) %>%
    select(-rank, -Parameter)
  
  write.csv(regression_res[ , c(10, seq(1, 9))], paste0('../results-GHA-', spi, '.csv'), row.names=F, na = "")
  
  
  #Bangladesh
  for (f in list.files(pattern=paste0('bgd_.*_admin1_factor.*', spi))){
    load(f)
  }
  
  #HAZ
  haz_bgd <- parse_model(haz_bgd_irrig, "HAZ")
  
  #WHZ
  whz_bgd <- parse_model(whz_bgd_irrig, "WHZ")
  
  #HHS
  hhs_bgd <- parse_model(hhs_bgd_irrig, "HHS")
  
  regression_res <- Reduce(function(x, y){merge(x, y, all.x=T, all.y=T)}, 
                           list(haz_bgd, whz_bgd, hhs_bgd)) %>%
    merge(labels, all.x=T, all.y=F) %>%
    arrange(rank) %>%
    select(-rank, -Parameter)
  
  write.csv(regression_res[ , c(10, seq(1, 9))], paste0('../results-BGD-', spi, '.csv'), row.names=F, na = "")
  
}
