setwd('G://My Drive/Feed the Future')

library(spdep)
library(dplyr)
library(ggplot2)

options(scipen=1000)

parse_model <- function(mod, cty){
  coefs <- as.data.frame(summary(mod)$Coef)
  
  coefs <- signif(coefs, 3)
  
  coefs$stars <- ifelse(coefs$`Pr(>|z|)` > 0.1, "", 
                        ifelse(coefs$`Pr(>|z|)` > 0.05, "*", 
                               ifelse(coefs$`Pr(>|z|)` > 0.01, "**", "***")))
  
  coefs <- coefs %>% select(Estimate, stars, StdError=`Std. Error`)

  coefs$Parameter <- row.names(coefs)

  #Lambda
  lambda <- mod$lambda
  lambdase <- mod$lambda.se
  lambdap <- pnorm(0, abs(lambda), lambdase)
  lambdastars <- ifelse(lambdap > 0.1, "", 
                       ifelse(lambdap > 0.05, "*", 
                              ifelse(lambdap > 0.01, "**", "***")))
  Lambda <- data.frame(Parameter="Lambda", stars=lambdastars, Estimate=mod$lambda, "StdError"=lambdase)
  
  #AIC
  AIC <- data.frame(Parameter="AIC", Estimate=AIC(mod))
  
  #Chisq
  
  #Wald statistic
  waldp <- summary(mod)$Wald1$p.value
  
  waldstars <- ifelse(waldp > 0.1, "", 
                        ifelse(waldp > 0.05, "*", 
                               ifelse(waldp > 0.01, "**", "***")))
  
  Wald <- data.frame(Parameter="Wald", Estimate=summary(mod)$Wald1$statistic, stars=waldstars)
  
  coefs <- Reduce(bind_rows, list(coefs, Lambda, AIC, Wald))
  
  
  names(coefs)[names(coefs)!="Parameter"] <- paste0(names(coefs)[names(coefs)!="Parameter"], '-', cty)
  
  return(coefs)
}

labels <- read.csv('regression_labels.csv')

load('GHA_mods.Rdata')
load('BGD_mods.Rdata')

child_bgd <- parse_model(mod_child_bgd, "Bangladesh-HAZ")
child_irrig_bgd <- parse_model(mod_child_irrig_bgd, "Bangladesh-Irrigation-HAZ")
child_gha <- parse_model(mod_child_gha, "Ghana-HAZ")
hh_bgd <- parse_model(mod_hh_bgd, "Bangladesh-HHS")
hh_irrig_bgd <- parse_model(mod_hh_irrig_bgd, "Bangladesh-Irrigation-HHS")
hh_gha <- parse_model(mod_hh_gha, "Ghana-HHS")

regression_res <- Reduce(function(x, y){merge(x, y, all.x=T, all.y=T)}, 
                         list(child_bgd, child_irrig_bgd, child_gha, hh_bgd, hh_irrig_bgd, hh_gha)) %>%
  merge(labels, all.x=T, all.y=F) %>%
  arrange(rank) %>%
  select(-rank, -Parameter)

write.csv(regression_res, 'regression_results.csv', row.names=F, na = "")
