setwd('G://My Drive/Feed the Future')

library(spdep)

parse_model <- function(mod, cty, level, irrig){
  coefs <- as.data.frame(summary(mod)$Coef)
  coefs$Term <- row.names(coefs)
  
  coefs$Country <- cty
  coefs$Level <- level
  coefs$Irrigation <- irrig
  
  return(coefs)
}

labels <- read.csv('TermMap.csv')

load('GHA_mods.Rdata')
load('BGD_mods.Rdata')

child_bgd <- parse_model(mod_child_bgd, "Bangladesh", "Height-for-Age Z-Score", '')
child_irrig_bgd <- parse_model(mod_child_irrig_bgd, "Bangladesh", "Height-for-Age Z-Score", "Irrigation")
child_gha <- parse_model(mod_child_gha, "Ghana", "Height-for-Age Z-Score", "")

hh_bgd <- parse_model(mod_hh_bgd, "Bangladesh", "Household Hunger Scale", "")
hh_irrig_bgd <- parse_model(mod_hh_irrig_bgd, "Bangladesh", "Household Hunger Scale", "Irrigation")
hh_gha <- parse_model(mod_hh_gha, "Ghana", "Household Hunger Scale", "")


#Child HAZ DF

