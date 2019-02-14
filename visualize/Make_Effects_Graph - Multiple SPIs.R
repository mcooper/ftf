setwd('G://My Drive/Feed the Future/Cloud Model Results')

library(spdep)
library(dplyr)
library(ggplot2)

alpha <- 0.05/30

z <- qnorm(1 - alpha/2, 0, 1, lower.tail=T)

##############################
#Ghana
##########################

gha <- data.frame()

for (spi in c('spi12', 'spi24', 'spi36', 'spi48', 'spi60')){
  #Ghana Child
  for (f in list.files(pattern=paste0('gha_child_admin1.*', spi))){
    load(f)
  }
  
  #HAZ
  coefs <- as.data.frame(summary(haz_gha)$coefficients)
  
  est <- coefs[grepl('spi', row.names(coefs)), 'Estimate']
  se <- coefs[grepl('spi', row.names(coefs)), 'Std. Error']
  
  gha <- bind_rows(gha, data.frame(Estimate=est, SE=se, Country="GHA", Outcome="Height-for-Age Z-Score",
                                 Scale=spi))
  
  #WHZ
  coefs <- as.data.frame(summary(whz_gha)$coefficients)
  
  est <- coefs[grepl('spi', row.names(coefs)), 'Estimate']
  se <- coefs[grepl('spi', row.names(coefs)), 'Std. Error']
  
  gha <- bind_rows(gha, data.frame(Estimate=est, SE=se, Country="GHA", Outcome="Weight-for-Height Z-Score",
                                 Scale=spi))
  
  #Ghana HHS
  for (f in list.files(pattern=paste0('gha_hh_admin1_.*', spi))){
    load(f)
  }
  
  coefs <- as.data.frame(summary(hhs_gha)$Coef)
  
  est <- coefs[grepl('spi', row.names(coefs)), 'Estimate']
  se <- coefs[grepl('spi', row.names(coefs)), 'Std. Error']
  
  gha <- bind_rows(gha, data.frame(Estimate=est, SE=se, Country="GHA", Outcome="Household Hunger Scale",
                                 Scale=spi))
  
}

gha$upper <- gha$Estimate + gha$SE*z
gha$lower <- gha$Estimate - gha$SE*z

gha$signif <- ifelse(gha$upper*gha$lower > 0, "Significant", "Not Significant")

sel <- gha %>% filter(Scale %in% c('spi12', 'spi24', 'spi36', 'spi48', 'spi60') &
                       (!Outcome %in% c('')))

sel <- merge(sel, data.frame(Scale=c('spi12', 'spi24', 'spi36', 'spi48', 'spi60'),
                             `SPI Window`=c('12', '24', '36',
                                            '48', '60')))

sel$Outcome <- relevel(as.factor(sel$Outcome), ref='Household Hunger Scale')

ggplot(sel) + 
  geom_pointrange(aes(x=SPI.Window, ymin=lower, ymax=upper, y=Estimate, fill=signif), shape=21, size=1.25) + 
  geom_hline(aes(yintercept=0)) + 
  facet_wrap(~Outcome) + 
  xlab('SPI Window (Months)') + 
  ylab("Coefficient Estimate") +
  theme_bw() + 
  theme(legend.title=element_blank(), legend.position="bottom")

ggsave('../Fig7.eps', width = 8, height=4, units = 'in')

################################################
#Bangladesh with Just Irrigation (No Interaction)
##################################################

bgd_justirrig <- data.frame()

for (spi in c('spi12', 'spi24', 'spi36', 'spi48', 'spi60')){
  #Bangladesh Child
  for (f in list.files(pattern=paste0('bgd_child_admin1_factor.*', spi))){
    load(f)
  }
  
  coefs <- as.data.frame(summary(haz_bgd)$Coef)
  
  est <- coefs[grepl('spi', row.names(coefs)), 'Estimate']
  se <- coefs[grepl('spi', row.names(coefs)), 'Std. Error']
  
  bgd_justirrig <- bind_rows(bgd_justirrig, data.frame(Estimate=est, SE=se, Country="BGD", Outcome="Height-for-Age Z-Score",
                                               Variable=c('SPI', 'Irrigation'),
                                               Scale=spi))
  
  coefs <- as.data.frame(summary(whz_bgd)$Coef)
  
  est <- coefs[grepl('spi', row.names(coefs)), 'Estimate']
  se <- coefs[grepl('spi', row.names(coefs)), 'Std. Error']
  
  bgd_justirrig <- bind_rows(bgd_justirrig, data.frame(Estimate=est, SE=se, Country="BGD", Outcome="Weight-for-Height Z-Score",
                                               Scale=spi))
  
  #Bangladesh HHS
  for (f in list.files(pattern=paste0('bgd_hh_admin1_factor.*', spi))){
    load(f)
  }
  print(spi)

  coefs <- as.data.frame(summary(hhs_bgd)$Coef)

  est <- coefs[grepl('spi', row.names(coefs)), 'Estimate']
  se <- coefs[grepl('spi', row.names(coefs)), 'Std. Error']

  bgd_justirrig <- bind_rows(bgd_justirrig, data.frame(Estimate=est, SE=se, Country="BGD", Outcome="Household Hunger Scale",
                                               Scale=spi))
  
}

bgd_justirrig$upper <- bgd_justirrig$Estimate + bgd_justirrig$SE*z
bgd_justirrig$lower <- bgd_justirrig$Estimate - bgd_justirrig$SE*z

bgd_justirrig$signif <- ifelse(bgd_justirrig$upper*bgd_justirrig$lower > 0, "Significant", "Not Significant")

sel <- bgd_justirrig %>% filter(Scale %in% c('spi12', 'spi24', 'spi36', 'spi48', 'spi60') &
                              (!Outcome %in% c('')))

sel <- merge(sel, data.frame(Scale=c('spi12', 'spi24', 'spi36', 'spi48', 'spi60'),
                             `SPI Window`=c('12', '24', '36',
                                            '48', '60')))

sel$Outcome <- relevel(as.factor(sel$Outcome), ref='Household Hunger Scale')

ggplot(sel) + 
  geom_pointrange(aes(x=SPI.Window, ymin=lower, ymax=upper, y=Estimate, fill=signif), position=position_dodge(width=1.5), shape=21, size=1.25) + 
  geom_hline(aes(yintercept=0)) + 
  facet_wrap(~Outcome) + 
  xlab('SPI Window (Months)') + 
  ylab("Coefficient Estimate") +
  theme_bw() + 
  theme(legend.title=element_blank(), legend.position="bottom")

ggsave('../Fig8.eps', width = 8, height=4, units = 'in')
