setwd('G://My Drive/Feed the Future/Cloud Model Results')

library(spdep)
library(dplyr)

df <- data.frame()

#Ghana Child
for (f in list.files(pattern='gha_child_spi')){
  load(f)
  print(f)
  
  coefs <- as.data.frame(summary(haz_gha)$Coef)
  
  est <- coefs[grepl('spi', row.names(coefs)), 'Estimate']
  se <- coefs[grepl('spi', row.names(coefs)), 'Std. Error']
  
  df <- bind_rows(df, data.frame(Estimate=est, SE=se, Country="GHA", Outcome="Height-for-Age Z-Score",
                                 Scale=gsub('gha_child_', '', gsub('.Rdata', '', f))))
  
  coefs <- as.data.frame(summary(whz_gha)$Coef)
  
  est <- coefs[grepl('spi', row.names(coefs)), 'Estimate']
  se <- coefs[grepl('spi', row.names(coefs)), 'Std. Error']
  
  df <- bind_rows(df, data.frame(Estimate=est, SE=se, Country="GHA", Outcome="WHZ",
                                 Scale=gsub('gha_child_', '', gsub('.Rdata', '', f))))
}


#Ghana HHS
for (f in list.files(pattern='gha_hh_spi')){
  load(f)
  print(f)
  
  coefs <- as.data.frame(summary(hhs_gha)$Coef)
  
  est <- coefs[grepl('spi', row.names(coefs)), 'Estimate']
  se <- coefs[grepl('spi', row.names(coefs)), 'Std. Error']
  
  df <- bind_rows(df, data.frame(Estimate=est, SE=se, Country="GHA", Outcome="Household Hunger Scale",
                                 Scale=gsub('gha_hh_', '', gsub('.Rdata', '', f))))
}


#Bangladesh Child
for (f in list.files(pattern='bgd_child_spi')){
  load(f)
  print(f)
  
  coefs <- as.data.frame(summary(haz_bgd)$Coef)
  
  est <- coefs[grepl('spi', row.names(coefs)), 'Estimate']
  se <- coefs[grepl('spi', row.names(coefs)), 'Std. Error']
  
  df <- bind_rows(df, data.frame(Estimate=est, SE=se, Country="BGD", Outcome="Height-for-Age Z-Score",
                                 Scale=gsub('bgd_child_', '', gsub('.Rdata', '', f))))
  
  coefs <- as.data.frame(summary(whz_bgd)$Coef)
  
  est <- coefs[grepl('spi', row.names(coefs)), 'Estimate']
  se <- coefs[grepl('spi', row.names(coefs)), 'Std. Error']
  
  df <- bind_rows(df, data.frame(Estimate=est, SE=se, Country="BGD", Outcome="WHZ",
                                 Scale=gsub('bgd_child_', '', gsub('.Rdata', '', f))))
}


#Bangladesh HHS
for (f in list.files(pattern='bgd_hh_spi')){
  load(f)
  print(f)
  
  coefs <- as.data.frame(summary(hhs_bgd)$Coef)
  
  est <- coefs[grepl('spi', row.names(coefs)), 'Estimate']
  se <- coefs[grepl('spi', row.names(coefs)), 'Std. Error']
  
  df <- bind_rows(df, data.frame(Estimate=est, SE=se, Country="BGD", Outcome="Household Hunger Scale",
                                 Scale=gsub('bgd_hh_', '', gsub('.Rdata', '', f))))
}

library(ggplot)

alpha <- 0.05

z <- qnorm(1 - alpha/2, 0, 1, lower.tail=T)

df$upper <- df$Estimate + df$SE*z
df$lower <- df$Estimate - df$SE*z

df$signif <- ifelse(df$upper*df$lower > 0, "Significant", "Not Significant")

sel <- df %>% filter(Scale %in% c('spi12', 'spi24', 'spi36', 'spi48', 'spi60') &
                       (!Outcome %in% c('')))

sel <- merge(sel, data.frame(Scale=c('spi12', 'spi24', 'spi36', 'spi48', 'spi60'),
                             `SPI Window`=c('12', '24', '36',
                                            '48', '60')))

ggplot(sel) + 
  geom_pointrange(aes(x=SPI.Window, ymin=lower, ymax=upper, y=Estimate, fill=signif), shape=21, size=1.5) + 
  geom_hline(aes(yintercept=0)) + 
  facet_wrap(Country~Outcome) + 
  xlab('SPI Window (Months)') + 
  ylab("Coefficient Estimate") +
  theme_bw() + 
  theme(legend.title=element_blank(), legend.position="bottom")

ggsave('')