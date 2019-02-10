setwd('G://My Drive/Feed the Future/Cloud Model Results')

library(ncf)
library(dplyr)

load('Correlogram_Fits.Rdata')

df <- data.frame()

df <- bind_rows(df, data.frame(Distance=bgd_haz_fit$mean.of.class,
                               Correlation=bgd_haz_fit$correlation,
                               Regression="HAZ - Bangladesh"))

df <- bind_rows(df, data.frame(Distance=bgd_hhs_fit$mean.of.class,
                               Correlation=bgd_hhs_fit$correlation,
                               Regression="HHS - Bangladesh"))

df <- bind_rows(df, data.frame(Distance=gha_hhs_fit$mean.of.class,
                               Correlation=gha_hhs_fit$correlation,
                               Regression="HHS - Ghana"))

library(ggplot2)

df$Distance <- df$Distance/1000

ggplot(df) +   geom_hline(aes(yintercept=0), color='red', linetype=2) + 
  geom_line(aes(x=Distance, y=Correlation)) + 
  facet_grid( ~ Regression) + xlim(0, 300) +
  ylim(-0.05, 0.15) + 
  theme_bw()

ggsave('../Correlograms.eps', width = 8, height=2.25, units = 'in')
