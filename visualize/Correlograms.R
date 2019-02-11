setwd('G://My Drive/Feed the Future')

library(ncf)
library(dplyr)

load('Correlogram_Fits.Rdata')

df <- data.frame()

df <- bind_rows(df, data.frame(Distance=bgd_haz_fit$mean.of.class,
                               Correlation=bgd_haz_fit$correlation,
                               PValue=bgd_haz_fit$p,
                               Regression="HAZ - Bangladesh"))

df <- bind_rows(df, data.frame(Distance=bgd_hhs_fit$mean.of.class,
                               Correlation=bgd_hhs_fit$correlation,
                               PValue=bgd_hhs_fit$p,
                               Regression="HHS - Bangladesh"))

df <- bind_rows(df, data.frame(Distance=gha_hhs_fit$mean.of.class,
                               Correlation=gha_hhs_fit$correlation,
                               PValue=gha_hhs_fit$p,
                               Regression="HHS - Ghana"))

confInt <- function(p, est, side=c('high', 'low')){
  z = -0.862 + sqrt(0.743 - 2.404*log(p))
  
  SE = abs(est/z)
  
  if (side=='high'){
    return(est + 1.96*SE)
  }
  if (side=='low'){
    return(est - 1.96*SE)
  }
}

df$max <- confInt(df$PValue, df$Correlation, side='high')
df$min <- confInt(df$PValue, df$Correlation, side='low')

library(ggplot2)

df$Distance <- df$Distance/1000

ggplot(df) + geom_hline(aes(yintercept=0), color='red', linetype=2) +
  geom_ribbon(aes(ymin=min, ymax=max, x=Distance), fill='grey90') +  
  geom_line(aes(x=Distance, y=Correlation)) + 
  facet_grid( ~ Regression) + 
  theme_bw() + 
  coord_cartesian(ylim=c(-0.05, 0.25), xlim=c(0, 150))

ggsave('Correlograms.eps', width = 8, height=2.25, units = 'in')
