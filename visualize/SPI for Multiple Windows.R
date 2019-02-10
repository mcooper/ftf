setwd('G://My Drive/Feed the Future/')

library(dplyr)
library(ggplot2)
library(tidyr)

load('GHA_data.Rdata')

gha <- allhh %>%
  dplyr::select(spi12, spi24, spi36, spi48, spi60) %>%
  mutate(country='Ghana')

load('BGD_data.Rdata')

bgd <- allhh %>%
  dplyr::select(spi12, spi24, spi36, spi48, spi60, survey_year) %>%
  mutate(country=paste0('Bangladesh ', survey_year),
         survey_year=NULL)

all <- bind_rows(gha, bgd) %>%
  gather(Period, SPI, -country) %>%
  mutate(Period = paste0(gsub('spi', '', Period), ' Months'))

ggplot(all, aes(x=SPI, color=Period, linetype=Period)) +
  geom_density() +
  xlab('Standardized Precipitation Index') +
  ylab('Density') + theme_bw() +
  facet_wrap(~country, nrow=1)

ggsave('Fig2.eps', width = 6, height=2.25, units = 'in')
