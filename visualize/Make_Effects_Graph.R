setwd('G://My Drive/Papers/Feed the Future/')

library(ggplot2)
library(dplyr)

bgd <- read.csv('Bangladesh_Effects.csv')
gha <- read.csv('Ghana_Effects.csv')
zam <- read.csv('Zambia_Effects.csv')

bgd$Country <- 'Bangladesh'
gha$Country <- 'Ghana'
zam$Country <- 'Zambia'

all <- Reduce(bind_rows, list(bgd, gha, zam))

all <- all %>%
  mutate(max=Estimate + Std..Error*2,
         min=Estimate - Std..Error*2) %>%
  filter(X %in% c('pop', 'mean_annual_precip', 'market',
         'hhhead_sexFemale', 'hh_size',
         'dependents', 'asset_index', 'spi24'))

mdf <- data.frame(matrix(c('pop', 'Population Density (100 People/sqkm)', 3,
                            'mean_annual_precip', 'Mean Annual Precip (100m)', 2,
                            'market', 'Market Distance (Days)', 4,
                            'hhhead_sexFemale', 'Female Household Head', 6,
                            'hh_size', 'Household Size', 7,
                            'dependents', 'Fraction of Household not Working', 8,
                            'asset_index', 'Asset Index Score (1-5)', 5,
                            'spi24', '24-Month Standardized Precipitation Index (SPI)', 1),
                          byrow = TRUE, ncol=3))
names(mdf) <- c('X', 'Label', 'rank')

mdf$Label <- factor(mdf$Label, levels=unique(mdf$Label[rev(order(as.numeric(as.character(mdf$rank))))]), ordered=T)

all <- merge(all, mdf)

ggplot(all, aes(Label, Estimate, color=Country)) + 
  geom_pointrange(aes(ymin=min, ymax=max), position=position_dodge(width=0.5)) + 
  coord_flip() + 
  geom_hline(yintercept=0) + 
  ylab(expression("" %<-% " Less Hungry                                                   More Hungry " %->% "")) + 
  xlab("") + 
  theme_bw() + 
  ggtitle("Geographic and Household Effects on Hunger Scores")
ggsave("Geographic and Household Effects on Hunger Scores.png")
