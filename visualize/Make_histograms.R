setwd('D://Documents and Settings/mcooper/Google Drive/Feed the Future/')

library(dplyr)
library(ggplot2)

bgd <- read.csv('Bangladesh_regressiondf.csv') %>%
  mutate(country='Bangladesh')
zam <- read.csv('Zambia_regressiondf.csv') %>%
  mutate(country='Zambia',
         year=2012)
gha <- read.csv('Ghana_regressiondf.csv') %>%
  mutate(country='Ghana',
         year=2012)
gha$hhhead_education <- NULL

all <- Reduce(bind_rows, list(bgd, zam, gha))

all$CountryYear <- paste(all$country, all$year)

bgdc <- read.csv('Bangladesh_regressiondf_child.csv') %>%
  mutate(country='Bangladesh')
zamc <- read.csv('Zambia_regressiondf_child.csv') %>%
  mutate(country='Zambia',
         year=2012)
ghac <- read.csv('Ghana_regressiondf_child.csv') %>%
  mutate(country='Ghana',
         year=2012)
ghac$hhhead_education <- NULL

allc <- Reduce(bind_rows, list(bgdc, zamc, ghac))

allc$CountryYear <- paste(allc$country, allc$year)


##SPI frequency plot

ggplot(all, aes(spi24)) + 
  geom_density() +
  xlab('24-Month Standardized Precipitation Index') +
  ylab('Density') + theme_bw() + 
  facet_wrap(~CountryYear, nrow=1)

ggsave('24-Month Standardized Precipitation Index PDF.png', width = 8, height=2.25, units = 'in')

##Forest Cover
ggplot(all, aes(forestsavanna)) + 
  geom_density()+
  xlab('Percent of Nearby Land Cover Classified As Natural Land Cover Types') + 
  ylab('Density') + theme_bw() + 
  facet_wrap(~CountryYear, nrow=1)
ggsave('Percent of Nearby Land Cover Classified As Natural Land Cover Types.png', width = 8, height=2.25, units = 'in')


#hhs
allsum <- all %>%
  group_by(CountryYear) %>%
  summarize(divide=n()) %>%
  merge(all %>%
          group_by(CountryYear, hhs) %>%
          summarize(count=n())) %>%
  mutate(y=count/divide)

ggplot(allsum, aes(x=hhs, y=y)) +
  geom_bar(stat='identity') +
  xlab('Household Hunger Scale') +
  ylab('Probability') + theme_bw() +
  facet_wrap(~CountryYear, nrow=1)
ggsave('Household Hunger Scale.png', width = 8, height=2.25, units = 'in')


##HHS PDF
ggplot(all, aes(hhs)) + 
  geom_density(adjust=3)+
  xlab('Household Hunger Scale') + 
  ylab('Density') + theme_bw() + 
  facet_wrap(~CountryYear, nrow=1)
ggsave('Household Hunger Scale PDF.png', width = 8, height=2.25, units = 'in')



##haz
allc2 <- bind_rows(allc, data.frame(CountryYear=sample(unique(allc$CountryYear), size = 100000, replace = TRUE),
                                    haz=rnorm(100000, mean=0, sd = 1), 
                                    gender='Healthy Population'))
ggplot(allc2, aes(haz, linetype=gender)) + 
  geom_density()+
  xlab('Child\'s Height-for-Age Z-score') + 
  ylab('Density') + theme_bw() + 
  facet_wrap(~CountryYear, nrow=1) + 
  theme(legend.title = element_blank())
ggsave('Child\'s Height-for-Age Z-score.png', width = 8, height=2.25, units = 'in')

#Anova for male vs female children
for (cy in unique(allc$CountryYear)){
  sel <- allc[allc$CountryYear == cy, ]
  print(cy)
  print(summary(aov(haz~gender, data=sel)))
  cat('\n\n')
}


##female_5DE and male_5de
all2 <- bind_rows(all %>% select(CountryYear, Empowerment=male_5de) %>% mutate(Gender="Male"),
                  all %>% select(CountryYear, Empowerment=female_5de) %>% mutate(Gender="Female"))


ggplot(all2, aes(Empowerment, linetype=Gender)) + 
  geom_density(adjust=1.75) +
  xlab('Empowerment Scores Across 5 Domains of Empowerment') +
  ylab('Density') + theme_bw() + 
  facet_wrap(~CountryYear, nrow=1)
ggsave('Empowerment Score Across 5 Domains of Empowerment.png', width = 8, height=2.25, units = 'in')

#Anova for male vs female empowerment
for (cy in unique(all2$CountryYear)){
  sel <- all2[all2$CountryYear == cy, ]
  print(cy)
  print(summary(aov(Empowerment~Gender, data=sel)))
  cat('\n\n')
}

##Bivariate plots

ggplot(allc, aes(spi24, haz)) +
  geom_point(color='#666666', size=0.1) + 
  geom_smooth(method='lm', se=FALSE, color='#FF0000') + 
  facet_wrap(~CountryYear, nrow=1, scales="free_x") + 
  xlab('24-Month Standardized Precipitation Index') +
  ylab('Height-for-Age Z-Score') + 
  theme_bw()
ggsave('Bivariate - SPI vs HAZ.png', width = 8, height=2.25, units = 'in')

ggplot(all, aes(spi24, hhs)) +
  geom_point(color='#666666', size=0.1) + 
  geom_smooth(method='lm', se=FALSE, color='#FF0000') + 
  facet_wrap(~CountryYear, nrow=1, scales="free_x") + 
  xlab('24-Month Standardized Precipitation Index') +
  ylab('Household Hunger Scale') + 
  theme_bw()
ggsave('Bivariate - SPI vs HHS.png', width = 8, height=2.25, units = 'in')
