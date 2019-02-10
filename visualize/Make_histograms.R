setwd('G://My Drive/Feed the Future/')

library(dplyr)
library(ggplot2)

all <- data.frame()

load('GHA_data.Rdata')

allhh <- allhh %>%
  mutate(country='Ghana',
         survey_year="2012",
         hhhead_education=NULL)

all <- bind_rows(all, allhh)

load('BGD_data.Rdata')

allhh <- allhh %>%
  mutate(country='Bangladesh',
         survey_year = as.character(survey_year))

all <- bind_rows(all, allhh)

all$CountryYear <- paste(all$country, all$survey_year)

allc <- data.frame()

load('GHA_data.Rdata')

allchild <- allchild %>%
  mutate(country='Ghana',
         survey_year=2012,
         hhhead_education=NULL)

allc <- bind_rows(allc, allchild)

load('BGD_data.Rdata')

allchild <- allchild %>%
  mutate(country='Bangladesh',
         hh_refno=NULL,
         survey_year=as.numeric(as.character(survey_year)))

allc <- bind_rows(allc, allchild)

allc$CountryYear <- paste(allc$country, allc$survey_year)

all$CountryYear <- relevel(as.factor(all$CountryYear), ref="Ghana 2012")

##SPI frequency plot
# ggplot(all, aes(spi24)) + 
#   geom_density() +
#   xlab('24-Month Standardized Precipitation Index') +
#   ylab('Density') + theme_bw() + 
#   facet_wrap(~CountryYear, nrow=1)
# 
# ggsave('Fig2.eps', width = 6, height=2.25, units = 'in')

#Long-term precipitation Norms
makelabels <- function(x){
  round(exp(x), -2)
}

ggplot(all, aes(log(mean_annual_precip))) + 
  geom_density() +
  xlab('Average Annual Precipitation (mm)') +
  scale_x_continuous(limits=c(6.75, 8.6), breaks =c(7, 7.5, 8, 8.5), labels=makelabels) + 
  ylab('Density') + theme_bw() + 
  facet_wrap(~country, nrow=1)

ggsave('Fig3.eps', width = 4, height=2.25, units = 'in')

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
ggsave('Fig4.eps', width = 6, height=2.25, units = 'in')


##HHS PDF
ggplot(all, aes(hhs)) + 
  geom_density(adjust=3)+
  xlab('Household Hunger Scale') + 
  ylab('Density') + theme_bw() + 
  facet_wrap(~CountryYear, nrow=1)
ggsave('Household Hunger Scale PDF.png', width = 8, height=2.25, units = 'in')



##haz
allc$Population <- 'Observed Children'
allc$CountryYear <- relevel(as.factor(allc$CountryYear), ref="Ghana 2012")


allc2 <- bind_rows(allc, data.frame(CountryYear=sample(unique(allc$CountryYear), size = 100000, replace = TRUE),
                                    haz=rnorm(100000, mean=0, sd = 1), 
                                    Population='Reference Population'))

ggplot(allc2, aes(haz, color=Population, linetype=Population)) + 
  geom_density()+
  xlab('Child\'s Height-for-Age Z-score') + 
  ylab('Density') + theme_bw() + 
  scale_color_manual(values=c(`Observed Children`='#FF6969', `Reference Population`="Black")) + 
  facet_wrap(~CountryYear, nrow=1) + 
  theme(legend.title = element_blank())
ggsave('Fig5.eps', width = 7, height=2.25, units = 'in')

#Irrigation
ggplot(allhh, aes(irrigation)) + 
  geom_density()+
  xlab('Irrigation Rate') + 
  ylab('Density') + theme_bw() + 
  theme(legend.title = element_blank())
ggsave('Fig6.eps', width = 2.5, height=2.25, units = 'in')


##Bivariate plots

ggplot(allc, aes(spi24, haz)) +
  geom_point(color='#666666', size=0.1) + 
  geom_smooth(method='lm', se=FALSE, color='#FF0000') + 
  facet_wrap(~CountryYear, nrow=1, scales="free_x") + 
  xlab('24-Month Standardized Precipitation Index') +
  ylab('Height-for-Age Z-Score') + 
  theme_bw()
ggsave('Bivariate - SPI vs HAZ.png', width = 6, height=2.25, units = 'in')

ggplot(all, aes(spi24, hhs)) +
  geom_point(color='#666666', size=0.1) + 
  geom_smooth(method='lm', se=FALSE, color='#FF0000') + 
  facet_wrap(~CountryYear, nrow=1, scales="free_x") + 
  xlab('24-Month Standardized Precipitation Index') +
  ylab('Household Hunger Scale') + 
  theme_bw()
ggsave('Bivariate - SPI vs HHS.png', width = 8, height=2.25, units = 'in')
