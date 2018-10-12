setwd('G://My Drive/Feed the Future')

library(spdep)
library(dplyr)
library(ggplot2)

load('BGD_mods.Rdata')
load('GHA_mods.Rdata')

parse_model <- function(mod, cty, level, irrig){
  coefs <- as.data.frame(summary(mod)$Coef)
  coefs$Term <- row.names(coefs)
  
  coefs$Country <- cty
  coefs$Level <- level
  coefs$Irrigation <- irrig
  
  return(coefs)
}

child_bgd <- parse_model(mod_child_bgd, "Bangladesh", "Height-for-Age Z-Score", '')
child_irrig_bgd <- parse_model(mod_child_irrig_bgd, "Bangladesh", "Height-for-Age Z-Score", "Irrigation")
child_gha <- parse_model(mod_child_gha, "Ghana", "Height-for-Age Z-Score", "")

hh_bgd <- parse_model(mod_hh_bgd, "Bangladesh", "Household Hunger Scale", "")
hh_irrig_bgd <- parse_model(mod_hh_irrig_bgd, "Bangladesh", "Household Hunger Scale", "Irrigation")
hh_gha <- parse_model(mod_hh_gha, "Ghana", "Household Hunger Scale", "")

all <- Reduce(bind_rows, list(child_bgd, child_irrig_bgd, child_gha, hh_bgd, hh_irrig_bgd, hh_gha))

graphterms <- c("spi24", "irrigation", "irrigation:spi24", 
                "precip_mean", "irrigation:precip_mean"
)

termmap <- read.csv('TermMap.csv')

all <- merge(all, termmap)

graph <- all %>%
  filter(Term %in% graphterms & !(Country=='Bangladesh' & Irrigation=='')) %>%
  mutate(max=Estimate + `Std. Error`*1.644854, #qnorm(0.95, 0, 1)
         min=Estimate - `Std. Error`*1.644854,
         `Significance at p<0.1`=ifelse(min*max > 0, 'Significant', 'Not Significant'))

# cols <- c('Estimate', 'Std. Error', 'max', 'min')
# rows <- grepl('precip_mean', graph$Term) & graph$Country=='Bangladesh'
# 
# graph[rows , cols] <- (graph[rows , cols]/12)*1000

graph$Label <- factor(graph$Label, levels = rev(c("24-Month SPI",
                                              "Mean Annual Precipiation (1000mm)", 
                                              "Fraction of Agriculture with Irrigation", 
                                              "Irrigation * 24-Month SPI", 
                                              "Irrigation * Mean Precipitatation")))

ggplot(graph %>% filter(Country=='Ghana')) + 
  geom_pointrange(aes(x=Label, y=Estimate, ymin=min, ymax=max, color=`Significance at p<0.1`), size=1) + 
  coord_flip() + 
  geom_hline(yintercept=0, color='red', linetype=2) + 
  scale_color_manual(values=c("Not Significant"="Black", "Significant"="firebrick4")) + 
  ylab("Coefficient Estimates") + 
  xlab("") + 
  theme_bw() +
  facet_grid(. ~ Level, scales='free')
  
ggsave("Coefs - Ghana.png", width=8, height=2.5, units = 'in')


ggplot(graph %>% filter(Country=='Bangladesh')) + 
  geom_pointrange(aes(x=Label, y=Estimate, ymin=min, ymax=max, color=`Significance at p<0.1`), size=1) + 
  coord_flip() + 
  geom_hline(yintercept=0, color='red', linetype=2) + 
  scale_color_manual(values=c("Not Significant"="Black", "Significant"="firebrick4")) + 
  scale_size_manual(values=c(a=1.1, b=0.4)) + 
  ylab("Coefficient Estimates for Bangladesh") + 
  xlab("") + 
  theme_bw() + 
  facet_grid(. ~ Level, scales='free')

ggsave("Coefs - Bangladesh.png", width=8, height=3.5, units = 'in')
