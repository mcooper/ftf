setwd('D://Documents and Settings/mcooper/Google Drive/Feed the Future/')

options(stringsAsFactors = FALSE)

library(dplyr)

GHAhh <- read.csv('GHA_allhh.csv') %>%
  mutate(hhhead_education = as.character(hhhead_education))
ZAMhh <- read.csv('ZAM_allhh.csv')
BGDhh <- read.csv('BGD_allhh.csv') %>%
  mutate(asset_index=asset_score) %>%
  select(-asset_score)

GHAch <- read.csv('GHA_allchild.csv') %>%
  mutate(hhhead_education = as.character(hhhead_education))
ZAMch <- read.csv('ZAM_allchild.csv')
BGDch <- read.csv('BGD_allchild.csv') %>%
  mutate(asset_index=asset_score) %>%
  select(-asset_score)

hh <- Reduce(bind_rows, list(GHAhh, ZAMhh, BGDhh))
ch <- Reduce(bind_rows, list(GHAch, ZAMch, BGDch))

labeldf <- read.csv('regression_labels.csv')

sumfun <- function(df, mmnames, fcnames){
  accumdf <- data.frame()
  for(c in unique(df$country)){
    for (name in mmnames){
      sel <- df[df$country == c, name] %>% na.omit
      accumdf[paste0(name, '_min'), c] <- min(sel, na.rm=T)
      accumdf[paste0(name, '_mean'), c] <- mean(sel, na.rm=T)
      accumdf[paste0(name, '_max'), c] <- max(sel, na.rm=T)
      accumdf[paste0(name, '_std'), c] <- sd(sel, na.rm=T)
    }
    for (name in fcnames){
      sel <- df[df$country == c, name] %>% na.omit
      if (length(sel) > 1){
        tab <- table(sel)
        tabfrq <- tab/sum(tab)
        tabnames <- paste0(name, names(tab))
        accumdf[tabnames, c] <- tabfrq
      }
    }
  }
  return(accumdf)
}

##hhhead_religion hhhead_education hhhead_sex urban_rural year
hhdf <- sumfun(hh, c("hhs", "spi24", "asset_index", "pop", "market", 
                   "hh_size", "female_5de", "forestsavanna", 
                   "hhhead_age", "hhhead_literate", 
                   "dependants", "workers", "irrigation"),
             c("hhhead_religion", "hhhead_education", "hhhead_sex", "urban_rural", "year"))


write.csv(hhdf, 'AllHHsummary.csv')


chdf <- sumfun(ch, c("haz", "spi24", "asset_index", "pop", "market", 
                     "hh_size", "female_5de", "forestsavanna", 
                     "hhhead_age", "hhhead_literate", "dependants", "workers", 
                     "age", "birth_order", "within24", "irrigation"),
               c("hhhead_religion", "hhhead_education", "year", "urban_rural", "hhhead_sex", "gender"))


write.csv(chdf, 'AllCHsummary.csv')


