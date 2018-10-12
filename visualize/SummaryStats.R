setwd('G://My Drive/Feed the Future/')

options(stringsAsFactors = FALSE)

library(dplyr)

load('BGD_data.Rdata')
bgd_hh <- allhh
bgd_hh$country <- "Bangladesh"
bgd_child <- allchild
bgd_child$country <- "Bangladesh"

load('GHA_data.Rdata')
gha_hh <- allhh
gha_hh$country <- "Ghana"
gha_hh$hhhead_education <- as.character(gha_hh$hhhead_education)

gha_child <- allchild
gha_child$country <- "Ghana"
gha_child$hhhead_education <- as.character(gha_child$hhhead_education)
gha_child$hh_refno <- NULL

hh <- Reduce(bind_rows, list(gha_hh, bgd_hh))
ch <- Reduce(bind_rows, list(gha_child, bgd_child))

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
      sel <- df[df$country == c, name, drop=FALSE] %>% na.omit
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


hhdf <- sumfun(hh, c("hhs", "spi24", "asset_index", "pop", "market", 
                   "hh_size",
                   "hhhead_age", "hhhead_literate", 
                   "dependents", "irrigation"),
             c("hhhead_religion", "hhhead_education", "hhhead_sex", "year"))


write.csv(hhdf, 'AllHHsummary.csv')


chdf <- sumfun(ch, c("haz", "spi24", "asset_index", "pop", "market", 
                     "hh_size",
                     "hhhead_age", "hhhead_literate", "dependants",
                     "age", "birth_order", "within24", "irrigation"),
               c("hhhead_religion", "hhhead_education", "year", "hhhead_sex", "gender"))


write.csv(chdf, 'AllCHsummary.csv')


