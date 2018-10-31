setwd('G://My Drive/Feed the Future/')

options(stringsAsFactors = FALSE)

library(dplyr)
library(stringr)

load('BGD_data.Rdata')
bgd_hh <- allhh
bgd_hh$country <- "Bangladesh"
bgd_child <- allchild
bgd_child$country <- "Bangladesh"

load('GHA_data.Rdata')
gha_hh <- allhh
gha_hh$country <- "Ghana"
gha_hh$year <- 2012

gha_child <- allchild
gha_child$country <- "Ghana"
gha_child$hh_refno <- NULL
gha_child$year <- "2012"

hh <- Reduce(bind_rows, list(gha_hh, bgd_hh))
ch <- Reduce(bind_rows, list(gha_child, bgd_child))

labeldf <- read.csv('regression_labels.csv')

sumfun <- function(df, mmnames, fcnames){
  accumdf <- data.frame()
  for(c in unique(df$country)){
    for (name in mmnames){
      sel <- df[df$country == c, name] %>% na.omit
      accumdf[paste0(name, '-min'), c] <- min(sel, na.rm=T)
      accumdf[paste0(name, '-mean'), c] <- mean(sel, na.rm=T)
      accumdf[paste0(name, '-max'), c] <- max(sel, na.rm=T)
      #accumdf[paste0(name, '-std'), c] <- sd(sel, na.rm=T)
    }
    for (name in fcnames){
      sel <- df[df$country == c, name, drop=FALSE] %>% na.omit
      if (nrow(sel) > 1){
        tab <- table(sel)
        tabfrq <- (tab/sum(tab))*100
        tabnames <- paste0(name, '-', names(tab))
        accumdf[tabnames, c] <- tabfrq
      }
    }
    accumdf[ , c] <- round(accumdf[ , c], 1)
  }
  return(accumdf)
}

hhdf <- sumfun(hh, c("hhs", "spi24", "asset_index", "pop", 
                   "hh_size",
                   "hhhead_age", 
                   "dependents", "irrigation", "precip_mean"),
             c("hhhead_religion", "hhhead_literate", "hhhead_education", "hhhead_sex", "year"))

getWord <- function(string, pattern, before){
  ind <- gregexpr(pattern, string)[[1]][1]
  if (grepl(pattern, string)){
    if (before){
      return(substr(string, 0, ind - 1))
    }else{
      return(substr(string, ind + 1, nchar(string)))
    }
  }else{
    if (before){
      return(string)
    }else{
      return(NA)
    }
  }
}

hhdf$Parameter <- sapply(row.names(hhdf), FUN = getWord, pattern='-', before=TRUE)
hhdf$Function <- sapply(row.names(hhdf), FUN = getWord, pattern='-', before=FALSE)

hhdf <- merge(hhdf, labeldf, all.x=T, all.y=F) %>%
  arrange(rank)

write.csv(hhdf, 'AllHHsummary.csv', na = "")


chdf <- sumfun(ch, c("haz", "spi24", "asset_index", "pop", 
                     "hh_size",
                     "hhhead_age", "dependents",
                     "age", "birth_order", "within24", "irrigation", "precip_mean"),
               c("hhhead_religion", "hhhead_literate", "hhhead_education", "year", "hhhead_sex", "gender"))

chdf$Parameter <- sapply(row.names(chdf), FUN = getWord, pattern='-', before=TRUE)
chdf$Function <- sapply(row.names(chdf), FUN = getWord, pattern='-', before=FALSE)

chdf <- merge(chdf, labeldf, all.x=T, all.y=F) %>%
  arrange(rank)

write.csv(chdf, 'AllCHsummary.csv', na = "")


