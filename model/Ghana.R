setwd('G:/My Drive/Feed the Future/')

library(foreign)
library(dplyr)
library(readstata13)
library(tidyr)

options(stringsAsFactors = F)

######################################
#Prep for getting asset index from PCA
######################################

PCA_assets <- function(assets, ntiles){
  #https://academic.oup.com/heapol/article/21/6/459/612115/Constructing-socio-economic-status-indices-how-to
  
  #Drop NAs
  assets_narm <- na.omit(assets)
  
  #Pivot out factor columns
  for (n in names(assets_narm)){
    
    c <- class(assets_narm[ , n])
    if (c=='factor' | c=='character'){
      
      for (f in unique(assets_narm[ , n])){
        assets_narm[ , paste0(n, '_', f)] <- assets_narm[ , n] == f
      }
      
      assets_narm[ , n] <- NULL
      
    }
    
  }
  
  #Do PCA
  res <- prcomp(assets_narm)
  sumry <- summary(res)$importance[2, 1]*100
  
  cat('PC1 explains', sumry, 'percent of the variance')
  
  #get quantiles and cut vector
  out <- quantile(res$x[ , 1], probs=seq(0, 1, 1/ntiles))
  cuts <- cut(res$x[ , 1], out)
  
  #fill back in NAs based on row.names from original df
  temp <- data.frame(cuts=as.integer(cuts), rownames=row.names(assets_narm))
  
  assets$rownames <- row.names(assets)
  
  new <- merge(temp, assets, all=T)
  
  return(new$cuts)
}

############################
#Extract hh vars
############################

assets <- read.csv('GHA-PBS-12/Ghana FTF datasets/MODULE 9 - DWELLING CHARACTERISTICS.csv')
assets$asset_index <- PCA_assets(assets[ , c("q9_01", "q9_02", "q9_03", "q9_04", 
                                             "q9_05", "q9_06", "q9_07", "q9_08", "q9_09", "q9_10", "q9_11", 
                                             "q9_12", "q9_13", "q9_14", "q9_15", "q9_16", "q9_17", "q9_18", 
                                             "q9_19")],
                                 ntiles = 5)
assets <- assets %>% 
  select(hhserial, asset_index)

hunger <- read.csv('GHA-PBS-12/Ghana FTF datasets/MODULE 3A - HOUSEHOLD HUNGER SCALE.csv') %>%
  mutate(q1 = ifelse(q3_01=='No', 0, ifelse(q3_02=="Often (more than 10 times)", 2, 1)),
         q2 = ifelse(q3_03=='No', 0, ifelse(q3_04=="Often (more than 10 times)", 2, 1)),
         q3 = ifelse(q3_05=='No', 0, ifelse(q3_06=="Often (more than 10 times)", 2, 1)),
         hhs = q1 + q2 + q3) %>%
  select(hhserial, hhs)

#need to include some sort of grouping variable.
hhdem <- read.csv('GHA-PBS-12/Ghana FTF datasets/MODULE 1 - HOUSEHOLD IDENTIFICATION.csv') %>%
  mutate(hh_size = as.integer(hhsize),
         cluster = as.factor(eacode)) %>%
  select(hhserial, hh_size, hhhead_sex=hhhsex, hhhead_religion=q1_11, cluster)

depend_ratio <- read.csv('GHA-PBS-12/Ghana FTF datasets/MODULE 2 - HOUSEHOLD DEMOGRAPHY.csv') %>%
  mutate(dependant = ageyrs < 12 | ageyrs > 60,
         workers = ageyrs > 12 & ageyrs < 60) %>%
  group_by(hhserial) %>%
  summarize(dependents=sum(dependant),
            workers=sum(workers))

hhchar <- read.csv('GHA-PBS-12/Ghana FTF datasets/MODULE 2 - HOUSEHOLD DEMOGRAPHY.csv') %>%
  filter(status=='Primary Respondent') %>%
  mutate(hhhead_literate=q2_07!='Cannot Read and Write English' | q2_07a != 'None') %>%
  select(hhserial, urban_rural=urbrur, hhhead_age=ageyrs, hhhead_literate, hhhead_education=q2_10)


##########################################
#Combine
##########################################

hh <- Reduce(merge, list(hhdem, hunger, assets, depend_ratio, hhchar))
hh$hh_refno <- paste0('GHA-', hh$hhserial)

hh$dependents <- hh$dependents/hh$hh_size

sperr <- c('GHA-1080303601', 'GHA-1080505107', 'GHA-1081513210', 'GHA-1081915810', 'GHA-1082016411', 'GHA-2071400902', 'GHA-2071901212', 'GHA-2072001709', 'GHA-2072001810', 'GHA-2072002216', 'GHA-2072202707', 'GHA-2080203317', 'GHA-2081008715', 'GHA-2090317212', 'GHA-2090819514', 'GHA-2100321209', 'GHA-2100521616', 'GHA-2100722309')

hh <- hh %>%
  filter(!hh_refno %in% sperr & urban_rural == 'Rural')

######################################
#read in SPI and combine
#####################################

spi <- read.csv('Coords&SPI.csv')

ltn <- read.csv('RainfallLTN.csv') %>%
  filter(year == 2012) %>%
  select(hh_refno, mean_annual_precip) %>%
  unique

relmap <- data.frame(hhhead_religion=c("Ahmadi", "Catholic", "Islam", "No Religion", "Other", "Other Christian", 
                                       "Pentecostal/Charismatic", "Protestant (Anglican, Lutheran, Presbyterian, Methodist, etc)", 
                                       "Traditionalist"),
                     new_rel=c('Islam', 'Christianity', 'Islam', 'None', 'Traditional', 'Christianity',
                               'Christianity', 'Christianity', 'Traditional'))

allhh <- Reduce(merge, list(spi, lc, hh, ltn)) %>%
  select(hhs, spi24, asset_index, pop, market, hh_size, latitude, longitude, hh_refno,
         hhhead_religion, hhhead_age, hhhead_education, hhhead_literate, hhhead_sex, dependents, urban_rural, mean_annual_precip, cluster) %>%
  merge(relmap, all.x=T, all.y=F) %>%
  mutate(hhhead_religion=as.factor(new_rel),
         new_rel=NULL,
         hhhead_education=hhhead_education!='',
         hhhead_sex=factor(hhhead_sex, levels=c('Male', 'Female')),
         urban_rural=as.factor(urban_rural)) %>%
  na.omit

#####################
#Make spatial weights
######################
library(sp)
library(spdep)

sphh <- SpatialPoints(coords = allhh[ , c('longitude', 'latitude')], proj4string = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
spmhh <- spTransform(sphh, CRS('+proj=utm +zone=30 +ellps=clrk80 +towgs84=-124.76,53,466.79,0,0,0,0 +units=m +no_defs '))

distmat <- as.matrix(dist(spmhh@coords))
diag(distmat) <- 1000000
mins <- apply(distmat, 1, min)
max(mins)
#The most remote village is 72241 km away from other, so lag must be at least 73000

dnn75kmhh <- dnearneigh(coordinates(spmhh), 0, 45000, longlat=F)
wlisthh<-nb2listw(dnn75kmhh, style="W")

######################
#Rescale Vars
#####################

# rescale_many <- function(all, resc_vars){
#   rescale <- function(x){
#     (x - mean(x, na.rm=T))/max(x, na.rm=T)
#   }
#   for (r in resc_vars){
#     if (!r %in% names(all)){
#       cat(r, ' is missing, eedjit!')
#     }
#     all[ , paste0(r, '_orig')] <- all[ , r]
#     all[ , r] <- rescale(all[ , r])
#   }
#   all
# }
# 
# allhh <- rescale_many(allhh, c("spi24", "asset_index", "pop", "market", "hh_size", 
#                                "hhhead_age", "dependents"))
# 
# allchild <- rescale_many(allchild, c('haz', 'spi24', 'asset_index', 'pop', 'market', 'hh_size', 
#                                      "hhhead_age", "dependents", 
#                                      "age", "birth_order"))

#########################
#Regressions
#######################

#############Household Hunger###############################


#Check for autocorrelation in the outcome variable
moran.test(allhh$hhs, wlisthh)

#Significant autocorrelation, use spatail lag model
lmmod <- lm(hhs~asset_index + pop + market + hh_size + spi24 + mean_annual_precip + hhhead_religion + 
                     hhhead_age + hhhead_literate + 
                     hhhead_sex + dependents, 
                   data=allhh)
summary(lmmod)

#Significant autocorrelation, use spatail lag model
lagmod <- lagsarlm(hhs~asset_index + pop + market + hh_size + spi24 + mean_annual_precip + hhhead_religion + 
                hhhead_age + hhhead_literate + 
             hhhead_sex + dependents, 
                data=allhh, wlisthh)
summary(lagmod)

