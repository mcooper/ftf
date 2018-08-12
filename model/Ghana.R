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

#################################
#Prep for nutrition calculations
#################################
weianthro <- read.table("WHO Anthro reference tables/weianthro.txt", header=T)
lenanthro <- read.table("WHO Anthro reference tables/lenanthro.txt", header=T)
wflanthro <- read.table("WHO Anthro reference tables/wflanthro.txt", header=T)
wfhanthro <- read.table("WHO Anthro reference tables/wfhanthro.txt", header=T)
hcanthro <- read.table("WHO Anthro reference tables/hcanthro.txt", header=T)
acanthro <- read.table("WHO Anthro reference tables/acanthro.txt", header=T)
bmianthro <- read.table("WHO Anthro reference tables/bmianthro.txt", header=T)
ssanthro <- read.table("WHO Anthro reference tables/ssanthro.txt", header=T)
tsanthro <- read.table("WHO Anthro reference tables/tsanthro.txt", header=T)

source('igrowup_standard.r')

############################
#Extract hh vars
############################

#Anthropometry
#Asset Index
#

greater_less <- function(vect, cut){
  seq <-NULL
  for (i in 1:length(vect)){
    ct <- sum(vect < vect[i] + cut & vect > vect[i] - cut) - 1
    seq <- c(seq, ct)
  }
  seq
}

child <- read.csv('GHA-PBS-12/Ghana FTF datasets/MODULE 8 - CHILDREN ANTHROPOMETRY.csv') %>%
  select(hhserial,
         ind_refno=mid,
         age=q8_4,
         gender=q8_5,
         weight=q8_6,
         height=q8_7)

childem <- read.csv('GHA-PBS-12/Ghana FTF datasets/MODULE 2 - HOUSEHOLD DEMOGRAPHY.csv') %>%
  filter(q2_03 %in% c('Child (Son)', 'Child (Daughter)')) %>%
  mutate(q2_04=ifelse(q2_05=='Years', q2_04*12, q2_04)) %>%
  arrange(hhserial, desc(q2_04)) %>%
  group_by(hhserial) %>%
  mutate(birth_order=row_number(),
         within24=greater_less(q2_04, 24)) %>%
  select(hhserial,
         ind_refno=mid,
         birth_order,
         within24)

child <- merge(child, childem, all.x=T, all.y=F) %>% na.omit

igrowup.standard(mydf = child,
                 sex = gender,
                 age = age,
                 age.month = T, 
                 weight = weight, 
                 lenhei = height)

matz$zlen[matz$flen==1] <- NA
matz$zwei[matz$fwei==1] <- NA
matz$zwfl[matz$fwfl==1] <- NA
matz <- matz[matz$age > 6, ]

matz <- matz %>%
  select(hhserial, ind_refno, zlen, zwei, zwfl, gender, age, birth_order, within24)

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

WEAI <- read.dta13('GHA-PBS-12/Ghana WEAI indicators.dta')
WEAI$a <- rowMeans(WEAI[ , c('feelinputdecagr', 'raiprod_any')], na.rm=T)
WEAI$b <- rowMeans(WEAI[ , c('jown_count', 'jrightanyagr', 'credjanydec_any')], na.rm=T)
WEAI$c <- WEAI$incdec_count
WEAI$d <- rowMeans(WEAI[ , c('speakpublic_any', 'groupmember_any')], na.rm=T)
WEAI$e <- rowMeans(WEAI[ , c('leisuretime', 'npoor_z105')], na.rm=T)
WEAI$weai_fivede <- rowMeans(WEAI[ , c('a', 'b', 'c', 'd', 'e')])
WEAI$weai_fivede[is.nan(WEAI$weai_fivede)] <- NA

WEAI <- WEAI %>% 
  select(hhserial, sex, weai_fivede) %>%
  spread(sex, weai_fivede) %>%
  mutate(combo_5de=Female/(Male + Female)) %>%
  select(hhserial, male_5de=Male, female_5de=Female, combo_5de)

#################################
#' Missing:
#' FCS - there is detailed food consumption data for the previous week,
#' but now way to get a count of days for each category.  It's too detailed!
#' 


##########################################
#Combine
##########################################

hh <- Reduce(merge, list(hhdem, hunger, assets, WEAI, depend_ratio, hhchar))
hh$hh_refno <- paste0('GHA-', hh$hhserial)

hh$dependents <- hh$dependents/hh$hh_size

sperr <- c('GHA-1080303601', 'GHA-1080505107', 'GHA-1081513210', 'GHA-1081915810', 'GHA-1082016411', 'GHA-2071400902', 'GHA-2071901212', 'GHA-2072001709', 'GHA-2072001810', 'GHA-2072002216', 'GHA-2072202707', 'GHA-2080203317', 'GHA-2081008715', 'GHA-2090317212', 'GHA-2090819514', 'GHA-2100321209', 'GHA-2100521616', 'GHA-2100722309')

hh <- hh %>%
  filter(!hh_refno %in% sperr & urban_rural == 'Rural')

########################################
#Extract Landcover Vars
##################################################
lc <- read.csv('Landcover.csv')

lc[is.na(lc)] <- 0

ag <- paste0('cci_', c('10', '11', '12', '20', '190', '200', '30'))
savanna <- paste0('cci_', c('120', '122', '130', '180', '150', '151', '152', '40', '100'))
forest <- paste0('cci_', c('60', '61', '62', '80', '90', '160', '170', '110', '210'))
mos_ag_fr <- 'cci_30'
mos_fr_ag <- 'cci_40'
mos_fr_sv <- 'cci_100'
mos_sv_fr <- 'cci_110'

getPercetCover <- function(selcols, allcolmatch, df){
  if(length(selcols) > 1){
    selcolsum <- rowSums(df[ , selcols[selcols %in% names(df)]], na.rm=T)
  } else{
    selcolsum <- df[ , selcols]
  }
  allcolsum <- rowSums(df[ , grepl(allcolmatch, names(df))], na.rm=T)
  return(selcolsum/allcolsum)
}

lc$ag <- getPercetCover(c(ag, mos_ag_fr), 'cci_', lc)
lc$savanna <- getPercetCover(c(savanna, mos_sv_fr), 'cci_', lc)
lc$forest <- getPercetCover(c(forest, mos_fr_ag, mos_fr_sv), 'cci_', lc)
lc$forestsavanna = lc$forest + lc$savanna
lc$mos_ag_fr <- getPercetCover(mos_ag_fr, 'cci_', lc)
lc$mos_fr_ag <- getPercetCover(mos_fr_ag, 'cci_', lc)
lc$mos_fr_sv <- getPercetCover(mos_fr_sv, 'cci_', lc)
lc$mos_sv_fr <- getPercetCover(mos_sv_fr, 'cci_', lc)

lc <- lc %>%
  unique %>%
  mutate(pop=pop/1000,
         market=market/1000) %>%
  select(pop, market, ag, forestsavanna, hh_refno, year)


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
  select(hhs, spi24, asset_index, pop, market, hh_size, male_5de, female_5de, combo_5de, forestsavanna, latitude, longitude, hh_refno,
         hhhead_religion, hhhead_age, hhhead_education, hhhead_literate, hhhead_sex, dependents, urban_rural, mean_annual_precip, cluster) %>%
  merge(relmap, all.x=T, all.y=F) %>%
  mutate(hhhead_religion=as.factor(new_rel),
         new_rel=NULL,
         hhhead_education=hhhead_education!='',
         hhhead_sex=factor(hhhead_sex, levels=c('Male', 'Female')),
         urban_rural=as.factor(urban_rural)) %>%
  na.omit

allchild <- Reduce(merge, list(spi, lc, hh, ltn, matz)) %>%
  select(haz=zlen, spi24, asset_index, pop, market, hh_size, male_5de, female_5de, combo_5de, forestsavanna, latitude, longitude, hh_refno,
         hhhead_religion, hhhead_age, hhhead_education, hhhead_literate, hhhead_sex, dependents, urban_rural, gender, age, birth_order, within24, mean_annual_precip, cluster) %>%
  merge(relmap, all.x=T, all.y=F) %>%
  mutate(hhhead_religion=as.factor(new_rel),
         new_rel=NULL,
         hhhead_education=hhhead_education!='',
         hhhead_sex=as.factor(hhhead_sex),
         urban_rural=as.factor(urban_rural),
         gender=as.factor(gender)) %>%
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

spchild <- SpatialPoints(coords = allchild[ , c('longitude', 'latitude')], proj4string = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
spmchild <- spTransform(spchild, CRS('+proj=utm +zone=30 +ellps=clrk80 +towgs84=-124.76,53,466.79,0,0,0,0 +units=m +no_defs '))

dnn75kmchild <- dnearneigh(coordinates(spmchild), 0, 75000, longlat=F)
wlistchild<-nb2listw(dnn75kmchild, style="W")

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

############Child Nutrition############################

#Check for autocorrelation in the outcome variable
moran.test(allchild$haz, wlistchild)

#Significant autocorrelation, use spatial lag model
mod <- lagsarlm(haz~asset_index + pop + market + hh_size + spi24 + hhhead_religion + mean_annual_precip +
                hhhead_age + hhhead_literate + hhhead_sex + dependents + gender + age + birth_order + within24 + cluster,  
                data=allchild, wlistchild)
summary(mod)
#SPI not significant with Anthro, could be reduced sample size

allchild$country <- 'GHA'
write.csv(allchild, 'GHA_allchild.csv', row.names=F)

#############Household Hunger###############################


#Check for autocorrelation in the outcome variable
moran.test(allhh$hhs, wlisthh)

#Significant autocorrelation, use spatail lag model
mod2 <- lagsarlm(hhs~asset_index + pop + market + hh_size + spi24 + mean_annual_precip + hhhead_religion + 
                hhhead_age + hhhead_literate + 
             hhhead_sex + dependents, 
                data=allhh, wlisthh)
summary(mod2)


mod3 <- lagsarlm(hhs~asset_index + pop + market + hh_size + spi24 + mean_annual_precip + hhhead_religion + 
                   hhhead_age + hhhead_literate + 
                   hhhead_sex + dependents + female_5de, 
                 data=allhh, wlisthh)
summary(mod3)


moran.test(residuals(mod2), wlisthh)

#SPI greatly affects household hunger score, see if ES or WE affects outcome
#With ES Buffering
mod3 <- lmer(hhs~asset_index + pop + market + hh_size + spi24 + hhhead_religion + 
                   hhhead_age + hhhead_literate + hhhead_sex + dependents  + 
                   forestsavanna + spi24 + (1|cluster), data=allhh)
summary(mod3)
#ES does not buffer, in fact associated with worse outcomes

#With Gender Buffering
mod4 <- lmer(hhs~asset_index + pop + market + hh_size + spi24 + hhhead_religion + 
                   hhhead_age + hhhead_literate + hhhead_sex + dependents  + 
                   female_5de + (1|cluster), data=allhh)
summary(mod4)


#With Interaction Term
mod5 <- lmer(hhs~asset_index + pop + market + hh_size + spi24 + hhhead_religion + 
                   hhhead_age + hhhead_literate + hhhead_sex + dependents  + 
                   female_5de + forestsavanna + female_5de*forestsavanna + (1|cluster), data=allhh)
summary(mod5)
#WEAI has not relationship either

save.image(file = 'GhanaWorkspace.RData')

allhh$country <- 'GHA'
write.csv(allhh, 'GHA_allhh.csv', row.names=F)

###########################################################
#Write results for appendix
#######################################
library(reshape2)

cleanwrite <- function(mods, labeldf){
  r2 <- function(mod){
    actual <- mod$residuals + mod$fitted.values
    fitted <- mod$fitted.values
    sstot <- sum((actual - mean(actual))^2)
    ssres <- sum((actual - fitted)^2)
    rsq <- 1 - ssres/sstot
    rsq
  }
  prep <- function(num){
    as.character(signif(num, 2))
  }
  extract <- function(mod){
    if (!is.null(summary(mod)$Coef)){
      coef <- summary(mod)$Coef %>% round(5) %>% data.frame
    } else{
      coef <- summary(mod)$coefficients %>% round(5) %>% data.frame
    }
    coef <- signif(coef, digits=2)
    coef$Parameter <- row.names(coef)
    coef$Std..Error <- paste0('(', coef$Std..Error, ')')
    p <- names(coef)[4]
    coef$Estimate <- ifelse(coef[ , 4] < 0.001, paste0(coef$Estimate, '***'),
                            ifelse(coef[ , 4] < 0.01, paste0( coef$Estimate, '**'),
                                   ifelse(coef[ , 4] < 0.05, paste0(coef$Estimate, '*'), 
                                          ifelse(coef[ , 4] < 0.1, paste0(coef$Estimate, '.'),
                                                 coef$Estimate))))
    coef[ , 4] <- NULL
    coef[ , 3] <- NULL
    res <- coef %>%
      melt(id.vars=names(coef)[3]) %>%
      mutate(Parameter = ifelse(variable=='Std..Error', paste0(Parameter, ' SE'), Parameter),
             variable=NULL) %>%
      arrange(Parameter)
    
    if (class(mod)=="sarlm"){
      res <- bind_rows(res, data.frame(Parameter='Rho', value=prep(mod$rho)))
    }
    
    res <- res %>%
      bind_rows(data.frame(Parameter='AIC', value=as.character(round(AIC(mod), 2)))) %>%
      bind_rows(data.frame(Parameter='R-Squared', value=prep(r2(mod))))
    res
  }
  
  all <- Reduce(function(x, y){merge(x, y, by='Parameter', all=T)}, Map(extract, mods))
  
  all <- merge(all, labeldf, by='Parameter', all.x=T, all.y=F)
  
  all <- all[order(all$rank), ]
  
  all
}

labeldf <- read.csv('regression_labels.csv')

write.csv(cleanwrite(list(mod), labeldf), 'GHA_stunting.csv', row.names=F)

write.csv(cleanwrite(list(mod2, mod3, mod4, mod5), labeldf), 'GHA_hhs.csv', row.names=F)


write.csv(allhh, 'Ghana_regressiondf.csv', row.names=F)
write.csv(allchild, 'Ghana_regressiondf_child.csv', row.names=F)


# 
# 
# library(ggplot2)
# ggplot(na.omit(allhh)) + geom_histogram(aes(spi12), binwidth=0.1) + 
#   ylab('Number of Households') + 
#   xlab('12-Month Standardized Precipitation Index') + 
#   ggtitle('Variation in SPI in Ghana') + 
#   theme(plot.title = element_text(hjust = 0.5))
# ggsave('Ghana_SPI_Hist.png', height = 4, width = 6, units = 'in')
# 
# 
# 
