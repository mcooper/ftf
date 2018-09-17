setwd('G://My Drive/Feed the Future/')

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

##################################
#Extract hh vars
##################################
children <- read.dta('ZMB-RALS-12/Zambia ftf/Zambia_children_file_PR.dta') %>%
  select(haz,
         age=i06,
         gender=i03,
         pbs_id) %>%
  mutate(gender=factor(gender, levels=c(1, 2), labels=c('Male', 'Female')))

household <- read.dta('ZMB-RALS-12/Zambia ftf/Zambia_household_PR.dta')

household$asset_index <- PCA_assets(household[ , paste0('d0', seq(1, 8))], ntiles=5)
household <- household %>%
  mutate(cluster=as.factor(a02)) %>%
  select(hhs=hungerscale,
         pbs_id,
         hh_size,
         asset_index,
         cluster)

edumap <- data.frame(zic_edu_lvl=seq(0, 17),
                     hhhead_education=c('None', 'Standard', 'Standard', 'Standard', 'Standard', 'Standard', 'Standard', 'Standard', 'Form', 'Form', 'Form', 'Form', 'Form', 'Form', 'Form', 'University', 'University', 'University'))

hhmembers <- read.dta('ZMB-RALS-12/Zambia ftf/Zambia_hhmembers_PR.dta') %>%
  filter(c03 == 1) %>%
  filter(!duplicated(pbs_id) & c02!=99) %>%
  merge(edumap, all.x=T, all.y=F) %>%
  select(pbs_id, 
         hhhead_sex=c02, 
         hhhead_age=c04,
         hhhead_education,
         hhhead_literate=zic_literacy) %>%
  mutate(hhhead_sex=factor(hhhead_sex, levels=c(1, 2), labels=c('Male', 'Female')),
         hhhead_education=factor(hhhead_education, levels=c('None', 'Standard', 'Form', 'University')),
         hhhead_literate=hhhead_literate!=1 & hhhead_literate != 99)

depend_ratio <- read.dta('ZMB-RALS-12/Zambia ftf/Zambia_hhmembers_PR.dta') %>%
  mutate(dependant = c04 < 15 | c04 > 65,
         workers = c04 > 15 & c04 < 65) %>%
  group_by(pbs_id) %>%
  summarize(dependents=sum(dependant),
            workers=sum(workers))
         

WEAI_HH_m <- read.dta('ZMB-RALS-12/Zambia ftf/Zambia_WEAI_HH_PR.dta') %>%
  filter(a04==1) %>%
  mutate(male_5de=1-ci) %>%
  select(pbs_id, male_5de)

WEAI_HH_f <- read.dta('ZMB-RALS-12/Zambia ftf/Zambia_WEAI_HH_PR.dta') %>%
  filter(a04==2) %>%
  mutate(female_5de=1-ci) %>%
  select(pbs_id, female_5de)

WEAI_HH <- merge(WEAI_HH_f, WEAI_HH_m) %>%
  mutate(combo_5de=female_5de/(male_5de + female_5de))

match <- read.dta13('ZMB-RALS-12/Zambia ftf/georeferenced WEAI/Zambia_CrossWalkFile.dta') %>%
  mutate(hh_refno=paste0('ZAM-', cluster, '-', hh)) %>%
  mutate(pbs_id=PBS_ID) %>%
  select(-PBS_ID)

hh <- Reduce(f=function(x, y) merge(x, y, all.x=T, all.y=F), list(household, hhmembers, WEAI_HH, depend_ratio, match))
child <- merge(hh, children)

hh$pbs_id <- NULL
child$pbs_id <- NULL

hh$year <- 2012
child$year <- 2012

hh$dependents <- hh$dependents/hh$hh_size

#Remove errant points
hh <- hh %>%
  filter(!hh_refno %in% c('ZAM-3065-5', 'ZAM-3027-247', 'ZAM-3033-314', 'ZAM-3080-9'))
child <- child %>%
  filter(!hh_refno %in% c('ZAM-3065-5', 'ZAM-3027-247', 'ZAM-3033-314', 'ZAM-3080-9'))

########################################
#Extract Landcover Vars
##################################################
lc <- read.csv('Landcover.csv')

lc[is.na(lc)] <- 0

ag <- paste0('cci_', c('10', '11', '12', '20', '190', '200', '30'))
savanna <- paste0('cci_', c('120', '122', '130', '180', '150', '151', '152', '110', '210'))
forest <- paste0('cci_', c('60', '61', '62', '80', '90', '160', '170', '40', '100', '210'))
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
lc$mos_ag_fr <- getPercetCover(mos_ag_fr, 'cci_', lc)
lc$mos_fr_ag <- getPercetCover(mos_fr_ag, 'cci_', lc)
lc$mos_fr_sv <- getPercetCover(mos_fr_sv, 'cci_', lc)
lc$mos_sv_fr <- getPercetCover(mos_sv_fr, 'cci_', lc)

lc$forestsavanna <- lc$forest + lc$savanna

lc <- lc %>%
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

allhh <- Reduce(merge, list(hh, spi, lc, ltn)) %>% 
  select(cluster, hh_refno, hhs, hh_size, asset_index, hhhead_sex, hhhead_age, hhhead_education, hhhead_literate, female_5de, male_5de, combo_5de, 
         latitude, longitude, spi24, pop, market, dependents, forestsavanna, mean_annual_precip) %>%
  na.omit

allchild <- Reduce(merge, list(child, spi, lc, ltn)) %>% 
  select(cluster, hh_refno, haz, hh_size, asset_index, hhhead_sex, hhhead_age, hhhead_education, hhhead_literate, female_5de, male_5de, combo_5de, 
         latitude, longitude, spi24, pop, market, dependents, forestsavanna, age, gender, mean_annual_precip) %>%
  na.omit

#####################
#Make spatial weights
######################
library(sp)
library(spdep)

sphh <- SpatialPoints(coords = allhh[ , c('longitude', 'latitude')], proj4string = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
spmhh <- spTransform(sphh, CRS('+proj=utm +zone=36 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs '))

distmat <- as.matrix(dist(spmhh@coords))
diag(distmat) <- 1000000
mins <- apply(distmat, 1, min)
max(mins)
#The most remote village is 57009 km away from other, so lag must be at least 73000

wlisthh <- spmhh %>%
  #knearneigh(k=4) %>%
  dnearneigh(0, 75000, longlat=F) %>%
  nb2listw(style="W")

wlistchild <- SpatialPoints(coords = allchild[ , c('longitude', 'latitude')], 
                         proj4string = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')) %>%
  spTransform(CRS('+proj=utm +zone=36 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs')) %>%
  coordinates %>%
  #knearneigh(k=4) %>%
  dnearneigh(0, 75000, longlat=F) %>%
  nb2listw(style="W")

###################
#Rescale Variables
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
# allhh <- rescale_many(allhh, c('hhs', 'spi24', 'asset_index', 'pop', 'market', 'hh_size', 'asset_index', 'WEAI', 'forest', 'savanna'))
# 
# allchild <- rescale_many(allchild, c('haz', 'spi24', 'asset_index', 'pop', 'market', 'hh_size', 'asset_index', 'WEAI', 'forest', 'savanna'))

#########################
#Regressions
#######################

############Child Nutrition############################

#Check for autocorrelation in the outcome variable
moran.test(allchild$haz, wlistchild)
#Statistic is very close to significant, p-value=0.6.  Because we have a priori reasons to assume clustering, lets do it spatial

#Significant autocorrelation, use spatial lag model
mod <- lagsarlm(haz~hh_size + asset_index + hhhead_sex + hhhead_age + hhhead_education + hhhead_literate +
                spi24 + pop + market + dependents + age + gender + mean_annual_precip, data=allchild
                , wlistchild)
summary(mod)
#SPI not significant with Anthro, could be reduced sample size

moran.test(mod$residuals, wlistchild)

allchild$country <- 'ZAM'
write.csv(allchild, 'ZAM_allchild.csv', row.names=F)

#############Household Hunger###############################

#Check for autocorrelation in the outcome variable
moran.test(allhh$hhs, wlisthh)
#No significant autocorrelation, perform OLS, but check outcome vars as well

#Significant autocorrelation, use sptail lag model
mod2 <- lm(hhs~hh_size + asset_index + hhhead_sex + hhhead_education + hhhead_literate +
             spi24 + pop + market + dependents + mean_annual_precip, data=allhh)
summary(mod2)
#Very significant, but check error terms
moran.test(residuals(mod2), wlisthh)
#No clustering, continue with OLS in looking for mitigation

#SPI greatly affects household hunger score, see if ES or WE affects outcome
#With ES Buffering
mod3 <- lmer(hhs~hh_size + asset_index + hhhead_sex + hhhead_education + hhhead_literate +
             spi24 + pop + market + dependents + forestsavanna + mean_annual_precip + (1|cluster), data=allhh)
summary(mod3)

#Forest cover does not lowers explanantory power of SPI
moran.test(mod3$residuals, wlisthh)

#With Gender Buffering
mod4 <- lmer(hhs~hh_size + asset_index + hhhead_sex + hhhead_education + hhhead_literate +
             spi24 + pop + market + dependents + female_5de + mean_annual_precip + (1|cluster), data=allhh)
summary(mod4)
#WEAI has no relationship
moran.test(mod4$residuals, wlisthh)


#With Gender Buffering and ES interaction
mod5 <- lmer(hhs~hh_size + asset_index + hhhead_sex + hhhead_education + hhhead_literate +
             spi24 + pop + market + dependents + female_5de + forestsavanna  + female_5de*forestsavanna + mean_annual_precip + (1|cluster), data=allhh)
summary(mod5)
#WEAI has no relationship
moran.test(residuals(mod), wlisthh)

save.image(file = 'ZambiaWorkspace.RData')

allhh$country <- 'ZAM'
write.csv(allhh, 'ZAM_allhh.csv', row.names=F)

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

write.csv(cleanwrite(list(mod), labeldf), 'ZAM_stunting.csv', row.names=F)

write.csv(cleanwrite(list(mod2, mod3, mod4, mod5), labeldf), 'ZAM_hhs.csv', row.names=F)


write.csv(allhh, 'Zambia_regressiondf.csv', row.names=F)
write.csv(allchild, 'Zambia_regressiondf_child.csv', row.names=F)

# 
# library(ggplot2)
# ggplot(na.omit(allhh)) + geom_histogram(aes(spi12), binwidth=0.1) + 
#   ylab('Number of Households') + 
#   xlab('12-Month Standardized Precipitation Index') + 
#   ggtitle('Variation in SPI in Zambia') + 
#   theme(plot.title = element_text(hjust = 0.5))
# ggsave('Zambia_SPI_Hist.png', height = 4, width = 6, units = 'in')



