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

############################
#Extract hh vars
############################

hh <- read.dta13('Bangladesh/4. BIHS_household_2011_15.dta') %>%
  #filter(bio_y_rice_i==0) %>%
  mutate(hh_refno=paste0('BGD-', a01),
         female_5de=f_weai_fivede,
         male_5de=m_weai_fivede,
         workers=memb_15_44 + memb_45_65,
         dependents=memb_und15 + memb_65plus,
         irrigation=bio_a_rice_i/(bio_a_rice_r + bio_a_rice_i),
         year=as.factor(survey_year)) %>%
  select(hhhead_education,
         hhhead_literate,
         hhhead_sex=hhhead_gender,
         hhhead_religion,
         hh_size=memb_total,
         female_5de, #Female score on five domains of empowerment
         male_5de, #Male score on five domains of empowerment
         year,
         hh_refno,
         irrigation,
         dependents,
         workers,
         fcs) %>%
  mutate(combo_5de=female_5de/(female_5de + male_5de),
         hhhead_education=as.factor(hhhead_education),
         hhhead_literate=hhhead_literate!='cannot read and write',
         hhhead_religion=factor(hhhead_religion, labels=c("Muslim", "Hindu", "Christian")))

hhs11 <- read.dta13('Bangladesh/BIHS Raw Data (2011)/051_mod_x3_female.dta') %>%
  mutate(x3_01=ifelse(x3_01=='no', 0, 1),
         x3_03=ifelse(x3_03=='no', 0, 1),
         x3_05=ifelse(x3_05=='no', 0, 1),
         x3_02=ifelse(is.na(x3_02), 0, 
                      ifelse(x3_02=="often (> 10 times)", 2, 1)),
         x3_04=ifelse(is.na(x3_04), 0, 
                      ifelse(x3_04=="often (> 10 times)", 2, 1)),
         x3_06=ifelse(is.na(x3_06), 0, 
                      ifelse(x3_06=="often (> 10 times)", 2, 1)),
         hhs=x3_01*x3_02 + x3_03*x3_04 + x3_05*x3_06,
         hh_refno=paste0('BGD-', a01),
         year=2011) %>%
  select(hh_refno, hhs, year)

hhs15 <- read.dta13('Bangladesh/BIHS Raw Data (2015)/068_r2_mod_x3_female.dta') %>%
  mutate(x3_01=ifelse(x3_01=='no', 0, 1),
         x3_03=ifelse(x3_03=='no', 0, 1),
         x3_05=ifelse(x3_05=='no', 0, 1),
         x3_02=ifelse(is.na(x3_02), 0, 
                      ifelse(x3_02=="Often (> 10 times)", 2, 1)),
         x3_04=ifelse(is.na(x3_04), 0, 
                      ifelse(x3_04=="Often (> 10 times)", 2, 1)),
         x3_06=ifelse(is.na(x3_06), 0, 
                      ifelse(x3_06=="Often (> 10 times)", 2, 1)),
         hhs=x3_01*x3_02 + x3_03*x3_04 + x3_05*x3_06,
         hh_refno=paste0('BGD-', a01),
         year=2015) %>%
  select(hh_refno, hhs, year)

hhs <- bind_rows(hhs11, hhs15)                

edumap <- data.frame(hhhead_education=c("reads in class i", "completed class i", "completed class 2", 
                           "completed class 3", "completed class 4", "completed class 5", 
                           "completed class 6", "completed class 7", "completed class 8", 
                           "completed class 9", "completed ssc/dakhil", "hsc 1st year", 
                           "completed hsc/alim", "degree 1st year", "ba/bsc pass/fazil", 
                           "ba/bsc honors/fazil", "ma/msc and above/kamil", "ssc candidate", 
                           "hsc candidate", "preschool class (general)", "preschool (mosque based)", 
                           "medical/mbbs ", "nursing", "engineer", "diploma engineer", "vocational ", 
                           "other ", "never attended school"),
                     new=c('primary', 'primary', 'primary',
                           'primary', 'primary', 'primary', 
                           'primary', 'primary', 'primary', 
                           'primary', 'secondary', 'high school',
                           'high school', 'university', 'secondary', 
                           'university',  'university',  'university', 
                           'high school', 'primary', 'primary', 
                           'high school', 'high school', 'high school', 'high school', 'high school',
                           'high school', 'never'))

hh <- merge(hh, edumap, all.x=T, all.y=F) %>%
  mutate(hhhead_education=factor(new, levels=c('never', 'primary', 'secondary', 'high school', 'university')),
         new=NULL)

hh$asset_score <- read.dta13('Bangladesh/4. BIHS_household_2011_15.dta') %>%
  select(land_total,land_rented,land_owned_rentedout,land_owned_notoperated,land_owned_operated,
         house_owned,house_rooms,house_electricity,house_watersource,house_toilet,house_roof,
         house_walls,house_floor,asset_qty_cattle,asset_qty_poultry,asset_qty_sheepgoat,
         asset_qty_otherlivestock,fertilizer) %>%
  PCA_assets(ntiles = 5)

#hh$dependents <- hh$dependents/hh$hh_size

child <- read.dta13('Bangladesh/5. BIHS_child_2011_15.dta') %>%
  mutate(hh_refno=paste0('BGD-', a01),
         year=as.factor(survey_year),
         gender=ifelse(gender==1, 'Male',
                       ifelse(gender==2, 'Female', gender))) %>%
  select(haz,
         waz,
         whz,
         year,
         hh_refno,
         age,
         gender
         #Age of child, gender, birth order, siblings within 24 months,
         )

hhcluster <- read.dta("Bangladesh/BIHS Raw Data (2011)/001_mod_a_male.dta") %>%
  mutate(hh_refno=paste0('BGD-', a01),
         cluster=as.factor(vcode_n)) %>%
  select(hh_refno, cluster)

hh <- merge(hh, hhcluster)

sperr <- c(489, 905, 908, 955, 961, 1021, 1048, 1119, 1241, 1242, 1243, 1244, 1245, 1247, 1248, 1249, 1250, 1251, 1252, 1253, 1254, 1255, 1256, 1257, 1258, 1259, 1260, 1560, 2160, 2201, 2346, 2360, 2364, 2478, 2524, 2644, 2719, 2901, 2922, 3042, 3301, 3307, 3735, 3913, 3943, 4195, 4243, 4309, 4354, 4620, 4876, 4915, 4919, 5015, 5026, 5102, 5103, 5183, 5621, 5703, 5779, 5785, 5813, 5817, 5858, 5884, 5921, 5966, 5993, 6103, 6183, 6218, 6354, 6356, 6378, 6460)
sperr <- paste0('BGD-', sperr)

hh <- hh %>%
  filter(!hh_refno %in% sperr)

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
lc$forestsavanna <- lc$forest + lc$savanna
lc$mos_ag_fr <- getPercetCover(mos_ag_fr, 'cci_', lc)
lc$mos_fr_ag <- getPercetCover(mos_fr_ag, 'cci_', lc)
lc$mos_fr_sv <- getPercetCover(mos_fr_sv, 'cci_', lc)
lc$mos_sv_fr <- getPercetCover(mos_sv_fr, 'cci_', lc)

lc <- lc %>%
  unique %>%
  mutate(pop=pop/1000,
         market=market/1000) %>%
  select(pop, market, ag, forestsavanna, hh_refno, year)

lc$year[lc$year==2012 & grepl('BGD', lc$hh_refno)] <- 2011

###################
#Also Irrigation
###############
irrig <- read.csv('BGD_perc_irrig.csv') %>%
  select(hh_refno, perc_irrig) %>%
  unique

######################################
#read in SPI and combine
#####################################

spi <- read.csv('Coords&SPI.csv')
spi$year[spi$year==2012 & grepl('BGD', spi$hh_refno)] <- 2011


ltn <- read.csv('RainfallLTN.csv') %>%
  select(hh_refno, year, mean_annual_precip)

ltn$year[ltn$year==2012 & grepl('BGD', ltn$hh_refno)] <- 2011

allhh <- Reduce(merge, list(hh, lc, spi, hhs, ltn, hhcluster, irrig)) %>%
  select(hhs, year, hh_refno, hhhead_education, hhhead_literate, hhhead_sex, hhhead_religion, hh_size,
         female_5de, male_5de, irrigation, dependents, workers, combo_5de, asset_score, cluster,
         pop, market, forestsavanna, latitude, longitude, spi24, fcs, mean_annual_precip) %>%
  #filter(irrigation < 0.4) %>% 
  na.omit

# hhcount <- table(allhh$hh_refno)
# doubles <- names(hhcount)[hhcount==2]
# 
# allhh <- allhh %>%
#   filter(!hh_refno %in% doubles)

allchild <- merge(allhh, child) %>%
  select(haz, year, hh_refno, hhhead_education, hhhead_literate, hhhead_sex, hhhead_religion, hh_size,
         female_5de, male_5de, perc_irrig, dependents, combo_5de, asset_score, cluster,
         pop, market, forestsavanna, latitude, longitude, spi24, gender, age, mean_annual_precip) %>%
  na.omit


hhcount <- table(allchild$hh_refno)
doubles <- names(hhcount)[hhcount==2]

allchild <- allchild %>%
  filter(hh_refno %in% doubles)


######################
#Build spatial weights
######################
library(sp)
library(spdep)

sphh <- SpatialPoints(coords = allhh[ , c('longitude', 'latitude')], proj4string = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
spmhh <- spTransform(sphh, CRS('+proj=utm +zone=36 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs '))

distmat <- as.matrix(dist(spmhh@coords))
diag(distmat) <- 1000000
distmat[distmat == 0] <- 1000000
mins <- apply(distmat, 1, min)
max(mins)
#The most remote village is 25080 km away from other, so lag must be at least 26000
rm(distmat)

wlisthh <- spmhh %>%
  #knearneigh(k=4) %>%
  dnearneigh(0, 65000, longlat=F) %>%
  nb2listw(style="W")

wlistchild <- SpatialPoints(coords = allchild[ , c('longitude', 'latitude')], 
                            proj4string = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')) %>%
  spTransform(CRS('+proj=utm +zone=36 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs')) %>%
  coordinates %>%
  #knearneigh(k=4) %>%
  dnearneigh(0, 85000, longlat=F) %>%
  nb2listw(style="W")

########################################
#Let's do a quick Chow test
#######################################
library(strucchange)
library(splm)
library(plm)

allchild <- allchild %>% arrange(year)

sctest(haz~hhhead_education + hhhead_literate + hhhead_sex + hhhead_religion + hh_size +
         irrigation + dependents + asset_score  + 
         pop + market + spi24 + gender + age, type='Chow', point=439, data=allchild)

#Looks like we can treat both years as the same?

##MORAN'S I TEST
moran.test(allchild$haz, wlistchild)

allhh <- allhh %>% arrange(year)

sctest(hhs~hhhead_education + hhhead_literate + hhhead_religion + hh_size + hhhead_sex +
         irrigation + dependents + asset_score  + 
         pop + market + spi24 + year, type='Chow', data=allhh)

#Looks like we can treat both years as the same?

##MORAN'S I TEST
moran.test(allhh$hhs, wlisthh)


#For both stunting and HHS:
#Spatial Autocorrelation
#Chow test says both years are the same

#########################
#Regressions
#######################

############Child Nutrition############################
#Significant autocorrelation, use spatial lag model
mod <- lm(haz~hhhead_education + hhhead_literate + hhhead_religion + hh_size + 
                  perc_irrig + dependents + asset_score  + year + 
                  pop + market + spi24 + year + gender + age + mean_annual_precip, data=allchild)
summary(mod)
 #And increase in SPI leads to more stunting?  WTF?

moran.test(mod$residuals, wlistchild)

allchild$country <- 'BGD'
write.csv(allchild, 'BGD_allchild.csv', row.names=F)

#############Household Hunger###############################


#Significant autocorrelation, use sptail lag model
mod2 <- lagsarlm(hhs~hhhead_education + hhhead_literate + hh_size + hhhead_sex +
                   + dependents + asset_score  + perc_irrig + 
                   pop + market + spi24 + year + mean_annual_precip, 
                 data=allhh, wlisthh)
summary(mod2)

#Significant autocorrelation, use sptail lag model
mod3 <- lagsarlm(hhs~asset_score + dependents + workers + hhhead_education + hhhead_literate + hhhead_religion + hh_size + hhhead_sex +
                   pop + market + spi24 + year + mean_annual_precip + female_5de, 
                 data=allhh, wlisthh)
summary(mod3)


moran.test(residuals(mod2), wlisthh)
#SPI is related to decreased HHS

#SPI greatly affects household hunger score, see if ES or WE affects outcome
#With ES Buffering
mod3 <- lmer(hhs~spi24 + forestsavanna + dependents + year + 
             perc_irrig + asset_score + hhhead_literate + hhhead_religion + 
                   hhhead_education + hh_size + hhhead_sex  + 
                   pop + market  + (1|cluster), data=allhh)
summary(mod3)

allhh$gap_5de <- allhh$male_5de - allhh$female_5de

#With Gender Buffering
mod4 <- lmer(hhs~spi24 + year + female_5de + 
                 hhhead_education + hhhead_literate + hhhead_religion + hh_size + hhhead_sex +
               perc_irrig + dependents + asset_score  + 
                   pop + market + (1|cluster), data=allhh)
summary(mod4)

#Interaction Term 
mod5 <- lmer(hhs~spi24 + forestsavanna + female_5de + forestsavanna*female_5de + dependents + year + 
             perc_irrig + asset_score + hhhead_literate + hhhead_religion + 
                     hhhead_education + hh_size + hhhead_sex  + 
                     pop + market + mean_annual_precip + (1|cluster), data=allhh)
summary(mod5)

#Also test FCS
# mod5 <- lagsarlm(fcs~spi24 + dependents + year + 
#                    irrigation + asset_score + hhhead_literate + hhhead_religion + 
#                    hhhead_education + hh_size + hhhead_sex  + 
#                    pop + market, data=allhh, wlisthh, tol.solve=1.0e-12)
# summary(mod5)

save.image(file="BangladeshWorkspace.Rdata")

allhh$country <- 'BDG'
write.csv(allhh, 'BGD_allhh.csv', row.names=F)

###########################
##First order differencing
###########################

hh11 <- allhh[allhh$year==2011, ] %>%
  select(hh_refno,
         spi24_11=spi24,
         hhs_11=hhs)
hh15 <- allhh[allhh$year==2015, ]

hh <- merge(hh11, hh15)

hh$spi24_dif <- hh$spi24 - hh$spi24_11
hh$hhs <- hh$hhs - hh$hhs_11

mod6 <- lm(hhs~spi24_dif, data=hh)
summary(mod6)

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

write.csv(cleanwrite(list(mod), labeldf), 'BGD_stunting.csv', row.names=F)

write.csv(cleanwrite(list(mod2, mod3, mod4, mod5), labeldf), 'BGD_hhs.csv', row.names=F)


write.csv(allhh, 'Bangladesh_regressiondf.csv', row.names=F)
write.csv(allchild, 'Bangladesh_regressiondf_child.csv', row.names=F)





