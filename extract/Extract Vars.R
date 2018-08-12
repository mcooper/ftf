##########################################
#Assemble relevant variables
##########################################

##Nutrition
#  Stunting
#  Wasting
#  Underweight

#Food Security
#  HDDS
#  HFIAS
#  Other?

#Gender
#  Gender of head
#  WEIA

#Income?

#Ag Productivity?

#Household weights

setwd('D://Documents and Settings/mcooper/Google Drive/Feed the Future/')

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
#Zambia 2012
############################
children <- read.dta('ZMB-RALS-12/Zambia ftf/Zambia_children_file_PR.dta') %>%
  select(stunting_z=haz,
         wasting_z=whz,
         weight_z=waz,
         #Household number?
         pbs_id)

household <- read.dta('ZMB-RALS-12/Zambia ftf/Zambia_household_PR.dta')

household$asset_index <- PCA_assets(household[ , paste0('d0', seq(1, 8))], ntiles=5)
household <- household %>%
  select(hungerscale,
         hhhunger,
         pbs_id,
         hh_size,
         asset_index,
         a09) #household type

hhmembers <- read.dta('ZMB-RALS-12/Zambia ftf/Zambia_hhmembers_PR.dta') %>%
  filter(c04 > 13) %>%
  group_by(pbs_id) %>%
  summarize(perc_literate=mean(zic_literacy > 2))

WEAI_HH <- read.dta('ZMB-RALS-12/Zambia ftf/Zambia_WEAI_HH_PR.dta') %>%
  filter(a04==1) %>%
  select(pbs_id, WEAI=ci)

match <- read.dta13('ZMB-RALS-12/Zambia ftf/georeferenced WEAI/Zambia_CrossWalkFile.dta') %>%
  mutate(hh_refno=paste0('ZAM-', cluster, '-', hh)) %>%
  mutate(pbs_id=PBS_ID) %>%
  select(-PBS_ID)

hh <- Reduce(f=function(x, y) merge(x, y, all.x=T, all.y=F), list(household, hhmembers, WEAI_HH, match))

child <- merge(hh, children)

write.csv(hh, 'ZMB-RALS-12/household.csv', row.names=F)
write.csv(child, 'ZMB-RALS-12/children.csv', row.names=F)
############################
#Zambia 2015
############################
#No anthropometry data! Skipping for now

# out <- list.files('ZMB-RALS-15/Rawdata/Data/SPSS/')
# 
# df <- data.frame()
# for (o in out){
#   raw <- read.spss(paste0('ZMB-RALS-15/Rawdata/Data/SPSS/', o))
#   labels <- attr(raw, 'variable.labels')
#   new <- data.frame(code=labels, desc=names(labels))
#   df <- bind_rows(df, new)
# }


############################
#Ghana
############################

child <- read.csv('GHA-PBS-12/Ghana FTF datasets/MODULE 8 - CHILDREN ANTHROPOMETRY.csv') %>%
  select(hhserial,
         ind_refno=mid,
         age=q8_4,
         gender=q8_5,
         weight=q8_6,
         height=q8_7)

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


coords <- read.csv('Coords&SPI.csv')

matz$hh_refno <- paste0('GHA-', matz$hhserial)

out <- merge(matz, coords, all.x=T, all.y=F)

cor(out[ , c('zlen', 'zwei', 'zwfl', 'spi6', 'spi12', 'spi24', 'spi36')] %>% na.omit)

library(ggplot2)
ggplot(out) + geom_point(aes(x=longitude, y=latitude, col=spi6))


############################
#Bangladesh
############################

household <- read.dta13('Bangladesh/4. BIHS_household_2011_15.dta') %>%
  mutate(hh_refno=paste0('BGD-', a01)) %>%
  select(hhhead_education,
         hhhead_literate,
         hhhead_gender,
         hhsize=memb_total,
         f_weai_fivede, #Female score on five domains of empowerment
         m_weai_fivede, #Male score on five domains of empowerment
         weai_parity_gap, #Numeric gap between men's and women's scores
         fcs, #Food consumption score
         hhs_total, #household hunger scale
         survey_year,
         hh_refno)

household$asset_score <- read.dta13('Bangladesh/4. BIHS_household_2011_15.dta') %>%
  select(land_total,land_rented,land_owned_rentedout,land_owned_notoperated,land_owned_operated,
         house_owned,house_rooms,house_electricity,house_watersource,house_toilet,house_roof,
         house_walls,house_floor,asset_qty_cattle,asset_qty_poultry,asset_qty_sheepgoat,
         asset_qty_otherlivestock,fertilizer) %>%
  PCA_assets(ntiles = 5)

child <- read.dta13('Bangladesh/5. BIHS_child_2011_15.dta') %>%
  mutate(hh_refno=paste0('BGD-', a01)) %>%
  select(haz,
         waz,
         whz,
         survey_year,
         hh_refno)

spi <- read.csv('Feed the Future/Coords&SPI.csv') %>%
  filter(country=='Bangladesh') %>%
  mutate(survey_year = ifelse(year < 2015, 2011, 2015))

lc <- read.csv('Landcover.csv')%>%
  filter(grepl('BGD', hh_refno)) %>%
  mutate(survey_year = ifelse(year < 2015, 2011, 2015))

lc[is.na(lc)] <- 0

out1 <- merge(child, spi, all.x=T, all.y=F)

out <- merge(out1, household, all=T)

library(lme4)
library(lmerTest)

out$survey_year <- factor(as.character(out$survey_year), levels=c('2011', '2015'))

haz <- lm(haz ~ hhhead_gender + hhsize + weai_parity_gap + fcs + hhs_total + 
     asset_score + spi36, data=out)
summary(haz)


whz <- lm(whz ~ hhhead_gender + hhsize + weai_parity_gap + fcs + hhs_total + 
            asset_score + spi24, data=out)
summary(haz)




