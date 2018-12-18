### JUVENILE RATIO
# this script calculates the ratio of HY to AHY MAMU on surveys
# last updated: Nov 14, 2018 by E Kelsey and J Yee

## SET WORKING DIRECTORY
setwd("~/WERC-SC/MAMU")

## LOAD LIBRARIES
library(data.table)
library(dplyr)
library(lubridate)
library(mosaic)

### READ IN OBSERVATIONS FILES
## all previous years (datafile that Jon got from Bill)
read.csv('~/WERC-SC/MAMU/JuvenileRatioData2013.csv', # MAMU_All_locs_1_03.20.2017_DRAFT.csv',
         stringsAsFactors = FALSE) %>% 
  mutate(Surv_No = NA) -> obs_2013
## 2017
read.csv('~/WERC-SC/MAMU/JuvenileRatioData2017.csv', 
         stringsAsFactors = FALSE) %>% 
  # summarize by survey day:
  group_by(Surv_No, Date, Year, Survey, Julian) %>% 
  summarise(AHY = sum(AHY), HY = sum(HY)) %>% 
  as.data.frame() -> obs_2017
## 2018
read.csv('~/WERC-SC/MAMU/JuvenileRatioData2018.csv', 
         stringsAsFactors = FALSE) %>% 
  # summarize by survey day:
  group_by(Surv_No, Date, Year, Survey, Julian) %>% 
  summarise(AHY = sum(AHY), HY = sum(HY))  %>% 
  as.data.frame() -> obs_2018
## combine all years
obs_all <- rbind(obs_2013, obs_2017, obs_2018) 

### SELECT ONLY MAMU DETECTIONS WITHIN JUVENILE TIMEFRAME (JULY 10 - AUGUST 24) AND CALCUALTE ADJUSTED HY AND AHY DETECTIONS 
obs_mamu_all <- obs_all %>% 
  mutate(date = as_date(Date, format = "%m/%d/%Y"), 
         jdate = yday(date)) %>% # add column with Julian Date
  filter(jdate <= 236 + leap_year(Year),  
         jdate >= 191 + leap_year(Year)) %>% # select date btwn July 10 & Aug 24 (JD 191 - 236, or JD 192 - 237 in leap years)
  mutate(HYadj = HY/(-1.5433 + 0.0098 * jdate), # apply regression to account for HY birds that haven't fledged by survey date
         AHYadj = ifelse(jdate <= 199, # apply regression for AHY birds that are still incubting by survey date
                         AHY/(1 - (18.7145545 - 0.18445455 * jdate  +  0.00045455 * (jdate^2))), AHY),
         Surv_No = as.numeric(as.factor(date)))

### CALUCLATE JUVENILE RATIO FROM ADJUSTED HY AND AHY DETECTION VALUES
# un-adjusted data
juvRatio_all <- obs_mamu_all %>% 
  group_by(Year) %>%
  summarise(nSvy = n_distinct(Surv_No),
            cov = cov(AHY, HY, use = "pairwise.complete.obs"),
            AHYsum = sum(AHY, na.rm = TRUE), AHYvar = var(AHY, na.rm = TRUE), AHYave = mean(AHY, na.rm = TRUE),
            HYsum = sum(HY, na.rm = TRUE), HYvar = var(HY, na.rm = TRUE), HYave = mean(HY, na.rm = TRUE)) %>% 
  mutate(juvRat = HYsum/AHYsum, # juvenile ratio per survey, adjusted 
         juvRatVar = (((HYvar/(AHYave^2))+((HYave^2)*AHYvar)/(AHYave^4))-(((2*HYave)*cov)/(AHYave^3)))/nSvy,
         juvRatSE = juvRatVar^(0.5))

## data adjusted for HY birds that haven't yet fledged and AHY birds still on the nest
juvRatio_all_cor <- obs_mamu_all %>% 
  group_by(Year) %>%
  summarise(nSvy = n_distinct(Surv_No),
            covAdj = cov(HYadj, AHYadj, use = "pairwise.complete.obs"),
            AHYsumAdj = sum(AHYadj, na.rm = TRUE), AHYvarAdj = var(AHYadj, na.rm = TRUE), AHYaveAdj = mean(AHYadj, na.rm = TRUE), 
            HYsumAdj = sum(HYadj, na.rm = TRUE), HYvarAdj = var(HYadj, na.rm = TRUE), HYaveAdj = mean(HYadj, na.rm = TRUE)) %>% # annual covariance of adjusted HY and AHY
  mutate(juvRatAdj = HYsumAdj/AHYsumAdj, # juvenile ratio per survey
         juvRatVarAdj = (((HYvarAdj/(AHYaveAdj^2))+((HYaveAdj^2)*AHYvarAdj)/(AHYaveAdj^4))-(((2*HYaveAdj)*covAdj)/(AHYaveAdj^3)))/nSvy,
         juvRatSEadj = juvRatVarAdj^(0.5))

### SAVE juvenile ratio results and statistics
write.csv(juvRatio_all, file = '~/WERC-SC/MAMU/juvRatio_2013-2018.csv',
          row.names = FALSE) 
write.csv(juvRatio_all_cor, file = '~/WERC-SC/MAMU/juvRatio_2013-1018_adj.csv',
          row.names = FALSE) 


  