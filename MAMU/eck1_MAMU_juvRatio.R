### JUVENILE RATIO
# this script calculates the ratio of HY to AHY MAMU on surveys

## SET WORKING DIRECTORY
setwd("~/WERC-SC/MAMU")

## LOAD LIBRARIES
library(data.table)
library(dplyr)
library(lubridate)
library(mosaic)

## READ IN OBSERVATIONS FILES
# all previous years (datafile that Jon got from Bill)
read.csv('~/WERC-SC/MAMU/MAMU_All_locs_1_03.20.2017_DRAFT.csv',
         stringsAsFactors = FALSE) -> obs_all
# this year
read.csv('~/WERC-SC/MAMU/MAMU_Obs_2017_Distance.csv',
         stringsAsFactors = FALSE) -> obs_2017

## SELECT ONLY MAMU DETECTIONS WITHIN JUVENILE TIMEFRAME (JULY 10 - AUGUST 23)
## AND CALCUALTE ADJUSTED HY AND AHY DETECTIONS 
# previous years
obs_mamu_all <- obs_all %>% 
  mutate(date = as_date(Date, format = "%m/%d/%Y"), 
         jdate = yday(date)) %>% # add column with Julian Date
  filter(jdate > 191, jdate < 235) %>% # select date between JD 192 - 234 (time frame when juveniles are detected)
  mutate(HYadj = HY/(-1.5433 + 0.0098 * jdate), # apply regression to HY birds to account for those that haven't fledged by survey date
         AHYadj = AHY/(1 - (18.7145545 - 0.18445455 * jdate  +  0.00045455 * (jdate^2))), # apply regression for AHY birds that are still incubting by survey date
         Surv_No = as.numeric(as.factor(date)))
# this year
obs_mamu_2017 <- obs_2017 %>%
  select(-OBJECTID, -Field1) %>%
  distinct() %>%
  filter(SPP == "MAMU") %>%
  mutate(date = as_date(date, format = "%m/%d/%Y"),
         jdate = yday(date)) %>% # add column with Julian Date
  filter(jdate > 191, jdate < 235) %>% # select date between JD 192 - 234 (time frame when juveniles are detected)
  mutate(HYadj = HY/(-1.5433 + 0.0098 * jdate), # apply regression to HY birds to account for those that haven't fledged by survey date
         AHYadj = AHY/(1 - (18.7145545 - 0.18445455 * jdate  +  0.00045455 * (jdate^2)))) # apply regression for AHY birds that are still incubting by survey date

test <- obs_mamu %>%  
  group_by(Year) %>% 
  distinct()


## CALUCLATE JUVENILE RATIO FROM ADJUSTED HY AND AHY DETECTION VALUES
# previous years
juvRatio_all <- obs_mamu_all %>% 
  group_by(Year) %>%
  summarise(nSvy = n_distinct(Surv_No),
            HYsum = sum(HY, na.rm = TRUE), HYvar = var(HY, na.rm = TRUE), HYave = mean(HY, na.rm = TRUE), HYsd = sd(HY, na.rm = TRUE),
            AHYsum = sum(AHY, na.rm = TRUE), AHYvar = var(AHY, na.rm = TRUE), AHYave = mean(AHY, na.rm = TRUE), AHYsd = sd(AHY, na.rm = TRUE),
            cov = cov(AHY, HY, use = "pairwise.complete.obs"),
            HYsumAdj = sum(HYadj, na.rm = TRUE), HYvarAdj = var(HYadj, na.rm = TRUE), HYaveAdj = mean(HYadj, na.rm = TRUE), HYsdAdj = sd(HYadj, na.rm = TRUE),
            AHYsumAdj = sum(AHYadj, na.rm = TRUE), AHYvarAdj = var(AHYadj, na.rm = TRUE), AHYaveAdj = mean(AHYadj, na.rm = TRUE), AHYsdAdj = sd(AHYadj, na.rm = TRUE), 
            covAdj = cov(HYadj, AHYadj, use = "pairwise.complete.obs")) %>% # annual covariance of adjusted HY and AHY
  mutate(
         # cov = cov(HYsum, AHYsum, use = "pairwise.complete.obs"),
         # covAdj = cov(HYsumAdj, AHYsumAdj, use = "pairwise.complete.obs"),
         juvRat = HYsum/AHYsum, # juvenile ratio per survey, adjusted 
         juvRatSE = (1/nSvy)*(HYvar/AHYvar),
         juvRatSEadj = (1/nSvy)*(HYvarAdj/AHYvarAdj),
         juvRatAdj = HYsumAdj/AHYsumAdj, # juvenile ratio per survey
         juvRatVar = (1/nSvy)*(((HYvar/(AHYave^2))+((HYave^2)*AHYvar)/(AHYave^4))-(((2*HYave)*cov)/(AHYave^3))),
         juvRatVarAdj = (1/nSvy)*(((HYvarAdj/(AHYaveAdj^2))+((HYaveAdj^2)*AHYvarAdj)/(AHYaveAdj^4))-(((2*HYaveAdj)*covAdj)/(AHYaveAdj^3))))

# this year
juvRatio_2017 <- obs_mamu_2017 %>%
  group_by(YEAR_) %>%
  summarise(nSvy = n_distinct(Surv_No),
            HYsum = sum(HY, na.rm = TRUE), HYvar = var(HY, na.rm = TRUE), HYave = mean(HY, na.rm = TRUE), HYsd = sd(HY, na.rm = TRUE), # annual sum, variance and mean  number of HY
            AHYsum = sum(AHY, na.rm = TRUE), AHYvar = var(AHY, na.rm = TRUE), AHYave = mean(AHY, na.rm = TRUE), AHYsd = sd(AHY, na.rm = TRUE), # annual sum, variance, and mean number of AHY
            cov = cov(AHY, HY, use = "pairwise.complete.obs"), # annual covariance of HY and AHY
            HYsumAdj = sum(HYadj, na.rm = TRUE), HYvarAdj = var(HYadj, na.rm = TRUE), HYaveAdj = mean(HYadj, na.rm = TRUE), HYsdAdj = sd(HYadj, na.rm = TRUE), # annual sum, variance, and mean adjusted number of HY
            AHYsumAdj = sum(AHYadj, na.rm = TRUE), AHYvarAdj = var(AHYadj, na.rm = TRUE), AHYaveAdj = mean(AHYadj, na.rm = TRUE), AHYsdAdj = sd(AHYadj, na.rm = TRUE), # annual sum, variance, and mean adjusted number of AHY
            covAdj = cov(HYadj, AHYadj, use = "pairwise.complete.obs")) %>% # annual covariance of adjusted HY and AHY
  mutate(
         # cov = cov(HYsum, AHYsum, use = "pairwise.complete.obs"),
  #        covAdj = cov(HYsumAdj, AHYsumAdj, use = "pairwise.complete.obs"),
         juvRat = HYsum/AHYsum, # juvenile ratio per survey, adjusted
         juvRatAdj = HYsumAdj/AHYsumAdj, # juvenile ratio per survey
         juvRatSE = (1/nSvy)*(HYvar/AHYvar),
         juvRatSEadj = (1/nSvy)*(HYvarAdj/AHYvarAdj),
         juvRatVar = (1/nSvy)*(((HYvar/(AHYave^2))+((HYave^2)*AHYvar)/(AHYave^4))-(((2*HYave)*cov)/(AHYave^3))),
         juvRatVarAdj = (1/nSvy)*(((HYvarAdj/(AHYaveAdj^2))+((HYaveAdj^2)*AHYvarAdj)/(AHYaveAdj^4))-(((2*HYaveAdj)*covAdj)/(AHYaveAdj^3))))

# save juvenile ratio results and statistics
write.csv(juvRatio_all, file = '~/WERC-SC/MAMU/juvRatio_all.csv',
          row.names = FALSE) 
write.csv(juvRatio_2017, file = '~/WERC-SC/MAMU/juvRatio_2017.csv',
          row.names = FALSE) 
write.csv(obs_mamu_2017, file = '~/WERC-SC/MAMU/JuvenileRatio_AllData_2017.csv',
          row.names = FALSE) 

rbind(juvRatio_all, juvRatio_2017) -> juvRatio

  