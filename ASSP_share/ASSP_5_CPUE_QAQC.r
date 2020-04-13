#### STORM-PETREL CPUE METADATA
# this script calculates net time and CPUE
# created: March 24, 2020 by: E Kelsey
# last edited: March 25, 2020

### SET WORKING DIRECTORY
setwd("~/WERC-SC/ASSP_share")

## LOAD LIBRARIES
library(data.table)
library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)

### READ IN DATA
metadata <- read.csv('~/WERC-SC/ASSP_share/ASSP_4_metadata_CPUE_20200325.csv') %>% 
  mutate_at(c("App_sunset", "std_ending"), .funs = ~as.POSIXct(., format="%m/%d/%Y %H:%M")) %>% 
  mutate_at(c("net_open_1", "net_close_1", "net_open_2", "net_close_2", "net_open_3",
              "net_close_3", "net_open_4", "net_close_4", "net_open_5", "net_close_5"),
            .funs = ~as.POSIXct(., format="%Y-%m-%d %H:%M:%S")) %>% 
  filter(TRUE)
# catches <- read.csv('~/WERC-SC/ASSP_share/ASSP_4_catches_BANDING_20200320.csv')

# add columns neccessary for QAQC
metadata_qaqc <- metadata %>% 
  mutate_at(c("App_sunset", "std_ending", "net_open_1", "net_close_1"), 
            .funs = list(time = ~ hms::as.hms(.))) %>% 
  mutate(CPUE_ratio = CPUEstd/CPUEraw) %>% 
  filter(TRUE)

### App_sunset (Plot per month)
ggplot(metadata_qaqc, aes(month, App_sunset_time)) +
  geom_point(aes(color = year)) +
  scale_color_gradient(low="purple", high="orange") +
  theme_bw()
# --distribution looks accurate--

### Std_ending
ggplot(metadata_qaqc, aes(month, std_ending_time)) +
  geom_point(aes(color = year)) +
  scale_color_gradient(low="black", high="light blue") +
  theme_bw()
# --distribution looks accurate--

### net_open and net_close (could use max/min)
summary(as.POSIXct(metadata_qaqc$net_open_1_time))
# --                 Min.               1st Qu.                Median                  Mean               3rd Qu.                  Max. 
# -- "1970-01-01 00:00:00" "1970-01-01 20:45:00" "1970-01-01 21:02:30" "1970-01-01 20:33:23" "1970-01-01 21:36:00" "1970-01-01 23:35:00" 
summary(as.POSIXct(metadata_qaqc$net_close_1_time))
# --                  Min.               1st Qu.                Median                  Mean               3rd Qu.                  Max. 
# -- "1970-01-01 00:00:00" "1970-01-01 01:24:00" "1970-01-01 02:00:00" "1970-01-01 03:37:47" "1970-01-01 02:17:30" "1970-01-01 23:59:00" 

### min and min_std
ggplot(metadata_qaqc, aes(min)) +
  geom_histogram() +
  theme_bw()
summary(metadata_qaqc$min)
# Min.  1st Qu.  Median    Mean   3rd Qu.    Max.    NA's 
# 56.0   214.0   293.0    280.3   316.0     1022.0      22

ggplot(metadata_qaqc, aes(min_std)) +
  geom_histogram() +
  theme_bw()
summary(metadata_qaqc$min_std)
# Min.  1st Qu.  Median    Mean   3rd Qu.    Max.    NA's 
# 0.0   189.0     239.6   223.0   267.3     317.1      22

ggplot(metadata_qaqc, aes(min, min_std)) +
  geom_point(aes(color = Flagged_Y.N)) +
  # scale_fill_manual(breaks = c("NA", "Y"), values = c("black", "red")) +
  theme_bw()
# -- longer "min" periods shortened to <350 for "min_std" as expected --


### ASSP and ASSPstd
ggplot(metadata_qaqc, aes(ASSP)) +
  geom_histogram() +
  stat(summary) +
  theme_bw()
# -- lower counts more common, tail out to 60
summary(metadata_qaqc$ASSP)
# Min.    1st Qu.  Median    Mean   3rd Qu.    Max.    NA's 
# 1.00    8.00    14.00     17.18   23.00     56.00      27

ggplot(metadata_qaqc, aes(ASSPstd)) +
  geom_histogram() +
  theme_bw()
# -- distribution more spread out below 
summary(metadata_qaqc$ASSPstd)
# Min.    1st Qu.  Median    Mean   3rd Qu.    Max.    NA's 
# 0.00    7.00    12.00     14.03   19.00     45.00      27 

ggplot(metadata_qaqc, aes(Net_dim, ASSPstd)) +
  geom_boxplot() +
  theme_bw()

ggplot(metadata_qaqc, aes(ASSP, ASSPstd)) +
  geom_point() +
  theme_bw()

# check data with high number of catches, to see if data expalains why they could be high (e.g. longer net open periods, etc.)
ASSP_test <- metadata %>% 
  select(sessionID, App_sunset:net_close_1, min:CPUEstd) %>% 
  filter(ASSP > 40 | ASSPstd > 30)
# -- not necessarily clear why these are high but see previous comment about curve --

# CPUEraw
ggplot(metadata_qaqc, aes(CPUEraw)) +
  geom_histogram() +
  theme_bw()
summary(metadata_qaqc$CPUEraw)
#  Min.   1st Qu.  Median   Mean  3rd Qu.   Max.    NA's 
# 0.00282 0.03245 0.05298 0.06270 0.08795 0.20779      46

# CPUEstd
ggplot(metadata_qaqc, aes(CPUEstd)) +
  geom_histogram() +
  theme_bw()
summary(metadata_qaqc$CPUEstd)
#  Min.   1st Qu.  Median  Mean   3rd Qu.  Max.    NA's 
# 0.00000 0.02858 0.05351 0.06161 0.08511 0.25810      47 

ggplot(metadata_qaqc, aes(CPUEraw, CPUEstd)) +
  geom_point() +
  facet_wrap(.~Net_dim) +
  theme_bw()

ggplot(metadata_qaqc, aes(Net_dim, CPUEstd)) +
  geom_boxplot() +
  theme_bw()

CPUEstd_outliers <- metadata_qaqc %>% 
  select(sessionID, App_sunset:net_close_1, min:CPUEstd, test) %>% 
  filter(CPUEstd > 0.2)
