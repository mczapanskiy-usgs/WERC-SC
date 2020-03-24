#### STORM-PETREL CPUE METADATA
# this script calculates net time and CPUE
# created: March 24, 2020 by: E Kelsey
# last edited: 

### SET WORKING DIRECTORY
setwd("~/WERC-SC/ASSP_share")

## LOAD LIBRARIES
library(data.table)
library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)

### READ IN DATA
metadata <- read.csv('~/WERC-SC/ASSP_share/ASSP_4_metadata_CPUE_20200313.csv') %>% 
  mutate_at(c("App_sunset", "std_ending", "net_open_1", "net_close_1", "net_open_2", "net_close_2", "net_open_3",
              "net_close_3", "net_open_4", "net_close_4", "net_open_5", "net_close_5"),
            .funs = ~as.POSIXct(.)) %>% 
  filter(TRUE)
# catches <- read.csv('~/WERC-SC/ASSP_share/ASSP_4_catches_BANDING_20200320.csv')

metadata_qaqc <- metadata %>% 
  mutate(App_sunset_time = hms::as.hms(App_sunset),
         std_ending_time = hms::as.hms(std_ending)) %>% 
  filter(TRUE)

# App_sunset (Plot per month?)
ggplot(metadata_qaqc, aes(month, App_sunset_time)) +
  geom_point(aes(color = year)) +
  scale_color_gradient(low="purple", high="orange") +
  theme_bw()

# Std_ending
ggplot(metadata_qaqc, aes(month, std_ending_time)) +
  geom_point(aes(color = year)) +
  scale_color_gradient(low="black", high="light blue") +
  theme_bw()

# Net open (could use max/min)
summarize(metadata, min_open = min(net_open_1))

# Net close (could max/min)

# Min
ggplot(metadata_qaqc, aes(min)) +
  geom_histogram() +
  theme_bw()

min_test <- metadata %>% 
  filter(min > 500)

# Min_std
ggplot(metadata_qaqc, aes(min_std)) +
  geom_histogram() +
  theme_bw()

ggplot(metadata_qaqc, aes(min, min_std)) +
  geom_point() +
  theme_bw()

min_std_test <- metadata %>% 
  filter(min_std == 0) # > 500)

# Assp
ggplot(metadata_qaqc, aes(ASSP)) +
  geom_histogram() +
  theme_bw()

# Asspstd
ggplot(metadata_qaqc, aes(ASSPstd)) +
  geom_histogram() +
  theme_bw()

ggplot(metadata_qaqc, aes(ASSP, ASSPstd)) +
  geom_point() +
  theme_bw()

ASSP_test <- metadata %>% 
  filter(ASSP > 40, ASSPstd > 20)

# CPUEraw
ggplot(metadata_qaqc, aes(CPUEraw)) +
  geom_histogram() +
  theme_bw()

# CPUEstd
ggplot(metadata_qaqc, aes(CPUEstd)) +
  geom_histogram() +
  theme_bw()

ggplot(metadata_qaqc, aes(CPUEraw, CPUEstd)) +
  geom_point() +
  theme_bw()

