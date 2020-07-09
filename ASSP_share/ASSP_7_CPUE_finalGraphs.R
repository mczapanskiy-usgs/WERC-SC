#### STORM-PETREL MISTNETTING CPUE GRAPHS FOR FINAL NFWF REPORT
# this script creates final graphs representing CPUE across years, between sites, and related to other variables
# created: July 7, 2020 by: E Kelsey
# last edited: 

### SET WORKING DIRECTORY
setwd("~/WERC-SC/ASSP_share")

## LOAD LIBRARIES
library(tidyr)
library(dplyr)
library(lubridate)
library(hms)
library(tidyverse)
library(ggplot2)
library(EnvStats)
library(openxlsx)
library(readxl)

### READ IN DATA
metadata <- read.csv('~/WERC-SC/ASSP_share/ASSP_6_CPUE_broodpatch_20200709.csv') %>% 
  # mutate_at(c("app_sunset", "std_ending"), .funs = ~as.POSIXct(., format="%m/%d/%Y %H:%M")) %>% 
  mutate_at(c("app_sunset", "std_ending", "net_open_1", "net_close_1", "net_open_2", "net_close_2", "net_open_3",
              "net_close_3", "net_open_4", "net_close_4", "net_open_5", "net_close_5"),
            .funs = ~as.POSIXct(., format="%Y-%m-%d %H:%M:%S")) %>% 
  # mutate_at(c("app_sunset", "std_ending", "net_open_1", "net_close_1"), 
  #           .funs = list(time = ~ hms::as.hms(.))) %>% 
  mutate(CPUEbrd_test = CPUEstd*BPfreq_Y,
         CPUE_ratio = CPUEstd/CPUEraw, 
         month = as.character(month),
         year = as.character(year)) %>%
  filter(TRUE)

metadata_effort <- metadata %>% 
  select(site_code, session_ID, app_sunset, std_ending, lat, long)

catches <- read_excel("CHIS_ASSP_mistnet_database_04162020.xlsx", sheet = "Banding", 
                      col_names = TRUE, na = c("NA", "ND")) %>% 
  mutate_at(c("capture_time", "release_time"),
            .funs = ~as.POSIXct(., format="%Y-%m-%d %H:%M:%S")) %>%
  filter(species == "ASSP",
         BP != "NR", 
         BP != "NA") %>%
  mutate(assumeBreed = mosaic::derivedFactor(
    "Y" = (BP == "B" | BP == "b" | BP == "2" | BP == "3" | BP == "4"),
    "N" = (BP == "D" | BP == "d" | BP == "0" | BP == "5" | BP == "PD" | BP == "pd" | BP == "1" | BP == "1.5" | BP == "4.5"),
    .default = "ND")) %>% 
  left_join(metadata_effort, by = c("site_code", "session_ID", "lat", "long")) %>% 
  mutate(std = if_else(std_ending > capture_time, "1", "0"),
         capture_date = date(capture_time),
         captureT = hms::as_hms(capture_time),
         catchPastSS = app_sunset - capture_time) %>%
  select(catch_ID, session_ID, app_sunset, std_ending, month, day, year:release_time, capture_date, captureT, catchPastSS,
         species:recapture, std, diet, BP, assumeBreed, uncorr_mass:notes) %>%
  filter(TRUE)

metadata_test <- metadata %>% 
  filter(min > 600)

### summary stats
# CPUEraw
summary(metadata$CPUEraw)
# CPUEstd
summary(metadata$CPUEstd)
# broodpatch frequency
summary(metadata$BPfreq_Y)
summary(metadata$CPUEbrd)
summary(metadata$CPUEbrd_test)
summary(metadata$CPUEbrd_std)

# ggplot(metadata, aes(CPUEraw)) +
#   geom_histogram() +
#   theme_bw()
# 
# ggplot(metadata, aes(CPUEstd)) +
#   geom_histogram() +
#   theme_bw()
# 
# ggplot(metadata, aes(CPUEbrd_std)) +
#   geom_histogram() +
#   theme_bw()


## MONTH
month <- ggplot(metadata, aes(month, CPUEstd)) +
  geom_boxplot() +
  stat_n_text() +
  theme_bw()
month

month_isl <- ggplot(metadata, aes(month, CPUEstd)) +
  geom_boxplot() +
  facet_wrap(.~subisland_code) +
  stat_n_text() +
  theme_bw()
month_isl %+% subset(metadata, subisland_code %in% c("DR", "EAI", "PI", "SBI", "SR", "Sutil"))

## YEAR
year_isl <- ggplot(metadata, aes(year, CPUEstd)) +
  geom_boxplot() +
  facet_wrap(.~subisland_code) +
  xlab("Year") + ylab("Standardized CPUE") +
  stat_n_text(size = 2, y.pos = 0.3) +
  theme(axis.text.x = element_text(angle = -45)) 
  # theme_bw()
year_isl %+% subset(metadata, subisland_code %in% c("DR", "EAI", "PI", "SBI", "SR", "Sutil"))

year <- ggplot(metadata, aes(year, CPUEstd)) +
  geom_boxplot() +
  xlab("Year") + ylab("Standardized CPUE") +
  stat_n_text(size = 3, y.pos = 0.3) +
  theme_bw()
year

# MONTH VS YEAR
month_year <- ggplot(metadata, aes(month, CPUEstd)) +
  geom_boxplot() +
  facet_wrap(.~year) +
  xlab("Month") + ylab("Standardized CPUE") +
  stat_n_text(size = 2, y.pos = 0.3) +
  theme_bw()
month_year
# month_year %+% subset(metadata, subisland_code %in% c("DR", "EAI", "PI", "SBI", "SR", "Sutil"))


# ISLAND AND SITE
island <- ggplot(metadata, aes(subisland_code, CPUEstd)) +
  geom_boxplot() +
  xlab("Subisland Site") + ylab("Standardized CPUE") +
  stat_n_text(size = 4, y.pos = 0.3) +
  theme_bw()
island %+% subset(metadata, subisland_code %in% c("DR", "EAI", "PI", "SBI", "SR", "Sutil"))

site <- ggplot(metadata, aes(site_code, CPUEstd)) +
  geom_boxplot() +
  facet_wrap(.~island_code, scales = "free_x") +
  xlab("Sites Across Islands") + ylab("Standardized CPUE") +
  stat_n_text(size = 3, y.pos = 0.3) +
  theme_bw()
site # %+% subset(metadata, site_code %in% c("DR", "EAI", "PI", "SBI", "SR", "Sutil"))

# CATCH TIMES PAST SUNSET
geom_histogram(binwidth = 10) +
  geom_vline(xintercept = 318, color = "red") +
  geom_vline(xintercept = 0, color = "green") +
  xlab("Time past Sunset (min)") + ylab("Number of ASSP Catches") +
  facet_wrap(.~ year) +
  theme_bw()
endT_yr

endT_isl <- ggplot(catches, aes(catchPastSS)) +
  geom_histogram(binwidth = 10) +
  geom_vline(xintercept = 318, color = "red") +
  geom_vline(xintercept = 0, color = "green") +
  xlab("Time past Sunset (min)") + ylab("Number of ASSP Catches") +
  facet_wrap(.~ subisland_code) +
  theme_bw()
endT_isl %+% subset(catches, subisland_code %in% c("DR", "EAI", "PI", "SBI", "SR", "Sutil"))

# CPUE vs. cumulative mintues
CPUE_min <- ggplot(metadata, aes(min, CPUEraw)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  xlab("Total Netting Minutes") + ylab("CPUE") +
  theme_bw()
CPUE_min
