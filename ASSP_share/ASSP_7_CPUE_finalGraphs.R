#### STORM-PETREL MISTNETTING CPUE GRAPHS FOR FINAL NFWF REPORT
# this script creates final graphs representing CPUE across years, between sites, and related to other variables
# created: July 7, 2020 by: E Kelsey
# last edited: Sept 10, 2020

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
library(RColorBrewer)

### READ IN DATA
metadata <- read.csv('~/WERC-SC/ASSP_share/cpue.csv') %>% # ASSP_6_CPUE_broodpatch_20200709.csv') %>% 
  # mutate_at(c("app_sunset", "std_ending"), .funs = ~as.POSIXct(., format="%m/%d/%Y %H:%M")) %>% 
  mutate_at(c("app_sunset", "std_ending", "net_open_1", "net_close_1", "net_open_2", "net_close_2", "net_open_3",
              "net_close_3", "net_open_4", "net_close_4", "net_open_5", "net_close_5"),
            .funs = ~as.POSIXct(., format="%Y-%m-%d %H:%M:%S")) %>% 
  # mutate_at(c("app_sunset", "std_ending", "net_open_1", "net_close_1"), 
  #           .funs = list(time = ~ hms::as.hms(.))) %>% 
  mutate(CPUEbrd_test = CPUEstd*BPfreq_Y,
         CPUE_ratio = CPUEstd/CPUEraw, 
         month = as.character(session_month),
         year = as.character(session_year)) %>%
  filter(TRUE)

metadata_effort <- metadata %>% 
  select(site_code, session_ID, app_sunset, std_ending, lat, long)

catches <- read.csv('~/WERC-SC/ASSP_share/cpue.csv') %>% # read_excel("CHIS_ASSP_mistnet_database_04162020.xlsx", sheet = "Banding", col_names = TRUE, na = c("NA", "ND")) %>% 
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
  filter(min > 750)

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
  stat_n_text(y.pos = 0.3) +
  xlab("Month") + ylab("Standardized CPUE") +
  theme_bw()
month
ggsave(width = 20, height = 20, dpi=300, filename = "~/WERC-SC/ASSP_share/figures/CPUE_month.pdf")

month_isl <- ggplot(metadata, aes(month, CPUEstd)) +
  geom_boxplot() +
  facet_wrap(.~island_code) +
  stat_n_text(y.pos = 0.3) +
  xlab("Month") + ylab("Standardized CPUE") +
  # scale_x_discrete(name ="Month",
  #                  limits=c("April", "May", "June", "July", "August", "September")) +
  theme_bw()
month_isl # %+% subset(metadata, subisland_code %in% c("DR", "EAI", "PI", "SBI", "SR", "Sutil"))
ggsave(width = 7, height = 7, dpi=300, filename = "~/WERC-SC/ASSP_share/figures/CPUE_month_isl.pdf")

# MONTH AND ASSUMED BREEDERS
metadata_brd <- metadata %>% 
  select(CPUEstd, CPUEbrd_std, month)

metadata_brd <- gather(metadata_brd, "CPUEtype", "CPUE", CPUEstd:CPUEbrd_std)
  
month_brd <- ggplot(metadata_brd, aes(month, CPUE, color = CPUEtype)) +
  geom_boxplot() +
  stat_n_text(y.pos = 0.3) +
  xlab("Month") + ylab("Standardized CPUE") +
  theme_bw()
month_brd
ggsave(width = 20, height = 20, dpi=300, filename = "~/WERC-SC/ASSP_share/figures/CPUEbrd_month.pdf")


## YEAR
year_isl <- ggplot(metadata, aes(year, CPUEstd)) +
  geom_boxplot() +
  facet_wrap(.~island_code) +
  xlab("Year") + ylab("Standardized CPUE") +
  stat_n_text(size = 2, y.pos = 0.3) +
  theme(axis.text.x = element_text(angle = -45)) 
  # theme_bw()
year_isl # %+% subset(metadata, island_code %in% c("DR", "EAI", "PI", "SBI", "SR", "Sutil"))
ggsave(width = 20, height = 20, dpi=300, filename = "~/WERC-SC/ASSP_share/figures/CPUE_yr_isl.pdf")

year <- ggplot(metadata, aes(year, CPUEstd)) +
  geom_boxplot() +
  xlab("Year") + ylab("Standardized CPUE") +
  stat_n_text(size = 3, y.pos = 0.3) +
  theme_bw()
year
ggsave(width = 20, height = 20, dpi=300, filename = "~/WERC-SC/ASSP_share/figures/CPUE_yr.pdf")

# YEAR AND ASSUMED BREEDERS
metadata_brd_yr <- metadata %>% 
  select(CPUEstd, CPUEbrd_std, year)
metadata_brd_yr <- gather(metadata_brd_yr, "CPUEtype", "CPUE", CPUEstd:CPUEbrd_std)

year_brd <- ggplot(metadata_brd_yr, aes(year, CPUE, color = CPUEtype)) +
  geom_boxplot() +
  xlab("Year") + ylab("Standardized CPUE") +
  stat_n_text(size = 3, y.pos = 0.3) +
  theme(axis.text.x = element_text(angle = -45))
year_brd
ggsave(width = 20, height = 20, dpi=300, filename = "~/WERC-SC/ASSP_share/figures/CPUEbrd_yr.pdf")

# MONTH VS YEAR
month_year <- ggplot(metadata, aes(month, CPUEstd)) +
  geom_boxplot() +
  facet_wrap(.~year) +
  xlab("Month") + ylab("Standardized CPUE") +
  stat_n_text(size = 2, y.pos = 0.3) +
  theme_bw()
month_year %+% subset(metadata, year %in% c("1994", "1995", "1996", "2004", "2005", "2006", "2007", "2009", 
                                            "2010", "2011", "2014", "2015", "2016", "2017", "2018"))
ggsave(width = 20, height = 20, dpi=300, filename = "~/WERC-SC/ASSP_share/figures/CPUE_month_yr.pdf")


# ISLAND AND SITE
subisl <- ggplot(metadata, aes(subisland_code, CPUEstd)) +
  geom_boxplot() +
  xlab("Sub-Island Site") + ylab("Standardized CPUE") +
  stat_n_text(size = 4, y.pos = 0.3) +
  theme_bw()
subisl %+% subset(metadata, subisland_code %in% c("DR", "EAI", "PI", "SBI", "SR", "Sutil"))
ggsave(width = 20, height = 20, dpi=300, filename = "~/WERC-SC/ASSP_share/figures/CPUE_subIsl.pdf")

island <- ggplot(metadata, aes(island_code, CPUEstd)) +
  geom_boxplot() +
  xlab("Island") + ylab("Standardized CPUE") +
  stat_n_text(size = 4, y.pos = 0.3) +
  theme_bw()
island # %+% subset(metadata, subisland_code %in% c("DR", "EAI", "PI", "SBI", "SR", "Sutil"))
ggsave(width = 20, height = 20, dpi=300, filename = "~/WERC-SC/ASSP_share/figures/CPUE_isl.pdf")

island_brd <- ggplot(metadata, aes(island_code, CPUEbrd_std)) +
  geom_boxplot() +
  xlab("Island") + ylab("Standardized CPUEof Assumed Breeders") +
  stat_n_text(size = 4, y.pos = 0.3) +
  theme_bw()
island_brd # %+% subset(metadata, subisland_code %in% c("DR", "EAI", "PI", "SBI", "SR", "Sutil"))
ggsave(width = 20, height = 20, dpi=300, filename = "~/WERC-SC/ASSP_share/figures/CPUEbrd_isl.pdf")

site <- ggplot(metadata, aes(site_code, CPUEstd)) +
  geom_boxplot() +
  facet_wrap(.~island_code, scales = "free_x") +
  xlab("Sites Across Islands") + ylab("Standardized CPUE") +
  stat_n_text(size = 3, y.pos = 0.3) +
  theme_bw()
site # %+% subset(metadata, site_code %in% c("DR", "EAI", "PI", "SBI", "SR", "Sutil"))
ggsave(width = 20, height = 20, dpi=300, filename = "~/WERC-SC/ASSP_share/figures/CPUE_site_isl.pdf")


# CATCH TIMES PAST SUNSET
endT_yr <- ggplot(catches, aes(catchPastSS)) +
  geom_histogram(binwidth = 10) +
  geom_vline(xintercept = 318, color = "red") +
  geom_vline(xintercept = 0, color = "green") +
  xlab("Time past Sunset (min)") + ylab("Number of ASSP Catches") +
  facet_wrap(.~ year) +
  theme_bw()
endT_yr
ggsave(width = 20, height = 20, dpi=300, filename = "~/WERC-SC/ASSP_share/figures/catchT_yr_hist.pdf")

endT_isl <- ggplot(catches, aes(catchPastSS)) +
  geom_histogram(binwidth = 10) +
  geom_vline(xintercept = 318, color = "red") +
  geom_vline(xintercept = 0, color = "green") +
  xlab("Time past Sunset (min)") + ylab("Number of ASSP Catches") +
  facet_wrap(.~ island_code) +
  theme_bw()
endT_isl # %+% subset(catches, subisland_code %in% c("DR", "EAI", "PI", "SBI", "SR", "Sutil"))
ggsave(width = 20, height = 20, dpi=300, filename = "~/WERC-SC/ASSP_share/figures/catchT_isl_hist.pdf")

# CPUE vs. cumulative mintues
CPUE_min <- ggplot(metadata, aes(min, CPUEraw)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  xlab("Total Netting Minutes") + ylab("CPUE") +
  theme_bw()
CPUE_min
ggsave(width = 20, height = 20, dpi=300, filename = "~/WERC-SC/ASSP_share/figures/CPUE_v_min.pdf")

CPUE_minstd <- ggplot(metadata, aes(min_std, CPUEstd)) + # , color = year
  geom_point() +
  geom_smooth(method = 'lm') +
  xlab("Total Standardized Netting Minutes") + ylab("Standardized CPUE") +
  # scale_fill_brewer(palette="Dark2")
  theme_bw()
CPUE_minstd
ggsave(width = 20, height = 20, dpi=300, filename = "~/WERC-SC/ASSP_share/figures/CPUEstd_v_minStd.pdf")

CPUE_minstd_c <- ggplot(metadata, aes(min_std, CPUEstd, color = year)) + # 
  geom_point() +
  # geom_smooth(method = 'lm') +
  xlab("Total Standardized Netting Minutes") + ylab("Standardized CPUE") +
  # scale_fill_brewer(palette="Dark2")
  theme_bw()
CPUE_minstd_c
ggsave(width = 20, height = 20, dpi=300, filename = "~/WERC-SC/ASSP_share/figures/CPUEstd_v_minStd_color.pdf")
