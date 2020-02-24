#### STORM-PETREL CATCH ANALYSIS - REDO
# this script calculates net time and CPUE
# created: Feb 6, 2020 by: E Kelsey
# last edited: Feb 21, 2020

### SET WORKING DIRECTORY
setwd("~/WERC-SC/ASSP_share")

## LOAD LIBRARIES
library(data.table)
library(tidyr)
library(dplyr)
library(lubridate)
library(suncalc)
library(ggplot2)


### READ IN DATA
## banding catches data 
catches_raw <- read.csv('~/WERC-SC/ASSP_share/ASSP_BANDING_01282020.csv') %>% 
  mutate(capture_time = as.POSIXct(capture_time, format= '%m/%d/%Y %H:%M'),
         release_time = as.POSIXct(release_time, format='%m/%d/%Y %H:%M')) %>% 
  select(-P10:-R1, -X, -X.1, -X.2, -X.3) %>% # -sunset, -hrs_postSS, -tail, -sex, -release.time, 
  filter(TRUE)
## netting site data
sites_tbl <- read.csv('~/WERC-SC/ASSP_share/ASSP_mistnetting_locs_20190905.csv') %>% 
  select(-Notes, -Alias) #-Site_Name, 
## "clean" metdata
metadata <- read.csv('~/WERC-SC/ASSP_share/ASSP_metadata_session.csv', na.strings=c("","NA")) %>% 
  filter(Site != "RC", Site != "EAI_S") %>% 
  mutate_at(c("net_open_1", "net_close_1", "net_open_2", "net_close_2", "net_open_3",
              "net_close_3", "net_open_4", "net_close_4", "net_open_5", "net_close_5"),
            .funs = ~as.POSIXct(.)) %>% 
  left_join(sites_tbl, by = c("Site" = "Site", "island" = "Island")) %>% 
  mutate(Site = as.factor(Site),
         Island = as.factor(island)) %>% 
  select(-App_sunset) %>% 
  filter(TRUE)
    # filter(Site != c("EAI_S", "RC", "UNK"))
    
# summary(metadata$Site)
# summary(metadata$Island)
# summary(sites_tbl$Site)
# summary(sites_tbl$Island)

## update catches/banding datasheet: add catchID, locations, catch date (is diff from 'date' after midnight)
catches <- catches_raw %>% 
  mutate(Site = as.factor(Site),
         measurers = measurer.s.,
         uncorr_mass = mass,
         mass_tare = tare) %>% 
  # remove unnecessary columns
  select(catchID, sessionID, day, Month, year, island, Site, measurers, capture_time, release_time, 
         species:BP, uncorr_mass, mass_tare, culmen:Notes)
  # filter(Site != "UNK")
  # inner_join(sites_tbl, by = c("Site" = "Site", "island" = "Island")) 

## calculate sunset
# create dataframe to run sunset function on
sun_vec <- metadata %>%  
  mutate(date = paste(month, day, year),
         date = mdy(date)) %>% 
  transmute(date = date, lat = Lat, lon = Long, Site = Site, sessionID = sessionID) %>% #as_date()
  # drop_na() %>%
  filter(TRUE) 

# run sunset function
metadata_1stnight <- getSunlightTimes(data = sun_vec,
                               keep = c("sunset"), tz = "PST8PDT") %>% 
  mutate(std_ending = sunset + lubridate::hours(5) + lubridate::minutes(18), # standard ending = 5.3 hours after sunset
         Lat = lat,
         Long = lon,
         App_sunset = sunset) %>%
  left_join(sun_vec, by = c("date", "lat", "lon")) %>%
  select(-date, -lat, -lon, -sunset) %>%
  left_join(metadata, by = c("sessionID", "Site", "Lat", "Long")) %>% 
  mutate(Site = as.factor(Site),
         island = as.factor(island),
         net_close_1_std = if_else(net_close_1 > std_ending, std_ending, net_close_1),
         net_close_2_std = if_else(net_close_2 > std_ending, std_ending, net_close_2),
         net_close_3_std = if_else(net_close_3 > std_ending, std_ending, net_close_3),
         net_close_4_std = if_else(net_close_4 > std_ending, std_ending, net_close_4),
         net_close_5_std = if_else(net_close_5 > std_ending, std_ending, net_close_5), 
       min_1 = as.double(if_else(as.character(net_close_1_std - net_open_1) < 0, "0", as.character(net_close_1 - net_open_1))),
       min_2 = as.double(if_else(as.character(net_close_2_std - net_open_2) < 0, "0", as.character(net_close_2 - net_open_2))),
       min_3 = as.double(if_else(as.character(net_close_3_std - net_open_3) < 0, "0", as.character(net_close_3 - net_open_3))),
       min_4 = as.double(if_else(as.character(net_close_4_std - net_open_4) < 0, "0", as.character(net_close_4 - net_open_4))),
       min_5 = as.double(if_else(as.character(net_close_5_std - net_open_5) < 0, "0", as.character(net_close_5 - net_open_5)))) %>%  
  mutate_at(c("min_1", "min_2", "min_3", "min_4", "min_5"), .funs = ~replace_na(., 0)) %>% 
  mutate(min_std = min_1 + min_2 + min_3 + min_4 + min_5) %>%
  select(-min_1:-min_5) 


# compare Sites listed in catches vs. metadata datasets
summary(metadata_1stnight$Site)
summary(catches$Site)

summary(metadata_1stnight$island)
summary(catches$island)

catches_metadata <- catches %>%
  right_join(metadata_1stnight, by = c("Site", "sessionID", "day", "year", "island")) 

# standardize recapture and species
catches_filtered <- catches_metadata %>% 
  mutate(std = if_else(std_ending > as.POSIXct(capture_time), "1", "0"), # if bird was caught before std_ending = 1, after = 0
         # pre_close = if_else(net_close > capture_time, "1", "0"), # if bird was caught before net_close = 1, after = 0
         post_open = if_else(net_open_1 < capture_time, "1", "0"), # if bird was caught after net_open = 1, before = 0
         SNrecapture = mosaic::derivedFactor(
           "Y" = (recapture.=="SNR" | recapture.=="YSN"), # remove same night recaptures 
           "N" = (recapture.=="N" | recapture.=="Y" | recapture.=="y"), # recaps from other nights still count # 
           # "UNK" = (recapture.=="UNK" & recapture.=="X"),
           .default = "UNK"),
         spp = mosaic::derivedFactor(
           "ASSP" = (species =="ASSP"),
           "LESP" = (species =="LESP" | species =="LHSP"),
           "LSTP" = (species =="LTSP"), # species =="LSTP" | 
           "BLSP" = (species == "BLSP"),
           "OTHER" = (species == "CAAU" & species =="JOPE" & species =="WEGU" & species=="XAMU"), #  & species=="OTHER" 
           # "UNK" = (species == "UNK"), #  & species=="ASSP/LESP"
           .default = "UNK")) %>%
  filter(SNrecapture == "N",
         spp == "ASSP")

# summary(catches_metadata$recapture.)
# summary(catches_std$SNrecapture)

# sum catches for each species and night
metadata_catches <- catches_filtered %>%
  group_by(island, Site, sessionID) %>% 
  # count(species) %>% 
  summarise(ASSP = n(),
            ASSPstd = sum(std == "1")) %>% 
  mutate(ASSPraw = as.character(ASSP)) %>% 
  left_join(metadata_1stnight, by= c("sessionID", "island", "Site")) %>%
  # select() %>% 
  # filter(min_std > 0) %>%
  mutate(CPUEstd = ASSPstd/min_std) # %>% 
  # drop_na()
  # distinct()
  
CPUE <-  ggplot(metadata_catches, aes(as.character(year), CPUEstd)) +
  geom_boxplot() +
  facet_wrap(~Site) +
  # ylim(0, 0.5) +
  scale_x_discrete(name ="Year", 
                   limits=c("1994","1995","1996","1997","1998","1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006",
                            "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018")) +
  labs(y = 'Catch-Per-Unit-Effort') +
  theme_bw()
# CPUE %+% subset(metadata_catches, island %in% c("PI", "SR", "SBI"))
CPUE %+% subset(metadata_catches, Site %in% c("PI1", "SR1", "AP", "ESP"))
ggsave(width = 20, height = 20, dpi=300, filename = "~/WERC-SC/ASSP_share/CPUE_yr_island_PSG.pdf")

  

write.csv(metadata_catches, file = '~/WERC-SC/ASSP_share/metadata_catches_CPUE_PSG.csv',
          row.names = FALSE)
  
test <- catches_raw %>% 
  filter(island == "PI")

  