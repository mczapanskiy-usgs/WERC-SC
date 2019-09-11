#### STORM-PETREL CPUE - COMPARING NEW VALUES WITH JOSH'S
# created: Sept 10, 2019 by: E Kelsey
# last edited: September 10, 2019

### SET WORKING DIRECTORY
setwd("~/WERC-SC/ASSP_share")

### READ IN DATA
# banding catches data 
metadata_catches <- read.csv('~/WERC-SC/ASSP_share/metadata_catches_CPUE.csv')

metadata_old <- read.csv('~/WERC-SC/ASSP_share/ASSP_CPUE_1_metadata_SunMoon_all.csv')%>% 
  filter(std_CPUE_old != 'NA')

metadata_catches_comp <- metadata_catches %>% 
  inner_join(metadata_old, by = c("island", "Site", "nightID", "net_open", "Date", "Lat", "Long", "App_sunset", 
                                  "moonFrac", "moonMin", "moonIndex", "net_close", "std_ending", "Net_mesh", "Net_dim", 
                                  "Audio_file", "dB_level", "Speaker_system", "Data_repository", "notes"))

ggplot(metadata_catches_comp) +
  geom_point(aes(date, std_CPUE_old), color = "green") + 
  geom_point(aes(date, CPUEstd), color = "black") +
  facet_wrap(~Site) +
  theme_bw()

ggplot(metadata_catches_comp) +
  geom_point(aes(date, std_captured_old), color = "green") + 
  geom_point(aes(date, ASSPstd), color = "black") +
  facet_wrap(~Site) +
  theme_bw()

ggplot(metadata_catches_comp) +
  geom_point(aes(date, moon_min_old), color = "green") + 
  geom_point(aes(date, moonMin), color = "black") +
  facet_wrap(~Site) +
  theme_bw()

ggplot(metadata_catches_comp) +
  geom_point(aes(date, moon_index_old), color = "green") + 
  geom_point(aes(date, moonIndex), color = "black") +
  facet_wrap(~Site) +
  theme_bw()
# 
# ggplot(metadata_catches_comp) +
#   geom_point(aes(date, minutes_old), color = "green") +
#   geom_point(aes(date, minutes_raw), color = "red") +
#   geom_point(aes(date, minutes_std), color = "black") +
#   facet_wrap(~Site) +
#   theme_bw()