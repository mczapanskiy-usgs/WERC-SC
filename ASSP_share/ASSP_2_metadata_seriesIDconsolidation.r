#### STORM-PETREL CPUE METADATA
# this script takes metadata with seriesID added and open/close broken out into multiple columns and consolidates
# data input: ran through "ASSP_1", manually reviewed (session added)
# created: Jan 28, 2020 by: E Kelsey
# last edited: Feb 24, 2020

### SET WORKING DIRECTORY
setwd("~/WERC-SC/ASSP_share")

## LOAD LIBRARIES
library(data.table)
library(dplyr)
library(tidyr)
library(lubridate)
# library(mosaic)
# library(foreach)
# library(doParallel)

### READ IN BANDING CPUE DATA
metadata_raw <- read.csv('~/WERC-SC/ASSP_share/ASSP_CPUE_20200224_series_EK.csv', na.strings=c("","NA")) %>% 
  select(-mo_period, -duration:-birds.min.area)

# gather data by open/close time and then adjust dates and times to proper date
metadata <- gather(metadata_raw, net_stat, net_time, net_open_1:net_close_5) %>% 
  mutate(net_time_old = as.POSIXct(paste(date, net_time), format="%m/%d/%Y %H:%M"),
         net_time_nxt = net_time_old + days(1),
         net_time_hr = as.numeric(hour(net_time_old)),
         net_time = if_else(net_time_hr <=12, net_time_nxt, net_time_old)) %>%
  select(-net_time_old, -net_time_nxt, -net_time_hr) %>% 
  group_by(sessionID) %>% 
  spread(net_stat, net_time) %>% 
  filter() %>% 
  mutate(last_open = if_else(net_close_5 == "NA",
                    net_close_4))

table(metadata$seriesID)

write.csv(metadata, file = '~/WERC-SC/ASSP_share/ASSP_2_CPUE_metadata_session.csv', # '~/WERC-SC/ASSP_share/ASSP_CPUE_20200206.csv',
          row.names = FALSE)


# metadata <- metadata_raw %>% 
#   mutate_at(c("net_open_1", "net_close_1", "net_open_2", "net_close_2", "net_open_3",
#               "net_close_3", "net_open_4", "net_close_4", "net_open_5", "net_close_5"),
#             .funs = list(old = ~as.POSIXct(paste(date, .), format="%m/%d/%Y %H:%M"))) %>% 
#   mutate_at(c("net_open_1_old", "net_close_1_old", "net_open_2_old", "net_close_2_old", "net_open_3_old",
#               "net_close_3_old", "net_open_4_old", "net_close_4_old", "net_open_5_old", "net_close_5_old"),
#             .funs = list(nxt = ~. + days(1))) %>% 
#   mutate_at(c("net_open_1_old", "net_close_1_old", "net_open_2_old", "net_close_2_old", "net_open_3_old",
#               "net_close_3_old", "net_open_4_old", "net_close_4_old", "net_open_5_old", "net_close_5_old"),
#             .funs = list(hr = ~as.numeric(hour(.))))  %>% 
#   mutate(net_open_1 = if_else(net_open_1_old_hr <=12, net_open_1_old_nxt, net_open_1_old),
#          net_close_1 = if_else(net_close_1_old_hr <=12, net_close_1_old_nxt, net_close_1_old),
#          net_open_2 = if_else(net_open_2_old_hr <=12, net_open_2_old_nxt, net_open_2_old),
#          net_close_2 = if_else(net_close_2_old_hr <=12, net_close_2_old_nxt, net_close_2_old),
#          net_open_3 = if_else(net_open_3_old_hr <=12, net_open_3_old_nxt, net_open_3_old),
#          net_close_3 = if_else(net_close_3_old_hr <=12, net_close_3_old_nxt, net_close_3_old),
#          net_open_4 = if_else(net_open_4_old_hr <=12, net_open_4_old_nxt, net_open_1_old),
#          net_close_4 = if_else(net_close_4_old_hr <=12, net_close_4_old_nxt, net_close_4_old),
#          net_open_5 = if_else(net_open_5_old_hr <=12, net_open_5_old_nxt, net_open_5_old),
#          net_close_5 = if_else(net_close_5_old_hr <=12, net_close_5_old_nxt, net_close_5_old),
#          date = mdy(date)) %>%
#   select(-net_open_1_old:-net_close_5_old_hr) %>%
#   filter(TRUE) 
