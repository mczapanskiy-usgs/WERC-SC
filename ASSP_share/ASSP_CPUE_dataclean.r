#### STORM-PETREL CPUE METADATA
# this script takes metadata with seriesID added and open/close broken out into multiple columns
# created: Jan 28, 2020 by: E Kelsey
# last edited: Jan 28, 2020

### SET WORKING DIRECTORY
setwd("~/WERC-SC/ASSP_share")

## LOAD LIBRARIES
library(data.table)
library(dplyr)
library(tidyr)
library(lubridate)
library(mosaic)
library(oce)
library(foreach)
library(doParallel)
# library(stats)
library(suncalc)
# library(chron)
# library(rnoaa)

### READ IN BANDING CPUE DATA
metadata_raw <- read.csv('~/WERC-SC/ASSP_share/ASSP_CPUE_01282020_series.csv', na.strings=c("","NA")) %>% 
  select(-mo_period, -duration:-birds.min.area)

metadata <- metadata_raw %>% 
  mutate_at(c("net_open_1", "net_close_1", "net_open_2", "net_close_2", "net_open_3",
              "net_close_3", "net_open_4", "net_close_4", "net_open_5", "net_close_5"),
            .funs = list(old = ~as.POSIXct(paste(date, .), format="%m/%d/%Y %H:%M"))) %>% 
  mutate_at(c("net_open_1_old", "net_close_1_old", "net_open_2_old", "net_close_2_old", "net_open_3_old",
              "net_close_3_old", "net_open_4_old", "net_close_4_old", "net_open_5_old", "net_close_5_old"),
            .funs = list(nxt = ~. + days(1))) %>% 
  mutate_at(c("net_open_1_old", "net_close_1_old", "net_open_2_old", "net_close_2_old", "net_open_3_old",
              "net_close_3_old", "net_open_4_old", "net_close_4_old", "net_open_5_old", "net_close_5_old"),
            .funs = list(hr = ~as.numeric(hour(.)))) %>% 
  mutate(net_open_1 = if_else(net_open_1_old_hr <=12, net_open_1_old_nxt, net_open_1_old),
         net_close_1 = if_else(net_close_1_old_hr <=12, net_close_1_old_nxt, net_close_1_old),
         net_open_2 = if_else(net_open_2_old_hr <=12, net_open_2_old_nxt, net_open_2_old),
         net_close_2 = if_else(net_close_2_old_hr <=12, net_close_2_old_nxt, net_close_2_old),
         net_open_3 = if_else(net_open_3_old_hr <=12, net_open_3_old_nxt, net_open_3_old),
         net_close_3 = if_else(net_close_3_old_hr <=12, net_close_3_old_nxt, net_close_3_old),
         net_open_4 = if_else(net_open_4_old_hr <=12, net_open_4_old_nxt, net_open_1_old),
         net_close_4 = if_else(net_close_4_old_hr <=12, net_close_4_old_nxt, net_close_4_old),
         net_open_5 = if_else(net_open_5_old_hr <=12, net_open_5_old_nxt, net_open_5_old),
         net_close_5 = if_else(net_close_5_old_hr <=12, net_close_5_old_nxt, net_close_5_old),
         date = mdy(date)) %>% 
  select(-net_open_1_old:-net_close_5_old_hr) %>% 
  filter(TRUE) 

table(metadata$seriesID)

write.csv(metadata, file = '~/WERC-SC/ASSP_share/ASSP_CPUE_20200206.csv',
          row.names = FALSE)
