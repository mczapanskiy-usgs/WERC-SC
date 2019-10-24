### this script adds columns to final sciencebase file 
### identifying which fields were used for spatial and temporal analysis


library(data.table)
# library(plyr)
library(dplyr)
library(tidyr)

setwd("~/WERC-SC/HALE")


### read in files
# sciencebase
read.csv('~/WERC-SC/HALE/Kelsey2019_ScienceBase_v2.csv',
         stringsAsFactors = FALSE) -> sciencebase2
# spatial catchID
spatial <- read.csv('~/WERC-SC/HALE/sciencebase_spatial.csv',
         stringsAsFactors = FALSE) %>% 
  mutate(spat = "1")
# temporal catchID
temporal <- read.csv('~/WERC-SC/HALE/sciencebase_temporal.csv',
         stringsAsFactors = FALSE) %>% 
  mutate(temp = "1")

spat <- spatial$spatial_catchID
temp <- temporal$temporal_catchID

sciencebase3 <- sciencebase2 %>% 
  full_join(spatial, by = c("catchID" = "spatial_catchID")) %>% 
  full_join(temporal, by = c("catchID" = "temporal_catchID")) %>% 
  mutate(dupl = duplicated(catchID))

write.csv(sciencebase3, file = '~/WERC-SC/HALE/sciencebase_test.csv',
          row.names = FALSE)

# net_open = if_else(hour_open <= 12, nextDay_open, net_open_old),
