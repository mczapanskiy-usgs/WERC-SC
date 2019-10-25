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
  mutate(spat = catchID %in% spat,
         temp = catchID %in% temp,
         duplic = duplicated(catchID),
         spatial = if_else(spat == TRUE, "1", "0"),
         temporal = if_else(temp == TRUE, "1", "0"),
         spatial = as.numeric(spatial),
         temporal = as.numeric(temporal)) %>% 
  select(-spat, -temp, -duplic)

# view data
summary(sciencebase3$duplic)
summary(sciencebase3$temp)
summary(sciencebase3$spat)
sum(sciencebase3$temporal)
sum(sciencebase3$spatial)
summary(duplicated(spat))
summary(duplicated(temp))

write.csv(sciencebase3, file = '~/WERC-SC/HALE/Kelsey2019_ScienceBase_v3.csv',
          row.names = FALSE)
