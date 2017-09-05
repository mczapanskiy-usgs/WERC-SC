## This script is to analyze the success of individual traps
## to help assess traps are obviously not catching and can be removed.

# load packages
library(stats)
library(data.table)
library(plyr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(mosaic)


# load data (change directory to the folder holding 'TrapsGrid20170905.csv')
read.csv('~/WERC-SC/HALE/TrapsGrid20170905.csv',
         stringsAsFactors = FALSE) -> spatialCatch # catch data processed by Ben (elev, slope, prox to roads/trails/fences/structures, veg, etc.) & Jon (grid cells)

# summary data by trap events in each spatial grid cell (grid500)
gridEvents <- spatialCatch %>%
  group_by(Grid500, eventType) %>% 
  dplyr::summarise(NEvents = n())

summary(gridEvents)

upperGridEvents <- gridEvents %>% 
  filter(NEvents >= 827.8)\\\\\\\\\+
1
ggplot(upperGridEvents, aes(Grid500, NEvents)) +
  geom_bar(stat = "identity") +
  facet_wrap( ~ eventType) +
  theme_bw()

# summary data by trap events in each trap number (although these locations change between time periods)
trapEvents <- spatialCatch %>%
  group_by(trap, predEvent) %>% 
  dplyr::summarise(NEvents = n())