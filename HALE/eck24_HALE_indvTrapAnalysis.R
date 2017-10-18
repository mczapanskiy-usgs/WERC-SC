## This script is to analyze the success of individual traps
## to help assess traps are obviously not catching and can be removed.

setwd("~/WERC-SC/HALE")

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

# ___________________________________________________________________________
## GRID
# PRED EVENT
# summarize data by trap events in each spatial grid cell (grid500)
gridEvents <- spatialCatch %>%
  group_by(Grid500, eventType) %>% 
  dplyr::summarise(NEvents = n()) %>% 
  na.omit()
# select just predator events (e.g.- only trap events that caught a predator)
gridPreds <- gridEvents %>%
  filter(eventType == "predatorEvent") %>% 
  as.data.frame() %>% 
  mutate(quart = ntile(NEvents, 4)) # quart indicates if data is in the bottom (1), top (4), etc. quartile of data
# save data of frequency of predator events within each grid cell and quartiles
write.csv(gridPreds, file = '~/WERC-SC/HALE/gridPredFrequenciess20170922.csv',
          row.names = FALSE)

# # break predator event data into quartiles and select only the bottom quartile 
# # (e.g.- grid cells with the lowest 25% of predator catches)
# leastGridPreds <- gridPreds %>%
#   filter(quart == 1)
# ggplot(leastGridPreds, aes(Grid500, NEvents)) +
#   geom_bar(stat = "identity") +
#   theme_bw() +
#   theme(axis.text.x=element_text(angle = 90))
# 
# ggplot(gridPreds, aes(Grid500, NEvents)) +
#   geom_bar(stat = "identity") +
#   theme_bw() +
#   theme(axis.text.x=element_text(angle = 90))

# RAT, CAT, MONGOOSE
gridPredEvent <- spatialCatch %>%
  group_by(Grid500, predEvent) %>% 
  dplyr::summarise(NEvents = n()) %>% 
  na.omit()
# select just predator events (e.g.- only trap events that caught a rat, cat or mongoose)
gridRat <- gridPredEvent %>%
  filter(predEvent == "ratCaught") %>%
  as.data.frame() %>%
  mutate(quart = ntile(NEvents, 4)) # quart indicates if data is in the bottom (1), top (4), etc. quartile of data
gridCat <- gridPredEvent %>%
  filter(predEvent == "catCaught") %>% 
  as.data.frame() %>% 
  mutate(quart = ntile(NEvents, 4)) 
gridMongoose <- gridPredEvent %>%
  filter(predEvent == "mongooseCaught") %>% 
  as.data.frame() %>% 
  mutate(quart = ntile(NEvents, 4))
# save data of frequency of predator events within each grid cell and quartiles
write.csv(gridRat, file = '~/WERC-SC/HALE/gridRatFreq20171017.csv',
          row.names = FALSE)
write.csv(gridCat, file = '~/WERC-SC/HALE/gridCatFreq20171017.csv',
          row.names = FALSE)
write.csv(gridMongoose, file = '~/WERC-SC/HALE/gridMongooseFreq20171017.csv',
          row.names = FALSE)


# ___________________________________________________________________________
## TRAP
# summary data by trap events in each trap number (although these locations change between time periods)
trapEvents <- spatialCatch %>%
  group_by(trap, predEvent) %>% 
  dplyr::summarise(NEvents = n()) %>% 
  na.omit()
# select just predator events (e.g.- only trap events that caught a predator)
# break trap data (of pred events) into quartiles 
trapPreds <- trapEvents %>%
  filter(predEvent %in% c("ratCaught", "catCaught", "mongooseCaught")) %>% 
  as.data.frame() %>% 
  mutate(quart = ntile(NEvents, 4)) # quart indicates if data is in the bottom (1), top (4), etc. quartile of data
# save data of frequency of predator events within each grid cell and quartiles
write.csv(trapPreds, file = '~/WERC-SC/HALE/trapPredFrequenciess20170922.csv',
          row.names = FALSE) 
# select only the bottom quartile of predator events at each trap
# (e.g.- trap numbers with the lowest 25% of predator catches)
leastTrapPreds <- trapPreds %>%
  filter(quart == 1)
