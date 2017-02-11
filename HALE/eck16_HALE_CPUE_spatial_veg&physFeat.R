## this script imports catch data with spatail data and starts
## spatial analysis of trends in vegetation, structures, and physical features

library(stats)
library(data.table)
library(plyr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(mosaic)

read.csv('~/WERC-SC/HALE/catch_12_spatialCatches_20170109.csv',
         stringsAsFactors = FALSE) -> catch_spatial

## look at data
summary(catch_spatial)
dim(catch_spatial)
with(catch_spatial, table(MedSlope, predEvent))

veg <- catch_spatial %>% 
  select(Trapline, TrapNum, Year_, Month_, predEvent, Week, PctVeg, MajCover, MajClass) %>% 
  mutate(allVeg = "allVeg")
vegColors <- c("#333333", 
               "#663333", "#993333", "#CC3333",
               "#CC9900", "#CCCCC00", "#CCCCC33", "#CCCC66", "#CCCC99",
               "#99CC99", "#99CC33", "#99CC00", "#669900",  "#666600", "#006600", "#336600", "#336633",
               "#0000FF", "#0000CC", "#000099", "#000066")
    
## vegetation (bar fills)
pctVeg <- ggplot(catch_spatial, aes(predEvent, fill=PctVeg)) +
  geom_bar(position = "fill") + ## , stat="bin"
  theme_bw() ## + theme(axis.text.x = element_text(angle=60, hjust=1)) 
pctVeg %+% subset(catch_spatial, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "trapTriggered", "baitLost", "none"))

vegCover <- ggplot(veg, aes(predEvent, fill=MajCover)) +
  geom_bar(position = "fill") +
  theme_bw() + ## + theme(axis.text.x = element_text(angle=60, hjust=1)) 
  scale_fill_manual(values = vegColors)
vegCover %+% subset(veg, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "trapTriggered", "baitLost", "none"))
allVegCover  <- ggplot(veg, aes(allVeg, fill=MajCover)) +
  geom_bar(position = "fill") +
  guides(fill = FALSE) +
  theme_bw()
allVegCover %+% subset(veg, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "trapTriggered", "baitLost", "none"))

vegType <- ggplot(catch_spatial, aes(predEvent, fill=MajClass)) +
  geom_bar(position = "fill") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=60, hjust=1)) 
vegType %+% subset(catch_spatial, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "trapTriggered", "baitLost", "none"))

## STRUCTURES
# get frequency of structure distances
structures <- catch_spatial %>% 
  select(Trapline, TrapNum, Year_, Month_, predEvent, Week, DistRoad, DistTrail, DistFence, DistShelter) %>% 
  mutate(roadFreq = percent_rank(DistRoad),
         trailFreq = percent_rank(DistTrail),
         fenceFreq = percent_rank(DistFence),
         shelterFreq = percent_rank(DistShelter))
# graph data
roadHist <- ggplot(structures, aes(DistRoad)) +
  geom_histogram(binwidth = 100) +
  facet_wrap(~ predEvent, scales = "free") +
  theme_bw()
roadHist %+% subset(structures, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "baitLost", "trapTriggered", "none"))
road <- ggplot(structures, aes(predEvent, DistRoad)) +
  geom_boxplot() +
  # facet_wrap((~ predEvent)) +
  theme_bw()
road %+% subset(structures, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "baitLost", "trapTriggered", "none"))

trailHist <- ggplot(structures, aes(DistTrail)) +
  geom_histogram(binwidth = 100) +
  facet_wrap(~ predEvent, scales = "free") +
  theme_bw()
trailHist %+% subset(structures, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "baitLost", "trapTriggered", "none"))
trail <- ggplot(structures, aes(predEvent, DistTrail)) +
  geom_boxplot() +
  # facet_wrap((~ predEvent)) +
  theme_bw()
trail %+% subset(structures, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "baitLost", "trapTriggered", "none"))

fenceHist <- ggplot(structures, aes(DistFence)) +
  geom_histogram(binwidth = 100) +
  facet_wrap(~ predEvent, scales = "free") +
  theme_bw()
fenceHist %+% subset(structures, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "baitLost", "trapTriggered", "none"))
fence <- ggplot(structures, aes(predEvent, DistFence)) +
  geom_boxplot() +
  # facet_wrap((~ predEvent)) +
  theme_bw()
fence %+% subset(structures, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "baitLost", "trapTriggered", "none"))

shelterHist <- ggplot(structures, aes(DistShelter)) +
  geom_histogram(binwidth = 100) +
  facet_wrap(~ predEvent, scales = "free") +
  theme_bw()
shelterHist %+% subset(structures, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "baitLost", "trapTriggered", "none"))
shelter <- ggplot(structures, aes(predEvent, DistShelter)) +
  geom_boxplot() +
  # facet_wrap((~ predEvent)) +
  theme_bw()
shelter %+% subset(structures, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "baitLost", "trapTriggered", "none"))

## PHYSICAL FEATURES
# get frequency of features
physFeat <- catch_spatial %>% 
  select(Trapline, TrapNum, Year_, Month_, predEvent, Week, MedSlope, Elevation, PctVeg, MajCover, MajClass) %>% 
  mutate(slopeFreq = percent_rank(MedSlope),
         elevationFreq = percent_rank(Elevation))
# graph data
# slope <- ggplot(physFeat, aes(MedSlope, fill = predEvent)) +
#   geom_histogram(binwidth = 1) +
#   facet_wrap(~ predEvent, scales = "free") +
#   theme_bw()
# slope %+% subset(physFeat, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "baitLost", "trapTriggered", "none"))
slope <- ggplot(physFeat, aes(MedSlope, colour = predEvent)) +
  geom_freqpoly(aes(weight = MedSlope), binwidth = 1) +
  # facet_wrap(~ predEvent, scales = "free") +
  theme_bw()
slope %+% subset(physFeat, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "baitLost", "trapTriggered", "none"))

# elev <- ggplot(physFeat, aes(Elevation)) +
#   geom_histogram(binwidth = 50) +
#   facet_wrap(~ predEvent, scales = "free") +
#   theme_bw()
# elev %+% subset(physFeat, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "baitLost", "trapTriggered", "none"))
elev <- ggplot(physFeat, aes(Elevation, colour = predEvent)) +
  geom_freqpoly(binwidth = 50) +
  theme_bw()
elev %+% subset(physFeat, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "baitLost", "trapTriggered", "none"))

