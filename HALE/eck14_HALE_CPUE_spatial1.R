## this script imports catch data with spatail data and starts
## spatial analysis

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
with(catch_spatial, table(predEvent))
with(catch_spatial, table(Burrows10, predEvent))
with(catch_spatial, table(MedSlope, predEvent))
with(catch_spatial, table(Burrows10, predEvent, strYear))

#### graphs
## vegetation (bar fills)
pctVeg <- ggplot(catch_spatial, aes(predEvent, fill=PctVeg)) +
  geom_bar(position = "fill") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=60, hjust=1)) 
pctVeg %+% subset(catch_spatial, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "trapTriggered", "baitLost", "none"))

vegCover <- ggplot(catch_spatial, aes(predEvent, fill=MajCover)) +
  geom_bar(position = "fill") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=60, hjust=1)) 
vegCover %+% subset(catch_spatial, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "trapTriggered", "baitLost", "none"))

vegType <- ggplot(catch_spatial, aes(predEvent, fill=MajClass)) +
  geom_bar(position = "fill") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=60, hjust=1)) 
vegType %+% subset(catch_spatial, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "trapTriggered", "baitLost", "none"))


## BURROWS
# create columns for frequency of burrow counts and for whether or not there are burrows around (e.g.- inside or outside the colony)
burrows <- catch_spatial %>% 
  select(Trapline, TrapNum, Year_, Month_, predEvent, Week, Burrows10, Burrows50, Burrows100) %>% 
  mutate(burFreq10 = percent_rank(Burrows10),
         burFreq50 = percent_rank(Burrows50),
         burFreq100 = percent_rank(Burrows100),
         colony10 = derivedFactor(
           "in" = Burrows10 >= 1,
           "out" = Burrows10 == 0, 
           .method = "last", 
           .default = "out"),
         colony50 = derivedFactor(
           "in" = Burrows50 >= 1,
           "out" = Burrows50 == 0, 
           .method = "last", 
           .default = "out"),
         colony100 = derivedFactor(
           "in" = Burrows100 >=1,
           "out" = Burrows100 == 0,
           .method = "last", 
           .default = "out"))  # HOW DO I REMOVE THE NAs?
write.csv(burrows, file = '~/WERC-SC/HALE/catch_burrows.csv',
          row.names = FALSE) 

# recreate burrows matrix with Burrows10, Burrows50, and Burrows100 all in the same column
burrows_long <- burrows %>% 
  select(Trapline, TrapNum, Year_, Month_, predEvent, Week, Burrows10, Burrows50, Burrows100, burFreq10, burFreq50, burFreq100) %>% 
  gather(buffer, Burrows, starts_with("Burrows")) %>% 
  mutate(colony = derivedFactor(
           "in" = Burrows >= 1,
           "out" = Burrows == 0, 
           .method = "last", 
           .default = "out"))

# graph data
bur100 <- ggplot(burrows, aes(burFreq100)) +
  geom_histogram(binwidth = 0.1) +
  facet_wrap(~ predEvent) +
  theme_bw()
bur100 %+% subset(burrows, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "mouseCaught"))

col50 <- ggplot(burrows, aes(predEvent, fill=colony50)) +
  geom_bar(position = "fill") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=60, hjust=1)) 
col50 %+% subset(burrows, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "trapTriggered", "baitLost", "none"))

bur1 <- ggplot(burrows_long, aes(buffer, Burrows)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ predEvent)
bur1 %+% subset(burrows_long, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "trapTriggered", "baitLost", "none"))

bur2 <- ggplot(burrows_long, aes(buffer, fill = colony)) +
  geom_bar(position = "dodge") +
  facet_wrap(~ predEvent)
bur2 %+% subset(burrows_long, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "trapTriggered", "baitLost", "none"))
# bur2 %+% subset(burrows_long, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "trapTriggered", "baitLost", "none"))

col1 <- ggplot(burrows_long, aes(buffer, fill = colony)) +
  geom_bar(position = "fill") +
  facet_wrap(~ predEvent) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=60, hjust=1)) 
col1 %+% subset(burrows_long, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "trapTriggered", "baitLost", "none"))

col2 <- ggplot(burrows_long, aes(buffer, fill = predEvent)) +
  geom_bar(position = "fill") +
  facet_wrap(~ colony) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=60, hjust=1)) 
col2 %+% subset(burrows_long, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "trapTriggered", "baitLost", "none"))


## STRUCTURES
# get frequency of structure distances
structures <- catch_spatial %>% 
  select(Trapline, TrapNum, Year_, Month_, predEvent, Week, DistRoad, DistTrail, DistFence, DistShelter) %>% 
  mutate(roadFreq = percent_rank(DistRoad),
         trailFreq = percent_rank(DistTrail),
         fenceFreq = percent_rank(DistFence),
         shelterFreq = percent_rank(DistShelter))
# graph data
road <- ggplot(structures, aes(roadFreq)) +
  geom_histogram(binwidth = 0.05) +
  facet_wrap(~ predEvent) +
  theme_bw()
road %+% subset(structures, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "mouseCaught"))

trail <- ggplot(structures, aes(trailFreq)) +
  geom_histogram(binwidth = 0.05) +
  facet_wrap(~ predEvent) +
  theme_bw()
trail %+% subset(structures, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "mouseCaught"))

fence <- ggplot(structures, aes(fenceFreq)) +
  geom_histogram(binwidth = 0.05) +
  facet_wrap(~ predEvent) +
  theme_bw()
fence %+% subset(structures, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "mouseCaught"))

shelter <- ggplot(structures, aes(shelterFreq)) +
  geom_histogram(binwidth = 0.05) +
  facet_wrap(~ predEvent) +
  theme_bw()
shelter %+% subset(structures, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "mouseCaught"))


## PHYSICAL FEATURES
# get frequency of features
physFeat <- catch_spatial %>% 
  select(Trapline, TrapNum, Year_, Month_, predEvent, Week, MedSlope, Elevation, PctVeg, MajCover, MajClass) %>% 
  mutate(slopeFreq = percent_rank(MedSlope),
         elevationFreq = percent_rank(Elevation))
# graph data
slope <- ggplot(physFeat, aes(slopeFreq)) +
  geom_histogram(binwidth = 0.05) +
  facet_wrap(~ predEvent) +
  theme_bw()
slope %+% subset(physFeat, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "mouseCaught"))

elev <- ggplot(physFeat, aes(elevationFreq)) +
  geom_histogram(binwidth = 0.05) +
  facet_wrap(~ predEvent) +
  theme_bw()
elev %+% subset(physFeat, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "mouseCaught"))


