## this script analyzes the relationship between
## distance to features (roads, trails, fences, and shelters)
## and the frequency of trap events

read.csv('~/WERC-SC/HALE/spatialData_rev_eck18.csv',
         stringsAsFactors = FALSE) -> catch_spatial 


## STRUCTURES
# get frequency of structure distances
structures <- catch_spatial %>% 
  select(Trapline, TrapNum, Year_, Month_, predEvent, Week, DistRoad, DistTrail, DistFence, DistShelter) %>% 
  mutate(roadFreq = percent_rank(DistRoad),
         trailFreq = percent_rank(DistTrail),
         fenceFreq = percent_rank(DistFence),
         shelterFreq = percent_rank(DistShelter))
# ann_structures <- structures %>% 
#   group_by(Year_) %>% 
#   summarize(distRoad = ave(DistRoad),
#             distTrail = ave(DistTrail),
#             distFence = ave(DistFence),
#             distShelter = ave(DistShelter))

##### graph data
#road
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

roadFreq <- ggplot(structures, aes(DistRoad)) +
  geom_freqpoly(binwidth = 100) +
  geom_freqpoly(aes(colour = predEvent), binwidth = 100) +
  theme_bw()
roadFreq %+% subset(structures, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "baitLost", "trapTriggered", "none"))

# trail
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

trailFreq <- ggplot(structures, aes(DistTrail)) +
  geom_freqpoly(binwidth = 100) +
  geom_freqpoly(aes(colour = predEvent), binwidth = 100) +
  theme_bw()
trailFreq %+% subset(structures, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "baitLost", "trapTriggered", "none"))


# fence
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

fenceFreq <- ggplot(structures, aes(DistFence)) +
  geom_freqpoly(binwidth = 100) +
  geom_freqpoly(aes(colour = predEvent), binwidth = 100) +
  theme_bw()
fenceFreq %+% subset(structures, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "baitLost", "trapTriggered", "none"))


# shelter
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

shelterFreq <- ggplot(structures, aes(DistShelter)) +
  geom_freqpoly(binwidth = 100) +
  geom_freqpoly(aes(colour = predEvent), binwidth = 100) +
  theme_bw()
shelterFreq %+% subset(structures, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "baitLost", "trapTriggered", "none"))
