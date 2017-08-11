## this script analyzes the relationship between
## distance to features (roads, trails, fences, and shelters)
## and the frequency of trap events

read.csv('~/WERC-SC/HALE/spatialData_rev_eck18.csv',
         stringsAsFactors = FALSE) -> catch_spatial

## STRUCTURES
# get frequency of structure distances
structures <- catch_spatial %>% 
  select(Trapline, TrapNum, Year, predEvent, Week, DistRoad, DistTrail, DistFence, DistShelter) %>% 
  mutate(roadFreq = percent_rank(DistRoad),
         trailFreq = percent_rank(DistTrail),
         fenceFreq = percent_rank(DistFence),
         shelterFreq = percent_rank(DistShelter))

#### FREQUENCY OF ROAD EVENTS
# number of events at each distance
roadDist <- structures %>% 
  group_by(DistRoad) %>% 
  count()
# number of event types at each distance
roadDist_events <- structures %>% 
  group_by(DistRoad, predEvent) %>% 
  count() %>%  
  rename(nEvent = n)
# combine and find frequency of events at each distance
road <- left_join(roadDist_events, roadDist, by = "DistRoad") %>% 
  mutate(freq = (nEvent/n))
# graph
roadFreq <- ggplot(road, aes(DistRoad, freq)) +
  geom_smooth(method = lm, aes(colour = predEvent)) +  # linear smoothing
  ylim(c(0,1)) +
  labs(y = 'Proportion of Predator Events', x = 'Distance to Road (m)', predEvent = 'Predator Event Type') +
  theme_bw()
roadFreq %+% subset(road, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "baitLost", "trapTriggered", "none"))
ggsave(width = 8.5, height = 5, dpi=300, filename = "~/WERC-SC/HALE/outputs/roadFreq_eck21.pdf")

#### FREQUENCY OF TRAIL EVENTS
# number of events at each distance
trailDist <- structures %>% 
  group_by(DistTrail) %>% 
  count()
# number of event types at each distance
trailDist_events <- structures %>% 
  group_by(DistTrail, predEvent) %>% 
  count() %>%  
  rename(nEvent = n)
# combine and find frequency of events at each distance
trail <- left_join(trailDist_events, trailDist, by = "DistTrail") %>% 
  mutate(freq = (nEvent/n))
# graph
trailFreq <- ggplot(trail, aes(DistTrail, freq)) +
  geom_smooth(method = lm, aes(colour = predEvent)) +
  ylim(c(0,1)) +
  labs(y = 'Proportion of Predator Events', x = 'Distance to Trail (m)', predEvent = 'Predator Event Type') +
  theme_bw()
trailFreq %+% subset(trail, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "baitLost", "trapTriggered", "none"))
ggsave(width = 8.5, height = 5, dpi=300, filename = "~/WERC-SC/HALE/outputs/trailFreq_eck21.pdf")

#### FREQUENCY OF FENCE EVENTS
# number of events at each distance
fenceDist <-structures %>% 
  group_by(DistFence) %>% 
  count()
# number of event types at each distance
fenceDist_events <- structures %>% 
  group_by(DistFence, predEvent) %>% 
  count() %>%  
  rename(nEvent = n)
# combine and find frequency of events at each distance
fence <- left_join(fenceDist_events, fenceDist, by = "DistFence") %>% 
  mutate(freq = (nEvent/n))
# graph
fenceFreq <- ggplot(fence, aes(DistFence, freq)) +
  geom_smooth(method = lm, aes(colour = predEvent)) +
  ylim(c(0,1)) +
  labs(y = 'Proportion of Predator Events', x = 'Distance to Fence (m)', predEvent = 'Predator Event Type') +
  theme_bw()
fenceFreq %+% subset(fence, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "baitLost", "trapTriggered", "none"))
ggsave(width = 8.5, height = 5, dpi=300, filename = "~/WERC-SC/HALE/outputs/fenceFreq_eck21.pdf")

#### FREQUENCY OF SHELTER EVENTS
# number of events at each distance
shelterDist <- structures %>% 
  group_by(DistShelter) %>% 
  count()
# number of event types at each distance
shelterDist_events <- structures %>% 
  group_by(DistShelter, predEvent) %>% 
  count() %>%  
  rename(nEvent = n)
# combine and find frequency of events at each distance
shelter <- left_join(shelterDist_events, shelterDist, by = "DistShelter") %>% 
  mutate(freq = (nEvent/n))
# graph
shelterFreq <- ggplot(shelter, aes(DistShelter, freq)) +
  geom_smooth(method = lm, aes(colour = predEvent)) + # linear smoothing
  ylim(c(0,1)) +
  labs(y = 'Proportion of Predator Events', x = 'Distance to Structure (m)', predEvent = 'Predator Event Type') +
  theme_bw()
shelterFreq %+% subset(shelter, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "baitLost", "trapTriggered", "none"))
ggsave(width = 8.5, height = 5, dpi=300, filename = "~/WERC-SC/HALE/outputs/shelterFreq_eck21.pdf")




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
