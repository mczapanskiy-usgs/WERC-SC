## this script imports catch data with spatail data and starts
## spatial analysis of trends in vegetation and physical features

library(stats)
library(data.table)
library(plyr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(mosaic)
library(RColorBrewer)

# read.csv('~/WERC-SC/HALE/catch_11.5_spatialCatches_20170109.csv',
#          stringsAsFactors = FALSE) -> catch_spatial
read.csv('~/WERC-SC/HALE/spatialData_rev_eck18.csv',
         stringsAsFactors = FALSE) -> catch_spatial 

### preds only 
spatial_preds <- catch_spatial %>% 
  filter(eventType == "predatorEvent")

vegColors <- c("red4", 
               "orangered1", "orangered2", "orangered3",
               "palegreen", "palegreen1", "palegreen2", "palegreen3", "palegreen4",
               "deepskyblue1", "deepskyblue2", "deepskyblue3", "dodgerblue", "dodgerblue1", "dodgerblue2", "dodgerblue3", "dodgerblue4",
               "slateblue1", "slateblue2", "slateblue3", "slateblue4")

    
#### VEGETATION
# preds only
vegCover_preds <- ggplot(arrange(spatial_preds, MajCover), aes(predEvent, fill=majCoverType)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = "Set1") +
  labs(y = 'Proportion of Capture Events', x = 'Predator Event Type') +
  theme_bw()  
vegCover_preds

allVegCover_preds <- ggplot(arrange(spatial_preds, MajCover), aes(predEvent, fill=MajCover)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = vegColors) +
  labs(y = 'Proportion of Capture Events', x = 'Predator Event Type') +
  theme_bw()
allVegCover_preds 

# event type
vegCover_events <- ggplot(arrange(catch_spatial, MajCover), aes(eventType, fill=majCoverType)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = "Set1") +
  labs(y = 'Proportion of Trap Events', x = 'Event Type') +
  theme_bw()  
vegCover_events 

allVegCover_events <- ggplot(arrange(catch_spatial, MajCover), aes(eventType, fill=MajCover)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = vegColors) +
  labs(y = 'Proportion of Trap Events', x = 'Event Type') +
  theme_bw() 
allVegCover_events 

# all data
# pctVeg <- ggplot(catch_spatial, aes(predEvent, fill=PctVeg)) +
#   geom_bar(position = "fill") + ## , stat="bin"
#   # scale_fill_manual(values = vegColors) +
#   theme_bw() ## + theme(axis.text.x = element_text(angle=60, hjust=1)) 
# pctVeg %+% subset(catch_spatial, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "trapTriggered", "baitLost", "none"))

vegCover <- ggplot(arrange(catch_spatial, MajCover), aes(predEvent, fill=majCoverType)) +
  geom_bar(position = "fill") +
  # scale_fill_manual(values = vegColors) +
  theme_bw() ## + theme(axis.text.x = element_text(angle=60, hjust=1)) 
vegCover %+% subset(catch_spatial, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "trapTriggered", "baitLost", "none"))

allVegCover <- ggplot(arrange(catch_spatial, MajCover), aes(predEvent, fill=MajCover)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = vegColors) +
  theme_bw() ## + theme(axis.text.x = element_text(angle=60, hjust=1)) 
allVegCover %+% subset(catch_spatial, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "trapTriggered", "baitLost", "none"))



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


# veg <- catch_spatial %>% 
#   select(Trapline, TrapNum, Year, Date, predEvent, Week, PctVeg, MajCover, MajClass) %>% 
#   mutate(allVeg = "allVeg")
# vegColors <- c("#333333", 
#                "#663333", "#993300", "#993333",  
#                "#CC9900", "#CCCC00", "#CCCC33", "#CCCC66", "#99CC66",
#                "#99CC66", "#339966", "#669966", "#006600", "#006666", "#336666", "#003300", "#003333",
#                "#0000CC", "#000099", "#000066", "#000033")

# allVegCover  <- ggplot(arrange(catch_spatial, MajCover), aes(allVeg, fill=MajCover)) +
#   geom_bar(position = "fill") +
#   guides(fill = FALSE) +
#   theme_bw()
# allVegCover %+% subset(catch_spatial, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "trapTriggered", "baitLost", "none"))

