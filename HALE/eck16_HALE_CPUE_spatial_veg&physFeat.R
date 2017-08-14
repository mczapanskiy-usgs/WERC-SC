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
catch_spatial <- catch_spatial %>% 
  mutate(fill = 1, 
         predEvent = factor(predEvent, 
                            level = c('ratCaught', 'catCaught', 'mongooseCaught', 'mouseCaught', 'birdOtherCaught', 'baitLost', 'trapTriggered', 'none'), 
                            ordered = TRUE))
vegColors <- c("red4", 
               "orangered1", "orangered2", "orangered3",
               "palegreen", "palegreen1", "palegreen2", "palegreen3", "palegreen4",
               "deepskyblue1", "deepskyblue2", "deepskyblue3", "dodgerblue", "dodgerblue1", "dodgerblue2", "dodgerblue3", "dodgerblue4",
               "slateblue1", "slateblue2", "slateblue3", "slateblue4")
    
#### VEGETATION
## create dataset of proportion of traps located in different veg types
vegCover  <- ggplot(catch_spatial, aes(predEvent, fill=majCoverType)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = "BuGn") +
  labs(y = 'Proportion of Trap Events', x = 'Trap Event Type', majCoverType = 'Major Vegetation Cover') +
  theme_bw()
vegCover %+% subset(catch_spatial, predEvent %in% c("ratCaught", "catCaught", "mongooseCaught", "birdOtherCaught", "trapTriggered", "baitLost", "none"))
ggsave(width = 8, height = 5, dpi=300, filename = "~/WERC-SC/HALE/outputs/vegCover_events_eck16.pdf")

allVegCover  <- ggplot(arrange(catch_spatial, MajCover), aes(fill, fill=majCoverType)) +
  geom_bar(position = "fill") +
  guides(fill = FALSE) +
  scale_fill_brewer(palette = "BuGn") +
  labs(y = 'Proportion of Trap Events', x = 'Trap Event Type', majCoverType = 'Major Vegetation Cover') +
  theme_bw()
allVegCover 
ggsave(width = 1.5, height = 5, dpi=300, filename = "~/WERC-SC/HALE/outputs/allVegCover_events_eck16.pdf")

## major vegetation cover
# # event type
# vegCover_events <- ggplot(arrange(catch_spatial, MajCover), aes(eventType, fill=majCoverType)) +
#   geom_bar(position = "fill") +
#   scale_fill_brewer(palette = "BuGn") +
#   labs(y = 'Proportion of Trap Events', x = 'Event Type', majCoverType = 'Major Vegetation Cover') +
#   theme_bw()  
# vegCover_events 
# ggsave(width = 8.5, height = 5, dpi=300, filename = "~/WERC-SC/HALE/outputs/vegCover_events_eck16.pdf")
# 
# allVegCover_events <- ggplot(arrange(catch_spatial, MajCover), aes(eventType, fill=MajCover)) +
#   geom_bar(position = "fill") +
#   scale_fill_manual(values = vegColors) +
#   labs(y = 'Proportion of Trap Events', x = 'Event Type') +
#   theme_bw() 
# allVegCover_events 
# ggsave(width = 8.5, height = 5, dpi=300, filename = "~/WERC-SC/HALE/outputs/allVegCover_events_eck16.pdf")
# 
# # preds only
# spatial_preds <- catch_spatial %>% 
#   filter(eventType == "predatorEvent")

# vegCover_preds <- ggplot(arrange(spatial_preds, MajCover), aes(predEvent, fill=majCoverType)) +
#   geom_bar(position = "fill") +
#   scale_fill_brewer(palette = "BuGn") +
#   labs(y = 'Proportion of Capture Events', x = 'Predator Event Type', majCoverType = 'Major Vegetation Cover') +
#   theme_bw()  
# vegCover_preds
# ggsave(width = 8.5, height = 5, dpi=300, filename = "~/WERC-SC/HALE/outputs/vegCover_preds_eck16.pdf")
# 
# allVegCover_preds <- ggplot(arrange(spatial_preds, MajCover), aes(predEvent, fill=MajCover)) +
#   geom_bar(position = "fill") +
#   scale_fill_manual(values = vegColors) +
#   labs(y = 'Proportion of Capture Events', x = 'Predator Event Type') +
#   theme_bw()
# allVegCover_preds 
# ggsave(width = 8.5, height = 5, dpi=300, filename = "~/WERC-SC/HALE/outputs/allVegCover_preds_eck16.pdf")




#### PHYSICAL FEATURES
## ELEVATION
elev <- ggplot(physFeat, aes(Elevation)) +
  geom_freqpoly(binwidth = 50) +
  geom_freqpoly(binwidth = 50, aes(colour = predEvent)) +
  labs(y = 'Frequency of Trap Events', x = 'Elevation (m)', predEvent = 'Predator Event Type') +
  scale_y_log10() +
  theme_bw()
elev %+% subset(physFeat, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "baitLost", "trapTriggered", "none"))
ggsave(width = 8.5, height = 5, dpi=300, filename = "~/WERC-SC/HALE/outputs/elevFreq_eck16.pdf")

## FREQUENCY OF EVENTS VS ELEVATION
# number of events at each distance
elevation <- catch_spatial %>%  
  group_by(Elevation) %>% 
  dplyr::count() 
# number of event types at each distance
elevation_events <- catch_spatial %>% 
  group_by(Elevation, predEvent) %>% 
  dplyr::count() %>%  
  rename(nEvent = n)
# combine and find frequency of events at each distance
elev <- left_join(elevation_events, elevation, by = "Elevation") %>% 
  mutate(freq = (nEvent/n))
# graph
elevProp <- ggplot(elev, aes(Elevation, freq)) +
  geom_point(aes(color = predEvent), size = 0.5) + 
  geom_smooth(method = lm, aes(colour = predEvent)) +  # linear smoothing
  ylim(c(0,1)) +
  labs(y = 'Proportion of Trap Events', x = 'Elevation (m)', predEvent = 'Predator Event Type') +
  theme_bw()
elevProp
ggsave(width = 8.5, height = 5, dpi=300, filename = "~/WERC-SC/HALE/outputs/elevProp_eck16.pdf")
## FREQUENCY OF PREDATOR EVENTS VS ELEVATION
# number of events at each distance
elevation_p <- catch_spatial %>% 
  filter(eventType == "predatorEvent") %>% 
  group_by(Elevation) %>% 
  dplyr::count()
# number of event types at each distance
elevation_preds <- catch_spatial %>% 
  filter(eventType == "predatorEvent") %>% 
  group_by(Elevation, predEvent) %>% 
  dplyr::count() %>%   
  rename(nEvent = n)
# combine and find frequency of events at each distance
elev_p <- left_join(elevation_preds, elevation_p, by = "Elevation") %>% 
  mutate(freq = (nEvent/n))
# graph
elevProp_Preds <- ggplot(elev, aes(Elevation, freq)) +
  geom_point(aes(color = predEvent), size = 0.5) + 
  geom_smooth(method = lm, aes(colour = predEvent)) +  # linear smoothing
  ylim(c(0,0.5)) +
  labs(y = 'Proportion of Predator Events', x = 'Elevation (m)', predEvent = 'Predator Event Type') +
  scale_fill_brewer(palette = "Set1") +
  theme_bw()
elevProp_Preds %+% subset(elev, predEvent %in% c("ratCaught", "catCaught", "mongooseCaught"))
ggsave(width = 8.5, height = 5, dpi=300, filename = "~/WERC-SC/HALE/outputs/elevProp_preds_eck16.pdf")


# veg <- catch_spatial %>% 
#   select(Trapline, TrapNum, Year, Date, predEvent, Week, PctVeg, MajCover, MajClass) %>% 
#   mutate(allVeg = "allVeg")
# vegColors <- c("#333333", 
#                "#663333", "#993300", "#993333",  
#                "#CC9900", "#CCCC00", "#CCCC33", "#CCCC66", "#99CC66",
#                "#99CC66", "#339966", "#669966", "#006600", "#006666", "#336666", "#003300", "#003333",
#                "#0000CC", "#000099", "#000066", "#000033")


