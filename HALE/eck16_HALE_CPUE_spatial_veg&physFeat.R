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
## major vegetation cover
# event type
vegCover_events <- ggplot(arrange(catch_spatial, MajCover), aes(eventType, fill=majCoverType)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = "BuGn") +
  labs(y = 'Proportion of Trap Events', x = 'Event Type', majCoverType = 'Major Vegetation Cover') +
  theme_bw()  
vegCover_events 
ggsave(width = 8.5, height = 5, dpi=300, filename = "~/WERC-SC/HALE/outputs/vegCover_events_eck16.pdf")

allVegCover_events <- ggplot(arrange(catch_spatial, MajCover), aes(eventType, fill=MajCover)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = vegColors) +
  labs(y = 'Proportion of Trap Events', x = 'Event Type') +
  theme_bw() 
allVegCover_events 
ggsave(width = 8.5, height = 5, dpi=300, filename = "~/WERC-SC/HALE/outputs/allVegCover_events_eck16.pdf")

# preds only
vegCover_preds <- ggplot(arrange(spatial_preds, MajCover), aes(predEvent, fill=majCoverType)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = "BuGn") +
  labs(y = 'Proportion of Capture Events', x = 'Predator Event Type', majCoverType = 'Major Vegetation Cover') +
  theme_bw()  
vegCover_preds
ggsave(width = 8.5, height = 5, dpi=300, filename = "~/WERC-SC/HALE/outputs/vegCover_preds_eck16.pdf")

allVegCover_preds <- ggplot(arrange(spatial_preds, MajCover), aes(predEvent, fill=MajCover)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = vegColors) +
  labs(y = 'Proportion of Capture Events', x = 'Predator Event Type') +
  theme_bw()
allVegCover_preds 
ggsave(width = 8.5, height = 5, dpi=300, filename = "~/WERC-SC/HALE/outputs/allVegCover_preds_eck16.pdf")

# ## percent vegetation cover
# # predator events
# pctVeg_preds <- ggplot(spatial_preds, aes(PctVeg)) +
#   geom_freqpoly(aes(color = predEvent), binwidth = 0.05) + 
#   labs(y = 'Number of Catch Events', x = '% Vegetation Cover') +
#   scale_color_brewer(palette = "Set1") +
#   theme_bw() ## + theme(axis.text.x = element_text(angle=60, hjust=1))
# pctVeg_preds 
# ggsave(width = 8.5, height = 5, dpi=300, filename = "~/WERC-SC/HALE/outputs/vegPctCover_preds_eck16.pdf")
# # event type
# pctVeg_events <- ggplot(catch_spatial, aes(PctVeg)) +
#   geom_freqpoly(aes(color = eventType), binwidth = 0.05) +
#   labs(y = 'Number of Trap Events', x = '% Vegetation Cover') +
#   scale_color_brewer(palette = "Dark2") +
#   theme_bw() ## + theme(axis.text.x = element_text(angle=60, hjust=1))
# pctVeg_events 
# ggsave(width = 8.5, height = 5, dpi=300, filename = "~/WERC-SC/HALE/outputs/vegPctCover_events_eck16.pdf")
# # all data
# pctVeg <- ggplot(catch_spatial, aes(predEvent, PctVeg)) +
#   geom_boxplot() + ## , stat="bin"
#   # scale_fill_manual(values = vegColors) +
#   theme_bw() ## + theme(axis.text.x = element_text(angle=60, hjust=1))
# pctVeg 
# 
# vegCover <- ggplot(arrange(catch_spatial, MajCover), aes(predEvent, fill=majCoverType)) +
#   geom_point(position = "fill") +
#   # scale_fill_manual(values = vegColors) +
#   theme_bw() ## + theme(axis.text.x = element_text(angle=60, hjust=1)) 
# vegCover %+% subset(catch_spatial, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "trapTriggered", "baitLost", "none"))
# 
# allVegCover <- ggplot(arrange(catch_spatial, MajCover), aes(predEvent, fill=MajCover)) +
#   geom_bar(position = "fill") +
#   scale_fill_manual(values = vegColors) +
#   theme_bw() ## + theme(axis.text.x = element_text(angle=60, hjust=1)) 
# allVegCover %+% subset(catch_spatial, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "trapTriggered", "baitLost", "none"))



#### PHYSICAL FEATURES
## ELEVATION
elev <- ggplot(physFeat, aes(Elevation)) +
  geom_freqpoly(binwidth = 50) +
  geom_freqpoly(binwidth = 50, aes(colour = predEvent)) +
  labs(y = 'Frequency of Trap Events', x = 'Elevation (m)', predEvent = 'Predator Event Type') +
  theme_bw()
elev %+% subset(physFeat, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "baitLost", "trapTriggered", "none"))
ggsave(width = 8.5, height = 5, dpi=300, filename = "~/WERC-SC/HALE/outputs/elevFreq_eck16.pdf")

## FREQUENCY OF EVENTS VS ELEVATION
# number of events at each distance
elevation <- catch_spatial %>%  
  group_by(Elevation) %>% 
  count()
# number of event types at each distance
elevation_events <- catch_spatial %>% 
  group_by(Elevation, predEvent) %>% 
  count() %>%  
  rename(nEvent = n)
# combine and find frequency of events at each distance
elev <- left_join(elevation_events, elevation, by = "Elevation") %>% 
  mutate(freq = (nEvent/n))
# graph
elevProp <- ggplot(elev, aes(Elevation, freq)) +
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
  count()
# number of event types at each distance
elevation_preds <- catch_spatial %>% 
  filter(eventType == "predatorEvent") %>% 
  group_by(Elevation, predEvent) %>% 
  count() %>%  
  rename(nEvent = n)
# combine and find frequency of events at each distance
elev_p <- left_join(elevation_preds, elevation_p, by = "Elevation") %>% 
  mutate(freq = (nEvent/n))
# graph
elevProp_Preds <- ggplot(elev_p, aes(Elevation, freq)) +
  geom_smooth(method = lm, aes(colour = predEvent)) +  # linear smoothing
  ylim(c(0,1)) +
  labs(y = 'Frequency of Predator Events', x = 'Elevation (m)', predEvent = 'Predator Event Type') +
  scale_fill_brewer(palette = "Set1") +
  theme_bw()
elevProp_Preds
ggsave(width = 8.5, height = 5, dpi=300, filename = "~/WERC-SC/HALE/outputs/elevProp_preds_eck16.pdf")


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

