## this script graphically analyzes spatial results of CPUE data
## seasonal and yearly effects on:
##   event types (pred, other, none)
##   predator events (rat, cat, mongoose)

library(stats)
library(plyr)
library(dplyr)
library(data.table)
library(tidyr)
library(stringr)
library(ggplot2)
library(mosaic)

setwd("~/WERC-SC/HALE")

read.csv('~/WERC-SC/HALE/catch_11.5_spatialCatches_20170109.csv',
         stringsAsFactors = FALSE) -> catch_spatial
read.csv('~/WERC-SC/HALE/TraplinePredEventPUE_11_20161209.csv',
         stringsAsFactors = FALSE) -> catch_EventPUE

### SEASONAL ANALYSIS
seasonalEvents <- catch_spatial %>% 
  group_by(Season, predEvent) %>% 
  summarise(eventCount = sum(NEvents)) 
# change variable type
catch_spatial$Season <- factor(catch_spatial$Season, levels = c("Pre-laying", "Incubation", "Nestling", "offSeason"))
catch_spatial$predEvent <- factor(catch_spatial$predEvent, 
                                  levels = c("catCaught", "mongooseCaught", "ratCaught", "mouseCaught", 
                                             "birdOtherCaught", "baitLost", "trapTriggered", "none"))
# box plot of CPUE in different seasons
season_box <- ggplot(catch_spatial, aes(Season, CPUE)) +
  geom_boxplot() + #geom_bar(stat = "identity") +
  facet_wrap(~ predEvent) +
  theme_bw() 
# scale_fill_gradient(low = "green", high = "red")
season_box %+% subset(catch_spatial, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught")) # , "trapTriggered", "baitLost", "none"))

## bar graphs of proportion of events happening in different seasons
season_bar <- ggplot(catch_spatial, aes(Season, fill = predEvent)) + # season_bar <- ggplot(arrange(catch_EventPUE, Season), aes(Season, fill = predEvent)) +
  geom_bar(position = "fill") +
  theme_bw() 
# theme(axis.text.x = element_text(angle=60, hjust=1)) 
season_bar %+% subset(catch_spatial, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "birdOtherCaught", "trapTriggered", "baitLost", "none"))
ggsave(width = 8.5, height = 5, dpi=300, filename = "~/WERC-SC/HALE/outputs/seasonalProps_eck15.pdf")
# preds only
season_bar_pred <- ggplot(catch_spatial, aes(Season, fill = predEvent)) +
  geom_bar(position = "fill") +
  theme_bw()
season_bar_pred %+% subset(catch_spatial, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught"))
ggsave(width = 8.5, height = 5, dpi=300, filename = "~/WERC-SC/HALE/outputs/seasonalProps_preds_eck15.pdf")
# seasonalEvents <- catch_EventPUE %>% 
#   group_by(Season, predEvent) %>% 
#   summarise(eventCount = sum(NEvents)) 
# # change variable type
# catch_EventPUE$Season <- factor(catch_EventPUE$Season, levels = c("Pre-laying", "Incubation", "Nestling", "offSeason"))
# catch_EventPUE$predEvent <- factor(catch_EventPUE$predEvent, levels = c("catCaught", "ratCaught", "mongooseCaught", "mouseCaught", "birdOtherCaught", "baitLost", "trapTriggered", "none"))
# # box plot of CPUE in different seasons
# season_box <- ggplot(catch_EventPUE, aes(Season, CPUE)) +
#   geom_boxplot() + #geom_bar(stat = "identity") +
#   facet_wrap(~ predEvent) +
#   theme_bw() 
#   # scale_fill_gradient(low = "green", high = "red")
# season_box %+% subset(catch_EventPUE, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught")) # , "trapTriggered", "baitLost", "none"))
# 
# ## bar graphs of proportion of events happening in different seasons
# season_bar <- ggplot(catch_EventPUE, aes(Season, fill = predEvent)) + # season_bar <- ggplot(arrange(catch_EventPUE, Season), aes(Season, fill = predEvent)) +
#   geom_bar(position = "fill") +
#   theme_bw() 
#   # theme(axis.text.x = element_text(angle=60, hjust=1)) 
# season_bar %+% subset(catch_EventPUE, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "trapTriggered", "baitLost", "none"))
# # preds only
# season_bar_pred <- ggplot(catch_EventPUE, aes(Season, fill = predEvent)) +
#   geom_bar(position = "fill") +
#   theme_bw()
# season_bar_pred %+% subset(catch_EventPUE, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught"))


### YEAR
# GRAPH annual average CPUE and SD
CPUE_yr <- ggplot(annualCPUE, aes(Year_, annCPUE)) +
  geom_errorbar(aes(ymin=annCPUE-sdCPUE, ymax=annCPUE+sdCPUE), colour="black", width=.1) +
  geom_line() +
  geom_point() + 
  facet_wrap(~ predEvent) +
  theme_bw() +
  labs(x = 'Year', y = 'Annual Frequency of Events per Unit Effort')
CPUE_yr %+% subset(annualCPUE, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "trapTriggered", "baitLost", "none"))
CPUE_yr_preds <- ggplot(annualCPUE, aes(Year_, annCPUE)) +
  geom_errorbar(aes(ymin=annCPUE-sdCPUE, ymax=annCPUE+sdCPUE), colour="black", width=.1) +
  geom_line() +
  geom_point() + 
  facet_wrap(~ predEvent, nrow = 3) +
  ylim(NA, 1) +
  theme_bw() +
  labs(x = 'Year', y = 'Annual Frequency of Events per Unit Effort')
CPUE_yr_preds %+% subset(annualCPUE, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught"))


# apply(fitted)

# read.csv('~/WERC-SC/HALE/catch_burrows.csv',
#          stringsAsFactors = FALSE) -> burrows
