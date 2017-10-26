## This script compares the data selected for analysis in the report
## with the data for trap checks >13 days
## assessing whether the removal of the >13 day data effected the final results

setwd("~/WERC-SC/HALE")

## load packages
library(stats)
library(data.table)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(mosaic)

#### load data (change directory to the folder holding 'TrapsGrid20170905.csv')
## # catch data processed by Ben (elev, slope, prox to roads/trails/fences/structures, veg, etc.) & Jon (grid cells)

read.csv('~/WERC-SC/HALE/TrapsGrid20170905.csv',
         stringsAsFactors = FALSE) -> spatialCatch 
read.csv('~/WERC-SC/HALE/outputs/allCatch20171025.csv',
         stringsAsFactors = FALSE) -> allCatch

#### function to identify different predator event types
## from script 'eck10_HALE_CPUE_eventType.R' writen by M. Czapanskiy
is.predEvent <- function(predCaught, birdCaught, otherCaught, trapStatus, baitStatus) {
  case_when(
    predCaught == "FC" ~ "catCaught",
    predCaught == "HA" ~ "mongooseCaught",
    predCaught == "MM" ~ "mouseCaught",
    predCaught %in% c("RR", "RN", "RB") ~ "ratCaught",
    birdCaught != "" | otherCaught != "" ~ "birdOtherCaught",
    trapStatus == 'O' & baitStatus == "N" ~ "baitLost",
    predCaught == "" & birdCaught == "" & trapStatus == "C" ~ "trapTriggered", ## this includes events when predator escaped
    TRUE ~ "none")
}

## create predEvent column based on is.predEvent function (and remove mouse caught and NA events)
allCatch <- mutate(allCatch, predEvent = is.predEvent(predCaught, birdCaught, otherCaught, TrapStatus, BaitStatus)) %>% 
  filter(predEvent != 'mouseCaught',
         predEvent != 'NA') %>% 
  mutate(eventType = mosaic::derivedFactor(
      "predatorEvent" = predEvent %in% c('ratCaught', 'catCaught', 'mongooseCaught'),
      "otherEvent" = predEvent %in% c('birdOtherCaught', 'trapTriggered', 'baitLost'),
      "noEvent" = predEvent =="none",
      .default = "noEvent"),
    loc = mosaic::derivedFactor(
      front = Trapline %in% c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'),
      back = Trapline %in% c('HAL', 'KAP', 'KAU', 'KW', 'LAI', 'LAU', 'NAM', 'PAL', 'PUU', 'SS', 'WAI'),
      .default = "back"))

#### ANALYZING DATA
### tables
table(allCatch$TrapChecked)

### figures
## proportion of event types within and outside 13 day check window
eventRatio <- ggplot(allCatch, aes(TrapChecked, fill = eventType)) +
  geom_bar(position = "fill") +
  facet_wrap(~ loc) +
  theme_bw()
eventRatio 
ggsave(width = 8.5, height = 5, dpi=300, filename = "~/WERC-SC/HALE/outputs/eventRatio_eck26.pdf")

## proportion of trap events within and outside 13 day check window
eventRatio <- ggplot(allCatch, aes(TrapChecked, fill = predEvent)) +
  geom_bar(position = "fill") +
  facet_wrap(~ loc) +
  theme_bw()
eventRatio %+% subset(allCatch, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught"))
ggsave(width = 8.5, height = 5, dpi=300, filename = "~/WERC-SC/HALE/outputs/predEventRatio_eck26.pdf")

## histogram of pred events per trapline
ggplot(allCatch, aes(predEvent)) +
  geom_bar() +
  labs(x = 'Predator Event Type - False = outside 13 day check window (n = 24,114), True = within 13 day check window (n = 249,274)', 
       y = 'Number of Trap Events') +
  facet_wrap(~ TrapChecked, scales = "free") +
  # theme(axis.text.x = element_text(angle=60, hjust=1)) +
  theme_bw()
ggsave(width = 10, height = 5, dpi=300, filename = "~/WERC-SC/HALE/outputs/predEvent_hist_eck26.pdf")


catch_over13days <- allCatch %>% 
  filter(TrapChecked == FALSE)

catch_under13days <- allCatch %>% 
  filter(TrapChecked == TRUE)
