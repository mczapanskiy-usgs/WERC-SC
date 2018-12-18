## this script graphically analyzeses relationships between bait type and trap events

library(data.table)
library(plyr)
library(dplyr)
library(ggplot2)
library(ez)
library(mosaic)
library(AICcmodavg)
library(lubridate)

setwd("~/WERC-SC/HALE")

# load data types
read.csv('~/WERC-SC/HALE/TrapsGrid20170626.csv', # catch data processed by Ben (elev, slope, prox to roads/trails/fences/structures, veg, etc.) & Jon (grid cells)
         stringsAsFactors = FALSE) -> spatialData
read.csv('~/WERC-SC/HALE/catch_11.5_spatialCatches_20170109.csv', 
         stringsAsFactors = FALSE) -> baitData

spatialCatch2 <- spatialData %>% 
  # change date to correct format
  mutate(Date = as.Date(Date_, "%m/%d/%Y"),
         Year = year(Date),
         trap=paste0(Trapline,TrapNum),
         eventType = mosaic::derivedFactor(
           "predatorEvent" = predEvent %in% c('ratCaught', 'catCaught', 'mongooseCaught'),
           "otherEvent" = predEvent %in% c('birdOtherCaught', 'trapTriggered', 'baitLost'),
           # "noEvent" = predEvent =="none",
           .default = "noEvent"),
         location = mosaic::derivedFactor(
           front = Trapline %in% c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'),
           back = Trapline %in% c('HAL', 'KAP', 'KAU', 'KW', 'LAI', 'LAU', 'NAM', 'PAL', 'PUU', 'SS', 'WAI'),
           .default = "back")) %>%
  select(-OBJECTID, -Join_Count, -TARGET_FID, -Join_Count_1, -TARGET_FID_1, -trapLocFilename, -StartDate)

write.csv(spatialCatch2, file = '~/WERC-SC/HALE/TrapsGrid20170905.csv',
          row.names = FALSE) 


# add "bait type" to latest data
baitData <- baitData %>% 
  select(catchID, baitType)
baitData2 <- join(spatialData, baitData, by = "catchID") 
# EDIT DATA: remove the mouse events, separate front and backcountry traps, & group predator events (for rerun of mlogit analysis)
baitData_rev <- baitData2 %>% 
  filter(predEvent != 'mouseCaught') %>%
  select(6, 10:32) %>% # remove first 5 columns that are unneccessary
  mutate(Date = as.Date(Date_, "%m/%d/%Y"),
         Year = year(Date),
         Month = month(Date),
         trap=paste0(Trapline,TrapNum),
         eventType = mosaic::derivedFactor(
           "predatorEvent" = predEvent %in% c('ratCaught', 'catCaught', 'mongooseCaught'),
           "otherEvent" = predEvent %in% c('birdOtherCaught', 'trapTriggered', 'baitLost'),
           "noEvent" = predEvent =="none",
           .default = "noEvent"),
         loc = mosaic::derivedFactor(
           front = Trapline %in% c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'),
           back = Trapline %in% c('HAL', 'KAP', 'KAU', 'KW', 'LAI', 'LAU', 'NAM', 'PAL', 'PUU', 'SS', 'WAI'),
           .default = "back"), 
         bait = mosaic::derivedFactor(
           "Cat Food" = baitType %in% c('cannedCat+other', 'cannedCat'),
           "Cat and Dog Food" = baitType %in% c('cannedCat+cannedDog', 'cannedCat+cannedDog+dryDog+oil', 'cannedCat+cannedDog+other'),
           "Dog Food" = baitType %in% c('cannedDog', 'cannedDog+other', 'dryDog+oil(+other)'),
           "Lure" = baitType == "Lure",
           "Professional Bait" = baitType == "ProfessionalBait",
           "other" = baitType %in% c('None', 'NR', 'Other', 'UNK'),
           .default = "other"),
         majCoverType = mosaic::derivedFactor(
           "Barren" = grepl("Barren", MajCover, ignore.case = TRUE),
           "Developed" = grepl("Developed", MajCover, ignore.case = TRUE),
           "HerbCover" = grepl("Herb Cover", MajCover, ignore.case = TRUE),
           "ShrubCover" = grepl("Shrub Cover", MajCover, ignore.case = TRUE),
           "TreeCover" = grepl("Tree Cover", MajCover, ignore.case = TRUE)),
           # .default = "Barren"),
         predEvent = factor(predEvent,
                            level = unique(c("catCaught", "mongooseCaught", "ratCaught", "mouseCaught", "birdOtherCaught", "baitLost", "trapTriggered", "none"),
                            ordered = TRUE)))

#### SUMMARIZE DATA
summary(baitData2$baitType)

summary(baitData_rev$bait)

baitSummary <-
  baitData_rev %>%
  group_by(bait) %>%
  summarize(name_count = n())
baitSummary

bait_predEvent <- 
  baitData_rev %>%
  group_by(bait, predEvent) %>%
  summarize(name_count = n())
write.csv(bait_predEvent, file = '~/WERC-SC/HALE/outputs/bait_predEvent_eck23.csv',
          row.names = FALSE) 


#### GRAPH DATA
baitBarsFill  <- ggplot(baitData_rev, aes(predEvent, fill=bait)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = "Dark2") +
  labs(y = 'Proportion of Trap Events', x = 'Trap Event Type', bait = 'Bait Type Used') +
  theme_bw()
baitBarsFill %+% subset(baitData_rev, predEvent %in% c("ratCaught", "catCaught", "mongooseCaught", "birdOtherCaught", "trapTriggered", "baitLost", "none"))
ggsave(width = 8, height = 5, dpi=300, filename = "~/WERC-SC/HALE/outputs/baitBarsFill_eck23.pdf")


baitBars  <- ggplot(baitData_rev, aes(predEvent, fill=bait)) +
  geom_bar(position = "stack") +
  scale_fill_brewer(palette = "Dark2") +
  labs(y = 'Proportion of Trap Events', x = 'Trap Event Type', bait = 'Bait Type Used') +
  # scale_y_continuous(trans='log10') + # log scale doesn't really work because it doesn't scale each "stack" proportionatly 
  theme_bw()
baitBars %+% subset(baitData_rev, predEvent %in% c("ratCaught", "catCaught", "mongooseCaught", "birdOtherCaught", "trapTriggered", "baitLost", "none"))
ggsave(width = 8, height = 5, dpi=300, filename = "~/WERC-SC/HALE/outputs/baitBarsStack_eck23.pdf")


## by month
baitMonth <- ggplot(baitData_rev, aes(Month, fill=predEvent)) +
  geom_bar() +
  facet_grid(bait ~ ., scales = "free_y")
baitMonth %+% subset(baitData_rev, predEvent %in% c("ratCaught", "catCaught", "mongooseCaught", "birdOtherCaught", "trapTriggered", "baitLost", "none"))

## by season
baitSeason <- ggplot(baitData_rev, aes(Season, fill=bait)) +
  geom_bar() +
  facet_grid(predEvent ~ ., scales = "free_y")

## Vegetation
baitSeason <- ggplot(baitData_rev, aes(Season, fill=bait)) +
  geom_bar() +
  facet_grid(predEvent ~ ., scales = "free_y")
