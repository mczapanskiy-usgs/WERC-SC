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

spatialCatch2 <- spatialCatch %>% 
  # change date to correct format
  mutate(Date = as.Date(Date_, "%m/%d/%Y"),
         Year = year(Date),
         trap=paste0(Trapline,TrapNum),
         eventType = mosaic::derivedFactor(
           "predatorEvent" = predEvent %in% c('ratCaught', 'catCaught', 'mongooseCaught'),
           "otherEvent" = predEvent %in% c('birdOtherCaught', 'trapTriggered', 'baitLost'),
           "noEvent" = predEvent =="none",
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
         predEvent = factor(predEvent, 
                            level = c('ratCaught', 'catCaught', 'mongooseCaught', 'mouseCaught', 'birdOtherCaught', 'baitLost', 'trapTriggered', 'none'), 
                            ordered = TRUE))


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
  theme_bw()
baitBars %+% subset(baitData_rev, predEvent %in% c("ratCaught", "catCaught", "mongooseCaught", "birdOtherCaught", "trapTriggered", "baitLost", "none"))
# ggsave(width = 8, height = 5, dpi=300, filename = "~/WERC-SC/HALE/outputs/baitBars_eck23.pdf")

