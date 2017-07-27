## This script statistically analyzes Bait Type data
## using mlogit model

library(stats)
library(data.table)
library(plyr)
library(dplyr)
library(ggplot2)
library(ez)
library(mlogit)
library(mosaic)
library(AICcmodavg)
library(lubridate)

setwd("~/WERC-SC/HALE")

read.csv('~/WERC-SC/HALE/TrapsGrid.csv', # catch data processed by Ben (elev, slope, prox to roads/trails/fences/structures, veg, etc.) & Jon (grid cells)
         stringsAsFactors = FALSE) -> spatialData
## add in "baitType"
read.csv('~/WERC-SC/HALE/catch_11.5_spatialCatches_20170109.csv', # catch data processed by Ben (elev, slope, prox to roads/trails/fences/structures, veg, etc.) & Jon (grid cells)
         stringsAsFactors = FALSE) -> baitData
baitData <- baitData %>% 
  select(catchID, baitType)
baitData2 <- join(spatialData, baitData, by = "catchID") 
# EDIT DATA: remove the mouse events, separate front and backcountry traps, & group predator events (for rerun of mlogit analysis)
baitData_rev <- baitData2 %>% 
  filter(predEvent != 'mouseCaught') %>%
  select(6, 10:31) %>% # remove first 5 columns that are unneccessary
  mutate(Date = as.Date(Date, "%m/%d/%Y"),
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
           "cat" = baitType %in% c('cannedCat+other', 'cannedCat'),
           "cat_dog" = baitType %in% c('cannedCat+cannedDog', 'cannedCat+cannedDog+dryDog+oil', 'cannedCat+cannedDog+other'),
           "dog" = baitType %in% c('cannedDog', 'cannedDog+other', 'dryDog+oil(+other)'),
           "Lure" = baitType == "Lure",
           "ProfessionalBait" = baitType == "ProfessionalBait",
           "other" = baitType %in% c('None', 'NR', 'Other', 'UNK'), 
           .default = "other"))

#### RESTRUCTURE DATA FUNCTION
formatData <- function(data, var, subset = NA){
  if(!(var %in% colnames(data)))
    stop(sprintf('var [%s] not found in data columns', var))
  data$var <- getElement(data, var)
  # Reshape data so that there is one row for every option, for every choice situation.
  # Here are the possible outcomes every time the trap is set:
  events <- unique(data$var)
  # # And here are the number of choice situations:
  # trap <- data$trap
  # # Replicate the rows according to number of events:
  # data2 <- data[rep(row.names(data), data$trap),]
  # data2 <- data2 %>%
  #   mutate(chid = row.names(data2))
  
  if (!is.na(subset)){
    data <- data[sample(1:nrow(data), subset),]
  }
  # Expand each choice situation so that each alternative is on its own row.
  # Do this with the merge function.  The alternative names will be stored in column `x`.
  expanded_data <- merge(events, data)
  expanded_data <- expanded_data %>%
    mutate(choice = ifelse(x==var, TRUE, FALSE))
  return(expanded_data)
}

#### CREATE LONG DATA TABLES
exp_baitData <- formatSpatialData(baitData_rev, 'predEvent') # , subset = 100
## with only predator data
baitData_caughts_only <- baitData_rev %>% 
  filter(eventType == "predatorEvent")
exp_baitData_caughts <- formatSpatialData(baitData_caughts_only, 'predEvent', subset = 5000)

#### RUN mlogt MODELS
#_____________________________________________________________________________________________________________________
# PREDATOR ONLY DATA
bait_models_caughts <- list() # create model list

bait_caughts <- mlogit.data(exp_baitData_caughts,
                               #  %>% filter(loc == "front") %>% filter(!(Trapline %in% c('KAU', 'KW', 'LAU', 'PUU', 'SS'))),
                               choice="choice",
                               alt.var ="x", 
                               shape="long", 
                               id.var = "Trapline",
                               chid.var="chid")
## baseline model
bait_models_caughts[[1]] <- mlogit(choice ~ 0 | loc, 
                                      rpar=c('ratCaught:(intercept)'='n',
                                             'mongooseCaught:(intercept)'='n'),
                                      iterlim=1, print.level=1,
                                      data=bait_caughts)
bait_models_caughts[[2]] <- mlogit(choice ~ 0 | baitType, 
                                      rpar=c('ratCaught:(intercept)'='n',
                                             'mongooseCaught:(intercept)'='n'),
                                      iterlim=1, print.level=1,
                                      data=bait_caughts)
