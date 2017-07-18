## This script statistically analyzes spatial data
## (effect of elevation, slope, proximity to roads/trails/fences/structures, vegetation, etc. on trap events)
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
spatialData2 <- join(spatialData, baitData, by = "catchID") 
# EDIT DATA: remove the mouse events, separate front and backcountry traps, & group predator events (for rerun of mlogit analysis)
spatialData_rev <- spatialData2 %>% 
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
  
with(spatialData_rev, table(MajCover))
with(spatialData_rev, table(MajClass))

#### RESTRUCTURE DATA FUNCTION
formatSpatialData <- function(data, var, subset = NA){
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
exp_spatialData <- formatSpatialData(spatialData_rev, 'predEvent') # , subset = 100
## with only predator data
spatialData_caughts_only <- spatialData_rev %>% 
  filter(eventType == "predatorEvent")
exp_spatialData_caughts <- formatSpatialData(spatialData_caughts_only, 'predEvent', subset = 5000)

#### RUN mlogt MODELS
# PREDATOR ONLY DATA
spatial_models_caughts <- list() # create model list

spatial_caughts <- mlogit.data(exp_spatialData_caughts,
                      #  %>% filter(loc == "front") %>% filter(!(Trapline %in% c('KAU', 'KW', 'LAU', 'PUU', 'SS'))),
                    choice="choice",
                    alt.var ="x", 
                    shape="long", 
                    id.var = "Trapline",
                    chid.var="chid")
## baseline model
spatial_models_caughts[[1]] <- mlogit(choice ~ 0 | loc, 
                                      rpar=c('ratCaught:(intercept)'='n',
                                             'mongooseCaught:(intercept)'='n'),
                                      iterlim=1, print.level=1,
                                      data=spatial_caughts)
spatial_models_caughts[[2]] <- mlogit(choice ~ 0 | baitType, 
                                      rpar=c('ratCaught:(intercept)'='n',
                                             'mongooseCaught:(intercept)'='n'),
                                      iterlim=1, print.level=1,
                                      data=spatial_caughts)
spatial_models_caughts[[3]] <- mlogit(choice ~ 0 | Season, 
                                      rpar=c('ratCaught:(intercept)'='n',
                                             'mongooseCaught:(intercept)'='n'),
                                      iterlim=1, print.level=1,
                                      data=spatial_caughts)
spatial_models_caughts[[4]] <- mlogit(choice ~ 0 | Year, 
                                      rpar=c('ratCaught:(intercept)'='n',
                                             'mongooseCaught:(intercept)'='n'),
                                      iterlim=1, print.level=1,
                                      data=spatial_caughts)
spatial_models_caughts[[5]] <- mlogit(choice ~ 0 | loc + baitType + Season + Year, 
                                      rpar=c('ratCaught:(intercept)'='n',
                                             'mongooseCaught:(intercept)'='n'),
                                      iterlim=1, print.level=1,
                                      data=spatial_caughts)
AIC(spatial_models_caughts[[1]]) # loc
AIC(spatial_models_caughts[[2]]) # baitType
AIC(spatial_models_caughts[[3]]) # Season
AIC(spatial_models_caughts[[4]]) # Year
AIC(spatial_models_caughts[[5]]) # loc + baitType + Season + Year
## slope and elevation model
spatial_models_caughts[[1]] <- mlogit(choice ~ 0 | loc + MedSlope + Elevation, 
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                           iterlim=1, print.level=1,
                           data=spatial_caughts) 
spatial_models_caughts[[2]] <- mlogit(choice ~ 0 | loc + Elevation, 
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                              iterlim=1, print.level=1,
                              data=spatial_caughts) 
spatial_models_caughts[[3]] <- mlogit(choice ~ 0 | loc + MedSlope, 
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                              iterlim=1, print.level=1,
                              data=spatial_caughts) 
## burrow radius model
spatial_models_caughts[[4]] <- mlogit(choice ~ 0 | loc + MedSlope + Elevation + Burrows10, 
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                              iterlim=1, print.level=1,
                              data=spatial_caughts)
spatial_models_caughts[[5]] <- mlogit(choice ~ 0 | loc + MedSlope + Elevation + Burrows50, 
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                              iterlim=1, print.level=1,
                              data=spatial_caughts)
spatial_models_caughts[[6]] <- mlogit(choice ~ 0 | loc + MedSlope + Elevation + Burrows100, 
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                              iterlim=1, print.level=1,
                              data=spatial_caughts)
# distance to structures model
spatial_models_caughts[[7]] <- mlogit(choice ~ 0 | loc + DistRoad,
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                              iterlim=1, print.level=1,
                              data=spatial_caughts)
spatial_models_caughts[[8]] <- mlogit(choice ~ 0 | loc + DistTrail, 
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                              iterlim=1, print.level=1,
                              data=spatial_caughts)
spatial_models_caughts[[9]] <- mlogit(choice ~ 0 | loc + DistFence, 
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                              iterlim=1, print.level=1,
                              data=spatial_caughts)
spatial_models_caughts[[10]] <- mlogit(choice ~ 0 | loc + DistShelter, 
                               rpar=c('ratCaught:(intercept)'='n',
                                      'mongooseCaught:(intercept)'='n'),
                              iterlim=1, print.level=1,
                              data=spatial_caughts)
spatial_models_caughts[[11]] <- mlogit(choice ~ 0 | loc + DistRoad + DistTrail + DistFence + DistShelter, 
                               rpar=c('ratCaught:(intercept)'='n',
                                      'mongooseCaught:(intercept)'='n'),
                              iterlim=1, print.level=1,
                              data=spatial_caughts)
spatial_models_caughts[[12]] <- mlogit(choice ~ 0 | loc + MedSlope + Elevation + 
                                 Burrows100 + DistRoad + DistTrail + DistFence + DistShelter, 
                               rpar=c('ratCaught:(intercept)'='n',
                                      'mongooseCaught:(intercept)'='n'),
                              iterlim=1, print.level=1,
                              data=spatial_caughts)
# vegetation model
spatial_models_caughts[[13]] <- mlogit(choice ~ 0 | loc + PctVeg, 
                               rpar=c('ratCaught:(intercept)'='n',
                                      'mongooseCaught:(intercept)'='n'),
                              iterlim=1, print.level=1,
                              data=spatial_caughts)
spatial_models_caughts[[14]] <- mlogit(choice ~ 0 | loc + MajCover, 
                               rpar=c('ratCaught:(intercept)'='n',
                                      'mongooseCaught:(intercept)'='n'),
                              iterlim=1, print.level=1,
                              data=spatial_caughts)
spatial_models_caughts[[15]] <- mlogit(choice ~ 0 | loc + MajClass,
                               rpar=c('ratCaught:(intercept)'='n',
                                      'mongooseCaught:(intercept)'='n'),
                              iterlim=1, print.level=1,
                              data=spatial_caughts)
spatial_models_caughts[[16]] <- mlogit(choice ~ 0 | loc + PctVeg + MajCover + MajClass,
                               rpar=c('ratCaught:(intercept)'='n',
                                      'mongooseCaught:(intercept)'='n'),
                               iterlim=1, print.level=1,
                               data=spatial_caughts)
spatial_models_caughts[[17]] <- mlogit(choice ~ 0 | loc + MedSlope + Elevation + Burrows100 + 
                                 DistRoad + DistTrail + DistFence + DistShelter + MajCover,
                               rpar=c('ratCaught:(intercept)'='n',
                                      'mongooseCaught:(intercept)'='n'),
                               iterlim=1, print.level=1,
                               data=spatial_caughts)
# spatial_models[[18]] <- mlogit(choice ~ 0 |,
#                                iterlim=1, print.level=1,
#                                data=spatial_caughts)
AIC(spatial_models_caughts[[1]]) # loc + MedSlope + Elevation ***
AIC(spatial_models_caughts[[2]]) # loc + Elevation
AIC(spatial_models_caughts[[3]]) # loc + MedSlope

AIC(spatial_models_caughts[[4]]) # loc + MedSlope + Elevation + Burrows10 ***
AIC(spatial_models_caughts[[5]]) # loc + MedSlope + Elevation + Burrows50
AIC(spatial_models_caughts[[6]]) # loc + MedSlope + Elevation + Burrows100

AIC(spatial_models_caughts[[7]]) # loc + DistRoad
AIC(spatial_models_caughts[[8]]) # loc + DistTrail
AIC(spatial_models_caughts[[9]]) # loc + DistFence
AIC(spatial_models_caughts[[10]]) # loc + DistShelter
AIC(spatial_models_caughts[[11]]) # loc + DistRoad + DistTrail + DistFence + DistShelter
AIC(spatial_models_caughts[[12]]) # loc + MedSlope + Elevation + Burrows100 + DistRoad + DistTrail + DistFence + DistShelter ***

AIC(spatial_models_caughts[[13]]) # loc + PctVeg
AIC(spatial_models_caughts[[14]]) # loc + MajCover ***
AIC(spatial_models_caughts[[15]]) # loc + MajClass
# AIC(spatial_models_caughts[[16]]) # loc + PctVeg + MajCover + MajClass [needs more memory] 
AIC(spatial_models_caughts[[17]]) # loc + MedSlope + Elevation + Burrows100 + DistRoad + DistTrail + DistFence + DistShelter + MajCover 
AIC(spatial_models_caughts[[18]]) #
