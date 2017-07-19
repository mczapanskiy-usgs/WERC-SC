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

# EDIT DATA: remove the mouse events, separate front and backcountry traps, & group predator events (for rerun of mlogit analysis)
spatialData_rev <- spatialData %>% 
  filter(predEvent != 'mouseCaught') %>%
  select(6, 10:30) %>% # remove first 5 columns that are unneccessary
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
            .default = "back"))
  
with(spatialData_rev, table(MajCover))
with(spatialData_rev, table(MajClass))

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
exp_spatialData <- formatData(spatialData_rev, 'predEvent') # , subset = 100
## with only predator data
spatialData_caughts_only <- spatialData_rev %>% 
  filter(eventType == "predatorEvent")
exp_spatialData_caughts <- formatData(spatialData_caughts_only, 'predEvent') # , subset = 5000)

exp_spatialData_events<- formatData(spatialData_rev, 'eventType')

#### RUN mlogt MODELS
#_____________________________________________________________________________________________________________________
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
spatial_models_caughts[[2]] <- mlogit(choice ~ 0 | Season, 
                                      rpar=c('ratCaught:(intercept)'='n',
                                             'mongooseCaught:(intercept)'='n'),
                                      iterlim=1, print.level=1,
                                      data=spatial_caughts)
spatial_models_caughts[[3]] <- mlogit(choice ~ 0 | Year, 
                                      rpar=c('ratCaught:(intercept)'='n',
                                             'mongooseCaught:(intercept)'='n'),
                                      iterlim=1, print.level=1,
                                      data=spatial_caughts)
spatial_models_caughts[[4]] <- mlogit(choice ~ 0 | loc + Season + Year, 
                                      rpar=c('ratCaught:(intercept)'='n',
                                             'mongooseCaught:(intercept)'='n'),
                                      iterlim=1, print.level=1,
                                      data=spatial_caughts)
# loc + Season + Year ****
AIC(spatial_models_caughts[[1]]) # loc
AIC(spatial_models_caughts[[2]]) # Season
AIC(spatial_models_caughts[[3]]) # Year
AIC(spatial_models_caughts[[4]]) # loc + Season + Year


## slope and elevation model
spatial_models_caughts[[5]] <- mlogit(choice ~ 0 | loc + Season + Year + MedSlope + Elevation, 
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                           iterlim=1, print.level=1,
                           data=spatial_caughts) 
spatial_models_caughts[[6]] <- mlogit(choice ~ 0 | loc + Season + Year + Elevation, 
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                              iterlim=1, print.level=1,
                              data=spatial_caughts) 
spatial_models_caughts[[7]] <- mlogit(choice ~ 0 | loc + Season + Year + MedSlope, 
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                              iterlim=1, print.level=1,
                              data=spatial_caughts) 
## burrow radius model
spatial_models_caughts[[8]] <- mlogit(choice ~ 0 | loc + Season + Year + Burrows10, 
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                              iterlim=1, print.level=1,
                              data=spatial_caughts)
spatial_models_caughts[[9]] <- mlogit(choice ~ 0 | loc + Season + Year + Burrows50, 
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                              iterlim=1, print.level=1,
                              data=spatial_caughts)
spatial_models_caughts[[10]] <- mlogit(choice ~ 0 | loc + Season + Year + Burrows100, 
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                              iterlim=1, print.level=1,
                              data=spatial_caughts)
# distance to structures model
spatial_models_caughts[[11]] <- mlogit(choice ~ 0 | loc + Season + Year + DistRoad,
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                              iterlim=1, print.level=1,
                              data=spatial_caughts)
spatial_models_caughts[[12]] <- mlogit(choice ~ 0 | loc + Season + Year + DistTrail, 
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                              iterlim=1, print.level=1,
                              data=spatial_caughts)
spatial_models_caughts[[13]] <- mlogit(choice ~ 0 | loc + Season + Year + DistFence, 
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                              iterlim=1, print.level=1,
                              data=spatial_caughts)
spatial_models_caughts[[14]] <- mlogit(choice ~ 0 | loc + Season + Year + DistShelter, 
                               rpar=c('ratCaught:(intercept)'='n',
                                      'mongooseCaught:(intercept)'='n'),
                              iterlim=1, print.level=1,
                              data=spatial_caughts)
spatial_models_caughts[[15]] <- mlogit(choice ~ 0 | loc + Season + Year + DistRoad + DistTrail + DistFence + DistShelter, 
                               rpar=c('ratCaught:(intercept)'='n',
                                      'mongooseCaught:(intercept)'='n'),
                              iterlim=1, print.level=1,
                              data=spatial_caughts)
spatial_models_caughts[[16]] <- mlogit(choice ~ 0 | loc + Season + Year + MedSlope + Elevation + 
                                 Burrows100 + DistRoad + DistTrail + DistFence + DistShelter, 
                               rpar=c('ratCaught:(intercept)'='n',
                                      'mongooseCaught:(intercept)'='n'),
                              iterlim=1, print.level=1,
                              data=spatial_caughts)
# # vegetation model
# spatial_models_caughts[[17]] <- mlogit(choice ~ 0 | loc + Season + Year + PctVeg, 
#                                rpar=c('ratCaught:(intercept)'='n',
#                                       'mongooseCaught:(intercept)'='n'),
#                               iterlim=1, print.level=1,
#                               data=spatial_caughts)
# spatial_models_caughts[[18]] <- mlogit(choice ~ 0 | loc + Season + Year + MajCover, 
#                                rpar=c('ratCaught:(intercept)'='n',
#                                       'mongooseCaught:(intercept)'='n'),
#                               iterlim=1, print.level=1,
#                               data=spatial_caughts)
# spatial_models_caughts[[19]] <- mlogit(choice ~ 0 | loc + Season + Year + MajClass,
#                                rpar=c('ratCaught:(intercept)'='n',
#                                       'mongooseCaught:(intercept)'='n'),
#                               iterlim=1, print.level=1,
#                               data=spatial_caughts)
# spatial_models_caughts[[20]] <- mlogit(choice ~ 0 | loc + Season + Year + PctVeg + MajCover + MajClass,
#                                rpar=c('ratCaught:(intercept)'='n',
#                                       'mongooseCaught:(intercept)'='n'),
#                                iterlim=1, print.level=1,
#                                data=spatial_caughts)
# spatial_models_caughts[[21]] <- mlogit(choice ~ 0 | loc + Season + Year + MedSlope + Elevation + Burrows100 + 
#                                  DistRoad + DistTrail + DistFence + DistShelter + MajCover,
#                                rpar=c('ratCaught:(intercept)'='n',
#                                       'mongooseCaught:(intercept)'='n'),
#                                iterlim=1, print.level=1,
#                                data=spatial_caughts)

#### now compare AIC and log likelihood between models
varsCols <- sapply(spatial_models_caughts, function(m) substr(as.character(formula(m)[3]), start = 5, stop = 1e6))
dfCols <- sapply(spatial_models_caughts, function(m) attr(logLik(m), "df"))
logLikCols <- sapply(spatial_models_caughts, function(m) attr(logLik(m), "null"))
aicCols <- sapply(spatial_models_caughts, function(m) AIC(m))
aicWcols <- sapply(spatial_models_caughts, function(m) Weights(AIC(m)))

# combine into one table and save output
bestSpatialModel_preds <- data.frame(variables = varsCols, AIC = aicCols, `Log Likelihood` = logLikCols, DF = dfCols)
write.csv(bestSpatialModel_preds, file = '~/WERC-SC/HALE/outputs/bestWLmodel_preds_eck12.5.csv',
          row.names = FALSE)


#_____________________________________________________________________________________________________________________
# EVENT TYPE DATA
spatial_models_events <- list() # create model list

spatial_events <- mlogit.data(exp_spatialData_events,
                               #  %>% filter(loc == "front") %>% filter(!(Trapline %in% c('KAU', 'KW', 'LAU', 'PUU', 'SS'))),
                               choice="choice",
                               alt.var ="x", 
                               shape="long", 
                               id.var = "Trapline",
                               chid.var="chid")
## baseline model
spatial_models_events[[1]] <- mlogit(choice ~ 0 | loc, 
                                    rpar=c('predatorEvent:(intercept)'='n',
                                            'otherEvent:(intercept)'='n'),
                                    reflevel = "noEvent",
                                      iterlim=1, print.level=1,
                                      data=spatial_events)
spatial_models_events[[2]] <- mlogit(choice ~ 0 | Season, 
                                     rpar=c('predatorEvent:(intercept)'='n',
                                            'otherEvent:(intercept)'='n'),
                                     reflevel = "noEvent",
                                      iterlim=1, print.level=1,
                                      data=spatial_events)
spatial_models_events[[3]] <- mlogit(choice ~ 0 | Year, 
                                     rpar=c('predatorEvent:(intercept)'='n',
                                            'otherEvent:(intercept)'='n'),
                                     reflevel = "noEvent",
                                      iterlim=1, print.level=1,
                                      data=spatial_events)
spatial_models_events[[4]] <- mlogit(choice ~ 0 | loc + Season + Year, 
                                     rpar=c('predatorEvent:(intercept)'='n',
                                            'otherEvent:(intercept)'='n'),
                                     reflevel = "noEvent",
                                      iterlim=1, print.level=1,
                                      data=spatial_events)
# loc + Season + Year ****
AIC(spatial_models_caughts[[1]]) # loc
AIC(spatial_models_caughts[[2]]) # Season
AIC(spatial_models_caughts[[3]]) # Year
AIC(spatial_models_caughts[[4]]) # loc + Season + Year


## slope and elevation model
spatial_models_events[[5]] <- mlogit(choice ~ 0 | loc + Season + Year + MedSlope + Elevation, 
                                     rpar=c('predatorEvent:(intercept)'='n',
                                            'otherEvent:(intercept)'='n'),
                                     reflevel = "noEvent",
                                      iterlim=1, print.level=1,
                                      data=spatial_events) 
spatial_models_events[[6]] <- mlogit(choice ~ 0 | loc + Season + Year + Elevation, 
                                     rpar=c('predatorEvent:(intercept)'='n',
                                            'otherEvent:(intercept)'='n'),
                                     reflevel = "noEvent",
                                      iterlim=1, print.level=1,
                                      data=spatial_events) 
spatial_models_events[[7]] <- mlogit(choice ~ 0 | loc + Season + Year + MedSlope, 
                                     rpar=c('predatorEvent:(intercept)'='n',
                                            'otherEvent:(intercept)'='n'),
                                     reflevel = "noEvent",
                                      iterlim=1, print.level=1,
                                      data=spatial_events) 
## burrow radius model
spatial_models_events[[8]] <- mlogit(choice ~ 0 | loc + Season + Year + Burrows10, 
                                     rpar=c('predatorEvent:(intercept)'='n',
                                            'otherEvent:(intercept)'='n'),
                                     reflevel = "noEvent",
                                      iterlim=1, print.level=1,
                                      data=spatial_events)
spatial_models_events[[9]] <- mlogit(choice ~ 0 | loc + Season + Year + Burrows50, 
                                     rpar=c('predatorEvent:(intercept)'='n',
                                            'otherEvent:(intercept)'='n'),
                                     reflevel = "noEvent",
                                      iterlim=1, print.level=1,
                                      data=spatial_events)
spatial_models_events[[10]] <- mlogit(choice ~ 0 | loc + Season + Year + Burrows100, 
                                      rpar=c('predatorEvent:(intercept)'='n',
                                             'otherEvent:(intercept)'='n'),
                                      reflevel = "noEvent",
                                       iterlim=1, print.level=1,
                                       data=spatial_events)
# distance to structures model
spatial_models_events[[11]] <- mlogit(choice ~ 0 | loc + Season + Year + DistRoad,
                                      rpar=c('predatorEvent:(intercept)'='n',
                                             'otherEvent:(intercept)'='n'),
                                      reflevel = "noEvent",
                                       iterlim=1, print.level=1,
                                       data=spatial_events)
spatial_models_events[[12]] <- mlogit(choice ~ 0 | loc + Season + Year + DistTrail, 
                                      rpar=c('predatorEvent:(intercept)'='n',
                                             'otherEvent:(intercept)'='n'),
                                      reflevel = "noEvent",
                                       iterlim=1, print.level=1,
                                       data=spatial_events)
spatial_models_events[[13]] <- mlogit(choice ~ 0 | loc + Season + Year + DistFence, 
                                      rpar=c('predatorEvent:(intercept)'='n',
                                             'otherEvent:(intercept)'='n'),
                                      reflevel = "noEvent",
                                       iterlim=1, print.level=1,
                                       data=spatial_events)
spatial_models_events[[14]] <- mlogit(choice ~ 0 | loc + Season + Year + DistShelter, 
                                      rpar=c('predatorEvent:(intercept)'='n',
                                             'otherEvent:(intercept)'='n'),
                                      reflevel = "noEvent",
                                       iterlim=1, print.level=1,
                                       data=spatial_events)
spatial_models_events[[15]] <- mlogit(choice ~ 0 | loc + Season + Year + DistRoad + DistTrail + DistFence + DistShelter, 
                                      rpar=c('predatorEvent:(intercept)'='n',
                                             'otherEvent:(intercept)'='n'),
                                      reflevel = "noEvent",
                                       iterlim=1, print.level=1,
                                       data=spatial_events)
spatial_models_events[[16]] <- mlogit(choice ~ 0 | loc + Season + Year + MedSlope + Elevation + 
                                         Burrows100 + DistRoad + DistTrail + DistFence + DistShelter, 
                                      rpar=c('predatorEvent:(intercept)'='n',
                                             'otherEvent:(intercept)'='n'),
                                      reflevel = "noEvent",
                                       iterlim=1, print.level=1,
                                       data=spatial_events)
# # vegetation model
# spatial_models_events[[17]] <- mlogit(choice ~ 0 | loc + Season + Year + PctVeg,
#                                       rpar=c('predatorEvent:(intercept)'='n',
#                                              'otherEvent:(intercept)'='n'),
#                                       reflevel = "noEvent",
#                                       iterlim=1, print.level=1,
#                                       data=spatial_events)
# spatial_models_events[[18]] <- mlogit(choice ~ 0 | loc + Season + Year + MajCover,
#                                       rpar=c('predatorEvent:(intercept)'='n',
#                                              'otherEvent:(intercept)'='n'),
#                                       reflevel = "noEvent",
#                                       iterlim=1, print.level=1,
#                                       data=spatial_events)
# spatial_models_events[[19]] <- mlogit(choice ~ 0 | loc + Season + Year + MajClass,
#                                       rpar=c('predatorEvent:(intercept)'='n',
#                                              'otherEvent:(intercept)'='n'),
#                                       reflevel = "noEvent",
#                                       iterlim=1, print.level=1,
#                                       data=spatial_events)
# spatial_models_events[[20]] <- mlogit(choice ~ 0 | loc + Season + Year + PctVeg + MajCover + MajClass,
#                                       rpar=c('predatorEvent:(intercept)'='n',
#                                              'otherEvent:(intercept)'='n'),
#                                       reflevel = "noEvent",
#                                        iterlim=1, print.level=1,
#                                        data=spatial_events)
# spatial_models_events[[21]] <- mlogit(choice ~ 0 | loc + Season + Year + MedSlope + Elevation + Burrows100 +
#                                  DistRoad + DistTrail + DistFence + DistShelter + MajCover,
#                                        rpar=c('predatorEvent:(intercept)'='n',
#                                               'otherEvent:(intercept)'='n'),
#                                        reflevel = "noEvent",
#                                        iterlim=1, print.level=1,
#                                        data=spatial_events)

#### now compare AIC and log likelihood between models
varsCols <- sapply(spatial_models_caughts, function(m) substr(as.character(formula(m)[3]), start = 5, stop = 1e6))
dfCols <- sapply(spatial_models_caughts, function(m) attr(logLik(m), "df"))
logLikCols <- sapply(spatial_models_caughts, function(m) attr(logLik(m), "null"))
aicCols <- sapply(spatial_models_caughts, function(m) AIC(m))
aicWcols <- sapply(spatial_models_caughts, function(m) Weights(AIC(m)))

# combine into one table and save output
bestSpatialModel_preds <- data.frame(variables = varsCols, AIC = aicCols, `Log Likelihood` = logLikCols, DF = dfCols)
write.csv(bestSpatialModel_preds, file = '~/WERC-SC/HALE/outputs/bestWLmodel_preds_eck12.5.csv',
          row.names = FALSE)
