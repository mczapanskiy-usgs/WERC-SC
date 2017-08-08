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

read.csv('~/WERC-SC/HALE/TrapsGrid20170626.csv', # catch data processed by Ben (elev, slope, prox to roads/trails/fences/structures, veg, etc.) & Jon (grid cells)
         stringsAsFactors = FALSE) -> spatialData

# EDIT DATA: remove the mouse events, separate front and backcountry traps, & group predator events (for rerun of mlogit analysis)
spatialData_rev <- spatialData %>% 
  filter(predEvent != 'mouseCaught') %>%
  select(6, 10:30) %>% # remove first 5 columns that are unneccessary
  mutate(Date = as.Date(Date_, "%m/%d/%Y"),
         Year = year(Date),
         trap=paste0(Trapline,TrapNum),
         majCoverType = mosaic::derivedFactor(
           "Barren" = grepl("Barren", MajCover, ignore.case = TRUE),
           "Developed" = grepl("Developed", MajCover, ignore.case = TRUE),
           "HerbCover" = grepl("Herb Cover", MajCover, ignore.case = TRUE),
           "ShrubCover" = grepl("Shrub Cover", MajCover, ignore.case = TRUE),
           "TreeCover" = grepl("Tree Cover", MajCover, ignore.case = TRUE),
           .default = "Barren"),
         majCoverPct = mosaic::derivedFactor(
           "LowIntensity" = grepl("Low Intensity", MajCover, ignore.case = TRUE),
           "MedIntensity" = grepl("Medium Intensity", MajCover, ignore.case = TRUE),
           "HighIntensity" = grepl("High Intensity", MajCover, ignore.case = TRUE),
           "10-19%" = grepl(">= 10 and < 20%", MajCover, ignore.case = TRUE),
           "20-29%" = grepl(">= 20 and < 30%", MajCover, ignore.case = TRUE),
           "30-39%" = grepl(">= 30 and < 40%", MajCover, ignore.case = TRUE),
           "40-49%" = grepl(">= 40 and < 50%", MajCover, ignore.case = TRUE),
           "50-59%" = grepl(">= 50 and < 60%", MajCover, ignore.case = TRUE),
           "60-69%" = grepl(">= 60 and < 70%", MajCover, ignore.case = TRUE),
           "70-79%" = grepl(">= 70 and < 80%", MajCover, ignore.case = TRUE),
           "80-89%" = grepl(">= 80 and < 90%", MajCover, ignore.case = TRUE),
           "90-100%" = grepl(">= 90 and <= 100%", MajCover, ignore.case = TRUE),
           .default = "0"),
         majClassType = mosaic::derivedFactor(
           "Barren" = grepl("Barren", MajClass, ignore.case = TRUE),
           "Developed" = grepl("Developed", MajClass, ignore.case = TRUE),
           "Introduced" = grepl("Hawai'i Introduced", MajClass, ignore.case = TRUE),
           "Native" = grepl("Hawai'i Montane|Hawai'i Lowland|Hawai'i Subalpine", MajClass, ignore.case = TRUE),
           # "Native" = grepl("Hawai'i Lowland", MajClass, ignore.case = TRUE),
           # "Native" = grepl("Hawai'i Subalpine", MajClass, ignore.case = TRUE),
           "Plantation" = grepl("Hawai'i Managed", MajClass, ignore.case = TRUE),
           .default = "Barren"),
         eventType = mosaic::derivedFactor(
           "predatorEvent" = predEvent %in% c('ratCaught', 'catCaught', 'mongooseCaught'),
           "otherEvent" = predEvent %in% c('birdOtherCaught', 'trapTriggered', 'baitLost'),
           "noEvent" = predEvent =="none",
            .default = "noEvent"),
         loc = mosaic::derivedFactor(
           front = Trapline %in% c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'),
           back = Trapline %in% c('HAL', 'KAP', 'KAU', 'KW', 'LAI', 'LAU', 'NAM', 'PAL', 'PUU', 'SS', 'WAI'),
            .default = "back"))

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
    mutate(choice = ifelse(x==var, TRUE, FALSE),
           YearCat = as.factor(Year),
           YearCts = as.numeric(Year))
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

spatial_caughts <- mlogit.data(exp_spatialData_caughts %>% 
                                 mutate(trapyr=paste0(Trapline,'-',YearCat)),
                    choice="choice",
                    alt.var ="x", 
                    shape="long", 
                    id.var = "trayr",
                    chid.var="chid")
## baseline model
spatial_models_caughts[[1]] <- mlogit(choice ~ 0 | Season, 
                                      rpar=c('ratCaught:(intercept)'='n',
                                             'mongooseCaught:(intercept)'='n'),
                                      iterlim=1, print.level=1,
                                      data=spatial_caughts)
## slope and elevation model
spatial_models_caughts[[2]] <- mlogit(choice ~ 0 | Season + Elevation, 
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                              iterlim=1, print.level=1,
                              data=spatial_caughts) 
spatial_models_caughts[[3]] <- mlogit(choice ~ 0 | Season + MedSlope, 
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                              iterlim=1, print.level=1,
                              data=spatial_caughts) 
## burrow radius model
spatial_models_caughts[[4]] <- mlogit(choice ~ 0 | Season + Burrows10, 
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                              iterlim=1, print.level=1,
                              data=spatial_caughts)
spatial_models_caughts[[5]] <- mlogit(choice ~ 0 | Season + Burrows50, 
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                              iterlim=1, print.level=1,
                              data=spatial_caughts)
spatial_models_caughts[[6]] <- mlogit(choice ~ 0 | Season + Burrows100, 
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                              iterlim=1, print.level=1,
                              data=spatial_caughts)

# vegetation model
spatial_models_caughts[[7]] <- mlogit(choice ~ 0 | Season + PctVeg,
                               rpar=c('ratCaught:(intercept)'='n',
                                      'mongooseCaught:(intercept)'='n'),
                              iterlim=1, print.level=1,
                              data=spatial_caughts)
spatial_models_caughts[[8]] <- mlogit(choice ~ 0 | Season + majCoverType,
                               rpar=c('ratCaught:(intercept)'='n',
                                      'mongooseCaught:(intercept)'='n'),
                              iterlim=1, print.level=1,
                              data=spatial_caughts)
spatial_models_caughts[[9]] <- mlogit(choice ~ 0 | Season + PctVeg + majCoverType,
                               rpar=c('ratCaught:(intercept)'='n',
                                      'mongooseCaught:(intercept)'='n'),
                               iterlim=1, print.level=1,
                               data=spatial_caughts)


#### now compare AIC and log likelihood between models
p_varsColS <- sapply(spatial_models_caughts, function(m) substr(as.character(formula(m)[3]), start = 5, stop = 1e6))
p_dfColS <- sapply(spatial_models_caughts, function(m) attr(logLik(m), "df"))
p_logLikColS <- sapply(spatial_models_caughts, function(m) attr(logLik(m), "null"))
p_aicColS <- sapply(spatial_models_caughts, function(m) AIC(m))
p_minAicS <- min(p_aicColS)
p_deltAicColS <- p_aicColS - p_minAicS
p_aicWcolS <- Weights(p_aicColS)

# combine into one table and save output
bestSpatialModel_preds <- data.frame(variables = p_varsColS, AIC = p_aicColS, `delt AIC` = p_deltAicColS, `Weighted AIC` = p_aicWcolS,
                                     `Log Likelihood` = p_logLikColS, DF = p_dfColS)
write.csv(bestSpatialModel_preds, file = '~/WERC-SC/HALE/outputs/bestSpatialModel_preds_eck18.csv',
          row.names = FALSE)

### analyze results for best fit model: model ?? (Season + )
myfitted_S_preds <- fitted(spatial_models_caughts[[]], outcome=FALSE)
head(myfitted_S_preds)
# select data and thin it down to one row per chid
fitted_cpue_S_preds <- spatial_caughts %>%
  filter(choice == "TRUE") %>% 
  select(Trapline, Year, Season, Week, predEvent, )
  # unique()
dim(myfitted_S_preds)
dim(fitted_cpue_S_preds)
# then `cbind` the data in `fitted_cpue_WL` with the fitted values in `myfitted`
fitted_cpue_S_preds <- cbind(fitted_cpue_S_preds, myfitted_S_preds) %>%
  unique()
write.csv(fitted_cpue_S_preds, file = '~/WERC-SC/HALE/outputs/fitted_cpue_S_preds_eck18.csv',
          row.names = FALSE)

#_____________________________________________________________________________________________________________________
# EVENT TYPE DATA
spatial_models_events <- list() # create model list

spatial_events <- mlogit.data(exp_spatialData_events %>% 
                                mutate(trapyr=paste0(Trapline,'-',YearCat)),
                               choice="choice",
                               alt.var ="x", 
                               shape="long", 
                               id.var = "trapyr",
                               chid.var="chid")
## baseline model
spatial_models_events[[1]] <- mlogit(choice ~ 0 | Season, 
                                     rpar=c('predatorEvent:(intercept)'='n',
                                            'otherEvent:(intercept)'='n'),
                                     reflevel = "noEvent",
                                      iterlim=1, print.level=1,
                                      data=spatial_events)
## slope and elevation model
spatial_models_events[[2]] <- mlogit(choice ~ 0 | loc + Season + Year + Elevation, 
                                     rpar=c('predatorEvent:(intercept)'='n',
                                            'otherEvent:(intercept)'='n'),
                                     reflevel = "noEvent",
                                      iterlim=1, print.level=1,
                                      data=spatial_events) 
spatial_models_events[[3]] <- mlogit(choice ~ 0 | loc + Season + Year + MedSlope, 
                                     rpar=c('predatorEvent:(intercept)'='n',
                                            'otherEvent:(intercept)'='n'),
                                     reflevel = "noEvent",
                                      iterlim=1, print.level=1,
                                      data=spatial_events) 
## burrow radius model
spatial_models_events[[4]] <- mlogit(choice ~ 0 | loc + Season + Year + Burrows10, 
                                     rpar=c('predatorEvent:(intercept)'='n',
                                            'otherEvent:(intercept)'='n'),
                                     reflevel = "noEvent",
                                      iterlim=1, print.level=1,
                                      data=spatial_events)
spatial_models_events[[5]] <- mlogit(choice ~ 0 | loc + Season + Year + Burrows50, 
                                     rpar=c('predatorEvent:(intercept)'='n',
                                            'otherEvent:(intercept)'='n'),
                                     reflevel = "noEvent",
                                      iterlim=1, print.level=1,
                                      data=spatial_events)
spatial_models_events[[6]] <- mlogit(choice ~ 0 | loc + Season + Year + Burrows100, 
                                      rpar=c('predatorEvent:(intercept)'='n',
                                             'otherEvent:(intercept)'='n'),
                                      reflevel = "noEvent",
                                       iterlim=1, print.level=1,
                                       data=spatial_events)
# vegetation model
spatial_models_events[[7]] <- mlogit(choice ~ 0 | loc + Season + Year + PctVeg,
                                      rpar=c('predatorEvent:(intercept)'='n',
                                             'otherEvent:(intercept)'='n'),
                                      reflevel = "noEvent",
                                      iterlim=1, print.level=1,
                                      data=spatial_events)
spatial_models_events[[8]] <- mlogit(choice ~ 0 | loc + Season + Year + majCoverType,
                                      rpar=c('predatorEvent:(intercept)'='n',
                                             'otherEvent:(intercept)'='n'),
                                      reflevel = "noEvent",
                                      iterlim=1, print.level=1,
                                      data=spatial_events)
spatial_models_events[[9]] <- mlogit(choice ~ 0 | loc + Season + Year + PctVeg + majCoverType,
                                      rpar=c('predatorEvent:(intercept)'='n',
                                             'otherEvent:(intercept)'='n'),
                                      reflevel = "noEvent",
                                       iterlim=1, print.level=1,
                                       data=spatial_events)

#### now compare AIC and log likelihood between models
e_varsColS <- sapply(spatial_models_events, function(m) substr(as.character(formula(m)[3]), start = 5, stop = 1e6))
e_dfColS <- sapply(spatial_models_events, function(m) attr(logLik(m), "df"))
e_logLikColS <- sapply(spatial_models_events, function(m) attr(logLik(m), "null"))
e_aicColS <- sapply(spatial_models_events, function(m) AIC(m))
e_minAicS <- min(e_aicColS)
e_deltAicColS <- e_aicColS - e_minAicS
e_aicWcolS <- Weights(e_aicColS)
# combine into one table and save output
bestSpatialModel_events <- data.frame(variables = e_varsColS, AIC = e_aicColS, `delt AIC` = e_deltAicColS, `weighted AIC` = e_aicWcolS, 
                                      `Log Likelihood` = e_logLikColS, DF = e_dfColS)
write.csv(bestSpatialModel_events, file = '~/WERC-SC/HALE/outputs/bestSpatialModel_events_eck18.csv',
          row.names = FALSE)

### analyze results for best fit model: model 16 (loc + Season + Year + + MedSlope + Elevation + Burrows100 + DistRoad + DistTrail + DistFence + DistShelter + majCoverType + majClassType)
myfitted_S_events <- fitted(spatial_models_events[[21]], outcome=FALSE)
head(myfitted_S_events)
# select data and thin it down to one row per chid
fitted_cpue_S_events <- spatial_events %>%
  filter(choice == "TRUE") %>% 
  select(Trapline, Year, Season, Week, predEvent, loc, MedSlope, Elevation, Burrows100,
           DistRoad, DistTrail, DistFence, DistShelter, PctVeg, majCoverType, majClassType)
dim(myfitted_S_events)
dim(fitted_cpue_S_events)
# then `cbind` the data in `fitted_cpue_WL` with the fitted values in `myfitted`
fitted_cpue_S_events <- cbind(fitted_cpue_S_events, myfitted_S_events) %>%
  unique()
write.csv(fitted_cpue_S_events, file = '~/WERC-SC/HALE/outputs/fitted_cpue_S_events_eck18.csv',
          row.names = FALSE)
