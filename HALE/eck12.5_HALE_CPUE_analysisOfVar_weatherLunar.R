## this script statistically analyzes the effect of year, season, weather, and lunar effects on 
## Event Type (predator, other none) and 
## Predator Event Type (cat, rat, mongoose)
## this analysis builds off of best models from both analyses (model 23 and 45)


library(stats)
library(data.table)
library(plyr)
library(dplyr)
library(ggplot2)
library(ez)
library(mlogit)
library(mosaic)


read.csv('~/WERC-SC/HALE/predEventPUE_WL_11.5.csv',
         stringsAsFactors = FALSE) -> CPUEdata_WL
#### EDIT DATA: remove the mouse events, separate front and backcountry traps, & group predator events (for rerun of mlogit analysis)
data_rev_WL <- CPUEdata_WL %>% 
  filter(predEvent != 'mouseCaught') %>% 
  mutate(eventType = mosaic::derivedFactor(
    "predatorEvent" = predEvent %in% c('ratCaught', 'catCaught', 'mongooseCaught'),
    "otherEvent" = predEvent %in% c('birdOtherCaught', 'trapTriggered', 'baitLost'),
    "noEvent" = predEvent =="none",
    .default = "noEvent"),
    loc = mosaic::derivedFactor(
      front = Trapline %in% c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'),
      back = Trapline %in% c('HAL', 'KAP', 'KAU', 'KW', 'LAI', 'LAU', 'NAM', 'PAL', 'PUU', 'SS', 'WAI'),
      .default = "back"),
    eventCnoC = mosaic::derivedFactor(
      "event" = predEvent %in% c('ratCaught', 'catCaught', 'mongooseCaught', 'birdOtherCaught', 'trapTriggered', 'baitLost'),
      "none" = predEvent == "none", 
      .default = "none")
  ) 

#### RESTRUCTURE DATA FUNCTION
formatData <- function(data, var, subset = NA){
  if(!(var %in% colnames(data)))
    stop(sprintf('var [%s] not found in data columns', var))
  data$var <- getElement(data, var)
  # Reshape data so that there is one row for every option, for every choice situation. 
  # Here are the possible outcomes every time the trap is set:
  events <- unique(data$var) 
  # And here are the number of choice situations:
  nEvents <- sum(data$NEvents)
  
  # Replicate the rows according to number of events:
  data2 <- data[rep(row.names(data), data$NEvents),]
  data2 <- data2 %>%
    mutate(chid = row.names(data2))
  
  if (!is.na(subset)){
    data2 <- data2[sample(1:nrow(data2), subset),]
  }
  
  # Expand each choice situation so that each alternative is on its own row.  
  # Do this with the merge function.  The alternative names will be stored in column `x`.
  expanded_data <- merge(events, data2)
  expanded_data <- expanded_data %>% 
    mutate(choice = ifelse(x==var, TRUE, FALSE), 
           YearCat = as.factor(Year_),
           YearCts = as.numeric(Year_))
  return(expanded_data)
}

#### CREATE LONG DATA TABLES
expanded_data_WL <- formatData(data_rev_WL, 'predEvent')
## with only predator data
data.Caughts_only_WL <- data_rev_WL %>% 
  filter(eventType == "predatorEvent")
expanded_data.Caughts_only_WL <- formatData(data.Caughts_only_WL, 'predEvent')
## all data grouped by 'event' (predator, other, no) variable.
data_events_WL <- data_rev_WL %>%
  group_by(Trapline, Week, Year_, Season, Month_, NTraps, loc, eventType) %>%
  dplyr::summarise(NEvents=sum(NEvents)) %>%
  mutate(CPUE = NEvents/NTraps) %>% # this line optional
  as.data.frame()
expanded_data.events_WL <- formatData(data_events_WL, 'eventType')


#### RUN mlogt MODELS
## create model list
cpue_WL.models <- list()
### PREDS ONLY ANALYSIS: compare 3 random effect options
cpue.caughts.WL <- mlogit.data(expanded_data.Caughts_only_WL, # %>% filter(loc == "front"), 
                                  choice="choice",
                                  alt.var ="x", 
                                  id.var = "Trapline",
                                  shape="long", 
                                  chid.var="chid")
## LUNAR
cpue_WL.models[[1]] <- mlogit(choice ~ 0 | Season,
                            rpar=c('ratCaught:(intercept)'='n',
                                   'mongooseCaught:(intercept)'='n'),
                            R=50, halton=NA,
                            panel=TRUE,
                            iterlim=1, print.level=1,
                            data=cpue.caughts.WL)
cpue_WL.models[[2]] <- mlogit(choice ~ 0 | Season + MoonTime1wk + MoonIllum1wk,
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                              R=50, halton=NA,
                              panel=TRUE,
                              iterlim=1, print.level=1,
                              data=cpue.caughts.WL)
cpue_WL.models[[3]] <- mlogit(choice ~ 0 | Season + MoonTime1wk,
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                              R=50, halton=NA,
                              panel=TRUE,
                              iterlim=1, print.level=1,
                              data=cpue.caughts.WL)
cpue_WL.models[[4]] <- mlogit(choice ~ 0 | Season + MoonIllum1wk,
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                              R=50, halton=NA,
                              panel=TRUE,
                              iterlim=1, print.level=1,
                              data=cpue.caughts.WL)
# now compare AIC models between models
AIC(cpue_WL.models[[1]]) # Season 
logLik(cpue_WL.models[[1]])
AIC(cpue_WL.models[[2]]) # Season + MoonTime1wk + MoonIllum1wk
logLik(cpue_WL.models[[2]])
AIC(cpue_WL.models[[3]]) # Season + MoonTime1wk 
logLik(cpue_WL.models[[3]])
AIC(cpue_WL.models[[4]]) # Season + MoonIllum1wk
logLik(cpue_WL.models[[4]])


## WEATHER
# rain
cpue_WL.models[[5]] <- mlogit(choice ~ 0 | Season + total3monRain + totalWeekRain,
                            rpar=c('ratCaught:(intercept)'='n',
                                   'mongooseCaught:(intercept)'='n'),
                            R=50, halton=NA,
                            panel=TRUE,
                            iterlim=1, print.level=1,
                            data=cpue.caughts.WL)
cpue_WL.models[[6]] <- mlogit(choice ~ 0 | Season + total3monRain,
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                              R=50, halton=NA,
                              panel=TRUE,
                              iterlim=1, print.level=1,
                              data=cpue.caughts.WL) 

cpue_WL.models[[7]] <- mlogit(choice ~ 0 | Season + totalWeekRain,
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                              R=50, halton=NA,
                              panel=TRUE,
                              iterlim=1, print.level=1,
                              data=cpue.caughts.WL)
# now compare AIC models between models
AIC(cpue_WL.models[[5]]) # Season + total3monRain + totalWeekRain
logLik(cpue_WL.models[[5]])
AIC(cpue_WL.models[[6]]) # Season + total3monRain ***
logLik(cpue_WL.models[[6]])
AIC(cpue_WL.models[[7]]) # Season + totalWeekRain 
logLik(cpue_WL.models[[7]])
AIC(cpue_WL.models[[1]]) # Season 
logLik(cpue_WL.models[[1]])

# all weather
cpue_WL.models[[8]] <- mlogit(choice ~ 0 | Season + meanRelHum + meanSoilMois + meanSolRad + meanTmin + meanTmax,
                            rpar=c('ratCaught:(intercept)'='n',
                                   'mongooseCaught:(intercept)'='n'),
                            R=50, halton=NA,
                            panel=TRUE,
                            iterlim=1, print.level=1,
                            data=cpue.caughts.WL)
cpue_WL.models[[9]] <- mlogit(choice ~ 0 | Season + meanTmin + meanTmax,
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                              R=50, halton=NA,
                              panel=TRUE,
                              iterlim=1, print.level=1,
                              data=cpue.caughts.WL)
cpue_WL.models[[10]] <- mlogit(choice ~ 0 | Season + meanTmax,
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                              R=50, halton=NA,
                              panel=TRUE,
                              iterlim=1, print.level=1,
                              data=cpue.caughts.WL)
cpue_WL.models[[11]] <- mlogit(choice ~ 0 | Season + meanTmin,
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                              R=50, halton=NA,
                              panel=TRUE,
                              iterlim=1, print.level=1,
                              data=cpue.caughts.WL)
cpue_WL.models[[12]] <- mlogit(choice ~ 0 | Season + totalWeekRain + meanTmin + meanTmin,
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                              R=50, halton=NA,
                              panel=TRUE,
                              iterlim=1, print.level=1,
                              data=cpue.caughts.WL)
cpue_WL.models[[13]] <- mlogit(choice ~ 0 | Season + totalWeekRain + meanRelHum + meanSoilMois + meanSolRad + meanTmin + meanTmax,
                               rpar=c('ratCaught:(intercept)'='n',
                                      'mongooseCaught:(intercept)'='n'),
                               R=50, halton=NA,
                               panel=TRUE,
                               iterlim=1, print.level=1,
                               data=cpue.caughts.WL)
# now compare AIC models between models
AIC(cpue_WL.models[[8]]) # Season + meanRelHum + meanSoilMois + meanSolRad + meanTmin + meanTmax
# logLik(cpue_WL.models[[8]])
AIC(cpue_WL.models[[9]]) # Season + meanTmin + meanTmax
# logLik(cpue_WL.models[[9]])
AIC(cpue_WL.models[[10]]) # Season + meanTmax **
# logLik(cpue_WL.models[[10]])
AIC(cpue_WL.models[[11]]) # Season + meanTmin
# logLik(cpue_WL.models[[11]])
AIC(cpue_WL.models[[12]]) # Season + totalWeekRain + meanTmin + meanTmin
# logLik(cpue_WL.models[[12]])
AIC(cpue_WL.models[[13]]) # Season + totalWeekRain + meanRelHum + meanSoilMois + meanSolRad + meanTmin + meanTmax
# logLik(cpue_WL.models[[13]])
AIC(cpue_WL.models[[1]]) # Season 
# logLik(cpue_WL.models[[1]])


cpue_WL.models[[14]] <- mlogit(choice ~ 0 | Season + meanRelHum,
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                              R=50, halton=NA,
                              panel=TRUE,
                              iterlim=1, print.level=1,
                              data=cpue.caughts.WL)
cpue_WL.models[[15]] <- mlogit(choice ~ 0 | Season + meanSoilMois,
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                              R=50, halton=NA,
                              panel=TRUE,
                              iterlim=1, print.level=1,
                              data=cpue.caughts.WL)

cpue_WL.models[[16]] <- mlogit(choice ~ 0 | Season + meanSolRad,
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                              R=50, halton=NA,
                              panel=TRUE,
                              iterlim=1, print.level=1,
                              data=cpue.caughts.WL)

## LUNAR AND WEATHER
cpue_WL.models[[12]] <- mlogit(choice ~ 0 | Season + MoonTime1wk + MoonIllum1wk + totalWeekRain + totalWeekRain + 
                                meanRelHum + meanSoilMois + meanSolRad + meanTmin + meanTmin,
                            iterlim=1, print.level=1,
                            data=cpue.caughts.WL) 
cpue_WL.models[[13]] <- mlogit(choice ~ 0 | Season + MoonIllum1wk + totalWeekRain + meanTmin + meanTmin,
                               iterlim=1, print.level=1,
                               data=cpue.caughts.WL) 

# now compare AIC models between models
AIC(cpue_WL.models[[12]])
AIC(cpue_WL.models[[13]]) # model 13 = 1722.412 = lowest AIC of 12 - 13


### EVENTS ANALYSIS
cpue.events.WL <- mlogit.data(expanded_data.events_WL %>% 
                                mutate(trapyr=paste0(Trapline,'-',YearCat)),
                               choice="choice",
                               alt.var ="x", 
                               id.var = "trapyr",
                               shape="long", 
                               chid.var="chid")
## LUNAR
cpue_WL.models[[]] <- mlogit(choice ~ 0 | Season,
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                              R=50, halton=NA,
                              panel=TRUE,
                              iterlim=1, print.level=1,
                              data=cpue.caughts.WL)
cpue_WL.models[[]] <- mlogit(choice ~ 0 | Season + MoonTime1wk + MoonIllum1wk,
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                              R=50, halton=NA,
                              panel=TRUE,
                              iterlim=1, print.level=1,
                              data=cpue.caughts.WL)
cpue_WL.models[[]] <- mlogit(choice ~ 0 | Season + MoonTime1wk,
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                              R=50, halton=NA,
                              panel=TRUE,
                              iterlim=1, print.level=1,
                              data=cpue.caughts.WL)
cpue_WL.models[[]] <- mlogit(choice ~ 0 | Season + MoonIllum1wk,
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                              R=50, halton=NA,
                              panel=TRUE,
                              iterlim=1, print.level=1,
                              data=cpue.caughts.WL)
# now compare AIC models between models
AIC(cpue_WL.models[[]]) # Season 
logLik(cpue_WL.models[[]])
AIC(cpue_WL.models[[]]) # Season + MoonTime1wk + MoonIllum1wk
logLik(cpue_WL.models[[]])
AIC(cpue_WL.models[[]]) # Season + MoonTime1wk 
logLik(cpue_WL.models[[]])
AIC(cpue_WL.models[[]]) # Season + MoonIllum1wk
logLik(cpue_WL.models[[]])


## WEATHER
# rain
cpue_WL.models[[]] <- mlogit(choice ~ 0 | Season + total3monRain + totalWeekRain,
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                              R=50, halton=NA,
                              panel=TRUE,
                              iterlim=1, print.level=1,
                              data=cpue.caughts.WL)
cpue_WL.models[[]] <- mlogit(choice ~ 0 | Season + total3monRain,
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                              R=50, halton=NA,
                              panel=TRUE,
                              iterlim=1, print.level=1,
                              data=cpue.caughts.WL) 

cpue_WL.models[[]] <- mlogit(choice ~ 0 | Season + totalWeekRain,
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                              R=50, halton=NA,
                              panel=TRUE,
                              iterlim=1, print.level=1,
                              data=cpue.caughts.WL)
# now compare AIC models between models
AIC(cpue_WL.models[[]])
AIC(cpue_WL.models[[]])
AIC(cpue_WL.models[[]]) # model 6 = 1585.373 = lowest AIC of 4 - 6 

# all weather
cpue_WL.models[[]] <- mlogit(choice ~ 0 | Season + total3monRain + totalWeekRain + meanRelHum + meanSoilMois + 
                                meanSolRad + meanTmin + meanTmax,
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                              R=50, halton=NA,
                              panel=TRUE,
                              iterlim=1, print.level=1,
                              data=cpue.caughts.WL)
cpue_WL.models[[]] <- mlogit(choice ~ 0 | Season + totalWeekRain + meanRelHum + meanSoilMois + 
                                meanSolRad + meanTmin + meanTmax,
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                              R=50, halton=NA,
                              panel=TRUE,
                              iterlim=1, print.level=1,
                              data=cpue.caughts.WL)
cpue_WL.models[[]] <- mlogit(choice ~ 0 | Season + totalWeekRain + meanRelHum + meanSoilMois + meanTmin + meanTmax,
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                              R=50, halton=NA,
                              panel=TRUE,
                              iterlim=1, print.level=1,
                              data=cpue.caughts.WL)
cpue_WL.models[[]] <- mlogit(choice ~ 0 | Season + totalWeekRain + meanSoilMois + meanTmin + meanTmax,
                               rpar=c('ratCaught:(intercept)'='n',
                                      'mongooseCaught:(intercept)'='n'),
                               R=50, halton=NA,
                               panel=TRUE,
                               iterlim=1, print.level=1,
                               data=cpue.caughts.WL)
cpue_WL.models[[]] <- mlogit(choice ~ 0 | Season + totalWeekRain + meanTmin + meanTmin,
                               rpar=c('ratCaught:(intercept)'='n',
                                      'mongooseCaught:(intercept)'='n'),
                               R=50, halton=NA,
                               panel=TRUE,
                               iterlim=1, print.level=1,
                               data=cpue.caughts.WL)
AIC(cpue_WL.models[[]]) 
AIC(cpue_WL.models[[]]) 
AIC(cpue_WL.models[[]]) 
AIC(cpue_WL.models[[]]) # model 10 = 1577.997 = lowest AIC of 7 - 11
AIC(cpue_WL.models[[]]) 

## LUNAR AND WEATHER
cpue_WL.models[[]] <- mlogit(choice ~ 0 | Season + MoonTime1wk + MoonIllum1wk + totalWeekRain + totalWeekRain + 
                                 meanRelHum + meanSoilMois + meanSolRad + meanTmin + meanTmin,
                               iterlim=1, print.level=1,
                               data=cpue.caughts.WL) 
cpue_WL.models[[]] <- mlogit(choice ~ 0 | Season + MoonIllum1wk + totalWeekRain + meanTmin + meanTmin,
                               iterlim=1, print.level=1,
                               data=cpue.caughts.WL) 

# now compare AIC models between models
AIC(cpue_WL.models[[]])
AIC(cpue_WL.models[[]]) # model 13 = 1722.412 = lowest AIC of 12 - 13


### analyze results for model 10 (the best fit for the "caughts_only" data)
## get fitted frequencies of each event type on unique combos of Trapline, Year, & Season
myfitted_WL <- fitted(cpue_WL.models[[10]], outcome=FALSE)
# head(myfitted)
# dim(myfitted)
# dim(expanded_data.Caughts_only)

# ## select year, season, and trapline data for the fitted values
# # Copy data and thin it down to one row per chid
# fitted_cpue_WL <- expanded_data.Caughts_only_WL %>%
#   select(chid, Trapline, Year_, Season, Week) %>%
#   unique()
# # then `cbind` the data in `fitted_cpue_WL` with the fitted values in `myfitted`
# fitted_cpue_WL <- cbind(fitted_cpue_WL, myfitted_WL) %>%
#   # thin the fitted values further (i.e. remove replicates and keep the unique combos of Trapline, Year, & Season)
#   select(-chid) %>%
#   unique()


