## this script statistically analyzes the effect of year, season, weather, and lunar effects on 
## Event Type (predator, other none) and 
## Predator Event Type (cat, rat, mongoose)
## this analysis builds off of best models from Event Type & Predator Event Type analyses (model 23 and 45 in eck12)

library(stats)
library(data.table)
library(plyr)
library(dplyr)
library(ggplot2)
library(ez)
library(mlogit)
library(MuMIn)
library(stargazer)

setwd("~/WERC-SC/HALE")

read.csv('~/WERC-SC/HALE/predEventPUE_WL_11.5.csv',
         stringsAsFactors = FALSE) -> CPUEdata_WL

#### EDIT DATA: remove the mouse events, separate front and backcountry traps, & group predator events (for rerun of mlogit analysis)
data_rev_WL <- CPUEdata_WL %>% 
  filter(predEvent != 'mouseCaught') %>% 
  rename(Year = Year_) %>% 
  rename(Month = Month_) %>% 
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
      .default = "none"))
## all data grouped by 'event' (predator, other, no) variable.
data_events_WL <- data_rev_WL %>%
  group_by(MoonTime1wk, MoonIllum1wk, total3monRain, totalWeekRain, meanRelHum, meanSoilMois, meanSolRad,
           meanTmin, meanTmax,Trapline, Week, Year, Season, Month, NTraps, loc, eventType) %>%
  dplyr::summarise(NEvents=sum(NEvents)) %>%
  ungroup %>% 
  mutate(CPUE = NEvents/NTraps) %>% # this line optional
  as.data.frame()
# expanded_data_events_WL <- formatData(data_events_WL, 'eventType', subset=10000)

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
           YearCat = as.factor(Year),
           YearCts = as.numeric(Year))
  return(expanded_data)
}

#### CREATE LONG DATA TABLES
expanded_data_WL <- formatData(data_rev_WL, 'predEvent')
## with only predator data
data_Caughts_only_WL <- data_rev_WL %>% 
  filter(eventType == "predatorEvent")
expanded_data_Caughts_only_WL <- formatData(data_Caughts_only_WL, 'predEvent')


#### RUN mlogt MODELS
# ____________________________________________________________________________________________________________________
### PREDS ONLY ANALYSIS: compare 3 random effect options
## LUNAR
cpue_WL_models <- list() # model list
cpue_caughts_WL <- mlogit.data(expanded_data_Caughts_only_WL %>% 
                                 mutate(trapyr=paste0(Trapline,'-',YearCat),
                                        moon = MoonTime1wk * MoonIllum1wk) %>% 
                                 filter(!is.na(meanTmin)), 
                               choice="choice",
                               alt.var ="x", 
                               id.var = "trapyr",
                               shape="long", 
                               chid.var="chid")
cpue_WL_models[[1]] <- mlogit(choice ~ 0 | Season,
                            rpar=c('ratCaught:(intercept)'='n',
                                   'mongooseCaught:(intercept)'='n'),
                            R=50, halton=NA,
                            panel=TRUE,
                            iterlim=1, print.level=1,
                            data=cpue_caughts_WL)
cpue_WL_models[[2]] <- mlogit(choice ~ 0 | Season + moon,
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                              R=50, halton=NA,
                              panel=TRUE,
                              iterlim=1, print.level=1,
                              data=cpue_caughts_WL)
## WEATHER
# rain
cpue_WL_models[[3]] <- mlogit(choice ~ 0 | Season + total3monRain,
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                              R=50, halton=NA,
                              panel=TRUE,
                              iterlim=1, print.level=1,
                              data=cpue_caughts_WL) 

cpue_WL_models[[4]] <- mlogit(choice ~ 0 | Season + totalWeekRain,
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                              R=50, halton=NA,
                              panel=TRUE,
                              iterlim=1, print.level=1,
                              data=cpue_caughts_WL)
# temperature
cpue_WL_models[[5]] <- mlogit(choice ~ 0 | Season + meanTmax,
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                              R=50, halton=NA,
                              panel=TRUE,
                              iterlim=1, print.level=1,
                              data=cpue_caughts_WL)
cpue_WL_models[[6]] <- mlogit(choice ~ 0 | Season + meanTmin,
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                              R=50, halton=NA,
                              panel=TRUE,
                              iterlim=1, print.level=1,
                              data=cpue_caughts_WL)
## ALL CLIMATE
cpue_WL_models[[7]] <- mlogit(choice ~ 0 | Season + moon + totalWeekRain,
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                              R=50, halton=NA,
                              panel=TRUE,
                              iterlim=1, print.level=1,
                              data=cpue_caughts_WL)
cpue_WL_models[[8]] <- mlogit(choice ~ 0 | Season + moon + total3monRain,
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                              R=50, halton=NA,
                              panel=TRUE,
                              iterlim=1, print.level=1,
                              data=cpue_caughts_WL)
cpue_WL_models[[9]] <- mlogit(choice ~ 0 | Season + moon + meanTmin,
                               rpar=c('ratCaught:(intercept)'='n',
                                      'mongooseCaught:(intercept)'='n'),
                               R=50, halton=NA,
                               panel=TRUE,
                               iterlim=1, print.level=1,
                               data=cpue_caughts_WL)

cpue_WL_models[[10]] <- mlogit(choice ~ 0 | Season + moon + meanTmax,
                               rpar=c('ratCaught:(intercept)'='n',
                                      'mongooseCaught:(intercept)'='n'),
                               R=50, halton=NA,
                               panel=TRUE,
                               iterlim=1, print.level=1,
                               data=cpue_caughts_WL)


#### now compare AIC and log likelihood between models
varsColWL <- sapply(cpue_WL_models, function(m) substr(as.character(formula(m)[3]), start = 5, stop = 1e6))
dfColWL <- sapply(cpue_WL_models, function(m) attr(logLik(m), "df"))
logLikColWL <- sapply(cpue_WL_models, function(m) attr(logLik(m), "null"))
aicColWL <- sapply(cpue_WL_models, function(m) AIC(m))
minAic <- min(aicColWL)
deltAicColWL <- aicColWL - minAic
aicWcolWL <- Weights(aicColWL)

# combine into one table and save output
WLmodels_preds <- data.frame(variables = varsColWL, AIC = aicColWL, `Weighted AIC` = aicWcolWL,  `delt AIC` = deltAicColWL,
                                `Log Likelihood` = logLikColWL, DF = dfColWL)
write.csv(WLmodels_preds, file = '~/WERC-SC/HALE/outputs/WLmodels_preds_eck12.5.csv',
          row.names = FALSE)

### analyze results for best fit model: model 5 (Season + meanTmax)
myfitted_WL_preds <- fitted(cpue_WL_models[[5]], outcome=FALSE)
head(myfitted_WL_preds)
# select data and thin it down to one row per chid
fitted_cpue_WL_preds <- cpue_caughts_WL %>%
  filter(!is.na(meanTmax)) %>% 
  select(chid, Trapline, Year, Season, Week, meanTmax) %>%
  unique()
dim(myfitted_WL_preds)
dim(fitted_cpue_WL_preds)
# then `cbind` the data in `fitted_cpue_WL` with the fitted values in `myfitted`
fitted_cpue_WL_preds <- cbind(fitted_cpue_WL_preds, myfitted_WL_preds) %>%
  select(-chid) %>% # thin the fitted values further (i.e. remove replicates, keep unique combos of variables (Trapline, Year, & Season?)
  unique()
write.csv(fitted_cpue_WL_preds, file = '~/WERC-SC/HALE/outputs/fitted_cpue_WL_preds_eck12.5.csv',
          row.names = FALSE)


# ____________________________________________________________________________________________________________________
### EVENTS (predator, other, none) ANALYSIS
#### BOOTSTRAP SUBSETS OF EVENTTYPE DATA FOR  CLIMATE ANALYSIS
nb = 1000 # number of bootstraps
s = 50000 # size of subset
subset_modelsWL_aic <- matrix(NA, ncol=10, nrow=nb) # ncol = number of models
subset_modelsWL_aicW <- matrix(NA, ncol=10, nrow=nb)
subset_modelsWL_logLik <- matrix(NA, ncol=10, nrow=nb)

for (k in 1:nb) { 
  set.seed(k)
  ### create nb iterations of formatted (expanded) data
  WLData <- formatData(data_events_WL, 'eventType', s)
  ### run all 7 models through the bootstrap
  WLmodels <- list()
  WL_data = mlogit.data(WLData %>% mutate(trapyr=paste0(Trapline,'-',YearCat),
                                        moon = MoonTime1wk * MoonIllum1wk) %>% 
                                 filter(!is.na(meanTmax)),
                               choice="choice", alt.var ="x", shape="long",
                               id.var = "trapyr",
                               chid.var="chid")
  ## LUNAR
  WLmodels[[1]] <- mlogit(choice ~ 0 | Season,
                        rpar=c('predatorEvent:(intercept)'='n', 'otherEvent:(intercept)'='n'), 
                        R=50, halton=NA, panel=TRUE,
                        reflevel = "noEvent", iterlim=1,
                        data=WL_data)
  WLmodels[[2]] <- mlogit(choice ~ 0 | Season + moon,
                        rpar=c('predatorEvent:(intercept)'='n', 'otherEvent:(intercept)'='n'),
                        R=50, halton=NA, panel=TRUE,
                        reflevel = "noEvent", iterlim=1,
                        data=WL_data)
  ## WEATHER
  # rain
  WLmodels[[3]] <- mlogit(choice ~ 0 | Season + total3monRain,
                                 rpar=c('predatorEvent:(intercept)'='n', 'otherEvent:(intercept)'='n'), 
                                 R=50, halton=NA, panel=TRUE,
                                 reflevel = "noEvent", iterlim=1,
                                 data=WL_data) 
  WLmodels[[4]] <- mlogit(choice ~ 0 | Season  + totalWeekRain,
                                 rpar=c('predatorEvent:(intercept)'='n', 'otherEvent:(intercept)'='n'), 
                                 R=50, halton=NA, panel=TRUE,
                                 reflevel = "noEvent", iterlim=1,
                                 data=WL_data)
  # temperature
  WLmodels[[5]] <- mlogit(choice ~ 0 | Season + meanTmax,
                                 rpar=c('predatorEvent:(intercept)'='n', 'otherEvent:(intercept)'='n'), 
                                 R=50, halton=NA, panel=TRUE,
                                 reflevel = "noEvent", iterlim=1,
                                 data=WL_data)
  WLmodels[[6]] <- mlogit(choice ~ 0 | Season + meanTmin,
                                 rpar=c('predatorEvent:(intercept)'='n', 'otherEvent:(intercept)'='n'), 
                                 R=50, halton=NA, panel=TRUE,
                                 reflevel = "noEvent", iterlim=1,
                                 data=WL_data)
  # all weather
  WLmodels[[7]] <- mlogit(choice ~ 0 | Season + moon + totalWeekRain,
                                 rpar=c('predatorEvent:(intercept)'='n', 'otherEvent:(intercept)'='n'), 
                                 R=50, halton=NA, panel=TRUE,
                                 reflevel = "noEvent", iterlim=1,
                                 data=WL_data)
  WLmodels[[8]] <- mlogit(choice ~ 0 | Season + moon + total3monRain,
                                 rpar=c('predatorEvent:(intercept)'='n', 'otherEvent:(intercept)'='n'), 
                                 R=50, halton=NA, panel=TRUE,
                                 reflevel = "noEvent", iterlim=1,
                                 data=WL_data) 
  WLmodels[[9]] <- mlogit(choice ~ 0 | Season + moon + meanTmin,
                                 rpar=c('predatorEvent:(intercept)'='n', 'otherEvent:(intercept)'='n'), 
                                 R=50, halton=NA, panel=TRUE,
                                 reflevel = "noEvent", iterlim=1,
                                 data=WL_data)
  WLmodels[[10]] <- mlogit(choice ~ 0 | Season + moon + meanTmax,
                                 rpar=c('predatorEvent:(intercept)'='n', 'otherEvent:(intercept)'='n'), 
                                 R=50, halton=NA, panel=TRUE,
                                 reflevel = "noEvent", iterlim=1,
                                 data=WL_data)
  ### summarize and rank AICs for each itteration of bootstrap
  subset_modelsWL_aic[k, ] <- ldply(WLmodels, .fun=AIC)$V1
  subset_modelsWL_aicW[k, ] <- Weights(ldply(WLmodels, .fun=AIC)$V1)
  subset_modelsWL_logLik[k, ] <- ldply(WLmodels, .fun=logLik)$V1
}

### examine AIC and AIC weights
bestWLmodel_events <- table(apply(subset_modelsWL_aic, MARGIN=1, FUN=which.min),
                           apply(subset_modelsWL_aicW, MARGIN=1, FUN=which.max))
bestWLmodel_events
write.csv(bestWLmodel_events, file = '~/WERC-SC/HALE/outputs/bestWLmodel_events_eck12.5.csv',
          row.names = FALSE)

## save model statistics
modelsWL_aic <- data.frame(subset_modelsWL_aic)
modelsWL_aicW <- data.frame(subset_modelsWL_aicW)
modelsWL_logLik <- data.frame(subset_modelsWL_logLik)
# create table columns
varsColWL_E <- sapply(WLmodels, function(m) substr(as.character(formula(m)[3]), start = 5, stop = 1e6))
dfColWL_E <- sapply(WLmodels, function(m) attr(logLik(m), "df"))
logLikColWL_E <- sapply(modelsWL_logLik, FUN = mean)
aicColWL_E <- sapply(modelsWL_aic, FUN = mean)
minAicWL_E <- min(aicColWL_E)
deltAicColWL_E <- aicColWL_E - minAicWL_E
aicWcolWL_E <- sapply(modelsWL_aicW, FUN = mean)
# combine into one table and save output
WLmodels_events <- data.frame(variables = varsColWL_E, AIC = aicColWL_E, `delt AIC` = deltAicColWL_E, `Weighted AIC` = aicWcolWL_E, 
                               `Log Likelihood` = logLikColWL_E, DF = dfColWL_E)
write.csv(WLmodels_events, file = '~/WERC-SC/HALE/outputs/WLmodels_events_eck12.5.csv',
          row.names = FALSE)


### analyze results for best fit model: model ?? (Season + )
expanded_data_WL_events <- formatData(data_rev_WL, 'eventType', subset = 55000)
WL_data_events = mlogit.data(expanded_data_WL_events %>% 
                              mutate(trapyr=paste0(Trapline,'-',YearCat)) %>%
                              filter(!is.na(meanTmax)),
                            choice="choice", alt.var ="x", shape="long",
                            id.var = "trapyr",
                            chid.var="chid")
model_WL_events <- mlogit(choice ~ 0 | Season + ,
                         rpar=c('predatorEvent:(intercept)'='n', 'otherEvent:(intercept)'='n'), 
                         R=50, halton=NA, panel=TRUE,
                         reflevel = "noEvent", iterlim=1,
                         data=WL_data_events)
myfitted_WL_events <- fitted(model_WL_events, outcome=FALSE)
head(myfitted_WL_events)
# select data and thin it down to one row per chid
fitted_cpue_WL_events <- WL_data_events %>%
  filter(!is.na(meanTmax)) %>% 
  select(chid, Trapline, Year, Season, Week, ) %>%
  unique()
dim(myfitted_WL_events)
dim(fitted_cpue_WL_events)
# then `cbind` the data in `fitted_cpue_WL` with the fitted values in `myfitted`
fitted_cpue_WL_events <- cbind(fitted_cpue_WL_events, myfitted_WL_events) %>%
  select(-chid) %>% # thin the fitted values further (i.e. remove replicates, keep unique combos of variables (Trapline, Year, & Season?)
  unique()
write.csv(fitted_cpue_WL_events, file = '~/WERC-SC/HALE/outputs/fitted_cpue_WL_events_eck12.5.csv',
          row.names = FALSE)

