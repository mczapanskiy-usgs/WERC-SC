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
library(MuMIn)
library(stargazer)

setwd("~/WERC-SC/HALE")

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
## all data grouped by 'event' (predator, other, no) variable.
data_events_WL <- data_rev_WL %>%
  group_by(MoonTime1wk, MoonIllum1wk, total3monRain, totalWeekRain, meanTmin, meanTmax,
           Trapline, Week, Year_, Season, Month_, NTraps, loc, eventType) %>%
  # group_by(Trapline, Week, Year_, Season, Month_, NTraps, loc, eventType) %>%
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
           YearCat = as.factor(Year_),
           YearCts = as.numeric(Year_))
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
cpue_WL_models <- list() # model list

cpue_caughts_WL <- mlogit.data(expanded_data_Caughts_only_WL %>% 
                                 mutate(moon = MoonTime1wk * MoonIllum1wk) %>% 
                                 filter(!is.na(moon) & !is.na(meanTmin)), 
                               choice="choice",
                               alt.var ="x", 
                               id.var = "Trapline",
                               shape="long", 
                               chid.var="chid")
cpue_WL_models[[1]] <- mlogit(choice ~ 0 | Season + YearCts,
                            rpar=c('ratCaught:(intercept)'='n',
                                   'mongooseCaught:(intercept)'='n'),
                            R=50, halton=NA,
                            panel=TRUE,
                            iterlim=1, print.level=1,
                            data=cpue_caughts_WL)
# AIC(cpue_WL_models[[1]]) # Season + Year
# logLik(cpue_WL_models[[1]])

## LUNAR
cpue_WL_models[[2]] <- mlogit(choice ~ 0 | Season + YearCts + MoonTime1wk + MoonIllum1wk,
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                              R=50, halton=NA,
                              panel=TRUE,
                              iterlim=1, print.level=1,
                              data=cpue_caughts_WL)
cpue_WL_models[[3]] <- mlogit(choice ~ 0 | Season + YearCts + moon,
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                              R=50, halton=NA,
                              panel=TRUE,
                              iterlim=1, print.level=1,
                              data=cpue_caughts_WL)
cpue_WL_models[[4]] <- mlogit(choice ~ 0 | Season + YearCts + MoonIllum1wk,
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                              R=50, halton=NA,
                              panel=TRUE,
                              iterlim=1, print.level=1,
                              data=cpue_caughts_WL)
cpue_WL_models[[5]] <- mlogit(choice ~ 0 | Season + YearCts + MoonTime1wk,
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                              R=50, halton=NA,
                              panel=TRUE,
                              iterlim=1, print.level=1,
                              data=cpue_caughts_WL)
## WEATHER
# rain
cpue_WL_models[[6]] <- mlogit(choice ~ 0 | Season + YearCts + total3monRain + totalWeekRain,
                            rpar=c('ratCaught:(intercept)'='n',
                                   'mongooseCaught:(intercept)'='n'),
                            R=50, halton=NA,
                            panel=TRUE,
                            iterlim=1, print.level=1,
                            data=cpue_caughts_WL)
cpue_WL_models[[7]] <- mlogit(choice ~ 0 | Season + YearCts + total3monRain,
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                              R=50, halton=NA,
                              panel=TRUE,
                              iterlim=1, print.level=1,
                              data=cpue_caughts_WL) 

cpue_WL_models[[8]] <- mlogit(choice ~ 0 | Season + YearCts + totalWeekRain,
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                              R=50, halton=NA,
                              panel=TRUE,
                              iterlim=1, print.level=1,
                              data=cpue_caughts_WL)
# all weather
# cpue_WL.models[[8]] <- mlogit(choice ~ 0 | Season + YearCts + meanRelHum + meanSoilMois + meanSolRad + meanTmin + meanTmax,
#                             rpar=c('ratCaught:(intercept)'='n',
#                                    'mongooseCaught:(intercept)'='n'),
#                             R=50, halton=NA,
#                             panel=TRUE,
#                             iterlim=1, print.level=1,
#                             data=cpue_caughts_WL)
cpue_WL_models[[9]] <- mlogit(choice ~ 0 | Season + YearCts + meanTmin + meanTmax,
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                              R=50, halton=NA,
                              panel=TRUE,
                              iterlim=1, print.level=1,
                              data=cpue_caughts_WL)
cpue_WL_models[[10]] <- mlogit(choice ~ 0 | Season + YearCts + meanTmax,
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                              R=50, halton=NA,
                              panel=TRUE,
                              iterlim=1, print.level=1,
                              data=cpue_caughts_WL)
cpue_WL_models[[11]] <- mlogit(choice ~ 0 | Season + YearCts + meanTmin,
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                              R=50, halton=NA,
                              panel=TRUE,
                              iterlim=1, print.level=1,
                              data=cpue_caughts_WL)
cpue_WL_models[[12]] <- mlogit(choice ~ 0 | Season + YearCts + total3monRain + meanTmin + meanTmax,
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                              R=50, halton=NA,
                              panel=TRUE,
                              iterlim=1, print.level=1,
                              data=cpue_caughts_WL)
cpue_WL_models[[13]] <- mlogit(choice ~ 0 | Season + YearCts + totalWeekRain + meanTmin + meanTmax,
                               rpar=c('ratCaught:(intercept)'='n',
                                      'mongooseCaught:(intercept)'='n'),
                               R=50, halton=NA,
                               panel=TRUE,
                               iterlim=1, print.level=1,
                               data=cpue_caughts_WL)
## LUNAR AND WEATHER
cpue_WL_models[[14]] <- mlogit(choice ~ 0 | Season + YearCts + MoonTime1wk + MoonIllum1wk + totalWeekRain +
                                 totalWeekRain + meanTmin + meanTmax,
                               rpar=c('ratCaught:(intercept)'='n',
                                      'mongooseCaught:(intercept)'='n'),
                               R=50, halton=NA,
                               panel=TRUE,
                               iterlim=1, print.level=1,
                               data=cpue_caughts_WL)
cpue_WL_models[[15]] <- mlogit(choice ~ 0 | Season + YearCts + MoonIllum1wk + total3monRain + meanTmax,
                               rpar=c('ratCaught:(intercept)'='n',
                                      'mongooseCaught:(intercept)'='n'),
                               R=50, halton=NA,
                               panel=TRUE,
                               iterlim=1, print.level=1,
                               data=cpue_caughts_WL)
cpue_WL_models[[16]] <- mlogit(choice ~ 0 | Season + YearCts + MoonIllum1wk + totalWeekRain + 
                                 totalWeekRain + meanTmin + meanTmax,
                               rpar=c('ratCaught:(intercept)'='n',
                                      'mongooseCaught:(intercept)'='n'),
                               R=50, halton=NA,
                               panel=TRUE,
                               iterlim=1, print.level=1,
                               data=cpue_caughts_WL)
cpue_WL_models[[17]] <- mlogit(choice ~ 0 | Season + YearCts + MoonTime1wk + totalWeekRain +
                                 totalWeekRain + meanTmin + meanTmax,
                               rpar=c('ratCaught:(intercept)'='n',
                                      'mongooseCaught:(intercept)'='n'),
                               R=50, halton=NA,
                               panel=TRUE,
                               iterlim=1, print.level=1,
                               data=cpue_caughts_WL)


#### now compare AIC and log likelihood between models
varsCol <- sapply(cpue_WL_models, function(m) substr(as.character(formula(m)[3]), start = 5, stop = 1e6))
dfCol <- sapply(cpue_WL_models, function(m) attr(logLik(m), "df"))
logLikCol <- sapply(cpue_WL_models, function(m) attr(logLik(m), "null"))
aicCol <- sapply(cpue_WL_models, function(m) AIC(m))
aicWcol <- sapply(cpue_WL_models, function(m) Weights(AIC(m)))

# combine into one table and save output
bestWLmodel_preds <- data.frame(variables = varsCol, AIC = aicCol, `Log Likelihood` = logLikCol, DF = dfCol)
write.csv(bestWLmodel_preds, file = '~/WERC-SC/HALE/outputs/bestWLmodel_preds_eck12.5.csv',
          row.names = FALSE)


### analyze results for best fit model: model 10 (Season + Year + meanTmax)
## get fitted frequencies of each event type on unique combos of Trapline, Year, & Season
myfitted_WL_preds <- fitted(cpue_WL_models[[10]], outcome=FALSE)
head(myfitted_WL_preds)
dim(myfitted_WL_preds)
dim(expanded_data_Caughts_only_WL)

## select year, season, and trapline data for the fitted values
# Copy data and thin it down to one row per chid
fitted_cpue_WL_preds <- cpue_caughts_WL %>%
  filter(!is.na(meanTmax)) %>% 
  select(chid, Trapline, Year_, Season, Week, meanTmax) %>%
  unique()
# then `cbind` the data in `fitted_cpue_WL` with the fitted values in `myfitted`
fitted_cpue_WL_preds <- cbind(fitted_cpue_WL_preds, myfitted_WL_preds) %>%
  select(-chid) %>% # thin the fitted values further (i.e. remove replicates, keep unique combos of variables (Trapline, Year, & Season?)
  unique()

# ____________________________________________________________________________________________________________________
### EVENTS ANALYSIS
#### BOOTSTRAP SUBSETS OF EVENTTYPE DATA FOR ANALYSIS
set.seed(20170718)
nb = 1000 # number of bootstraps
s = 5000 # size of subset
subset_modelsWL_aic <- matrix(NA, ncol=17, nrow=nb) # ncol = number of models
subset_modelsWL_aicW <- matrix(NA, ncol=17, nrow=nb)

for (k in 1:nb) { 
  ### create nb iterations of formatted (expanded) data
  Data <- formatData(data_events_WL, 'eventType', s)
  ### run all 7 models through the bootstrap
  models <- list()
  # no random effects
  WL_data = mlogit.data(Data %>% mutate(trapyr=paste0(Trapline,'-',YearCat),
                                        moon = MoonTime1wk * MoonIllum1wk),
                               choice="choice", alt.var ="x", shape="long",
                               id.var = "trapyr",
                               chid.var="chid")
  models[[1]] <- mlogit(choice ~ 0 | Season + YearCts,
                                 rpar=c('predatorEvent:(intercept)'='n', 'otherEvent:(intercept)'='n'), 
                                 R=50, halton=NA, panel=TRUE,
                                 reflevel = "noEvent", iterlim=1,
                                 data=WL_data)
  ## LUNAR
  models[[2]] <- mlogit(choice ~ 0 | Season + YearCts + MoonTime1wk + MoonIllum1wk,
                                 rpar=c('predatorEvent:(intercept)'='n', 'otherEvent:(intercept)'='n'),
                                 R=50, halton=NA, panel=TRUE,
                                 reflevel = "noEvent", iterlim=1,
                                 data=WL_data)
  models[[3]] <- mlogit(choice ~ 0 | Season + YearCts + moon,
                                 rpar=c('predatorEvent:(intercept)'='n', 'otherEvent:(intercept)'='n'),
                                 R=50, halton=NA, panel=TRUE,
                                 reflevel = "noEvent", iterlim=1,
                                 data=WL_data)
  models[[4]] <- mlogit(choice ~ 0 | Season + YearCts + MoonTime1wk,
                                 rpar=c('predatorEvent:(intercept)'='n', 'otherEvent:(intercept)'='n'),
                                 R=50, halton=NA, panel=TRUE,
                                 reflevel = "noEvent", iterlim=1,
                                 data=WL_data)
  models[[5]] <- mlogit(choice ~ 0 | Season + YearCts + MoonIllum1wk,
                                 rpar=c('predatorEvent:(intercept)'='n', 'otherEvent:(intercept)'='n'), 
                                 R=50, halton=NA, panel=TRUE,
                                 reflevel = "noEvent", iterlim=1,
                                 data=WL_data)
  ## WEATHER
  # rain
  models[[6]] <- mlogit(choice ~ 0 | Season + YearCts + total3monRain + totalWeekRain,
                                 rpar=c('predatorEvent:(intercept)'='n', 'otherEvent:(intercept)'='n'), 
                                 R=50, halton=NA, panel=TRUE,
                                 reflevel = "noEvent", iterlim=1,
                                 data=WL_data)
  models[[7]] <- mlogit(choice ~ 0 | Season + YearCts + total3monRain,
                                 rpar=c('predatorEvent:(intercept)'='n', 'otherEvent:(intercept)'='n'), 
                                 R=50, halton=NA, panel=TRUE,
                                 reflevel = "noEvent", iterlim=1,
                                 data=WL_data) 
  models[[8]] <- mlogit(choice ~ 0 | Season + YearCts + totalWeekRain,
                                 rpar=c('predatorEvent:(intercept)'='n', 'otherEvent:(intercept)'='n'), 
                                 R=50, halton=NA, panel=TRUE,
                                 reflevel = "noEvent", iterlim=1,
                                 data=WL_data)
  # temperature
  models[[9]] <- mlogit(choice ~ 0 | Season + YearCts + meanTmin + meanTmax,
                                 rpar=c('predatorEvent:(intercept)'='n', 'otherEvent:(intercept)'='n'), 
                                 R=50, halton=NA, panel=TRUE,
                                 reflevel = "noEvent", iterlim=1,
                                 data=WL_data)
  models[[10]] <- mlogit(choice ~ 0 | Season + YearCts +  meanTmax,
                                 rpar=c('predatorEvent:(intercept)'='n', 'otherEvent:(intercept)'='n'), 
                                 R=50, halton=NA, panel=TRUE,
                                 reflevel = "noEvent", iterlim=1,
                                 data=WL_data)
  models[[11]] <- mlogit(choice ~ 0 | Season + YearCts + meanTmin,
                                 rpar=c('predatorEvent:(intercept)'='n', 'otherEvent:(intercept)'='n'), 
                                 R=50, halton=NA, panel=TRUE,
                                 reflevel = "noEvent", iterlim=1,
                                 data=WL_data)
  # all weather
  models[[12]] <- mlogit(choice ~ 0 | Season + YearCts + total3monRain + meanTmin + meanTmax,
                                 rpar=c('predatorEvent:(intercept)'='n', 'otherEvent:(intercept)'='n'), 
                                 R=50, halton=NA, panel=TRUE,
                                 reflevel = "noEvent", iterlim=1,
                                 data=WL_data)
  models[[13]] <- mlogit(choice ~ 0 | Season + YearCts + totalWeekRain + meanTmin + meanTmax,
                                 rpar=c('predatorEvent:(intercept)'='n', 'otherEvent:(intercept)'='n'), 
                                 R=50, halton=NA, panel=TRUE,
                                 reflevel = "noEvent", iterlim=1,
                                 data=WL_data)
  ## LUNAR AND WEATHER
  models[[14]] <- mlogit(choice ~ 0 | Season + YearCts + MoonIllum1wk + total3monRain + meanTmax,
                                 rpar=c('predatorEvent:(intercept)'='n', 'otherEvent:(intercept)'='n'), 
                                 R=50, halton=NA, panel=TRUE,
                                 reflevel = "noEvent", iterlim=1,
                                 data=WL_data) 
  models[[15]] <- mlogit(choice ~ 0 | Season + YearCts + MoonIllum1wk + totalWeekRain + 
                                   totalWeekRain + meanTmin + meanTmax,
                                 rpar=c('predatorEvent:(intercept)'='n', 'otherEvent:(intercept)'='n'), 
                                 R=50, halton=NA, panel=TRUE,
                                 reflevel = "noEvent", iterlim=1,
                                 data=WL_data)
  models[[16]] <- mlogit(choice ~ 0 | Season + YearCts + MoonTime1wk + MoonIllum1wk + totalWeekRain +
                           totalWeekRain + meanTmin + meanTmax,
                                 rpar=c('predatorEvent:(intercept)'='n', 'otherEvent:(intercept)'='n'), 
                                 R=50, halton=NA, panel=TRUE,
                                 reflevel = "noEvent", iterlim=1,
                                 data=WL_data)
  models[[17]] <- mlogit(choice ~ 0 | Season + YearCts + MoonTime1wk + totalWeekRain +
                           totalWeekRain + meanTmin + meanTmax,
                                 rpar=c('predatorEvent:(intercept)'='n', 'otherEvent:(intercept)'='n'), 
                                 R=50, halton=NA, panel=TRUE,
                                 reflevel = "noEvent", iterlim=1,
                                 data=WL_data)
  ### summarize and rank AICs for each itteration of bootstrap
  subset_modelsWL_aic[k, ] <- ldply(models, .fun=AIC)$V1
  subset_modelsWL_aicW[k, ] <- Weights(ldply(models, .fun=AIC)$V1)
}

### examine AIC and AIC weights
bestWLmodel_events <- table(apply(subset_modelsWL_aicW, MARGIN=1, FUN=which.max),
                   apply(subset_modelsWL_aic, MARGIN=1, FUN=which.min))
bestWLmodel_events
modelsWL_bs_aic <- summary(subset_modelsWL_aic)
modelsWL_bs_aicW <- summary(subset_modelsWL_aicW)

write.csv(bestWLmodel_events, file = '~/WERC-SC/HALE/outputs/bestWLmodel_events_eck12.5.csv',
          row.names = FALSE)
write.csv(modelsWL_bs_aic, file = '~/WERC-SC/HALE/outputs/modelsWL_bs_aic_eck12.5.csv',
          row.names = FALSE)
write.csv(modelsWL_bs_aicW, file = '~/WERC-SC/HALE/outputs/modelsWL_bs_aicW_eck12.5.csv',
          row.names = FALSE)


### analyze results for best fit model: model 9 (Season + Year + meanTmax + meanTmin)
expanded_data_WL_events <- formatData(data_events_WL, 'eventType', subset = 50000)
WL_data_events = mlogit.data(expanded_data_WL_events %>% 
                               mutate(trapyr=paste0(Trapline,'-',YearCat)),
                      choice="choice", alt.var ="x", shape="long",
                      id.var = "trapyr",
                      chid.var="chid")
model_WL_events <- mlogit(choice ~ 0 | Season + YearCts + meanTmin + meanTmax,
                      rpar=c('predatorEvent:(intercept)'='n', 'otherEvent:(intercept)'='n'), 
                      R=50, halton=NA, panel=TRUE,
                      reflevel = "noEvent", iterlim=1,
                      data=WL_data_events)
## get fitted frequencies of each event type on unique combos of Trapline, Year, & Season
myfitted_WL_events <- fitted(model_WL_events, outcome=FALSE)
head(myfitted_WL_events)
dim(myfitted_WL_events)
dim(expanded_data_WL_events)

## select year, season, and trapline data for the fitted values
# Copy data and thin it down to one row per chid
fitted_cpue_WL_events <- expanded_data_WL_events %>%
  select(chid, Trapline, Year_, Season, Week, meanTmax, meanTmin) %>%
  unique()
# then `cbind` the data in `fitted_cpue_WL` with the fitted values in `myfitted`
fitted_cpue_WL <- cbind(fitted_cpue_WL_events, myfitted_WL_events) %>%
  select(-chid) %>% # thin the fitted values further (i.e. remove replicates and keep the unique combos of Trapline, Year, & Season)
  unique()

