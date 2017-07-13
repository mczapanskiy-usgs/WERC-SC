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
## create model list
cpue_WL_models <- list()
### PREDS ONLY ANALYSIS: compare 3 random effect options
cpue_caughts_WL <- mlogit.data(expanded_data_Caughts_only_WL %>% 
                                 mutate(moon = MoonTime1wk * MoonIllum1wk), 
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

# 
# #### now compare AIC models between models
# AIC <- as.data.table(table(ldply(cpue_WL_models, .fun=AIC)$V1))
# loglik <- as.data.table(table(ldply(cpue_WL_models, .fun=logLik)$V1))
# 
# bestWLmodel_preds <- full_join(AIC, loglik, by="N") %>% 
#   select(-N) %>% 
#   rename(AIC=V1.x, loglik=V1.y) %>% 
#   mutate(vars= c("Season + Year", "Season + YearCts + MoonTime1wk + MoonIllum1wk", "Season + YearCts + moon", "Season + YearCts + MoonIllum1wk",
#                  "Season + YearCts + MoonTime1wk", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17"))


AIC(cpue_WL_models[[1]]) # Season + Year
logLik(cpue_WL_models[[1]])
AIC(cpue_WL_models[[2]]) # Season + Year + MoonTime1wk + MoonIllum1wk
logLik(cpue_WL_models[[2]])
AIC(cpue_WL_models[[3]]) # Season + Year +moon (MoonTime1wk * MoonIllum1wk)
logLik(cpue_WL.models[[3]])
AIC(cpue_WL_models[[4]]) # Season + Year + MoonIllum1wk
logLik(cpue_WL_models[[4]])
AIC(cpue_WL_models[[5]]) # Season + Year + MoonTime1wk
logLik(cpue_WL_models[[5]])

AIC(cpue_WL_models[[6]]) # Season + Year + total3monRain + totalWeekRain
logLik(cpue_WL_models[[6]])
AIC(cpue_WL_models[[7]]) # Season + Year + total3monRain ***
logLik(cpue_WL_models[[7]])
AIC(cpue_WL_models[[8]]) # Season + Year + totalWeekRain 
logLik(cpue_WL_models[[8]])

AIC(cpue_WL_models[[9]]) # Season + Year + meanTmin + meanTmax 
logLik(cpue_WL_models[[9]])
AIC(cpue_WL_models[[10]]) # Season + Year + meanTmax ****
logLik(cpue_WL_models[[10]])
AIC(cpue_WL_models[[11]]) # Season + Year + meanTmin
logLik(cpue_WL_models[[11]])

AIC(cpue_WL_models[[12]]) # Season + Year + total3monRain + meanTmin + meanTmin 
logLik(cpue_WL_models[[12]])
AIC(cpue_WL_models[[13]]) # Season + Year + totalWeekRain + meanTmin + meanTmin
logLik(cpue_WL_models[[13]])

AIC(cpue_WL_models[[14]]) # Season + YearCts + MoonTime1wk + MoonIllum1wk + totalWeekRain + totalWeekRain + meanTmin + meanTmax
logLik(cpue_WL_models[[14]])
AIC(cpue_WL_models[[15]]) # Season + Year + MoonIllum1wk + total3monRain + meanTmax ***
logLik(cpue_WL_models[[15]])
AIC(cpue_WL_models[[16]]) # Season + YearCts + MoonIllum1wk + totalWeekRain + totalWeekRain + meanTmin + meanTmax
logLik(cpue_WL_models[[16]])
AIC(cpue_WL_models[[17]]) # Season + YearCts + MoonTime1wk + totalWeekRain + totalWeekRain + meanTmin + meanTmax
logLik(cpue_WL_models[[17]])



subset_modelsWL_aicW[k, ] <- Weights(ldply(models, .fun=AIC)$V1)
### EVENTS ANALYSIS
#### BOOTSTRAP SUBSETS OF EVENTTYPE DATA FOR ANALYSIS
set.seed(006)
nb = 100 # number of bootstraps
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
bestWLmodel <- table(apply(subset_modelsWL_aicW, MARGIN=1, FUN=which.max),
                   apply(subset_modelsWL_aic, MARGIN=1, FUN=which.min))
bestWLmodel
summary(subset_modelsWL_aic)
summary(subset_modelsWL_aicW)

# bestWLmodel_aic <- table(apply(subset_modelsWL_aic, MARGIN=1, FUN=which.min))
# bestWLmodel_aicW <- table(apply(subset_modelsWL_aicW, MARGIN=1, FUN=which.max))

### analyze results for model 10 (the best fit for the "caughts_only" data)
## get fitted frequencies of each event type on unique combos of Trapline, Year, & Season
myfitted_WL <- fitted(cpue_WL_models[[10]], outcome=FALSE)
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


