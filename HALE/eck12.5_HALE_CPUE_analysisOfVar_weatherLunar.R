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
cpue_L_models <- list() # model list

cpue_caughts_L <- mlogit.data(expanded_data_Caughts_only_WL %>% 
                                 mutate(moon = MoonTime1wk * MoonIllum1wk) %>% 
                                 filter(!is.na(moon)), 
                               choice="choice",
                               alt.var ="x", 
                               id.var = "Trapline",
                               shape="long", 
                               chid.var="chid")
cpue_L_models[[1]] <- mlogit(choice ~ 0 | Season + YearCts,
                            rpar=c('ratCaught:(intercept)'='n',
                                   'mongooseCaught:(intercept)'='n'),
                            R=50, halton=NA,
                            panel=TRUE,
                            iterlim=1, print.level=1,
                            data=cpue_caughts_L)
cpue_L_models[[2]] <- mlogit(choice ~ 0 | Season + YearCts + MoonTime1wk + MoonIllum1wk,
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                              R=50, halton=NA,
                              panel=TRUE,
                              iterlim=1, print.level=1,
                              data=cpue_caughts_L)
cpue_L_models[[3]] <- mlogit(choice ~ 0 | Season + YearCts + moon,
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                              R=50, halton=NA,
                              panel=TRUE,
                              iterlim=1, print.level=1,
                              data=cpue_caughts_L)
cpue_L_models[[4]] <- mlogit(choice ~ 0 | Season + YearCts + MoonIllum1wk,
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                              R=50, halton=NA,
                              panel=TRUE,
                              iterlim=1, print.level=1,
                              data=cpue_caughts_L)
cpue_L_models[[5]] <- mlogit(choice ~ 0 | Season + YearCts + MoonTime1wk,
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                              R=50, halton=NA,
                              panel=TRUE,
                              iterlim=1, print.level=1,
                              data=cpue_caughts_L)

#### now compare AIC and log likelihood between models
varsColL <- sapply(cpue_L_models, function(m) substr(as.character(formula(m)[3]), start = 5, stop = 1e6))
dfColL <- sapply(cpue_L_models, function(m) attr(logLik(m), "df"))
logLikColL <- sapply(cpue_L_models, function(m) attr(logLik(m), "null"))
aicColL <- sapply(cpue_L_models, function(m) AIC(m))
aicWcolL <- sapply(cpue_L_models, function(m) Weights(AIC(m)))

# combine into one table and save output
bestLmodel_preds <- data.frame(variables = varsColL, AIC = aicColL, `Weighted AIC` = aicWcolL,  `Log Likelihood` = logLikColL, DF = dfColL)
write.csv(bestLmodel_preds, file = '~/WERC-SC/HALE/outputs/bestLmodel_preds_eck12.5.csv',
          row.names = FALSE)

### analyze results for best fit model: model 3 (Season + Year + moon)
myfitted_L_preds <- fitted(cpue_L_models[[3]], outcome=FALSE)
head(myfitted_L_preds)
# select data and thin it down to one row per chid
fitted_cpue_L_preds <- cpue_caughts_L %>%
  filter(!is.na(moon)) %>% 
  select(chid, Trapline, Year, Season, Week, moon) %>%
  unique()
dim(myfitted_L_preds)
dim(fitted_cpue_L_preds)
# then `cbind` the data in `fitted_cpue_WL` with the fitted values in `myfitted`
fitted_cpue_L_preds <- cbind(fitted_cpue_L_preds, myfitted_L_preds) %>%
  select(-chid) %>% # thin the fitted values further (i.e. remove replicates, keep unique combos of variables (Trapline, Year, & Season?)
  unique()
write.csv(fitted_cpue_L_preds, file = '~/WERC-SC/HALE/outputs/fitted_cpue_L_preds_eck12.5.csv',
          row.names = FALSE)


# ___________________________________
## WEATHER
cpue_W_models <- list() # model list

cpue_caughts_W <- mlogit.data(expanded_data_Caughts_only_WL %>% 
                                 mutate(moon = MoonTime1wk * MoonIllum1wk) %>% 
                                 filter(!is.na(meanTmin)), 
                               choice="choice",
                               alt.var ="x", 
                               id.var = "Trapline",
                               shape="long", 
                               chid.var="chid")
cpue_W_models[[1]] <- mlogit(choice ~ 0 | Season + YearCts,
                             rpar=c('ratCaught:(intercept)'='n',
                                    'mongooseCaught:(intercept)'='n'),
                             R=50, halton=NA,
                             panel=TRUE,
                             iterlim=1, print.level=1,
                             data=cpue_caughts_W)
# rain
cpue_W_models[[2]] <- mlogit(choice ~ 0 | Season + YearCts + total3monRain + totalWeekRain,
                            rpar=c('ratCaught:(intercept)'='n',
                                   'mongooseCaught:(intercept)'='n'),
                            R=50, halton=NA,
                            panel=TRUE,
                            iterlim=1, print.level=1,
                            data=cpue_caughts_W)
cpue_W_models[[3]] <- mlogit(choice ~ 0 | Season + YearCts + total3monRain,
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                              R=50, halton=NA,
                              panel=TRUE,
                              iterlim=1, print.level=1,
                              data=cpue_caughts_W) 

cpue_W_models[[4]] <- mlogit(choice ~ 0 | Season + YearCts + totalWeekRain,
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                              R=50, halton=NA,
                              panel=TRUE,
                              iterlim=1, print.level=1,
                              data=cpue_caughts_W)
# temperature
cpue_W_models[[5]] <- mlogit(choice ~ 0 | Season + YearCts + meanTmin + meanTmax,
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                              R=50, halton=NA,
                              panel=TRUE,
                              iterlim=1, print.level=1,
                              data=cpue_caughts_W)
cpue_W_models[[6]] <- mlogit(choice ~ 0 | Season + YearCts + meanTmax,
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                              R=50, halton=NA,
                              panel=TRUE,
                              iterlim=1, print.level=1,
                              data=cpue_caughts_W)
cpue_W_models[[7]] <- mlogit(choice ~ 0 | Season + YearCts + meanTmin,
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                              R=50, halton=NA,
                              panel=TRUE,
                              iterlim=1, print.level=1,
                              data=cpue_caughts_W)
# all weather
cpue_W_models[[8]] <- mlogit(choice ~ 0 | Season + YearCts + meanRelHum + meanSoilMois + meanSolRad + 
                               total3monRain+ totalWeekRain + meanTmin + meanTmax,
                             rpar=c('ratCaught:(intercept)'='n',
                                    'mongooseCaught:(intercept)'='n'),
                             R=50, halton=NA,
                             panel=TRUE,
                             iterlim=1, print.level=1,
                             data=cpue_caughts_W)
cpue_W_models[[9]] <- mlogit(choice ~ 0 | Season + YearCts + total3monRain + totalWeekRain + meanTmin + meanTmax,
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                              R=50, halton=NA,
                              panel=TRUE,
                              iterlim=1, print.level=1,
                              data=cpue_caughts_W)
cpue_W_models[[10]] <- mlogit(choice ~ 0 | Season + YearCts + total3monRain + meanTmin + meanTmax,
                              rpar=c('ratCaught:(intercept)'='n',
                                     'mongooseCaught:(intercept)'='n'),
                              R=50, halton=NA,
                              panel=TRUE,
                              iterlim=1, print.level=1,
                              data=cpue_caughts_W)
cpue_W_models[[11]] <- mlogit(choice ~ 0 | Season + YearCts + totalWeekRain + meanTmin + meanTmax,
                               rpar=c('ratCaught:(intercept)'='n',
                                      'mongooseCaught:(intercept)'='n'),
                               R=50, halton=NA,
                               panel=TRUE,
                               iterlim=1, print.level=1,
                               data=cpue_caughts_W)

cpue_W_models[[12]] <- mlogit(choice ~ 0 | Season + YearCts + total3monRain + meanTmax,
                               rpar=c('ratCaught:(intercept)'='n',
                                      'mongooseCaught:(intercept)'='n'),
                               R=50, halton=NA,
                               panel=TRUE,
                               iterlim=1, print.level=1,
                               data=cpue_caughts_W)


#### now compare AIC and log likelihood between models
varsColW <- sapply(cpue_W_models, function(m) substr(as.character(formula(m)[3]), start = 5, stop = 1e6))
dfColW <- sapply(cpue_W_models, function(m) attr(logLik(m), "df"))
logLikColW <- sapply(cpue_W_models, function(m) attr(logLik(m), "null"))
aicColW <- sapply(cpue_W_models, function(m) AIC(m))
aicWcolW <- sapply(cpue_W_models, function(m) Weights(AIC(m)))

# combine into one table and save output
bestWmodel_preds <- data.frame(variables = varsColW, AIC = aicColW, `Weighted AIC` = aicWcolW, `Log Likelihood` = logLikColW, DF = dfColW)
write.csv(bestWmodel_preds, file = '~/WERC-SC/HALE/outputs/bestWmodel_preds_eck12.5.csv',
          row.names = FALSE)

### analyze results for best fit model: model 6 (Season + Year + meanTmax)
myfitted_W_preds <- fitted(cpue_W_models[[6]], outcome=FALSE)
# select data and thin it down to one row per chid
fitted_cpue_W_preds <- cpue_caughts_W %>%
  filter(!is.na(meanTmax)) %>% 
  select(chid, Trapline, Year, Season, Week, meanTmax) %>%
  unique()
dim(myfitted_W_preds)
dim(fitted_cpue_W_preds)
# then `cbind` the data in `fitted_cpue_WL` with the fitted values in `myfitted`
fitted_cpue_W_preds <- cbind(fitted_cpue_W_preds, myfitted_W_preds) %>%
  select(-chid) %>% # thin the fitted values further (i.e. remove replicates, keep unique combos of variables (Trapline, Year, & Season?)
  unique()
write.csv(fitted_cpue_W_preds, file = '~/WERC-SC/HALE/outputs/fitted_cpue_W_preds_eck12.5.csv',
          row.names = FALSE)

# ____________________________________________________________________________________________________________________
### EVENTS ANALYSIS
#### BOOTSTRAP SUBSETS OF EVENTTYPE DATA FOR ANALYSIS
# LUNAR
nb = 1000 # number of bootstraps
s = 5000 # size of subset
subset_modelsL_aic <- matrix(NA, ncol=5, nrow=nb) # ncol = number of models
subset_modelsL_aicW <- matrix(NA, ncol=5, nrow=nb)
subset_modelsL_logLik <- matrix(NA, ncol=5, nrow=nb)

for (k in 1:nb) { 
  set.seed(k)
  ### create nb iterations of formatted (expanded) data
  LData <- formatData(data_events_WL, 'eventType', s)
  ### run all 7 models through the bootstrap
  Lmodels <- list()
  # no random effects
  L_data = mlogit.data(LData %>% mutate(trapyr=paste0(Trapline,'-',YearCat),
                                        moon = MoonTime1wk * MoonIllum1wk) %>% 
                                 filter(!is.na(moon)),  # 8150 NAs
                               choice="choice", alt.var ="x", shape="long",
                               id.var = "trapyr",
                               chid.var="chid")
  Lmodels[[1]] <- mlogit(choice ~ 0 | Season + YearCts,
                        rpar=c('predatorEvent:(intercept)'='n', 'otherEvent:(intercept)'='n'), 
                        R=50, halton=NA, panel=TRUE,
                        reflevel = "noEvent", iterlim=1,
                        data=L_data)
  Lmodels[[2]] <- mlogit(choice ~ 0 | Season + YearCts + MoonTime1wk + MoonIllum1wk,
                        rpar=c('predatorEvent:(intercept)'='n', 'otherEvent:(intercept)'='n'),
                        R=50, halton=NA, panel=TRUE,
                        reflevel = "noEvent", iterlim=1,
                        data=L_data)
  Lmodels[[3]] <- mlogit(choice ~ 0 | Season + YearCts + moon,
                        rpar=c('predatorEvent:(intercept)'='n', 'otherEvent:(intercept)'='n'),
                        R=50, halton=NA, panel=TRUE,
                        reflevel = "noEvent", iterlim=1,
                        data=L_data)
  Lmodels[[4]] <- mlogit(choice ~ 0 | Season + YearCts + MoonTime1wk,
                        rpar=c('predatorEvent:(intercept)'='n', 'otherEvent:(intercept)'='n'),
                        R=50, halton=NA, panel=TRUE,
                        reflevel = "noEvent", iterlim=1,
                        data=L_data)
  Lmodels[[5]] <- mlogit(choice ~ 0 | Season + YearCts + MoonIllum1wk,
                        rpar=c('predatorEvent:(intercept)'='n', 'otherEvent:(intercept)'='n'), 
                        R=50, halton=NA, panel=TRUE,
                        reflevel = "noEvent", iterlim=1,
                        data=L_data)
  ### summarize and rank AICs for each itteration of bootstrap
  subset_modelsL_aic[k, ] <- ldply(Lmodels, .fun=AIC)$V1
  subset_modelsL_aicW[k, ] <- Weights(ldply(Lmodels, .fun=AIC)$V1)
  subset_modelsL_logLik[k, ] <- ldply(Lmodels, .fun=logLik)$V1
}

### examine AIC and AIC weights
# varsColW <- sapply(cpue_W_models, function(m) substr(as.character(formula(m)[3]), start = 5, stop = 1e6))
bestLmodel_events <- table(apply(subset_modelsL_aicW, MARGIN=1, FUN=which.max),
                            apply(subset_modelsL_aic, MARGIN=1, FUN=which.min))
bestLmodel_events
write.csv(bestLmodel_events, file = '~/WERC-SC/HALE/outputs/bestLmodel_events_eck12.5.csv',
          row.names = FALSE)

## save model statistics
modelsL_aic <- data.frame(subset_modelsL_aic)
modelsL_aicW <- data.frame(subset_modelsL_aicW)
modelsL_logLik <- data.frame(subset_modelsL_logLik)
# create table columns
varsColL <- sapply(Lmodels, function(m) substr(as.character(formula(m)[3]), start = 5, stop = 1e6))
dfColL <- sapply(Lmodels, function(m) attr(logLik(m), "df"))
logLikColL <- sapply(modelsL_logLik, FUN = mean)
aicColL <- sapply(modelsL_aic, FUN = mean)
aicWcolL <- sapply(modelsL_aicW, FUN = mean)
# combine into one table and save output
bs_Lmodel_events <- data.frame(variables = varsColL, AIC = aicColL, `Weighted AIC` = aicWcolL,
                                `Log Likelihood` = logLikColL, DF = dfColL)
write.csv(bs_Lmodel_events, file = '~/WERC-SC/HALE/outputs/bs_Lmodel_events_eck12.5.csv',
          row.names = FALSE)

### analyze results for best fit model: model 4 (Season + Year + MoonTime1wk)
expanded_data_L_events <- formatData(data_rev_WL, 'eventType', subset = 50000)
L_data_events = mlogit.data(expanded_data_L_events %>% 
                              mutate(trapyr=paste0(Trapline,'-',YearCat),
                                     moon = MoonTime1wk * MoonIllum1wk) %>%
                              filter(!is.na(moon)), # 8150 NAs
                             choice="choice", alt.var ="x", shape="long",
                             id.var = "trapyr",
                             chid.var="chid")
model_L_events <- mlogit(choice ~ 0 | Season + YearCts + MoonTime1wk,
                          rpar=c('predatorEvent:(intercept)'='n', 'otherEvent:(intercept)'='n'), 
                          R=50, halton=NA, panel=TRUE,
                          reflevel = "noEvent", iterlim=1,
                          data=L_data_events)
myfitted_L_events <- fitted(model_L_events, outcome=FALSE)
head(myfitted_L_events)
# select data and thin it down to one row per chid
fitted_cpue_L_events <- L_data_events %>%
  filter(!is.na(moon)) %>% 
  select(chid, Trapline, Year, Season, Week, MoonTime1wk) %>%
  unique()
dim(myfitted_L_events)
dim(fitted_cpue_L_events)
# then `cbind` the data in `fitted_cpue_WL` with the fitted values in `myfitted`
fitted_cpue_L_events <- cbind(fitted_cpue_L_events, myfitted_L_events) %>%
  select(-chid) %>% # thin the fitted values further (i.e. remove replicates, keep unique combos of variables (Trapline, Year, & Season?)
  unique()
write.csv(fitted_cpue_L_events, file = '~/WERC-SC/HALE/outputs/fitted_cpue_L_events_eck12.5.csv',
          row.names = FALSE)

# _______________________________________  
# WEATHER
set.seed(1003)
nb = 5 # number of bootstraps
s = 5000 # size of subset
subset_modelsW_aic <- matrix(NA, ncol=12, nrow=nb) # ncol = number of models
subset_modelsW_aicW <- matrix(NA, ncol=12, nrow=nb)
subset_modelsW_logLik <- matrix(NA, ncol=12, nrow=nb)

for (j in 1:nb) { 
  set.seed(j)
  ### create nb iterations of formatted (expanded) data
  WData <- formatData(data_events_WL, 'eventType', s)
  ### run all 7 models through the bootstrap
  Wmodels <- list()
  # no random effects
  W_data = mlogit.data(WData %>% mutate(trapyr=paste0(Trapline,'-',Year)) %>% 
                                 filter(!is.na(meanTmax)),
                        choice="choice", alt.var ="x", shape="long",
                        id.var = "trapyr",
                        chid.var="chid")
  Wmodels[[1]] <- mlogit(choice ~ 0 | Season + YearCts,
                                 rpar=c('predatorEvent:(intercept)'='n', 'otherEvent:(intercept)'='n'), 
                                 R=50, halton=NA, panel=TRUE,
                                 reflevel = "noEvent", iterlim=1,
                                 data=W_data)
  # rain
  Wmodels[[2]] <- mlogit(choice ~ 0 | Season + YearCts + total3monRain + totalWeekRain,
                                 rpar=c('predatorEvent:(intercept)'='n', 'otherEvent:(intercept)'='n'), 
                                 R=50, halton=NA, panel=TRUE,
                                 reflevel = "noEvent", iterlim=1,
                                 data=W_data)
  Wmodels[[3]] <- mlogit(choice ~ 0 | Season + YearCts + total3monRain,
                                 rpar=c('predatorEvent:(intercept)'='n', 'otherEvent:(intercept)'='n'), 
                                 R=50, halton=NA, panel=TRUE,
                                 reflevel = "noEvent", iterlim=1,
                                 data=W_data) 
  Wmodels[[4]] <- mlogit(choice ~ 0 | Season + YearCts + totalWeekRain,
                                 rpar=c('predatorEvent:(intercept)'='n', 'otherEvent:(intercept)'='n'), 
                                 R=50, halton=NA, panel=TRUE,
                                 reflevel = "noEvent", iterlim=1,
                                 data=W_data)
  # temperature
  Wmodels[[5]] <- mlogit(choice ~ 0 | Season + YearCts + meanTmin + meanTmax,
                                 rpar=c('predatorEvent:(intercept)'='n', 'otherEvent:(intercept)'='n'), 
                                 R=50, halton=NA, panel=TRUE,
                                 reflevel = "noEvent", iterlim=1,
                                 data=W_data)
  Wmodels[[6]] <- mlogit(choice ~ 0 | Season + YearCts +  meanTmax,
                                 rpar=c('predatorEvent:(intercept)'='n', 'otherEvent:(intercept)'='n'), 
                                 R=50, halton=NA, panel=TRUE,
                                 reflevel = "noEvent", iterlim=1,
                                 data=W_data)
  Wmodels[[7]] <- mlogit(choice ~ 0 | Season + YearCts + meanTmin,
                                 rpar=c('predatorEvent:(intercept)'='n', 'otherEvent:(intercept)'='n'), 
                                 R=50, halton=NA, panel=TRUE,
                                 reflevel = "noEvent", iterlim=1,
                                 data=W_data)
  # all weather
  Wmodels[[8]] <- mlogit(choice ~ 0 | Season + YearCts + meanRelHum + meanSoilMois + meanSolRad + 
                            total3monRain + totalWeekRain + meanTmin + meanTmax,
                                 rpar=c('predatorEvent:(intercept)'='n', 'otherEvent:(intercept)'='n'), 
                                 R=50, halton=NA, panel=TRUE,
                                 reflevel = "noEvent", iterlim=1,
                                 data=W_data)
  Wmodels[[9]] <- mlogit(choice ~ 0 | Season + YearCts + total3monRain + totalWeekRain + meanTmin + meanTmax,
                                 rpar=c('predatorEvent:(intercept)'='n', 'otherEvent:(intercept)'='n'), 
                                 R=50, halton=NA, panel=TRUE,
                                 reflevel = "noEvent", iterlim=1,
                                 data=W_data)
  Wmodels[[10]] <- mlogit(choice ~ 0 | Season + YearCts + total3monRain + meanTmin + meanTmax,
                                 rpar=c('predatorEvent:(intercept)'='n', 'otherEvent:(intercept)'='n'), 
                                 R=50, halton=NA, panel=TRUE,
                                 reflevel = "noEvent", iterlim=1,
                                 data=W_data) 
  Wmodels[[11]] <- mlogit(choice ~ 0 | Season + YearCts + totalWeekRain + meanTmin + meanTmax,
                                 rpar=c('predatorEvent:(intercept)'='n', 'otherEvent:(intercept)'='n'), 
                                 R=50, halton=NA, panel=TRUE,
                                 reflevel = "noEvent", iterlim=1,
                                 data=W_data)
  Wmodels[[12]] <- mlogit(choice ~ 0 | Season + YearCts + total3monRain + meanTmax,
                                 rpar=c('predatorEvent:(intercept)'='n', 'otherEvent:(intercept)'='n'), 
                                 R=50, halton=NA, panel=TRUE,
                                 reflevel = "noEvent", iterlim=1,
                                 data=W_data)
  ### summarize and rank AICs for each itteration of bootstrap
  subset_modelsW_aic[j, ] <- ldply(Wmodels, .fun=AIC)$V1
  subset_modelsW_aicW[j, ] <- Weights(ldply(Wmodels, .fun=AIC)$V1)
  subset_modelsW_logLik[j, ] <- ldply(Wmodels, .fun=logLik)$V1
}

### examine AIC and AIC weights
bestWmodel_events <- table(apply(subset_modelsW_aic, MARGIN=1, FUN=which.min),
                           apply(subset_modelsW_aicW, MARGIN=1, FUN=which.max))
bestWmodel_events
write.csv(bestWmodel_events, file = '~/WERC-SC/HALE/outputs/bestWmodel_events_eck12.5.csv',
          row.names = FALSE)

## save model statistics
modelsW_aic <- data.frame(subset_modelsW_aic)
modelsW_aicW <- data.frame(subset_modelsW_aicW)
modelsW_logLik <- data.frame(subset_modelsW_logLik)
# create table columns
varsColW <- sapply(Wmodels, function(m) substr(as.character(formula(m)[3]), start = 5, stop = 1e6))
dfColW <- sapply(Wmodels, function(m) attr(logLik(m), "df"))
logLikColW <- sapply(modelsW_logLik, FUN = mean)
aicColW <- sapply(modelsW_aic, FUN = mean)
aicWcolW <- sapply(modelsW_aicW, FUN = mean)
# combine into one table and save output
bs_Wmodel_events <- data.frame(variables = varsColW, AIC = aicColW, `Weighted AIC` = aicWcolW, 
                               `Log Likelihood` = logLikColW, DF = dfColW)
write.csv(bs_Wmodel_events, file = '~/WERC-SC/HALE/outputs/bs_Wmodel_events_eck12.5.csv',
          row.names = FALSE)


### analyze results for best fit model: model 12 (Season + Year + total3monRain + meanTmax)
expanded_data_W_events <- formatData(data_rev_WL, 'eventType', subset = 55000)
W_data_events = mlogit.data(expanded_data_W_events %>% 
                              mutate(trapyr=paste0(Trapline,'-',YearCat)) %>%
                              filter(!is.na(meanTmax)),
                            choice="choice", alt.var ="x", shape="long",
                            id.var = "trapyr",
                            chid.var="chid")
model_W_events <- mlogit(choice ~ 0 | Season + YearCts + total3monRain + meanTmax,
                         rpar=c('predatorEvent:(intercept)'='n', 'otherEvent:(intercept)'='n'), 
                         R=50, halton=NA, panel=TRUE,
                         reflevel = "noEvent", iterlim=1,
                         data=W_data_events)
myfitted_W_events <- fitted(model_W_events, outcome=FALSE)
head(myfitted_W_events)
# select data and thin it down to one row per chid
fitted_cpue_W_events <- W_data_events %>%
  filter(!is.na(meanTmax)) %>% 
  select(chid, Trapline, Year, Season, Week, total3monRain, meanTmax) %>%
  unique()
dim(myfitted_W_events)
dim(fitted_cpue_W_events)
# then `cbind` the data in `fitted_cpue_WL` with the fitted values in `myfitted`
fitted_cpue_W_events <- cbind(fitted_cpue_W_events, myfitted_W_events) %>%
  select(-chid) %>% # thin the fitted values further (i.e. remove replicates, keep unique combos of variables (Trapline, Year, & Season?)
  unique()
write.csv(fitted_cpue_W_events, file = '~/WERC-SC/HALE/outputs/fitted_cpue_W_events_eck12.5.csv',
          row.names = FALSE)

