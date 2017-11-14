## this script tests which subset can be used 
## to test the effect of: year, trap location, trap type, and bait type 
## on event types: PREDATOR, OTHER, NONE
## using the mlogit model
## THIS SCRIPT IS CARRIED OVER FROM eck12_HALE_CPUE_analysisOfVar.r

library(stats)
library(plyr)
library(data.table)
library(dplyr)
library(ggplot2)
library(ez)
library(mlogit)
# library(AICcmodavg)
library(MuMIn)
# library(mosaic)

setwd("~/WERC-SC/HALE")

read.csv('~/WERC-SC/HALE/TraplinePredEventPUE_11_20170118.csv',
         stringsAsFactors = FALSE) -> CPUEdata

#### EDIT DATA: remove the mouse events, separate front and backcountry traps, & group predator events (for rerun of mlogit analysis)
data_rev <- CPUEdata %>% 
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

#### aggregate the data by summing the NEvents within each event group.
data_events <- data_rev %>%
  group_by(Trapline, Week, Year, Season, Month, NTraps, loc, eventType) %>%
  dplyr::summarise(NEvents=sum(NEvents)) %>%
  mutate(CPUE = NEvents/NTraps) %>% # this line optional
  as.data.frame()
# ## run function using grouped 'event' (predator, other, no) variable.
# expanded_data.events <- formatData(data_events, 'eventType') 

#### RESTRUCTURE DATA FUNCTION
formatData <- function(data, var, subset){
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

#### BOOTSTRAP SUBSETS OF EVENTTYPE DATA FOR ANALYSIS
nb = 1000 # number of bootstraps
s = 5000 # size of subset
subset_models_aic <- matrix(NA, ncol=13, nrow=nb) # nrow = number of models
subset_models_aicW <- matrix(NA, ncol=13, nrow=nb)
subset_models_logLik <- matrix(NA, ncol=13, nrow=nb)

for (k in 1:nb) { 
  set.seed(k)
  ### create nb iterations of formatted (expanded) data
  Data <- formatData(data_events, 'eventType', s)
  ### run all 7 models through the bootstrap
  models <- list()
  # no random effects
  m.data = mlogit.data(Data,  
                        choice="choice", alt.var ="x", shape="long", chid.var="chid")
  models[[1]] <- mlogit(choice ~ 0 | Season + Trapline + Year,   # no random effects, Year + Trapline + Season = individual-specific variables
                              reflevel = "noEvent", iterlim=1, 
                              data=m.data)
  models[[2]] <- mlogit(choice ~ 1 | Season + YearCts,  # no random effects, Season + Year = individual-specific variables
                              reflevel = "noEvent", iterlim=1, 
                              data=m.data)
  models[[3]] <- mlogit(choice ~ 1 | Season,  # no random effects, Season = individual-specific variables
                              reflevel = "noEvent", iterlim=1,
                              data=m.data)
  models[[4]] <- mlogit(choice ~ 1 | YearCts,  # no random effects, Year = individual-specific variables
                              reflevel = "noEvent", iterlim=1,
                              data=m.data)
  # year as random effect
  m.data.year <- mlogit.data(Data,
                             choice="choice", alt.var ="x", shape="long", chid.var="chid",
                             id.var = "YearCat")
  models[[5]] <- mlogit(choice ~ 1 | Season + YearCts,
                              rpar=c('predatorEvent:(intercept)'='n', 'otherEvent:(intercept)'='n'),  R=50, halton=NA, panel=TRUE,
                              reflevel = "noEvent", iterlim=1,
                              data=m.data.year)
  # trapline as random effect
  m.data.trap <- mlogit.data(Data,
                             choice="choice", alt.var ="x", shape="long", chid.var="chid",
                             id.var = "Trapline")
  models[[6]] <- mlogit(choice ~ 1 | Season + YearCts,
                              rpar=c('predatorEvent:(intercept)'='n','otherEvent:(intercept)'='n'), R=50, halton=NA, panel=TRUE,
                              reflevel = "noEvent", iterlim=1,
                              data=m.data.trap)
  # trapline + yr as random effect
  m.data.trapyr <- mlogit.data(Data %>%
                                 mutate(trapyr=paste0(Trapline,'-',YearCat)),
                               choice="choice", alt.var ="x", shape="long", chid.var="chid",
                               id.var = "trapyr")
  models[[7]] <- mlogit(choice ~ 1 | Season + YearCts,
                              rpar=c('predatorEvent:(intercept)'='n','otherEvent:(intercept)'='n'), R=50, halton=NA, panel=TRUE,
                              reflevel = "noEvent", iterlim=1,
                              data=m.data.trapyr)
  ## MONTH
  models[[8]] <- mlogit(choice ~ 0 | Month + Trapline + Year,   # no random effects, Year + Trapline + Month = individual-specific variables
                        reflevel = "noEvent", iterlim=1, 
                        data=m.data)
  models[[9]] <- mlogit(choice ~ 1 | Month + YearCts,  # no random effects, Month + Year = individual-specific variables
                        reflevel = "noEvent", iterlim=1, 
                        data=m.data)
  models[[10]] <- mlogit(choice ~ 1 | Month,  # no random effects, Month = individual-specific variables
                        reflevel = "noEvent", iterlim=1,
                        data=m.data)
  # year as random effect
  models[[11]] <- mlogit(choice ~ 1 | Season + YearCts,
                        rpar=c('predatorEvent:(intercept)'='n', 'otherEvent:(intercept)'='n'),  R=50, halton=NA, panel=TRUE,
                        reflevel = "noEvent", iterlim=1,
                        data=m.data.year)
  # trapline as random effect
  models[[12]] <- mlogit(choice ~ 1 | Season + YearCts,
                        rpar=c('predatorEvent:(intercept)'='n','otherEvent:(intercept)'='n'), R=50, halton=NA, panel=TRUE,
                        reflevel = "noEvent", iterlim=1,
                        data=m.data.trap)
  # trapline + yr as random effect
  models[[13]] <- mlogit(choice ~ 1 | Season + YearCts,
                        rpar=c('predatorEvent:(intercept)'='n','otherEvent:(intercept)'='n'), R=50, halton=NA, panel=TRUE,
                        reflevel = "noEvent", iterlim=1,
                        data=m.data.trapyr)
  ### summarize and rank AICs for each itteration of bootstrap
  subset_models_aic[k, ] <- ldply(models, .fun=AIC)$V1 
  subset_models_aicW[k, ] <- Weights(ldply(models, .fun=AIC)$V1)
  subset_models_logLik[k, ] <- ldply(models, .fun=logLik)$V1
}

### examine AIC and AIC weights
bestModel <- table(apply(subset_models_aicW, MARGIN=1, FUN=which.max),
                   apply(subset_models_aic, MARGIN=1, FUN=which.min))

summary(subset_models_aic)
summary(subset_models_aicW)

models_aic <- data.frame(subset_models_aic)
models_aicW <- data.frame(subset_models_aicW)
models_logLik <- data.frame(subset_models_logLik)
# create table columns
varsCol <- sapply(models, function(m) substr(as.character(formula(m)[3]), start = 5, stop = 1e6))
dfCol <- sapply(models, function(m) attr(logLik(m), "df"))
logLikCol <- sapply(models_logLik, FUN = mean)
aicCol <- sapply(models_aic, FUN = mean)
aicWcol <- sapply(models_aicW, FUN = mean)
# combine into one table and save output
bs_model_events <- data.frame(variables = varsCol, AIC = aicCol, `Weighted AIC` = aicWcol,
                               `Log Likelihood` = logLikCol, DF = dfCol)
write.csv(bs_model_events, file = '~/WERC-SC/HALE/outputs/bs_model_events_Month_eck12_20171114.csv',
          row.names = FALSE)
