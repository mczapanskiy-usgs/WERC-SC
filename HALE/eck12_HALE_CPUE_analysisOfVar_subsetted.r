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

#### CREATE LONG DATA TABLES
## run function using grouped 'event' (predator, other, no) variable.
# First, aggregate the data by summing the NEvents within each event group.
data_events <- data_rev %>%
  group_by(Trapline, Week, Year, Season, Month, NTraps, loc, eventType) %>%
  dplyr::summarise(NEvents=sum(NEvents)) %>%
  mutate(CPUE = NEvents/NTraps) %>% # this line optional
  as.data.frame()
expanded_data.events <- formatData(data_events, 'eventType') # expanded_data.events_old <- formatData_old(data_events)


#### BOOTSTRAP SUBSETS OF EVENTTYPE DATA FOR ANALYSIS
nb = 10 # number of bootstraps
s = 5000 # size of subset
cpue.models.sub1 <- matrix(NA, ncol=2, nrow=nb) # nrow = number of models

for (k in 1:nb) { 
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
  ### 
  cpue.models.sub1[k, ] <- Weights(ldply(models, .fun=AIC)$V1)
}

bestModel <- table(apply(cpue.models.sub1, MARGIN=1, FUN=which.max))




### EVENTS ANALYSIS: dependent var = predator, other, none (events)
cpue3events <- mlogit.data(expanded_data.events %>% 
                             filter(!(Trapline %in% c('KAU', 'KW', 'LAU', 'PUU', 'SS'))), 
                                choice="choice",
                                alt.var ="x", 
                                shape="long", 
                                chid.var="chid")
cpue3events_trapyr <- mlogit.data(expanded_data.events %>% 
                                    mutate(trapyr=paste0(Trapline,'-',YearCat)) %>% 
                                    filter(!(Trapline %in% c('KAU', 'KW', 'LAU', 'PUU', 'SS'))),  
                               choice="choice",
                               alt.var ="x", 
                               id.var = "trapyr",
                               shape="long", 
                               chid.var="chid")
# subsetted
cpue3events_sub <- mlogit.data(expanded_data.events_subset %>% 
                                 mutate(trapyr=paste0(Trapline,'-',YearCat)) %>% 
                                 filter(!(Trapline %in% c('KAU', 'KW', 'LAU', 'PUU', 'SS'))),
                                       choice="choice",
                                       alt.var ="x", 
                                       id.var = "trapyr",
                                       shape="long", 
                                       chid.var="chid")
cpue3events_sub2 <- mlogit.data(expanded_data.events_subset2 %>% 
                                  mutate(trapyr=paste0(Trapline,'-',YearCat)) %>% 
                                  filter(!(Trapline %in% c('KAU', 'KW', 'LAU', 'PUU', 'SS'))),  
                                       choice="choice",
                                       alt.var ="x", 
                                       id.var = "trapyr",
                                       shape="long", 
                                       chid.var="chid")
cpue3events_sub3 <- mlogit.data(expanded_data.events_subset3 %>% 
                                  mutate(trapyr=paste0(Trapline,'-',YearCat)) %>% 
                                  filter(!(Trapline %in% c('KAU', 'KW', 'LAU', 'PUU', 'SS'))),  
                                       choice="choice",
                                       alt.var ="x", 
                                       id.var = "trapyr",
                                       shape="long", 
                                       chid.var="chid")

#### CHOSEN DATASET: SUBSET 2
### expand data to compare models
# no random effects
cpue3events_sub2 <- mlogit.data(expanded_data.events_subset2,  
                                     choice="choice",
                                     alt.var ="x", 
                                     shape="long", 
                                     chid.var="chid")
# year as random effect
cpue3events_year_sub2 <- mlogit.data(expanded_data.events_subset2, 
                                     choice="choice",
                                     alt.var ="x", 
                                     id.var = "YearCat",
                                     shape="long", 
                                     chid.var="chid")
# trapline as random effect
cpue3events_trap_sub2 <- mlogit.data(expanded_data.events_subset2,  
                                     choice="choice",
                                     alt.var ="x", 
                                     id.var = "Trapline",
                                     shape="long", 
                                     chid.var="chid")

# trapline + yr as random effect
cpue3events_trapyr_sub2 <- mlogit.data(expanded_data.events_subset2 %>% 
                                       mutate(trapyr=paste0(Trapline,'-',YearCat)),  
                                     choice="choice",
                                     alt.var ="x", 
                                     id.var = "trapyr",
                                     shape="long", 
                                     chid.var="chid")




#### RUN mlogt MODELS
## create model list
cpue.models <- list()
## Year + Trapline + Season = individual-specific variables                 
cpue.models[[11]] <- mlogit(choice ~ 0 | Season + Trapline + Year,
                            reflevel = "noEvent",
                            iterlim=1, print.level=1,
                            data=cpue3events) # runs
## and random effect
cpue.models[[12]] <- mlogit(choice ~ 1 | Season + Trapline + Year,
                              rpar=c('predatorEvent:(intercept)'='n',
                                     'otherEvent:(intercept)'='n'),
                              R=50, halton=NA,
                              panel=TRUE, # correlation = TRUE,
                              reflevel = "noEvent",
                              iterlim=1, print.level=1,
                              data=cpue3events_trap) # error: missing value where TRUE/FALSE needed

## REMOVED TRAPLINE AS INDIV-SPP EFFECT
# Year & Season = individual-specific variables
cpue.models[[13]] <- mlogit(choice ~ 0 | Season + YearCts,
                            reflevel = "noEvent",
                            iterlim=1, print.level=1,
                            data=cpue3events) # runs
## and random effect
cpue.models[[14]] <- mlogit(choice ~ 1 | Season + YearCts,
                              rpar=c('predatorEvent:(intercept)'='n',
                                     'otherEvent:(intercept)'='n'),
                              R=50, halton=NA,
                              panel=TRUE, # correlation = TRUE,
                              reflevel = "noEvent",
                              iterlim=1, print.level=1,
                              data=cpue3events_trap) # error: missing value where TRUE/FALSE needed
## subsetted data
cpue.models[[15]] <- mlogit(choice ~ 1 | Season + YearCts,
                            rpar=c('predatorEvent:(intercept)'='n',
                                   'otherEvent:(intercept)'='n'),
                            R=50, halton=NA,
                            panel=TRUE,
                            reflevel = "noEvent",
                            iterlim=1, print.level=1,
                            data=cpue3events_sub)
cpue.models[[16]] <- mlogit(choice ~ 1 | Season + YearCts,
                            rpar=c('predatorEvent:(intercept)'='n',
                                   'otherEvent:(intercept)'='n'),
                            R=50, halton=NA,
                            panel=TRUE, 
                            reflevel = "noEvent",
                            iterlim=1, print.level=1,
                            data=cpue3events_sub2)
cpue.models[[17]] <- mlogit(choice ~ 1 | Season + YearCts,
                            rpar=c('predatorEvent:(intercept)'='n',
                                   'otherEvent:(intercept)'='n'),
                            R=50, halton=NA,
                            panel=TRUE,
                            reflevel = "noEvent",
                            iterlim=1, print.level=1,
                            data=cpue3events_sub3)
# compare AIC
AIC(cpue.models[[11]])
AIC(cpue.models[[13]])
AIC(cpue.models[[15]])
AIC(cpue.models[[16]])
AIC(cpue.models[[17]])

### compare models for dataset subset 2
# start at model 39
# without any random effects, Year + Trapline + Season = individual-specific variables                 
cpue.models[[39]] <- mlogit(choice ~ 0 | Season + Trapline + Year,
                            reflevel = "noEvent",
                            iterlim=1, print.level=1,
                            data=cpue3events_sub2) # runs
# without any random effects, Season + Year = individual-specific variables
cpue.models[[40]] <- mlogit(choice ~ 1 | Season + YearCts,
                            reflevel = "noEvent",
                            iterlim=1, print.level=1,
                            data=cpue3events_sub2)
# without any random effects, Season = individual-specific variables
cpue.models[[41]] <- mlogit(choice ~ 1 | Season,
                            reflevel = "noEvent",
                            iterlim=1, print.level=1,
                            data=cpue3events_sub2)
# without any random effects, Year = individual-specific variables
cpue.models[[42]] <- mlogit(choice ~ 1 | YearCts,
                            reflevel = "noEvent",
                            iterlim=1, print.level=1,
                            data=cpue3events_sub2)
# year as random effect
cpue.models[[43]] <- mlogit(choice ~ 1 | Season + YearCts,
                            rpar=c('predatorEvent:(intercept)'='n',
                                   'otherEvent:(intercept)'='n'),
                            R=50, halton=NA,
                            panel=TRUE, 
                            reflevel = "noEvent",
                            iterlim=1, print.level=1,
                            data=cpue3events_year_sub2)
# trapline as random effect
cpue.models[[44]] <- mlogit(choice ~ 1 | Season + YearCts,
                            rpar=c('predatorEvent:(intercept)'='n',
                                   'otherEvent:(intercept)'='n'),
                            R=50, halton=NA,
                            panel=TRUE, 
                            reflevel = "noEvent",
                            iterlim=1, print.level=1,
                            data=cpue3events_trap_sub2)
# trapline + year as random effects
cpue.models[[45]] <- mlogit(choice ~ 1 | Season + YearCts,
                            rpar=c('predatorEvent:(intercept)'='n',
                                   'otherEvent:(intercept)'='n'),
                            R=50, halton=NA,
                            panel=TRUE, 
                            reflevel = "noEvent",
                            iterlim=1, print.level=1,
                            data=cpue3events_trapyr_sub2)


# compare AIC
AIC(cpue.models[[39]]) # no random effects, Year + Trapline + Season = individual-specific variables
logLik(cpue.models[[39]])
AIC(cpue.models[[40]]) # no random effects, Season + Year = individual-specific variables
logLik(cpue.models[[40]])
AIC(cpue.models[[41]])# no random effects, Season = individual-specific variables
logLik(cpue.models[[41]])
AIC(cpue.models[[42]]) # no random effects, Year = individual-specific variables
logLik(cpue.models[[42]])
AIC(cpue.models[[43]]) # year as random effect, Season + Year = individual-specific variables
logLik(cpue.models[[43]])
AIC(cpue.models[[44]]) # trapline as random effect, Season + Year = individual-specific variables
logLik(cpue.models[[44]])
AIC(cpue.models[[45]]) # trapline + year as random effects, Season + Year = individual-specific variables
logLik(cpue.models[[45]])




