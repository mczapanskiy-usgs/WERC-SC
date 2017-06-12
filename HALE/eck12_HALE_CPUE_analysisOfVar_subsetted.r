## this script statistically analyzes the effect of year, trap location, trap type, and bait type on event types: PREDATOR, OTHER, NONE
## using the mlogit model
## THIS SCRIPT IS CARRIED OVER FROM eck12_HALE_CPUE_analysisOfVar.r

library(stats)
library(plyr)
library(data.table)
library(dplyr)
library(ggplot2)
library(ez)
library(mlogit)
library(mosaic)

setwd("~/WERC-SC/HALE")

read.csv('~/WERC-SC/HALE/TraplinePredEventPUE_11_20161209.csv',
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
## run function using grouped 'event' (predator, other, no) variable.
# First, aggregate the data by summing the NEvents within each event group.
data_events <- data_rev %>%
  group_by(Trapline, Week, Year, Season, Month, NTraps, loc, eventType) %>%
  dplyr::summarise(NEvents=sum(NEvents)) %>%
  mutate(CPUE = NEvents/NTraps) %>% # this line optional
  as.data.frame()
expanded_data.events <- formatData(data_events, 'eventType')
## subset 'data_events' b/c original dataset is too big
set.seed(20170612)
expanded_data.events_subset <- formatData(data_events, 'eventType', subset=55000)
expanded_data.events_subset2 <- formatData(data_events, 'eventType', subset=55000)
# head(expanded_data.events_subset)
# head(expanded_data.events_subset2)
# summary(expanded_data.events_subset)
# summary(expanded_data.events_subset2)

# # also run for 2 event types: event, no event
# data_CnoC <- data_rev %>%
#   group_by(Trapline, Week, Year, Season, Month, NTraps, loc, eventCnoC) %>%
#   dplyr::summarise(NEvents=sum(NEvents)) %>%
#   as.data.frame()
# expanded_data.CnoC<- formatData(data_CnoC)

### EVENTS ANALYSIS: dependent var = predator, other, none (events)
cpue3events <- mlogit.data(expanded_data.events %>% 
                                 # filter(loc == "front") %>%
                                 mutate(trapyr=paste0(Trapline,'-',YearCat)), # mutate(trapyr=paste0(Trapline,'-',Year)), 
                               choice="choice",
                               alt.var ="x", 
                               id.var = "trapyr",
                               shape="long", 
                               chid.var="chid")
# subsetted
cpue3events_sub <- mlogit.data(expanded_data.events_subset %>% 
                             # filter(loc == "front") %>%
                             mutate(trapyr=paste0(Trapline,'-',YearCat)), # mutate(trapyr=paste0(Trapline,'-',Year)), 
                           choice="choice",
                           alt.var ="x", 
                           id.var = "trapyr",
                           shape="long", 
                           chid.var="chid")

cpue3events_sub2 <- mlogit.data(expanded_data.events_subset2 %>% 
                              # filter(loc == "front") %>%
                              mutate(trapyr=paste0(Trapline,'-',YearCat)), # mutate(trapyr=paste0(Trapline,'-',Year)), 
                            choice="choice",
                            alt.var ="x", 
                            id.var = "trapyr",
                            shape="long", 
                            chid.var="chid")

# cpue2events <- mlogit.data(expanded_data.CnoC %>% 
#                              filter(!(Trapline %in% c('KAU', 'KW', 'LAU', 'PUU', 'SS'))) %>%# filter(loc == "front") %>%
#                              mutate(trapyr=paste0(Trapline,'-',YearCat)), # mutate(trapyr=paste0(Trapline,'-',Year)), 
#                            choice="choice",
#                            alt.var ="x", 
#                            id.var = "trapyr",
#                            shape="long", 
#                            chid.var="chid")
#### RUN mlogt MODELS
## create model list
cpue.models <- list()
## Year + Trapline + Season = individual-specific variables                 
cpue.models[[11]] <- mlogit(choice ~ 0 | Season + Trapline + Year,
                            reflevel = "noEvent",
                            iterlim=1, print.level=1,
                            data=cpue3events) # runs
## and random effects
cpue.models[[12]] <- mlogit(choice ~ 1 | Season + Trapline + Year,
                              rpar=c('predatorEvent:(intercept)'='n',
                                     'otherEvent:(intercept)'='n'),
                              R=50, halton=NA,
                              panel=TRUE, # correlation = TRUE,
                              reflevel = "noEvent",
                              iterlim=1, print.level=1,
                              data=cpue3events) # error: missing value where TRUE/FALSE needed

## REMOVED TRAPLINE AS INDIV-SPP EFFECT
# Year & Season = individual-specific variables
cpue.models[[13]] <- mlogit(choice ~ 0 | Season + YearCts,
                            reflevel = "noEvent",
                            iterlim=1, print.level=1,
                            data=cpue3events) # runs
## and random effects
cpue.models[[14]] <- mlogit(choice ~ 1 | Season + YearCts,
                              rpar=c('predatorEvent:(intercept)'='n',
                                     'otherEvent:(intercept)'='n'),
                              R=50, halton=NA,
                              panel=TRUE, # correlation = TRUE,
                              reflevel = "noEvent",
                              iterlim=1, print.level=1,
                              data=cpue3events) # error: missing value where TRUE/FALSE needed
## subsetted data
cpue.models[[15]] <- mlogit(choice ~ 1 | Season + YearCts,
                            rpar=c('predatorEvent:(intercept)'='n',
                                   'otherEvent:(intercept)'='n'),
                            R=50, halton=NA,
                            panel=TRUE, # correlation = TRUE,
                            reflevel = "noEvent",
                            iterlim=1, print.level=1,
                            data=cpue3events_sub)
cpue.models[[16]] <- mlogit(choice ~ 1 | Season + YearCts,
                            rpar=c('predatorEvent:(intercept)'='n',
                                   'otherEvent:(intercept)'='n'),
                            R=50, halton=NA,
                            panel=TRUE, # correlation = TRUE,
                            reflevel = "noEvent",
                            iterlim=1, print.level=1,
                            data=cpue3events_sub2)
# summaries
summary(cpue.models[[11]], cpue.models[[13]])
# compare AIC
AIC(cpue.models[[11]])
AIC(cpue.models[[13]])
AIC(cpue.models[[15]])
AIC(cpue.models[[16]])

# ## run on 2 events: event, no event
# cpue.models[[17]] <- mlogit(choice ~ 0 | Season + YearCts,
#                             reflevel = "none",
#                             iterlim=1, print.level=1,
#                             data=cpue2events)
# summary(cpue.models[[17]]) # runs
# # add random effect
# cpue.models[[18]] <- mlogit(choice ~ 1 | Season + YearCts,
#                               rpar=c('event:(intercept)'='n'),
#                               R=50, halton=NA,
#                               panel=TRUE, # correlation = TRUE,
#                               reflevel = "none",
#                               iterlim=1, print.level=1,
#                               data=cpue2events)
# summary(cpue.models[[18]]) # "missing value where TRUE/FALSE needed"


# view model summaries
summary(cpue.models[[23]])
AIC(cpue.models[[23]])
logLik(cpue.models[[23]])


# Trapline = random variable, Trapline, Season, Year = individual-specific variables
cpue.test <- mlogit.data(expanded_data.test, # %>%
                         # filter(loc == "front"), 
                         choice="choice",
                         alt.var ="x", 
                         id.var = "Trapline",
                         shape="long", 
                         chid.var="chid")
cpue.models[[27]] <- mlogit(choice ~ 0 | Season + YearCts,
                            rpar=c('trapTriggered:(intercept)'='n'),
                            #        'trapTriggered:(intercept)'='n'),
                            R=50, halton=NA,
                            panel=TRUE,
                            # reflevel = "ratCaught",
                            iterlim=1, print.level=1,
                            data=cpue.test)



