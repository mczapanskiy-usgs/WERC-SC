## this script statistically analyzes the effect of year, trap location, trap type, and bait type 
## on event types: PREDATOR, OTHER, NONE
## using the mlogit model on subset of data determined in "eck12_HALE_CPUE_analysisOfVar_subsetted.r"
## THIS SCRIPT IS CARRIED OVER FROM eck12_HALE_CPUE_analysisOfVar.r

library(stats)
library(plyr)
library(data.table)
library(dplyr)
library(ggplot2)
library(ez)
library(mlogit)
# library(mosaic)

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
expanded_data.events <- formatData(data_events, 'eventType') # expanded_data.events_old <- formatData_old(data_events)
## subset 'data_events' b/c original dataset is too big
set.seed(20170620)
expanded_data.events_subset2 <- formatData(data_events, 'eventType', subset=55000)

### expand data to compare models
# no random effects
# trapline + yr as random effect
cpue3events_trapyr_sub2 <- mlogit.data(expanded_data.events_subset2 %>% 
                                         mutate(trapyr=paste0(Trapline,'-',YearCat)) %>% 
                                         filter(!(Trapline %in% c('KAU', 'KW', 'LAU', 'PUU', 'SS'))),  
                                       choice="choice",
                                       alt.var ="x", 
                                       id.var = "trapyr",
                                       shape="long", 
                                       chid.var="chid")

#### RUN mlogt MODELS
## create model list
cpue.models <- list()
cpue.models[[40]] <- mlogit(choice ~ 1 | Season + YearCts,
                            rpar=c('predatorEvent:(intercept)'='n',
                                   'otherEvent:(intercept)'='n'),
                            R=50, halton=NA,
                            panel=TRUE, 
                            reflevel = "noEvent",
                            iterlim=1, print.level=1,
                            data=cpue3events_trapyr_sub2)


#### ANALYSIS
# view model summaries
summary(cpue.models[[40]])
AIC(cpue.models[[40]])
logLik(cpue.models[[40]])
write.table(exp(coefficients(cpue.models[[40]])))


### analyze results for model 23 (the best fit for the "caughts_only" data)
## get fitted frequencies of each event type on unique combos of Trapline, Year, & Season
myfitted <- fitted(cpue.models[[40]], outcome=FALSE)
# head(myfitted)
# dim(myfitted)
# dim(expanded_data.Caughts_only)

## select year, season, and trapline data for the fitted values
# Copy data and thin it down to one row per chid (i.e. 33543 rows becomes 11181; because myfitted (above) is already thinned to 11181 rows)
fitted_cpue_eventSub <- expanded_data.events_subset2 %>%
  select(chid, Trapline, Year, Season) %>%
  unique()
# then `cbind` the data in `fitted_cpue_eventSub` with the fitted values in `myfitted`
fitted_cpue_eventSub <- cbind(fitted_cpue_eventSub, myfitted) %>%
  # thin the fitted values further (i.e. remove replicates and keep the unique combos of Trapline, Year, & Season)
  select(-chid) %>%
  unique()

write.csv(fitted_cpue_eventSub, file = '~/WERC-SC/HALE/fitted_cpue_model40.csv',
          row.names = FALSE)

