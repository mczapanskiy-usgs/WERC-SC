## this script statistically analyzes the effect of year, trap location, trap type, and bait type 
## on event types: PREDATOR, OTHER, NONE
## using the mlogit model on subset of data determined in "eck12_HALE_CPUE_analysisOfVar_subsetted.r"
## THIS SCRIPT IS CARRIED OVER FROM "eck12_HALE_CPUE_analysisOfVar.r"

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
expanded_data_events <- formatData(data_events, 'eventType', subset=NA) 
## subset 'data_events' b/c original dataset is too big
set.seed(20170628)
expanded_data_events_subset <- formatData(data_events, 'eventType', subset=50000)

### CHOSEN MODEL
# trapline + yr as random effect, Season + Year as independent-specific events
cpue_events_trapyr <- mlogit.data(expanded_data_events_subset %>% 
                                         mutate(trapyr=paste0(Trapline,'-',YearCat)),  
                                       choice="choice",
                                       alt.var ="x", 
                                       id.var = "trapyr",
                                       shape="long", 
                                       chid.var="chid")
cpue_model_events_trapyr <- mlogit(choice ~ 1 | Season + YearCts,
                            rpar=c('predatorEvent:(intercept)'='n',
                                   'otherEvent:(intercept)'='n'),
                            R=50, halton=NA,
                            panel=TRUE, 
                            reflevel = "noEvent",
                            iterlim=1, print.level=1,
                            data=cpue_events_trapyr)
# view model summaries
summary(cpue_model_events_trapyr)
AIC(cpue_model_events_trapyr)
logLik(cpue_model_events_trapyr)

### ANALYZE RESULTS
## get fitted frequencies of each event type on unique combos of Trapline, Year, & Season
myfitted <- fitted(cpue_model_events_trapyr, outcome=FALSE)
# head(myfitted) # dim(myfitted) # dim(expanded_data.events.subset)

## select year, season, and trapline data for the fitted values
# Copy data and thin it down to one row per chid
fitted_cpue_event_sub <- expanded_data_events_subset %>%
  select(chid, Trapline, Year, Season) %>%
  unique()
# then `cbind` the data in `fitted_cpue_eventSub` with the fitted values in `myfitted`
fitted_cpue_event_sub <- cbind(fitted_cpue_event_sub, myfitted) %>%
  # thin the fitted values further (i.e. remove replicates and keep the unique combos of Trapline, Year, & Season)
  select(-chid) %>%
  unique()

write.csv(fitted_cpue_event_sub, file = '~/WERC-SC/HALE/fitted_cpue_event_sub.csv',
          row.names = FALSE)

### graphs and analysis in "eck17_HALE_CPUE_SeasonYear_mlogit_analysis"
