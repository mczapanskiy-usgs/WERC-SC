## this script statistically analyzes the effect of year, trap location, trap type, and bait type on predator event


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
formatData <- function(data){
  # Reshape data so that there is one row for every option, for every choice situation. 
  # Here are the possible outcomes every time the trap is set:
  events <- unique(data$predEvent) # events <- unique(data$eventType) # events <- unique(data$eventCnoC) # 
  # And here are the number of choice situations:
  nEvents <- sum(data$NEvents)
  
  # Replicate the rows according to number of events:
  data2 <- data[rep(row.names(data), data$NEvents),]
  data2 <- data2 %>%
    mutate(chid = row.names(data2))
  
  # Expand each choice situation so that each alternative is on its own row.  
  # Do this with the merge function.  The alternative names will be stored in column `x`.
  expanded_data <- merge(events, data2)
  expanded_data <- expanded_data %>% 
    mutate(choice = ifelse(x==predEvent, TRUE, FALSE),  # mutate(choice = ifelse(x==eventType, TRUE, FALSE), # mutate(choice = ifelse(x==eventCnoC, TRUE, FALSE),  
           YearCat = as.factor(Year_),
           YearCts = as.numeric(Year_))
  return(expanded_data)
}

#### CREATE LONG DATA TABLES
# e <- "predEvent"
expanded_data_WL <- formatData(data_rev_WL)

## with only predator data
data.Caughts_only_WL <- data_rev_WL %>% 
  filter(eventType == "predatorEvent")
expanded_data.Caughts_only_WL <- formatData(data.Caughts_only_WL)


## run function using grouped 'event' (predator, other, no) variable.
# First, aggregate the data by summing the NEvents within each event group.
data_events_WL <- data_rev_WL %>%
  group_by(Trapline, Week, Year, Season, Month, NTraps, loc, eventType) %>%
  dplyr::summarise(NEvents=sum(NEvents)) %>%
  mutate(CPUE = NEvents/NTraps) %>% # this line optional
  as.data.frame()
expanded_data.events_WL <- formatData(data_events_WL)
# also run for 2 event types: event, no event
data_CnoC_WL <- data_rev_WL %>%
  group_by(Trapline, Week, Year, Season, Month, NTraps, loc, eventCnoC) %>%
  dplyr::summarise(NEvents=sum(NEvents)) %>%
  as.data.frame()
expanded_data.CnoC_WL<- formatData(data_CnoC_WL)

#### RUN mlogt MODELS
## create model list
cpue_WL.models <- list()
### PREDS ONLY ANALYSIS: compare 3 random effect options

cpue.caughts.WL <- mlogit.data(expanded_data.Caughts_only_WL,
                                  # %>% filter(loc == "front"), # %>% filter(Trapline %in% c('A')) # >>> too many zeros
                                  choice="choice",
                                  alt.var ="x", 
                                  id.var = "Trapline",
                                  shape="long", 
                                  chid.var="chid")
## LUNAR
cpue_WL.models[[1]] <- mlogit(choice ~ 0 | Season + MoonTime1wk + MoonIllum1wk,
                            rpar=c('ratCaught:(intercept)'='n',
                                   'mongooseCaught:(intercept)'='n'),
                            R=50, halton=NA,
                            panel=TRUE,
                            iterlim=1, print.level=1,
                            data=cpue.caughts.WL) 
# WEATHER
cpue_WL.models[[2]] <- mlogit(choice ~ 0 | Season + total3monRain + totalWeekRain,
                            rpar=c('ratCaught:(intercept)'='n',
                                   'mongooseCaught:(intercept)'='n'),
                            R=50, halton=NA,
                            panel=TRUE,
                            iterlim=1, print.level=1,
                            data=cpue.caughts.WL)
## 
cpue_WL.models[[3]] <- mlogit(choice ~ 0 | Season + YearCts,
                            rpar=c('ratCaught:(intercept)'='n',
                                   'mongooseCaught:(intercept)'='n'),
                            R=50, halton=NA,
                            panel=TRUE,
                            iterlim=1, print.level=1,
                            data=cpue.caughts.WL)
# 
cpue_WL.models[[4]] <- mlogit(choice ~ 0 | Season + YearCts + Trapline,
                            iterlim=1, print.level=1,
                            data=cpue.caughts.WL) 
