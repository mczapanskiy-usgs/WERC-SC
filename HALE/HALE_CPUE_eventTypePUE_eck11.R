### this script divides the number of pred events by the number of traps in the trapline for each week (CPUE proportion)

## load libraries
library("data.table", lib.loc="~/R/win-library/3.2")
library("dplyr", lib.loc="~/R/win-library/3.2")
library(lubridate)

## read in file
read.csv('~/WERC-SC/HALE/catch_traploc_weeks_baitTypes_edited_predEvent.csv',
         stringsAsFactors = FALSE) -> catch

## add variable IDing the check week for each trapline (Jan 1-7, 2000 = week 1...)
# also rank predEvent by importance (catCaught > mongooseCaught > ratCaught > mouseCaught > birdOtherCaught > baitStolen > trapTriggered > none)
catch <- mutate(catch, 
                date = mdy(date), # change data from character to POSIXct
                week = as.numeric(as.period(interval(min(date), date) %/% weeks(1))),
                predEvent = factor(predEvent, level = c('catCaught', 'mongooseCaught', 'ratCaught', 'mouseCaught', 'birdOtherCaught', 'baitStolen', 'trapTriggered', 'none'), ordered = TRUE)) 

## count the number of traps in a trapline each week
# if there are multiple predEvents in a week, choose the most important one 
weeklyCatches <- catch %>%
  group_by(Trapline, TrapNum, week) %>% 
  summarize(predEvent = min(predEvent))
# then summarize the number of Trapline events per week (aka number of traps in the trapline)
TrapsPerLineWeek <- weeklyCatches %>%
  group_by(Trapline, week) %>%
  summarize(NTraps = n())

# count number of trap events per week per trapline
PredEventsPerLineWeek <- weeklyCatches %>% 
  group_by(Trapline, week, predEvent) %>% 
  summarize(NEvents = n())

## number of predEvents per number of traps, for each week on each Trapline
PredEventPUE <- merge(TrapsPerLineWeek, PredEventsPerLineWeek) %>% 
  mutate(CPUE = NEvents/NTraps) %>% 
  arrange(Trapline, week, predEvent)

## ID how many times a trap was checked multiple times in a week (and thus only the most important trap event was chosen)
uniqueTrapsPerWeek <- catch %>% 
  group_by(Trapline, week, TrapNum) %>% 
  summarize(N = n()) %>% 
  filter(N > 1) %>% 
  arrange(-N)


# # take a look at which traps were checked multiple times in one week 
# filter(catch, Trapline == "D", week ==519, TrapNum == 11) %>% View
