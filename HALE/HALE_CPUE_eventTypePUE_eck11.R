### this script divides the number of pred events by the number of traps in the trapline for each week (CPUE proportion)

## load libraries
library("data.table", lib.loc="~/R/win-library/3.2")
library("dplyr", lib.loc="~/R/win-library/3.2")
library(lubridate)

## read in file
read.csv('~/WERC-SC/HALE/catch_traploc_weeks_baitTypes_edited_predEvent.csv',
         stringsAsFactors = FALSE) -> catch

## add variable IDing the check week so all trapsin a given week are group together by trapline (Jan 1-7, 2000 = week 1...)
# also rank predEvent by importance (catCaught > mongooseCaught > ratCaught > mouseCaught > birdOtherCaught > baitLost > trapTriggered > none)
catch <- mutate(catch, 
                date = mdy(date), # change data from character to POSIXct
                week = as.numeric(as.period(interval(min(date), date) %/% weeks(1))), #lubridate function
                predEvent = factor(predEvent, level = c('catCaught', 'mongooseCaught', 'ratCaught', 'mouseCaught', 'birdOtherCaught', 'baitLost', 'trapTriggered', 'none'), ordered = TRUE)) 

## if there are multiple predEvents in a week, choose the most important one 
weeklyCatches <- catch %>%
  group_by(Trapline, TrapNum, Year, week) %>% 
  summarize(predEvent = min(predEvent))

## count the number of Trapline events (weeklyCatches) per week (aka number of traps in the trapline)
trapsPerLineWeek <- weeklyCatches %>%
  group_by(Trapline, Year, week) %>%
  summarize(NTraps = n())
# count number of each predEvent per week per trapline
predEventsPerLineWeek <- weeklyCatches %>% 
  group_by(Trapline, week, predEvent) %>% 
  summarize(NEvents = n())

## number of predEvents per number of traps, for each week on each Trapline
predEventPUE <- merge(trapsPerLineWeek, predEventsPerLineWeek) %>% 
  mutate(CPUE = NEvents/NTraps) %>% 
  arrange(Trapline, week, predEvent)

## data validation: ID how many times a trap was checked multiple times in a week (and thus only the most important trap event was chosen)
uniqueTrapsPerWeek <- catch %>% 
  filter(!TrapChecked) # first remove dates when trap hadn't been checked in >14 days
  group_by(Trapline, week, TrapNum) %>% 
  summarize(N = n()) %>% 
  filter(N > 1) %>% 
  arrange(-N)

### summary stats and graphs of predEventPUE
# frequency of predEvents per trapline per year
traplineCPUE <- 
  predEventPUE %>%
  group_by(Trapline, Year, predEvent) %>%
  summarize(annualFreq = mean(CPUE)) # summarize(sumWeeklyFreq = sum(CPUE))
  
# plot of predEvents per trapline per year
ggplot(traplineCPUE, aes(Year, annualFreq, color=predEvent)) +
  geom_point() +
  labs(x = 'Year', y = 'Annual Frequency of Events per Unit Effort') +
  facet_wrap(~ Trapline, nrow = 4) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=60, hjust=1))

#plot cat, rat, mongoose or mice caught per trapline per year
preds <- ggplot(traplineCPUE, aes(Year, annualFreq, color=predEvent)) +
  geom_point() +
  labs(x = 'Year', y = 'Annual Frequency of Events per Unit Effort') +
  facet_wrap(~ Trapline, nrow = 4) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  ylim(0, 0.4)
preds %+% subset(traplineCPUE, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "mouseCaught"))

# 
# # histogram of pred events per trapline
# ggplot(predEventPUE, aes(CPUE)) +
#   geom_bar() +
#   labs(x = 'Predator event type', y = 'frequency') +
#   facet_wrap(~ Trapline, nrow = 4) +
#   theme(axis.text.x = element_text(angle=60, hjust=1))


# # take a look at which traps were checked multiple times in one week 
# filter(catch, Trapline == "D", week ==519, TrapNum == 11) %>% View
