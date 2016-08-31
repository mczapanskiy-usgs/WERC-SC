### this script divides the number of pred events by the number of traps in the trapline for each week (CPUE proportion)

## load libraries
library("data.table", lib.loc="~/R/win-library/3.2")
library("dplyr", lib.loc="~/R/win-library/3.2")
library(lubridate)
library(mosaic)

## read in file
read.csv('~/WERC-SC/HALE/catch_traploc_weeks_baitTypes_edited_predEvent.csv',
         stringsAsFactors = FALSE) -> raw_catch
catch <- mutate(raw_catch, 
                date = mdy(date), 
                Week = interval(min(date), date))
## add variable IDing the check week so all trapsin a given week are group together by trapline (Jan 1-7, 2000 = week 1...)
# also rank predEvent by importance (catCaught > mongooseCaught > ratCaught > mouseCaught > birdOtherCaught > baitLost > trapTriggered > none)
catch <- mutate(raw_catch, 
                date = mdy(date), # change data from character to POSIXct
                Week = as.numeric(as.period(lubridate::interval(min(date), date) %/% weeks(1))), #lubridate function
                predEvent = factor(predEvent, 
                                   level = c('catCaught', 'mongooseCaught', 'ratCaught', 'mouseCaught', 'birdOtherCaught', 'baitLost', 'trapTriggered', 'none'), 
                                   ordered = TRUE), 
                Season = derivedFactor(
                          "offSeason" = Month == 1,
                          "Pre-laying" = Month >= 2,
                          "Incubation" = Month >= 5,
                          "Nestling" = Month >= 7,
                          "offSeason" = Month >= 11,
                          .method = "last", 
                          .default = "offSeason"))

## if there are multiple predEvents in a week, choose the most important one 
weeklyCatches <- catch %>%
  group_by(Trapline, TrapNum, Season, Year, Month, Week) %>% 
  summarize(predEvent = min(predEvent))

## count the number of Trapline events (weeklyCatches) per week (aka number of traps in the trapline)
trapsPerLineWeek <- weeklyCatches %>%
  group_by(Trapline, Season, Year, Month, Week) %>%
  summarize(NTraps = n())

# count number of each predEvent per week per trapline
predEventsPerLineWeek <- weeklyCatches %>% 
  group_by(Trapline, Season, Month, Week, predEvent) %>% 
  summarize(NEvents = n())

## number of predEvents per number of traps, for each week on each Trapline
predEventPUE <- merge(trapsPerLineWeek, predEventsPerLineWeek) %>% 
  mutate(CPUE = NEvents/NTraps) %>% 
  arrange(Trapline, Season, Year, Month, Week, predEvent)

## data validation: ID how many times a trap was checked multiple times in a week (and thus only the most important trap event was chosen)
uniqueTrapsPerWeek <- catch %>% 
  filter(!TrapChecked) # first remove dates when trap hadn't been checked in >14 days
  group_by(Trapline, Week, TrapNum) %>% 
  summarize(N = n()) %>% 
  filter(N > 1) %>% 
  arrange(-N)

## save predEventPUE data file to GitHub file
write.csv(predEventPUE, file = '~/WERC-SC/HALE/TraplinePredEventPUE.csv',
            row.names = FALSE) 
# # weekly catch for each trap
# write.csv(weeklyCatches, file = '~/WERC-SC/HALE/TraplineWeeklyCatches.csv',
#           row.names = FALSE) 
  
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

## seasonal trends
# frequency of predEvents per trapline per season
seasonalTraplineCPUE <- predEventPUE %>%
  group_by(Trapline, Year, season, predEvent) %>%
  summarize(seasonalFreq = mean(CPUE)) ## does this make sense?
# frequency of predEvents per trapline per month
monthlyTraplineCPUE <- predEventPUE %>%
  group_by(Trapline, Year, Month, predEvent) %>%
  summarize(monthlyFreq = mean(CPUE)) ## does this make sense?

## plot of predEvents per trapline per month
ggplot(monthlyTraplineCPUE, aes(Month, monthlyFreq, color=Year)) + # , group = Year)) +
  geom_point() + # geom_line() +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12)) +
  labs(x = 'Month', y = 'Monthly Frequency') +
  facet_wrap(~ predEvent, nrow = 4) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=60, hjust=1))
# predEvents per trapline per month, for just predators
monthlyPreds <- ggplot(monthlyTraplineCPUE, aes(Month, monthlyFreq)) +
  geom_point() +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12)) + # theme(axis.text.x = element_text(angle=60, hjust=1))
  labs(x = 'Month', y = 'Monthly Frequency') +
  facet_wrap(~ predEvent, nrow = 4) +
  theme_bw() 
monthlyPreds %+% subset(monthlyTraplineCPUE, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "mouseCaught"))

## plot of predEvents per trapline per season
seasonalPreds <- ggplot(TraplineCPUE, aes(season, seasonalFreq, color=Year, group = Year)) +
  geom_point() +
  geom_line() +
  labs(x = 'Season', y = 'Seasonal Frequency') +
  facet_wrap(~ predEvent, nrow = 5) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=60, hjust=1))

# # predEvents per trapline per season, for just predators
# seasonalPreds %+% subset(seasonalTraplineCPUE, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "mouseCaught"))

# 
# # histogram of pred events per trapline
# ggplot(predEventPUE, aes(CPUE)) +
#   geom_bar() +
#   labs(x = 'Predator event type', y = 'frequency') +
#   facet_wrap(~ Trapline, nrow = 4) +
#   theme(axis.text.x = element_text(angle=60, hjust=1))


# # take a look at which traps were checked multiple times in one week 
# filter(catch, Trapline == "D", week ==519, TrapNum == 11) %>% View
