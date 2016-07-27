## this script is used to further analyze CPUE by season

library("data.table", lib.loc="~/R/win-library/3.2")
library("dplyr", lib.loc="~/R/win-library/3.2")
library(mosaic)

read.csv('~/WERC-SC/HALE/catch_traploc_weeks_baitTypes_edited_weeklyPredEvent.csv',
         stringsAsFactors = FALSE) %>% 
  mutate(season = derivedFactor(
    "offSeason" = Month == 1,
    "Pre-laying" = Month >= 2,
    "Incubation" = Month >= 5,
    "Nestling" = Month >= 7,
    "offSeason" = Month >= 11,
    .method = "last", 
    .defualt = "offSeason")) -> catch_seasons

## if there are multiple predEvents in a week, choose the most important one, create small datatable for analysis
weeklySeasonalCatches <- catch_seasons %>%
  group_by(Trapline, TrapNum, Year, season, Month, week) %>% 
  summarize(predEvent = min(predEvent))

## count the number of Trapline events (weeklyCatches) per week (aka number of traps in the trapline)
seasonalTrapsPerLineWeek <- weeklySeasonalCatches %>%
  group_by(Trapline, Year, season, Month, week) %>%
  summarize(NTraps = n())

## count number of each predEvent per week per trapline
seasonalPredEventsPerLineWeek <- weeklySeasonalCatches %>% 
  group_by(Trapline, season, Month, week, predEvent) %>% 
  summarize(NEvents = n())

## number of predEvents per number of traps, for each week on each Trapline
seasonalPredEventPUE <- merge(seasonalTrapsPerLineWeek, seasonalPredEventsPerLineWeek) %>% 
  mutate(CPUE = NEvents/NTraps) %>% 
  arrange(Trapline, season, Month, week, predEvent)

#### summary stats and graphs of seasonlaPredEventPUE
# frequency of predEvents per trapline per season
seasonalTraplineCPUE <- 
  seasonalPredEventPUE %>%
  group_by(Trapline, Year, season, predEvent) %>%
  summarize(seasonalFreq = mean(CPUE)) # summarize(sumWeeklyFreq = sum(CPUE))
# frequency of predEvents per trapline per month
monthlyTraplineCPUE <- 
  seasonalPredEventPUE %>%
  group_by(Trapline, Year, Month, predEvent) %>%
  summarize(monthlyFreq = mean(CPUE)) # summarize(sumWeeklyFreq = sum(CPUE))

## plot of predEvents per trapline per month
ggplot(monthlyTraplineCPUE, aes(Month, monthlyFreq, color=Year)) + # , group = Year)) +
  geom_point() + # geom_line() +
  labs(x = 'Month', y = 'Monthly Frequency') +
  facet_wrap(~ predEvent, nrow = 4) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=60, hjust=1))
# predEvents per trapline per month, for just predators
monthlyPreds <- ggplot(monthlyTraplineCPUE, aes(Month, monthlyFreq, color=Year)) +
  geom_point() +
  labs(x = 'Month', y = 'Monthly Frequency') +
  facet_wrap(~ predEvent, nrow = 4) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=60, hjust=1))
monthlyPreds %+% subset(monthlyTraplineCPUE, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "mouseCaught"))

## plot of predEvents per trapline per season
seasonalPreds <- ggplot(seasonalTraplineCPUE, aes(season, seasonalFreq, color=Year, group = Year)) +
  geom_point() +
  geom_line() +
  labs(x = 'Season', y = 'Seasonal Frequency') +
  facet_wrap(~ predEvent, nrow = 5) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=60, hjust=1))
# predEvents per trapline per season, for just predators
seasonalPreds %+% subset(seasonalTraplineCPUE, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "mouseCaught"))




# ## determine monthly frequency of trap checks
# months  <- data.frame(table(catch$Month)) 
# sum(months$Freq) # 283192
# mutate(months, percent=(Freq/283192)*100) -> months
# 
# ## make table of HAPE seasons
# season <- data.frame(
#   season = c("offSeason", "Pre-laying", "Incubation", "Nestling"),
#   startMonth = c(11, 2, 5, 7), ## as.Date(c('2000-01-01', ...))
#   startDay = c(1, 23, 1, 1),
#   endMonth = c(2, 4, 6, 10),
#   endDay = c(23, 30, 30, 31)
# )
# 


