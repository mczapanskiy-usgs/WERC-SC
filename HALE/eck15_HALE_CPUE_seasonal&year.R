## this script uses catch data with spatail data
## graphs of seasonal and yearly effects

library(stats)
library(data.table)
library(plyr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(mosaic)

read.csv('~/WERC-SC/HALE/catch_12_spatialCatches_20170109.csv',
         stringsAsFactors = FALSE) -> catch_spatial
# read.csv('~/WERC-SC/HALE/TraplinePredEventPUE_11_20161209.csv',
#          stringsAsFactors = FALSE) -> catch_EventPUE
# read.csv('~/WERC-SC/HALE/catch_burrows.csv',
#          stringsAsFactors = FALSE) -> burrows


### SEASONS
# bar graphs of proportion of events happening in different seasons
season_trend <- ggplot(catch_EventPUE, aes(Season, CPUE)) +
  geom_bar(stat = "identity") +
  # facet_wrap(~ predEvent) +
  theme_bw() 
  # scale_fill_gradient(low = "green", high = "red")
season_trend %+% subset(catch_EventPUE, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "trapTriggered", "baitLost", "none"))

pred_seas <- ggplot(catch_EventPUE, aes(Season, fill = predEvent)) +
  geom_bar(position = "fill") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=60, hjust=1)) 
pred_seas %+% subset(catch_EventPUE, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught")) # c("catCaught", "mongooseCaught", "ratCaught", "trapTriggered", "baitLost", "none"))

seas_pred <- ggplot(catch_EventPUE, aes(predEvent, fill = Season)) +
  geom_bar(position = "fill") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=60, hjust=1)) 
seas_pred %+% subset(catch_EventPUE, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "trapTriggered", "baitLost", "none"))


### ANNUAL TRENDS
## if there are multiple predEvents in a week, choose the most important one 
## make "weeklyCatches" a datatable containing all the factors needed for weekly analysis
weeklyCatches <- catch_spatial %>%
  group_by(Trapline, TrapNum, Year_, Week) %>%
  filter(predEvent == min(predEvent)) %>% 
  slice(1) %>% 
  ungroup

## count the number of Trapline events (weeklyCatches) per week (aka number of traps in the trapline)
trapsPerLineWeek <- weeklyCatches %>%
  group_by(Trapline, Year_, Season, Month_, Week) %>%
  summarize(NTraps = n()) %>% 
  ungroup

# count number of each predEvent per week per trapline
predEventsPerLineWeek <- weeklyCatches %>% 
  group_by(Trapline, Year_, Week, Season, Month_, predEvent) %>% 
  summarise(NEvents = n()) %>% 
  ungroup

## number of predEvents per number of traps, for each week on each Trapline
predEventPUE <- merge(trapsPerLineWeek, predEventsPerLineWeek) %>% 
  mutate(CPUE = NEvents/NTraps) %>% 
  arrange(Trapline, Year_, Week, predEvent) %>% 
  select(Trapline, Year_, Week, Season, Month_, predEvent, NTraps, NEvents, CPUE, Burrows10:MajClass)



annualBurrows <- burrows %>% 
  group_by(Year_, colony100, predEvent) %>% 
  summarise(eventCount = n()) %>% 
  complete(Year_, colony100, predEvent, fill = list(predCount = 0))
              
col_yr <- ggplot(annualBurrows, aes(Year_, predCount)) +
  geom_line(aes(colour = colony100)) +
  facet_wrap(~ predEvent, nrow = 3) +
  theme_bw()
col_yr_preds <- col_yr %+% subset(annualBurrows, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught"))


# by_predEvent <- tbl_df(burrows) %>% 
#   group_by(Year_, colony100, predEvent)
# annualBurrows <- by_predEvent %>% 
#   summarise(predcount = n())
# exp_annBurrows <- left_join(expand.grid(predEvent = levels(burrows$predEvent)), annualBurrows)
# final_counts <- exp_annBurrows[is.na(exp_annBurrows)] <- 0
# 
# 
# ## now add back in all predCount = 0
# # identify what variables should be consistent for each week
# fill <- (annualBurrows, predEvent)
#   summarise(Year_ = first(Year_))
# # make a grid of each trapline, week, predEvent
# BurrowsGrid <- expand.grid(colony100 = unique(annualBurrows$colony100), predEvent = unique(annualBurrows$predEvent)) %>% 
#   mutate(dummyPredCount = 0) %>% 
#   merge(annualBurrows, all.x = TRUE) %>% 
#   # merge(fill, all.x = TRUE, by = c('colony100', 'Week')) %>% 
#   arrange(colony100, predEvent) %>% 
#   select(colony100:predEvent, Year_ = Year.y, predCount) %>% 
#   mutate(predCount = ifelse(is.na(predCount), 0, predCount))
