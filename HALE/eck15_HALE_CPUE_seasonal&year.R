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
read.csv('~/WERC-SC/HALE/TraplinePredEventPUE_11_20161209.csv',
         stringsAsFactors = FALSE) -> catch_EventPUE
read.csv('~/WERC-SC/HALE/catch_burrows.csv',
         stringsAsFactors = FALSE) -> burrows


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
annualBurrows <- burrows %>% 
  group_by(Year_, colony100, predEvent) %>% 
  summarise(predCount = n()) %>% 
  complete(Year_, colony100, predEvent, fill = list(predCount = 0))
              
by_predEvent <- tbl_df(burrows) %>% 
  group_by(Year_, colony100, predEvent)
annualBurrows <- by_predEvent %>% 
  summarise(predcount = n())
exp_annBurrows <- left_join(expand.grid(predEvent = levels(burrows$predEvent)), annualBurrows)
final_counts <- exp_annBurrows[is.na(exp_annBurrows)] <- 0





## now add back in all predCount = 0
# identify what variables should be consistent for each week
fill <- (annualBurrows, predEvent)
  summarise(Year_ = first(Year_))
# make a grid of each trapline, week, predEvent
BurrowsGrid <- expand.grid(colony100 = unique(annualBurrows$colony100), predEvent = unique(annualBurrows$predEvent)) %>% 
  mutate(dummyPredCount = 0) %>% 
  merge(annualBurrows, all.x = TRUE) %>% 
  # merge(fill, all.x = TRUE, by = c('colony100', 'Week')) %>% 
  arrange(colony100, predEvent) %>% 
  select(colony100:predEvent, Year_ = Year.y, predCount) %>% 
  mutate(predCount = ifelse(is.na(predCount), 0, predCount))
