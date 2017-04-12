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

seasonalEvents <- catch_EventPUE %>% 
  group_by(Season, predEvent) %>% 
  summarise(eventCount = sum(NEvents)) 
### SEASONS
# bar graphs of proportion of events happening in different seasons
catch_EventPUE$Season <- factor(catch_EventPUE$Season, levels = c("Pre-laying", "Incubation", "Nestling", "offSeason"))
season_trend <- ggplot(catch_EventPUE, aes(Season, CPUE)) +
  geom_boxplot() + #geom_bar(stat = "identity") +
  facet_wrap(~ predEvent) +
  theme_bw() 
  # scale_fill_gradient(low = "green", high = "red")
season_trend %+% subset(catch_EventPUE, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught")) # , "trapTriggered", "baitLost", "none"))

pred_seas <- ggplot(seasonalEvents) +
  geom_col(aes(x = Season, y = eventCount, fill = predEvent)) + # , position = "fill") +
  theme_bw() 
pred_seas %+% subset(seasonalEvents, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "trapTriggered", "baitLost", "none"))

seas_pred <- ggplot(catch_EventPUE, aes(predEvent, fill = Season)) +
  geom_bar(position = "fill") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=60, hjust=1)) 
seas_pred %+% subset(catch_EventPUE, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "trapTriggered", "baitLost", "none"))

### YEAR
# GRAPH annual average CPUE and SD
CPUE_yr <- ggplot(annualCPUE, aes(Year_, annCPUE)) +
  geom_errorbar(aes(ymin=annCPUE-sdCPUE, ymax=annCPUE+sdCPUE), colour="black", width=.1) +
  geom_line() +
  geom_point() + 
  facet_wrap(~ predEvent) +
  theme_bw() +
  labs(x = 'Year', y = 'Annual Frequency of Events per Unit Effort')
CPUE_yr %+% subset(annualCPUE, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "trapTriggered", "baitLost", "none"))
CPUE_yr_preds <- ggplot(annualCPUE, aes(Year_, annCPUE)) +
  geom_errorbar(aes(ymin=annCPUE-sdCPUE, ymax=annCPUE+sdCPUE), colour="black", width=.1) +
  geom_line() +
  geom_point() + 
  facet_wrap(~ predEvent, nrow = 3) +
  ylim(NA, 1) +
  theme_bw() +
  labs(x = 'Year', y = 'Annual Frequency of Events per Unit Effort')
CPUE_yr_preds %+% subset(annualCPUE, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught"))

## ggplot(mpg, aes(reorder_size(class))) + geom_bar()




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
