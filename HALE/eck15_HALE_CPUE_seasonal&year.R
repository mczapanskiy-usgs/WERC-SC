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

read.csv('~/WERC-SC/HALE/catch_11.5_spatialCatches_20170109.csv',
         stringsAsFactors = FALSE) -> catch_spatial
read.csv('~/WERC-SC/HALE/TraplinePredEventPUE_11_20161209.csv',
         stringsAsFactors = FALSE) -> catch_EventPUE
read.csv('~/WERC-SC/HALE/catch_burrows.csv',
         stringsAsFactors = FALSE) -> burrows

seasonalEvents <- catch_EventPUE %>% 
  group_by(Season, predEvent) %>% 
  summarise(eventCount = sum(NEvents)) 
### SEASONS
catch_EventPUE$Season <- factor(catch_EventPUE$Season, levels = c("Pre-laying", "Incubation", "Nestling", "offSeason"))
catch_EventPUE$predEvent <- factor(catch_EventPUE$predEvent, levels = c("catCaught", "ratCaught", "mongooseCaught", "mouseCaught", "birdOtherCaught", "baitLost", "trapTriggered", "none"))
# box plot of CPUE in different seasons
season_box <- ggplot(catch_EventPUE, aes(Season, CPUE)) +
  geom_boxplot() + #geom_bar(stat = "identity") +
  facet_wrap(~ predEvent) +
  theme_bw() 
  # scale_fill_gradient(low = "green", high = "red")
season_box %+% subset(catch_EventPUE, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught")) # , "trapTriggered", "baitLost", "none"))

# bar graphs of proportion of events happening in different seasons
season_bar <- ggplot(catch_EventPUE, aes(Season, fill = predEvent)) + # season_bar <- ggplot(arrange(catch_EventPUE, Season), aes(Season, fill = predEvent)) +
  geom_bar(position = "fill") +
  theme_bw() 
  # theme(axis.text.x = element_text(angle=60, hjust=1)) 
season_bar %+% subset(catch_EventPUE, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "trapTriggered", "baitLost", "none"))

season_bar_pred <- ggplot(catch_EventPUE, aes(Season, fill = predEvent)) +
  geom_bar(position = "fill") +
  theme_bw()
season_bar_pred %+% subset(catch_EventPUE, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught"))


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


### TRAPLINE



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
