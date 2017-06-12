## this script uses catch data with spatail data
## graphs of seasonal and yearly effects

library(stats)
library(plyr)
library(dplyr)
library(data.table)
library(tidyr)
library(stringr)
library(ggplot2)
library(mosaic)

read.csv('~/WERC-SC/HALE/catch_11.5_spatialCatches_20170109.csv',
         stringsAsFactors = FALSE) -> catch_spatial
read.csv('~/WERC-SC/HALE/TraplinePredEventPUE_11_20161209.csv',
         stringsAsFactors = FALSE) -> catch_EventPUE
read.csv('~/WERC-SC/HALE/fitted_cpue_model23.csv',
         stringsAsFactors = FALSE) -> fitted_cpue

fitted_cpue_lg <- melt(fitted_cpue, id.vars = c("Trapline", "Year", "Season"),
                       measure.vars = c("catCaught", "mongooseCaught", "ratCaught"))

### SEASONS
seasonalEvents <- catch_EventPUE %>% 
  group_by(Season, predEvent) %>% 
  summarise(eventCount = sum(NEvents)) 
# change variable type
catch_EventPUE$Season <- factor(catch_EventPUE$Season, levels = c("Pre-laying", "Incubation", "Nestling", "offSeason"))
catch_EventPUE$predEvent <- factor(catch_EventPUE$predEvent, levels = c("catCaught", "ratCaught", "mongooseCaught", "mouseCaught", "birdOtherCaught", "baitLost", "trapTriggered", "none"))
# box plot of CPUE in different seasons
season_box <- ggplot(catch_EventPUE, aes(Season, CPUE)) +
  geom_boxplot() + #geom_bar(stat = "identity") +
  facet_wrap(~ predEvent) +
  theme_bw() 
  # scale_fill_gradient(low = "green", high = "red")
season_box %+% subset(catch_EventPUE, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught")) # , "trapTriggered", "baitLost", "none"))

## bar graphs of proportion of events happening in different seasons
season_bar <- ggplot(catch_EventPUE, aes(Season, fill = predEvent)) + # season_bar <- ggplot(arrange(catch_EventPUE, Season), aes(Season, fill = predEvent)) +
  geom_bar(position = "fill") +
  theme_bw() 
  # theme(axis.text.x = element_text(angle=60, hjust=1)) 
season_bar %+% subset(catch_EventPUE, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "trapTriggered", "baitLost", "none"))
# preds only
season_bar_pred <- ggplot(catch_EventPUE, aes(Season, fill = predEvent)) +
  geom_bar(position = "fill") +
  theme_bw()
season_bar_pred %+% subset(catch_EventPUE, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught"))

### use fitted results of mlogit model23 to graph fitted frequencies
fit_preds_year <- ggplot(fitted_cpue_lg, aes(x=Year, y=value, color=variable) +
                             geom_line())


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


# cSeason <- ggplot(fitted_cpue, aes(Season, catCaught))
# cSeason + geom_boxplot()
# 
# rSeason <- ggplot(fitted_cpue, aes(Season, ratCaught))
# rSeason + geom_boxplot()
# 
# mSeason <- ggplot(fitted_cpue, aes(Season, mongooseCaught))
# mSeason + geom_boxplot()
# 
# cTrapline <- ggplot(fitted_cpue, aes(Trapline, catCaught))
# cTrapline + geom_boxplot()
# 
# rTrapline <- ggplot(fitted_cpue, aes(Trapline, ratCaught))
# rTrapline + geom_boxplot()
# 
# mTrapline <- ggplot(fitted_cpue, aes(Trapline, mongooseCaught))
# mTrapline + geom_boxplot()


# apply(fitted)

# read.csv('~/WERC-SC/HALE/catch_burrows.csv',
#          stringsAsFactors = FALSE) -> burrows
