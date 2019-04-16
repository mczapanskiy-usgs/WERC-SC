## this script graphically analyzes spatial results of CPUE data
## seasonal and yearly effects on:
##   event types (pred, other, none)
##   predator events (rat, cat, mongoose)

library(stats)
library(plyr)
library(dplyr)
library(data.table)
library(tidyr)
library(stringr)
library(ggplot2)
library(mosaic)

setwd("~/WERC-SC/HALE")

read.csv('~/WERC-SC/HALE/catch_11.5_spatialCatches_20170109.csv',
         stringsAsFactors = FALSE) -> catch_spatial
read.csv('~/WERC-SC/HALE/TraplinePredEventPUE_11_20170118.csv',
         stringsAsFactors = FALSE) -> catch_EventPUE #  %>% filter(Month < 7)

### SEASONAL ANALYSIS
seasonalEvents <- catch_spatial %>% 
  group_by(Season, predEvent) %>% 
  summarise(eventCount = sum(NEvents)) 
# change variable type
catch_spatial$Season <- factor(catch_spatial$Season, levels = c("Pre-laying", "Incubation", "Nestling", "offSeason"))
catch_spatial$predEvent <- factor(catch_spatial$predEvent, 
                                  levels = c("catCaught", "mongooseCaught", "ratCaught", "mouseCaught", 
                                            "birdOtherCaught", "baitLost", "trapTriggered", "none"))


seasonalEvents <- catch_EventPUE %>%
  group_by(Season, predEvent) %>%
  summarise(eventCount = sum(NEvents))
# change variable type
catch_EventPUE$Season <- factor(catch_EventPUE$Season, levels = c("Pre-laying", "Incubation", "Nestling", "offSeason"))
catch_EventPUE$predEvent <- factor(catch_EventPUE$predEvent, 
                                   levels = c("catCaught", "ratCaught", "mongooseCaught", 
                                              "birdOtherCaught", "baitLost", "trapTriggered", "none"))
catch_EventPUE$Month <- as.character(catch_EventPUE$Month)
catch_EventPUE$Month <- as.factor(catch_EventPUE$Month)
catch_EventPUE$Month <- factor(catch_EventPUE$Month,
                                  levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))
                               
# box plot of CPUE in different seasons
season_box <- ggplot(catch_spatial, aes(Season, CPUE)) +
  geom_boxplot() + #geom_bar(stat = "identity") +
  facet_wrap(~ predEvent) +
  theme_bw() 
# scale_fill_gradient(low = "green", high = "red")
season_box %+% subset(catch_spatial, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught")) # , "trapTriggered", "baitLost", "none"))

#____________________________________________________________________________________________________________________________
### bar graphs of proportion of events happening in different SEASON
month_bar <- ggplot(catch_spatial, aes(Month_, fill = predEvent)) + # season_bar <- ggplot(arrange(catch_EventPUE, Season), aes(Season, fill = predEvent)) +
  geom_bar(position = "fill") +
  scale_x_discrete(name = "Month", limits = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  theme_bw() 
month_bar %+% subset(catch_spatial, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "birdOtherCaught", "trapTriggered", "baitLost", "none"))
ggsave(width = 8.5, height = 5, dpi=300, filename = "~/WERC-SC/HALE/outputs/monthProps_eck15.pdf")

## preds only
month_bar_pred <- ggplot(catch_spatial, aes(Month_, fill = predEvent)) +
  geom_bar(position = "fill") +
  scale_x_discrete(name = "Month", limits = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  theme_bw()
month_bar_pred %+% subset(catch_spatial, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught"))
ggsave(width = 8.5, height = 5, dpi=300, filename = "~/WERC-SC/HALE/outputs/monthProps_preds_eck15.pdf")


#____________________________________________________________________________________________________________________________
### bar graphs of proportion of events happening in different SEASON
season_bar <- ggplot(catch_spatial, aes(Season, fill = predEvent)) + # season_bar <- ggplot(arrange(catch_EventPUE, Season), aes(Season, fill = predEvent)) +
  geom_bar(position = "fill") +
  theme_bw() 
# theme(axis.text.x = element_text(angle=60, hjust=1)) 
season_bar %+% subset(catch_spatial, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "birdOtherCaught", "trapTriggered", "baitLost", "none"))
ggsave(width = 8.5, height = 5, dpi=300, filename = "~/WERC-SC/HALE/outputs/seasonalProps_eck15.pdf")

## preds only
season_bar_pred <- ggplot(catch_spatial, aes(Season, fill = predEvent)) +
  geom_bar(position = "fill") +
  theme_bw()
season_bar_pred %+% subset(catch_spatial, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught"))
ggsave(width = 8.5, height = 5, dpi=300, filename = "~/WERC-SC/HALE/outputs/seasonalProps_preds_eck15.pdf")


#____________________________________________________________________________________________________________________________
### bar graphs of proportion of events happening in different MONTHS
monthly_barfill <- ggplot(catch_spatial, aes(Month_, fill = predEvent)) + # season_bar <- ggplot(arrange(catch_EventPUE, Season), aes(Season, fill = predEvent)) +
  geom_bar(position = "fill") +
  theme_bw() +
  labs(x = 'Month', y = 'Proportion of Monthly Events') +
  scale_x_discrete(limits = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec")) 
# theme(axis.text.x = element_text(angle=60, hjust=1)) 
monthly_barfill %+% subset(catch_spatial, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "birdOtherCaught", "trapTriggered", "baitLost", "none"))
ggsave(width = 8.5, height = 5, dpi=300, filename = "~/WERC-SC/HALE/outputs/monthlyPropsFill_eck15.pdf")

## just predators
monthly_barfill_pred <- ggplot(catch_spatial, aes(Month_, fill = predEvent)) +
  geom_bar(position = "fill") +
  theme_bw() +
  labs(x = 'Month', y = 'Proportion of Monthly Events') +
  scale_x_discrete(limits = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec")) 
monthly_barfill_pred %+% subset(catch_spatial, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught"))
ggsave(width = 8.5, height = 5, dpi=300, filename = "~/WERC-SC/HALE/outputs/monthlyPropsFill_preds_eck15.pdf")


## each predator with a different bar chart
monthly_bar_pred <- ggplot(catch_spatial, aes(Month_)) +
  geom_bar() +
  facet_wrap(~ predEvent, scales = "free") +
  theme_bw() +
  labs(x = 'Month', y = 'Frequency of Events') +
  scale_x_discrete(limits = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec")) +
  theme(axis.text.x = element_text(angle=60, hjust=1))
monthly_bar_pred %+% subset(catch_spatial, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught"))
ggsave(width = 12, height = 8, dpi=300, filename = "~/WERC-SC/HALE/outputs/monthlyProps_preds_scales_eck15.pdf")

## each predator with a different bar chart with frequency N above each bar
monthlyCatch <- catch_spatial %>% 
  group_by(Month_, predEvent) %>% 
  summarize(count = n())  %>% 
  filter(!is.na(predEvent))
monthly_bar2_pred <- ggplot(monthlyCatch, aes(x = Month_, y = count)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ predEvent) +
  theme_bw() +
  geom_text(aes(label = count, x = Month_, y = count), position = position_dodge(width = 0.5), vjust = -0.6) +
  labs(x = 'Month', y = 'Frequency of Events') +
  scale_x_discrete(limits = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec"))
  # theme(axis.text.x = element_text(angle=60, hjust=1))
monthly_bar2_pred %+% subset(monthlyCatch, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught"))
ggsave(width = 12, height = 8, dpi=300, filename = "~/WERC-SC/HALE/outputs/monthlyProps_preds_count_eck15.pdf")

## save monthly predator data
write.csv(monthlyCatch, file = '~/WERC-SC/HALE/outputs/monthlyCatch.csv',
          row.names = FALSE)

#____________________________________________________________________________________________________________________________
##### CUPE

## box plot of CPUE per SEASON
season_box <- ggplot(catch_EventPUE, aes(Season)) +
  geom_bar(aes(color=predEvent)) + #geom_bar(stat = "identity") +
  facet_wrap(~ predEvent) + # facet_wrap(~ predEvent, nrow = 3) +
  theme_bw()
  # scale_fill_gradient(low = "green", high = "red")
season_box %+% subset(catch_EventPUE, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught"))
ggsave(width = 10, height = 5, dpi=300, filename = "~/WERC-SC/HALE/outputs/seasonal_preds_facet_eck15.pdf")


### box chart of PREDATOR CPUE by month
month_box <- ggplot(catch_EventPUE, aes(x=Month, y=CPUE)) + 
  geom_boxplot() +
  facet_wrap(~ predEvent) + # facet_wrap(~ predEvent, nrow = 3) +
  theme_bw() +
  labs(x = 'Month', y = 'Catch per Weekly Trapline Effort') 
month_box %+% subset(catch_EventPUE, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught"))
ggsave(width = 10, height = 5, dpi=300, filename = "~/WERC-SC/HALE/outputs/monthlyCPUE_preds_facet_eck15.pdf")

predCatph_eventPUE <- filter(catch_EventPUE, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught"))
predMonthCount <- count(predCatph_eventPUE, vars = Month, wt_var = predEvent)

#____________________________________________________________________________________________________________________________
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


# apply(fitted)

# read.csv('~/WERC-SC/HALE/catch_burrows.csv',
#          stringsAsFactors = FALSE) -> burrows
