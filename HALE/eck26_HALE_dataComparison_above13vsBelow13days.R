## This script answers questions by Seth, Raina, and Cathleen in response to the report:
### 1. compares the data selected for analysis in the report with the data for trap checks >13 days
#### assessing whether the removal of the >13 day data (in 'eck6_HALE_CPUE_timeSeries') effected the final results
### 2. Analyzes the frequency and distribution of cat catches in response to Seth's questions

setwd("~/WERC-SC/HALE")

## load packages
library(stats)
library(data.table)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(mosaic)

#### LOAD DATA
read.csv('~/WERC-SC/HALE/allCatch_25_20171120.csv.csv',
         stringsAsFactors = FALSE) -> allCatch

#### ANALYZING DATA
### TABLES
catch_over13days <- allCatch %>% 
  filter(TrapChecked == FALSE)

cats <- allCatch %>% 
  filter(predCaught == 'FC') %>% 
  select(-duplicate, -BaitPrevOld, -Dummy, -dateStr, -RatInvalid, -NotRebaited, -BaitPresent)
write.csv(cats, file = '~/WERC-SC/HALE/outputs/cats20171025.csv',
          row.names = FALSE)

rats <- allCatch %>% 
  filter(predCaught %in% c('RR', 'RN', 'RE')) %>% 
  select(-duplicate, -BaitPrevOld, -Dummy, -dateStr, -RatInvalid, -NotRebaited, -BaitPresent)
write.csv(rats, file = '~/WERC-SC/HALE/outputs/rats20171025.csv',
          row.names = FALSE)

mongoose <- allCatch %>% 
  filter(predCaught == 'HA') %>% 
  select(-duplicate, -BaitPrevOld, -Dummy, -dateStr, -RatInvalid, -NotRebaited, -BaitPresent)
write.csv(mongoose, file = '~/WERC-SC/HALE/outputs/mongoose20171025.csv',
          row.names = FALSE)

table(allCatch$TrapChecked)

catch_over13days <- allCatch %>% 
  filter(TrapChecked == FALSE)

catch_under13days <- allCatch %>% 
  filter(TrapChecked == TRUE)

### FIGURES
## proportion of event types within and outside 13 day check window
eventRatio <- ggplot(allCatch, aes(TrapChecked, fill = eventType)) +
  geom_bar(position = "fill") +
  facet_wrap(~ loc) +
  theme_bw()
eventRatio 
ggsave(width = 8.5, height = 5, dpi=300, filename = "~/WERC-SC/HALE/outputs/eventRatio_eck26.pdf")

## proportion of trap events within and outside 13 day check window
eventRatio <- ggplot(allCatch, aes(TrapChecked, fill = predEvent)) +
  geom_bar(position = "fill") +
  facet_wrap(~ loc) +
  theme_bw()
eventRatio %+% subset(allCatch, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught"))
ggsave(width = 8.5, height = 5, dpi=300, filename = "~/WERC-SC/HALE/outputs/predEventRatio_eck26.pdf")

## histogram of pred events per trapline
ggplot(allCatch, aes(predEvent)) +
  geom_bar() +
  labs(x = 'Predator Event Type - False = outside 13 day check window (n = 24,114), True = within 13 day check window (n = 249,274)', 
       y = 'Number of Trap Events') +
  facet_wrap(~ TrapChecked, scales = "free") +
  # theme(axis.text.x = element_text(angle=60, hjust=1)) +
  theme_bw()
ggsave(width = 10, height = 5, dpi=300, filename = "~/WERC-SC/HALE/outputs/predEvent_hist_eck26.pdf")

