## this script imports catch data with spatail data and starts
## spatial analysis of burrows & predEvents

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

## look at data
summary(catch_spatial)
dim(catch_spatial)
with(catch_spatial, table(predEvent))
with(catch_spatial, table(Burrows10, predEvent))
with(catch_spatial, table(Burrows10, predEvent, strYear))


# create columns for frequency of burrow counts and for whether or not there are burrows around (e.g.- inside or outside the colony)
burrows <- catch_spatial %>% 
  select(Trapline, TrapNum, Year_, Month_, predEvent, Week, Burrows10, Burrows50, Burrows100) %>% 
  mutate(burFreq10 = percent_rank(Burrows10),
         burFreq50 = percent_rank(Burrows50),
         burFreq100 = percent_rank(Burrows100),
         colony10 = derivedFactor(
           "in" = Burrows10 >= 1,
           "out" = Burrows10 == 0, 
           .method = "last", 
           .default = "out"),
         colony50 = derivedFactor(
           "in" = Burrows50 >= 1,
           "out" = Burrows50 == 0, 
           .method = "last", 
           .default = "out"),
         colony100 = derivedFactor(
           "in" = Burrows100 >=1,
           "out" = Burrows100 == 0,
           .method = "last", 
           .default = "out"))  # HOW DO I REMOVE THE NAs?
write.csv(burrows, file = '~/WERC-SC/HALE/catch_burrows.csv',
          row.names = FALSE) 
# recreate burrows matrix with Burrows10, Burrows50, and Burrows100 all in the same column
burrows_long <- burrows %>% 
  select(Trapline, TrapNum, Year_, Month_, predEvent, Week, Burrows10, Burrows50, Burrows100, burFreq10, burFreq50, burFreq100) %>% 
  gather(buffer, Burrows, starts_with("Burrows")) %>% 
  mutate(colony = derivedFactor(
           "in" = Burrows >= 1,
           "out" = Burrows == 0, 
           .method = "last", 
           .default = "out"))
# graph data
bur100 <- ggplot(burrows, aes(burFreq100)) +
  geom_histogram(binwidth = 0.1) +
  facet_wrap(~ predEvent) +
  theme_bw()
bur100 %+% subset(burrows, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "mouseCaught"))

col50 <- ggplot(burrows, aes(predEvent, fill=colony50)) +
  geom_bar(position = "fill") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=60, hjust=1)) 
col50 %+% subset(burrows, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "trapTriggered", "baitLost", "none"))

bur1 <- ggplot(burrows_long, aes(buffer, Burrows)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ predEvent)
bur1 %+% subset(burrows_long, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "trapTriggered", "baitLost", "none"))

bur2 <- ggplot(burrows_long, aes(buffer, fill = colony)) +
  geom_bar(position = "dodge") +
  facet_wrap(~ predEvent)
bur2 %+% subset(burrows_long, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "trapTriggered", "baitLost", "none"))
# bur2 %+% subset(burrows_long, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "trapTriggered", "baitLost", "none"))

col1 <- ggplot(burrows_long, aes(buffer, fill = colony)) +
  geom_bar(position = "fill") +
  facet_wrap(~ predEvent) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=60, hjust=1)) 
col1 %+% subset(burrows_long, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "trapTriggered", "baitLost", "none"))

col2 <- ggplot(burrows_long, aes(buffer, fill = predEvent)) +
  geom_bar(position = "fill") +
  facet_wrap(~ colony) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=60, hjust=1)) 
col2 %+% subset(burrows_long, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "trapTriggered", "baitLost", "none"))
# ANNUAL TRENDS
annualBurrows <- burrows %>% 
  group_by(Year_, colony100, predEvent) %>% 
  summarise(eventCount = n()) # %>% 
  # complete(Year_, colony100, predEvent, fill = list(eventCount = 0))

col_yr <- ggplot(annualBurrows, aes(Year_, eventCount)) +
  geom_line(aes(colour = colony100)) +
  facet_wrap(~ predEvent, nrow = 3) +
  theme_bw()
col_yr_preds <- col_yr %+% subset(annualBurrows, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught"))


## CPUE ANALYSIS
# use same analysis from eck11 but calc. "catch per trapline YEAR", & also ave. burrow density "per trapline YEAR" 
# make "weekly" a datatable containing all the factors needed for weekly analysis
weekly <- catch_spatial %>%
  group_by(Trapline, TrapNum, Year_, Week) %>%
  filter(predEvent == min(predEvent)) %>% 
  slice(1) %>% 
  ungroup
# count the number of Trapline events (weekly) per week (aka number of traps in the trapline)
perLineWeek <- weekly %>%
  group_by(Trapline, Year_, Season, Week) %>%
  summarize(NTraps = n()) %>% 
  ungroup
# count number of each predEvent and average number of burrows w/in ea. buffer per week per trapline
eventsPerLineWeek <- weekly %>% 
  group_by(Trapline, Year_, Week, Season, predEvent) %>% 
  summarise(NEvents = n(),
            aveBur10 = mean(Burrows10),
            aveBur50 = mean(Burrows50),
            aveBur100 = mean(Burrows100)) %>% 
  ungroup
# number of predEvents per number of traps, for each week on each Trapline
eventPUE <- merge(perLineWeek, eventsPerLineWeek) %>% 
  mutate(CPUE = NEvents/NTraps) %>% 
  arrange(Trapline, Week, Year_, predEvent) %>% 
  select(Trapline, Week, Year_, predEvent, NTraps, NEvents, CPUE, aveBur10:aveBur100) 
# get percent rank of average burrow densities an also a binary factor for "in" or "out" of HAPE colony
burrowCPUE <- eventPUE %>% 
  mutate(burFreq10 = percent_rank(aveBur10),
         burFreq50 = percent_rank(aveBur50),
         burFreq100 = percent_rank(aveBur100),
         colony10 = derivedFactor(
           "in" = aveBur10 > 0,
           "out" = aveBur10 == 0, 
           .method = "last", 
           .default = "out"),
         colony50 = derivedFactor(
           "in" = aveBur50 > 0,
           "out" = aveBur50 == 0, 
           .method = "last", 
           .default = "out"),
         colony100 = derivedFactor(
           "in" = aveBur100 > 0,
           "out" = aveBur100 == 0,
           .method = "last", 
           .default = "out"))  # HOW DO I REMOVE THE NAs?
write.csv(burrowCPUE, file = '~/WERC-SC/HALE/burrowCPUE.csv',
          row.names = FALSE) 
# recreate burrows matrix with Burrows10, Burrows50, and Burrows100 all in the same column
burrowCPUE_long <- burrowCPUE %>% 
  gather(buffer, aveBurrows, starts_with("aveBur")) %>% 
  mutate(colony = derivedFactor(
    "in" = aveBurrows >= 1,
    "out" = aveBurrows == 0, 
    .method = "last", 
    .default = "out"))
# group by year by taking the average CPUE for each year both inside and outside the colony
annualCPUEburrows <- burrowCPUE %>% 
  group_by(Year_, colony100, predEvent) %>% 
  summarise(annCPUE = mean(CPUE)) 
# GRAPH annual average CPUE inside and outside the colony
col_CPUE_yr <- ggplot(annualCPUEburrows, aes(Year_, annCPUE)) +
  geom_point(aes(colour = colony100)) +
  facet_wrap(~ predEvent, nrow = 3) +
  theme_bw() +
  labs(x = 'Year', y = 'Annual Frequency of Events per Unit Effort')
col_CPUE_yr_preds <- col_CPUE_yr %+% subset(annualCPUEburrows, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught"))
