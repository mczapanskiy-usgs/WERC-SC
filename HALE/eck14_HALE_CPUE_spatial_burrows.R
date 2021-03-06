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

read.csv('~/WERC-SC/HALE/spatialData_rev_eck18.csv',
         stringsAsFactors = FALSE) -> catch_spatial
# read.csv('~/WERC-SC/HALE/TrapsGrid20170626.csv', # catch data processed by Ben (elev, slope, prox to roads/trails/fences/structures, veg, etc.) & Jon (grid cells)
#          stringsAsFactors = FALSE) -> spatialCatch
read.csv('~/WERC-SC/HALE/TraplinePredEventPUE_11_20170118.csv',
         stringsAsFactors = FALSE) -> CPUEdata



# create columns for frequency of burrow counts and for whether or not there are burrows around (e.g.- inside or outside the colony)
burrows <- catch_spatial %>% 
  select(Trapline, TrapNum, Year, Season, eventType, predEvent, Week, Burrows10, Burrows50, Burrows100) %>% 
  mutate(burFreq100 = percent_rank(Burrows100),
         colony100 = mosaic::derivedFactor("in" = Burrows100 >=1,
                                           "out" = Burrows100 == 0,
                                           .method = "last", 
                                           .default = "out"),
         filler = 1)
write.csv(burrows, file = '~/WERC-SC/HALE/catch_burrows.csv',
          row.names = FALSE) 

# graph data
bur100_preds <- ggplot(burrows, aes(burFreq100)) +
  geom_histogram(binwidth = 0.1) +
  facet_wrap(~ predEvent) +
  theme_bw()
bur100_preds %+% subset(burrows, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught"))

bur100_events <- ggplot(burrows, aes(eventType, fill = colony100)) +
  geom_bar(position = "fill") + 
  theme_bw()
bur100_events
ggsave(width = 7, height = 5, dpi=300, filename = "~/WERC-SC/HALE/outputs/burrow100_events_eck14.pdf")

bur100 <- ggplot(burrows, aes(filler, fill = colony100)) +
  geom_bar(position = "fill") + 
  theme_bw()
bur100 
ggsave(width = 3, height = 5, dpi=300, filename = "~/WERC-SC/HALE/outputs/allBurrow100_events_eck14.pdf")

## YEAR
# proportion of events that are occuring w/in burrow radii vs. outside
yrBurrows_Event <- burrows %>% 
  group_by(Year, eventType, colony100) %>% 
  count()

yrBurrows_pred <- burrows %>% 
  filter(eventType == "predatorEvent") %>% 
  group_by(Year, predEvent, colony100) %>% 
  count()
# summarize(meanBur = mean(Burrows100, na.rm = TRUE), sdBur = sd(Burrows100, na.rm = TRUE))
  
bur100_yr_events <- ggplot(yrBurrows_Event, aes(Year, n)) +
  geom_point(aes(color = colony100), size = 2, shape = 18) +
  geom_line(aes(color = colony100), size = 1) +
  facet_wrap(~ eventType) +
  scale_fill_brewer(palette = "Set1") +
  labs(y = 'Number of Trap Events', x = 'Year') +
  xlim(c(2000,2015)) +
  theme_bw()
bur100_yr_events 
ggsave(width = 8.5, height = 5, dpi=300, filename = "~/WERC-SC/HALE/outputs/bur100_yr_events_eck14.pdf")

bur100_yr_preds <- ggplot(yrBurrows_pred, aes(Year, n)) +
  geom_point(aes(color = colony100), size = 2, shape = 18) +
  geom_line(aes(color = colony100), size = 1) +
  facet_wrap(~ predEvent) +
  labs(y = 'Number of Predator Events', x = 'Year') +
  xlim(c(2000,2015)) +
  theme_bw()
bur100_yr_preds 
ggsave(width = 8.5, height = 5, dpi=300, filename = "~/WERC-SC/HALE/outputs/bur100_yr_preds_eck14.pdf")


## SEASON
bur100_season_event <- ggplot(burrows, aes(Season, Burrows100)) +
  geom_bar(stat = "identity", aes(color = Season, fill = Season)) +
  facet_wrap(~ eventType) +
  labs(y = 'Number of `Ua`u Burrows within 100m', x = '`Ua`u Season') +
  theme_bw()
# theme(axis.text.x = element_text(angle=60, hjust=1)) 
bur100_season_event # %+% subset(burrows, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "trapTriggered", "baitLost", "none"))
ggsave(width = 8.5, height = 5, dpi=300, filename = "~/WERC-SC/HALE/outputs/bur100_season_event_eck14.pdf")

bur100_season_preds <- ggplot(burrows, aes(Season, Burrows100)) +
  geom_bar(stat = "identity", aes(color = Season, fill = Season)) +
  facet_wrap(~ predEvent) +
  labs(y = 'Number of `Ua`u Burrows within 100m', x = '`Ua`u Season') +
  theme_bw()
bur100_season_preds %+% subset(burrows, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught"))
ggsave(width = 8.5, height = 5, dpi=300, filename = "~/WERC-SC/HALE/outputs/bur100_season_preds_eck14.pdf")


# theme(axis.text.x = element_text(angle=60, hjust=1)) 
bur100_season_preds %+% subset(burrows, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught"))
ggsave(width = 8.5, height = 5, dpi=300, filename = "~/WERC-SC/HALE/outputs/bur100_season_event_eck14.pdf")


# ANNUAL TRENDS
annualBurrows <- burrows %>% 
  group_by(Year, colony100) %>% 
  summarise(count = n()) # %>% complete(Year_, colony100, predEvent, fill = list(eventCount = 0))
annualBurrowsEvents <- burrows %>% 
  group_by(Year, colony100, predEvent) %>% 
  summarise(eventCount = n())
annBurrowEvents <- left_join(annualBurrowsEvents, annualBurrows, by = "DistRoad") %>% 
  mutate(freq = (eventCount/count))


col_yr <- ggplot(annualBurrows, aes(Year, eventCount)) +
  geom_line(aes(colour = colony100)) +
  facet_wrap(~ predEvent, nrow = 3) +
  theme_bw()
col_yr %+% subset(annualBurrows, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "trapTriggered", "baitLost", "none"))


### CPUE ANALYSIS
## use same analysis from eck11 but calc. "catch per trapline YEAR", & also ave. burrow density "per trapline YEAR"
# make "weekly" a datatable containing all the factors needed for weekly analysis
weekly <- catch_spatial %>%
  group_by(Trapline, TrapNum, Year, Week) %>%
  filter(predEvent == min(predEvent)) %>% 
  slice(1) %>% 
  ungroup
# count the number of Trapline events (weekly) per week (aka number of traps in the trapline)
perLineWeek <- weekly %>%
  group_by(Trapline, Year, Season, Week) %>%
  summarize(NTraps = n()) %>% 
  ungroup
# count number of each predEvent and average number of burrows w/in ea. buffer per week per trapline
eventsPerLineWeek <- weekly %>% 
  group_by(Trapline, Year, Week, Season, predEvent) %>% 
  summarise(NEvents = n(),
            aveBur100 = mean(Burrows100),
            sdBur100 = sd(Burrows100)) %>% 
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
# group CPUE by year
annualCPUE <- burrowCPUE %>% 
  group_by(Year_, predEvent) %>% 
  summarise(annCPUE = mean(CPUE), 
            sdCPUE = sd(CPUE))
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


# # recreate burrows matrix with Burrows10, Burrows50, and Burrows100 all in the same column
# burrows_long <- burrows %>% 
#   select(Trapline, TrapNum, Year, predEvent, Week, Burrows10, Burrows50, Burrows100, burFreq10, burFreq50, burFreq100) %>% 
#   gather(buffer, Burrows, starts_with("Burrows")) %>% 
#   mutate(colony = derivedFactor(
#            "in" = Burrows >= 1,
#            "out" = Burrows == 0, 
#            .method = "last", 
#            .default = "out"))
# 
# bur1 <- ggplot(burrows_long, aes(buffer, Burrows)) +
#   geom_bar(stat = "identity") +
#   facet_wrap(~ predEvent)
# bur1 %+% subset(burrows_long, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "trapTriggered", "baitLost", "none"))
# 
# bur2 <- ggplot(burrows_long, aes(buffer, fill = colony)) +
#   geom_bar(position = "dodge") +
#   facet_wrap(~ predEvent)
# bur2 %+% subset(burrows_long, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "trapTriggered", "baitLost", "none"))
# 
# col1 <- ggplot(burrows_long, aes(buffer, fill = colony)) +
#   geom_bar(position = "fill") +
#   facet_wrap(~ predEvent) +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle=60, hjust=1)) 
# col1 %+% subset(burrows_long, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "trapTriggered", "baitLost", "none"))
# 
# col2 <- ggplot(burrows_long, aes(buffer, fill = predEvent)) +
#   geom_bar(position = "fill") +
#   facet_wrap(~ colony) +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle=60, hjust=1)) 
# col2 %+% subset(burrows_long, predEvent %in% c("catCaught", "mongooseCaught", "ratCaught", "trapTriggered", "baitLost", "none"))


# 
# CPUEdata <- CPUEdata %>% 
#   select(catchID, Season)
# 
# SpatialCatch_rev <- join(SpatialCatch2, CPUEdata, by = "catchID")
# 
# # read.csv('~/WERC-SC/HALE/TrapsGrid.csv', # catch data processed by Ben (elev, slope, prox to roads/trails/fences/structures, veg, etc.) & Jon (grid cells)
# #          stringsAsFactors = FALSE) -> spatialData
# # ## add in "baitType"
# # read.csv('~/WERC-SC/HALE/catch_11.5_spatialCatches_20170109.csv', # catch data processed by Ben (elev, slope, prox to roads/trails/fences/structures, veg, etc.) & Jon (grid cells)
# #          stringsAsFactors = FALSE) -> baitData
# # baitData <- baitData %>% 
# #   select(catchID, baitType)
# # baitData2 <- join(spatialData, baitData, by = "catchID") 
# 
# 
# 
# ## look at data
# summary(catch_spatial)
# dim(catch_spatial)
# with(catch_spatial, table(predEvent))
# with(catch_spatial, table(Burrows10, predEvent))
# with(catch_spatial, table(Burrows10, predEvent, Year))

# SpatialCatch_rev <- spatialCatch %>% 
#   mutate(Date = as.Date(Date_, "%m/%d/%Y"),
#          Year = year(Date),
#          Month = month(Date),
#          Season = derivedFactor("offSeason" = Month == 1,
#                                 "Pre-laying" = Month >= 2,
#                                 "Incubation" = Month >= 5,
#                                 "Nestling" = Month >= 7,
#                                 "offSeason" = Month >= 11,
#                                 .method = "last", 
#                                 .default = "offSeason")) %>% 
#   select(-c(OBJECTID, Join_Count, TARGET_FID, Join_Count_1, TARGET_FID_1, Date_))


