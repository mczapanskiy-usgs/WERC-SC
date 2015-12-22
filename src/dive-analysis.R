library(dplyr)
library(ggplot2)
library(RSQLite)

# Load data from sqlite database
MHIsrc <- src_sqlite('Hawaii_data/MHI_GPS_TDR.sqlite')
dives <- tbl(MHIsrc, 'Dive') %>% as.data.frame
trips <- tbl(MHIsrc, 'Trip') %>% as.data.frame
birds <- tbl(MHIsrc, 'BirdMetadata') %>% as.data.frame
deployments <- tbl(MHIsrc, 'DeploymentMetadata') %>% as.data.frame

# Join dive data with bird/deployment metadata
divesBySpecies <- inner_join(dives, birds, by = 'DeployID') %>% 
  inner_join(deployments, by = 'DeployID') %>% 
  filter(GPSRecovered == 1,
         TDRRecovered == 1)

# Summarize dives by species
divesBySpeciesSummary <- divesBySpecies %>%
  group_by(Species) %>%
  summarize(Dives = n(),
            Individuals = n_distinct(DeployID),
            MeanDepth = mean(MaxDepth),
            MedianDepth = median(MaxDepth),
            SDDepth = sd(MaxDepth),
            MaxDepth = max(MaxDepth)) %>%
  ungroup

ggplot(divesBySpecies,
       aes(x = MaxDepth,
           color = Species)) +
  geom_density() +
  ggtitle('Relative dive depths between RFBO, BRBO')

# WE FIND:
# BRBOs appear to dive deeper than RFBOs

# Test normality of dive depth
ggplot(divesBySpecies,
       aes(sample = MaxDepth,
           color = Species)) +
  stat_qq() +
  ggtitle('Normality of Dive Depth')

ggplot(divesBySpecies,
       aes(sample = log(MaxDepth),
           color = Species)) +
  stat_qq() +
  ggtitle('Normality of Log Dive Depth')

# WE FIND:
# Log of dive depth is close to normal due to long tail

# Run t test to determine if dive depth is significantly different
t.test(log(MaxDepth) ~ Species, divesBySpecies)

# WE FIND:
# Difference in dive depths between species is statistically significant

# Join dives to trips to calculate inter-dive intervals
POSIXct.from.db <- function(x) as.POSIXct(x, tz = 'UTC', origin = '1970-01-01 00:00.00 UTC')
MHIdb <- dbConnect(dbDriver('SQLite'), 'Hawaii_data/MHI_GPS_TDR.sqlite')
dbGetQuery(MHIdb, 
"SELECT D.DeployID, D.DiveID, D.Begin 'DiveBegin', D.Duration, D.MaxDepth 'DiveDepth',
  T.TripID, T.Begin 'TripBegin', T.End 'TripEnd', 
  BM.Species, BM.Sex
FROM Dive D
  JOIN Trip T
    ON D.DeployID = T.DeployID
      AND D.Begin >= T.Begin
      AND D.Begin < T.End
  JOIN DeploymentMetadata DM
    ON DM.DeployID = D.DeployID
  JOIN BirdMetadata BM
    ON BM.DeployID = D.DeployID
WHERE DM.TDRRecovered = 1
  AND DM.GPSRecovered = 1
  AND T.BeginComplete = 1
  AND T.EndComplete = 1") %>%
  as.data.frame %>%
  arrange(DeployID, DiveBegin) %>%
  mutate(InterDivePeriod = ifelse(DeployID == lead(DeployID) & TripID == lead(TripID), 
                                  lead(DiveBegin) - DiveBegin, 
                                  NA)) -> divesByTrip

# Summarize dive frequency by species
divesByTripSummary <- divesByTrip %>%
  group_by(Species) %>%
  summarize(Dives = n(),
            Individuals = n_distinct(DeployID),
            MeanPeriod = mean(InterDivePeriod, na.rm = TRUE),
            MedianPeriod = median(InterDivePeriod, na.rm = TRUE),
            SDPeriod = sd(InterDivePeriod, na.rm = TRUE),
            MaxPeriod = max(InterDivePeriod, na.rm = TRUE)) %>%
  ungroup

ggplot(divesByTrip,
       aes(x = InterDivePeriod,
           color = Species)) +
  geom_density() +
  scale_x_log10() +
  ggtitle('Relative inter-dive periods between RFBO, BRBO (log transformed)')

# WE FIND:
# Dive frequency within trips is very similar between the two species

# Test normality of inter-dive period
ggplot(divesByTrip,
       aes(sample = MaxDepth,
           color = Species)) +
  stat_qq() +
  ggtitle('Normality of Inter-dive Period')

ggplot(divesBySpecies,
       aes(sample = log(MaxDepth),
           color = Species)) +
  stat_qq() +
  ggtitle('Normality of Log Inter-dive Period')

# WE FIND:
# As with dive depth, log of inter-dive period is close to normal due to long tail

# Run t test to determine if inter-dive period is significantly different
t.test(log(InterDivePeriod) ~ Species, divesByTrip)

# WE FIND:
# No significant difference between the species as regards dive frequency


## Repeat analysis with sex as a variable
# Summarize dives by species and sex
divesBySexSummary <- divesBySpecies %>%
  filter(Sex != 'U') %>%
  group_by(Species, Sex) %>%
  summarize(Dives = n(),
            Individuals = n_distinct(DeployID),
            MeanDepth = mean(MaxDepth),
            MedianDepth = median(MaxDepth),
            SDDepth = sd(MaxDepth),
            MaxDepth = max(MaxDepth)) %>%
  ungroup

ggplot(divesBySpecies %>% filter(Species == 'BRBO'),
       aes(x = MaxDepth,
           color = Sex)) +
  geom_density() +
  ggtitle('Relative dive depths between BRBO males and females')

ggplot(divesBySpecies %>% filter(Species == 'RFBO', Sex != 'U'),
       aes(x = MaxDepth,
           color = Sex)) +
  geom_density() +
  ggtitle('Relative dive depths between RFBO males and females')

# WE FIND:
# In both species, females appear to dive to deeper depths than males. Is this in keeping with their greater size?

# Run t test to determine if dive depth is significantly different between sexes
t.test(log(MaxDepth) ~ Sex, filter(divesBySpecies, Species == 'BRBO', Sex != 'U'))
t.test(log(MaxDepth) ~ Sex, filter(divesBySpecies, Species == 'RFBO', Sex != 'U'))

# WE FIND:
# Difference in dive depths between sexes is significant for RFBOs, not for BRBOs. Real phenomenon or sample size issue?

# Compare inter-dive period between sexes
divePeriodBySexSummary <- divesByTrip %>%
  filter(Sex != 'U') %>%
  group_by(Species, Sex) %>%
  summarize(Dives = n(),
            Individuals = n_distinct(DeployID),
            MeanPeriod = mean(InterDivePeriod, na.rm = TRUE),
            MedianPeriod = median(InterDivePeriod, na.rm = TRUE),
            SDPeriod = sd(InterDivePeriod, na.rm = TRUE),
            MaxPeriod = max(InterDivePeriod, na.rm = TRUE)) %>%
  ungroup

divesByTrip %>%
  filter(Species == 'BRBO',
         Sex != 'U') %>%
ggplot(aes(x = InterDivePeriod,
           color = Sex)) +
  geom_density() +
  scale_x_log10() +
  ggtitle('Relative inter-dive periods between BRBO males and females (log transformed)')

divesByTrip %>%
  filter(Species == 'RFBO',
         Sex != 'U') %>%
  ggplot(aes(x = InterDivePeriod,
             color = Sex)) +
  geom_density() +
  scale_x_log10() +
  ggtitle('Relative inter-dive periods between RFBO males and females (log transformed)')

# WE FIND:
# Unlike with dive depth, differences between the sexes is pronounced in BRBOs and minor in RFBOs

# Run t test to determine if inter-dive period is significantly different between sexes
t.test(log(InterDivePeriod) ~ Sex, divesByTrip %>% filter(Species == 'BRBO', Sex != 'U'))
t.test(log(InterDivePeriod) ~ Sex, divesByTrip %>% filter(Species == 'RFBO', Sex != 'U'))

# WE FIND:
# RFBO females dive significantly more often than males, BRBOs show no significant difference in mean.
# However, BRBO females demonstrate much greater variability than males. But is that due to sample size?