library(dplyr)
library(ggplot2)
library(RSQLite)
library(oce)
library(geosphere)

# Load data from sqlite database
MHIsrc <- src_sqlite('Hawaii_data/MHI_GPS_TDR.sqlite')
dives <- tbl(MHIsrc, 'Dive') %>% as.data.frame
trips <- tbl(MHIsrc, 'Trip') %>% as.data.frame
birds <- tbl(MHIsrc, 'BirdMetadata') %>% as.data.frame %>% filter(SubColonyCode == 'LEH')
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
           fill = Species)) +
  geom_density(alpha = .25) +
  ggtitle('Distribution of dive depth') +
  xlab('Dive depth (m)')

ggplot(divesBySpecies,
       aes(x = Species,
           y = MaxDepth,
           fill = Species)) +
  geom_boxplot() +
  ggtitle('Distribution of dive depth') +
  scale_y_continuous(limits = c(.5, 4)) +
  annotate('text', x = .75, y = 3.75, label = '+3 deeper dives') +
  ylab('Dive depth (m)') +
  theme_gray(base_size = 24) +
  theme(plot.background = element_rect(color = '#000000')) + 
  guides(fill = FALSE)

ggsave(filename = '~/My Pictures/MHI/plots/divedist.png',
       width = 9.1,
       height = 4.9,
       units = 'in')

# WE FIND:
# BRBOs appear to dive deeper than RFBOs

# Test normality of dive depth
ggplot(divesBySpecies,
       aes(sample = MaxDepth,
           color = Species)) +
  stat_qq() +
  ggtitle('Normality of Dive Depth') +
  theme(legend.position = 'bottom')

ggplot(divesBySpecies,
       aes(sample = log(MaxDepth),
           color = Species)) +
  stat_qq() +
  ggtitle('Normality of Log Dive Depth') +
  theme(legend.position = 'bottom')

# WE FIND:
# Log of dive depth is closer to normal than untransformed data due to long tail

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
  AND T.EndComplete = 1
  AND BM.SubColonyCode = 'LEH'") %>%
  as.data.frame %>%
  mutate(DiveBeginDT = POSIXct.from.db(DiveBegin),
         TripBeginDT = POSIXct.from.db(TripBegin),
         TripEndDT = POSIXct.from.db(TripEnd)) %>%
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
           fill = Species)) +
  geom_density(alpha = .25) +
  scale_x_log10() +
  ggtitle('Distribution of inter-dive period') +
  xlab('Log of inter-dive period (s)')

ggplot(divesByTrip,
       aes(x = Species,
           y = InterDivePeriod)) +
  geom_boxplot() +
  scale_y_log10() +
  ggtitle('Distribution of inter-dive period') +
  ylab('Log of inter-dive period (s)')

# WE FIND:
# Dive frequency within trips is very similar between the two species

# Test normality of inter-dive period
ggplot(divesByTrip,
       aes(sample = DiveDepth,
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
# No significant difference between the species' dive frequency

# Do the species dive at different times of day?
divesByTrack <- dbGetQuery(MHIdb, 
"SELECT D.DeployID, D.DiveID, D.Begin 'DiveBegin', D.Duration, D.MaxDepth 'DiveDepth',
  RT.UTC, RT.Latitude, RT.Longitude,
  T.TripID, 
  BM.Species, BM.Sex, BM.ColonyLatitude, BM.ColonyLongitude
FROM Dive D
  JOIN RediscretizedTrack RT
    ON D.DeployID = RT.DeployID
      AND D.Begin >= RT.UTC
      AND D.Begin < RT.UTC + 180 -- rediscretization interval
  JOIN Trip T
    ON D.DeployID = T.DeployID
      AND RT.TripID = T.TripID
  JOIN DeploymentMetadata DM
    ON DM.DeployID = D.DeployID
  JOIN BirdMetadata BM
    ON BM.DeployID = D.DeployID
WHERE DM.TDRRecovered = 1
  AND DM.GPSRecovered = 1
  AND T.BeginComplete = 1
  AND T.EndComplete = 1
  AND BM.SubColonyCode = 'LEH'") %>%
  as.data.frame %>%
  mutate(UTC = POSIXct.from.db(UTC),
         SunAltitude = sunAngle(UTC, Longitude, Latitude)$altitude,
         ColonyDistance = distHaversine(cbind(Longitude, Latitude), cbind(ColonyLongitude, ColonyLatitude)))

divesBySunAngleSummary <- divesByTrack %>%
  group_by(Species) %>%
  summarize(Dives = n(),
            Individuals = n_distinct(DeployID),
            MeanSunAltitude = mean(SunAltitude, na.rm = TRUE),
            MedianSunAltitude = median(SunAltitude, na.rm = TRUE),
            SDSunAltitude = sd(SunAltitude, na.rm = TRUE),
            MinSunAltitude = min(SunAltitude, na.rm = TRUE),
            MaxSunAltitude = max(SunAltitude, na.rm = TRUE)) %>%
  ungroup

ggplot(divesByTrack,
       aes(x = SunAltitude,
           fill = Species)) +
  geom_density(alpha = .25) +
  annotate('rect', xmin = -6, xmax = 0, ymin = 0, ymax = .0175, alpha = .2) +
  xlab('Angle of sun over horizon (degrees)\n(Shaded region indicates twilight)') +
  ggtitle('Dive distribution by time of day') +
  theme_gray(base_size = 24) +
  theme(plot.background = element_rect(color = '#000000'))

ggsave(filename = '~/My Pictures/MHI/plots/divetemporaldist.png',
       width = 9.1,
       height = 4.9,
       units = 'in')

ggplot(divesByTrack,
       aes(y = SunAltitude,
           x = Species,
           fill = Species)) +
  geom_boxplot() +
  ylim(-10, 90) +
  xlab('Species\n(Shaded region indicates twilight)')+
  ylab('Angle of sun over horizon (degrees)') +
  annotate('rect', xmin = .5, xmax = 2.5, ymin = -6, ymax = 0, alpha = .2) +
  ggtitle('Dive distribution by time of day') +
  theme_gray(base_size = 24) +
  theme(plot.background = element_rect(color = '#000000')) + 
  guides(fill = FALSE)

# WE FIND:
# No apparent difference in dive timing between species

t.test(SunAltitude ~ Species, divesByTrack)
# No significant difference either

# Is there a difference in how spread out diving is? I.e. how far from colony.
diveDistanceFromColony <- divesByTrack %>%
  group_by(Species) %>%
  summarize(Dives = n(),
            Individuals = n_distinct(DeployID),
            MeanColonyDistance = mean(ColonyDistance, na.rm = TRUE),
            MedianColonyDistance = median(ColonyDistance, na.rm = TRUE),
            SDColonyDistance = sd(ColonyDistance, na.rm = TRUE),
            MinColonyDistance = min(ColonyDistance, na.rm = TRUE),
            MaxColonyDistance = max(ColonyDistance, na.rm = TRUE)) %>%
  ungroup

# RFBOs dive >2x as far from colony as BRBOs, as with track data

ggplot(divesByTrack,
       aes(y = ColonyDistance,
           x = Species,
           fill = Species)) +
  geom_boxplot() +
  ylab('Distance from colony (m)') +
  scale_y_continuous(labels = function(breaks) format(breaks, scientific = FALSE)) +
  ggtitle('Distribution of dives by distance from colony') +
  theme_gray(base_size = 24) +
  theme(plot.background = element_rect(color = '#000000')) +
  guides(fill = FALSE)

ggsave(filename = '~/My Pictures/MHI/plots/colonydistancedist.png',
       width = 9.1,
       height = 4.9,
       units = 'in')

# ggplot(divesByTrack,
#        aes(x = Longitude,
#            y = Latitude)) +
#   geom_hex(stat = function(x) x^2) +
#   annotate('point', x = -160.0975, y = 22.02, color = 'red', size = 10) +
#   xlab('Longitude\n(Red dot indicates Lehua') +
#   facet_grid(. ~ Species) +
#   ggtitle('Spatial distribution of dives')

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

ggplot(divesBySpecies %>% filter(Sex != 'U'),
       aes(x = Species,
           y = MaxDepth,
           fill = Sex)) +
  geom_boxplot() +
  ggtitle('Distribution of dive depth') +
  scale_y_continuous(limits = c(.5, 4)) +
  ylab('Dive depth (m)') +
  theme_gray(base_size = 20) +
  theme(plot.background = element_rect(color = '#000000'))


ggsave(filename = '~/My Pictures/MHI/plots/divedistsex.png',
       width = 5.1,
       height = 4.9,
       units = 'in')

ggplot(divesBySpecies %>% filter(Species == 'BRBO'),
       aes(x = MaxDepth,
           fill = Sex)) +
  geom_density(alpha = .25) +
  ggtitle('Relative dive depths between BRBO males and females')

ggplot(divesBySpecies %>% filter(Species == 'RFBO', Sex != 'U'),
       aes(x = MaxDepth,
           fill = Sex)) +
  geom_density(alpha = .25) +
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

ggplot(divesByTrip %>% filter(Sex != 'U'),
       aes(x = Species,
           y = InterDivePeriod,
           fill = Sex)) +
  geom_boxplot() +
  scale_y_log10() +
  ggtitle('Distribution of inter-dive period') +
  ylab('Log of inter-dive period (s)')

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
# Neither species shows a significant difference in dive frequency between sexes

# Do males venture farther from the colony?
ggplot(divesByTrack %>% filter(Sex != 'U'),
       aes(y = ColonyDistance / 1000,
           x = Species,
           fill = Sex)) +
  geom_boxplot() +
  ylab('Distance from colony (km)') +
  ylim(0, 250) +
  ggtitle('Distribution of dives by distance from colony') +
  theme_gray(base_size = 20) +
  theme(plot.background = element_rect(color = '#000000'))


ggsave(filename = '~/My Pictures/MHI/plots/colonydistsex.png',
       width = 9.1,
       height = 4.9,
       units = 'in')
  
t.test(ColonyDistance ~ Sex, divesByTrack %>% filter(Species == 'BRBO', Sex != 'U'))
t.test(ColonyDistance ~ Sex, divesByTrack %>% filter(Species == 'RFBO', Sex != 'U'))

# WE FIND:
# The opposite. Females forage significantly farther from the colony.
