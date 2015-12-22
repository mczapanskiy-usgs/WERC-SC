library(dplyr)
library(ggplot2)

# Load data from sqlite database
MHIdb <- src_sqlite('Hawaii_data/MHI_GPS_TDR.sqlite')
dives <- tbl(MHIdb, 'Dive') %>% as.data.frame
birds <- tbl(MHIdb, 'BirdMetadata') %>% as.data.frame
deployments <- tbl(MHIdb, 'DeploymentMetadata') %>% as.data.frame

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