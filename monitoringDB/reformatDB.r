## This script is used to restructure the Seabird Monitoring Database results
## data is 'spread' to fit with structure of Marine Mammal database
## the new structure created here will be uploaded to ScienceBase and Google Fusion

setwd("~/WERC-SC/monitoringDB")

## load packages
# library(stats)
library(data.table)
library(plyr)
library(dplyr)
library(tidyr)
library(splitstackshape)
library(ggplot2)
# library(mosaic)

## load data
read.csv('~/WERC-SC/monitoringDB/SeabirdStudyContents_old.csv',
         stringsAsFactors = FALSE) -> studyContents_old
read.csv('~/WERC-SC/monitoringDB/SeabirdMetadata_old.csv',
         stringsAsFactors = FALSE) -> metadata_old
read.csv('~/WERC-SC/monitoringDB/sppNames.csv',
         stringsAsFactors = FALSE) -> sppNames


## reformat study contents data so that theres a separate row for every method/spp combination
studyContents <- gather(studyContents_old, DataCollected, Species, ColonyCount:OtherDataTypeMethod) %>% 
  cSplit('Species', sep=", ", type.convert=FALSE) %>% 
  gather(SppCount, Species, Species_01:Species_24) %>% 
  select(-SppCount, -CreatedDate) %>% 
  filter(!is.na(Species))

studyContents$Species <- gsub(",","",studyContents$Species,
                              " ","",studyContents$Species)


# bind to species name
studyContents2 <- left_join(studyContents, sppNames, by = c("Species" = "AlphaCode")) %>% 
  mutate(AlphaCode = Species,
         Taxa = Family) %>% 
  select(-Family, -Order, -Species)

studyContents3 <- left_join(studyContents2, metadata_old, by = "SeabirdSurveyID") %>% 
  cSplit('StudyRegion', sep=", ", type.convert=FALSE) %>% 
  gather(regionCount, StudyRegion, StudyRegion_1:StudyRegion_9) %>%
  select(-regionCount) %>% 
  filter(!is.na(StudyRegion)) %>% 
  select(DataCollectedID, SeabirdSurveyID, AlphaCode, Taxa, SpeciesName, ScientificName, StateName, StudyRegion, MonitorSite, 
         Jurisdiction, DataCollected, StartYear:DataCollectionFrequency, Notes) 
  

studyContents3$StudyRegion <- gsub(",","",studyContents3$StudyRegion,
                                   " ","",studyContents3$StudyRegion)

## save updated database
write.csv(studyContents3, file = '~/WERC-SC/monitoringDB/SeabirdStudyContents_new.csv',
          row.names = FALSE)

