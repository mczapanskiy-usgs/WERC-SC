## This script is used to restructure the Seabird Monitoring Database 
## data is 'spread' to fit with structure of Marine Mammal database
## the new structure created here will enable database to be uploaded into online format

setwd("~/WERC-SC/monitoringDB")

## load packages
# library(stats)
library(data.table)
library(plyr)
library(dplyr)
library(tidyr)
library(splitstackshape)
library(ggplot2)
library(mosaic)

## load data
read.csv('~/WERC-SC/monitoringDB/SeabirdStudyContents_old.csv',
         stringsAsFactors = FALSE) -> studyContents_old
read.csv('~/WERC-SC/monitoringDB/SeabirdMetadata_old.csv',
         stringsAsFactors = FALSE) -> metadata_old
read.csv('~/WERC-SC/monitoringDB/sppNames.csv',
         stringsAsFactors = FALSE) -> sppNames
read.csv('~/WERC-SC/monitoringDB/MarMamStudyContents_13Nov2017.csv',
         stringsAsFactors = FALSE) -> studyContents_marMam


### reformat study contents data so that there's a separate row for every method/spp combination
studyContents <- gather(studyContents_old, DataCollected, Species, ColonyCount:OtherDataTypeMethod) %>% 
  cSplit('Species', sep=", ", type.convert=FALSE) %>% 
  gather(SppCount, Species, Species_01:Species_24) %>% 
  select(-SppCount, -CreatedDate) %>% 
  filter(!is.na(Species))
## remove commas and spaces from separated Species names
studyContents$Species <- gsub(",","",studyContents$Species,
                              " ","",studyContents$Species)

### Add species names to studyContents database by binding with sppName spreadsheet
studyContents2 <- left_join(studyContents, sppNames, by = c("Species" = "AlphaCode")) %>% 
  # rename columns to correspond with names in MarMam database
  mutate(AlphaCode = Species,
         Taxa = Family) %>% 
  select(-Family, -Order, -Species)

### reformat metadata to correct for spelling errors
metadata <- metadata_old %>% 
  lapply(gsub, pattern = "Straight", replacement = "Strait", fixed = TRUE) %>% 
  lapply(gsub, pattern = "Kodak", replacement = "Kodiak", fixed = TRUE)
  
### Add metadata columns to studyContents database and reformat so there is a separate row for each study region
studyContents3 <- left_join(studyContents2, metadata_old, by = "SeabirdSurveyID") %>% 
  cSplit('StudyRegion', sep=", ", type.convert=FALSE) %>% 
  gather(regionCount, StudyRegion, StudyRegion_1:StudyRegion_9) %>%
  select(-regionCount) %>% 
  filter(!is.na(StudyRegion)) %>% 
  # reorder database so it matches format of MarMam database
  select(DataCollectedID, SeabirdSurveyID, AlphaCode, Taxa, SpeciesName, ScientificName, StateName, StudyRegion, MonitorSite, 
         Jurisdiction, DataCollected, StartYear:DataCollectionFrequency, Notes, Organization, Affiliation) 
## remove commas and spaces from separated Study Region names
studyContents3$StudyRegion <- gsub(",","",studyContents3$StudyRegion,
                                   " ","",studyContents3$StudyRegion)

### join seabird and marine mammal databases
## make unique SurveyIDs
studyContents2_marMam <- studyContents_marMam %>% 
  mutate(SurveyID = paste("m", MarMamSurveyID, sep = ''),
         EntryID = paste("m", EntryID, sep = ''),
         SpeciesName = Species..common.,
         ScientificName = Species..Latin.,
         DataCollectionSeasons = Season,
         DataCollectionFrequency = Frequency,
         StateName = StudyState) %>% 
  select(-MarMamSurveyID, -Species..common., -Species..Latin., -Season, -Frequency, -StudyState)
studyContents2_marMam$ScientificName <- gsub("[()]","",studyContents2_marMam$ScientificName)

studyContents4 <- studyContents3 %>% 
  mutate(SurveyID = paste("s", SeabirdSurveyID, sep = ''),
         EntryID = paste("s", DataCollectedID, sep = ''),
         TaxaSet = mosaic::derivedFactor(
              "Mammal" = Taxa %in% c('Otter', 'Pinnipeds', 'Cetaceans'),
              .default = "Seabird"),
         Species = AlphaCode,
         StudyLocation = StudyRegion) %>% 
  select(-SeabirdSurveyID, -DataCollectedID, -AlphaCode)
            

studyContents_merged <- bind_rows(studyContents2_marMam, studyContents4) %>% 
  select(SurveyID, EntryID, TaxaSet, Taxa, SpeciesName, ScientificName, Species, Notes.on.species, 
         StudyLocation, StudyRegion, StateName, MonitorSite, Jurisdiction,  
         Organization, Affiliation, DataCollected, SurveyMethod, OtherMethods, 
         StartYear, EndYear, MissingYear, DataCollectionSeasons, DataCollectionFrequency, Notes) 


## save updated database
write.csv(studyContents3, file = '~/WERC-SC/monitoringDB/SeabirdStudyContents_new.csv',
          row.names = FALSE)
write.csv(studyContents4, file = '~/WERC-SC/monitoringDB/SeabirdStudyContents_2merge.csv',
          row.names = FALSE)
write.csv(studyContents2_marMam, file = '~/WERC-SC/monitoringDB/studyContents2_marMam_2merge.csv',
          row.names = FALSE)
write.csv(studyContents_merged, file = '~/WERC-SC/monitoringDB/studyContents_merged.csv',
          row.names = FALSE)

