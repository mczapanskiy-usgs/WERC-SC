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
         Taxa = Family)
  select(-Family, -Order, -Species)
studyContents2$StudyRegion <- gsub(",","",studyContents2$StudyRegion,
                                   " ","",studyContents2$StudyRegion)

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
  mutate(TaxaSet = mosaic::derivedFactor(
    "Mammal" = AlphaCode %in% c('Otter', 'Pinnipeds', 'Cetaceans'),
    .default = "Seabird"),
    Species = AlphaCode,
    StudyLocation = StudyRegion) %>% 
  # reorder database so it matches format of MarMam database
  select(DataCollectedID, SeabirdSurveyID, TaxaSet, Taxa, SpeciesName, ScientificName, Species, StateName, StudyLocation, StudyRegion, MonitorSite, 
         Jurisdiction, DataCollected, StartYear:DataCollectionFrequency, Notes, Organization, Affiliation, -AlphaCode) 
## remove commas and spaces from separated Study Region names

studyContents3$Taxa = case_when(studyContents3$Species == "Pinnipeds" ~ "Pinniped",
                                studyContents3$Species == "Otter" ~ "SeaOtter",
                                studyContents3$Species == "Cetaceans" ~ "Cetacean",
                                TRUE ~ as.character(studyContents3$Taxa))

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
         EntryID = paste("s", DataCollectedID, sep = '')) %>% 
  select(-SeabirdSurveyID, -DataCollectedID)
            

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



# #### input data from Cora (11/16/17)
# ## load data
# read.csv('~/WERC-SC/monitoringDB/BOEMmonitoringDatabase_masterWithMaps.csv',
#          stringsAsFactors = FALSE) -> monitorDB
# read.csv('~/WERC-SC/monitoringDB/BOEMpolygonMergewMaster_16Nov2017_update.csv',
#          header = TRUE) -> map
# read.csv('~/WERC-SC/monitoringDB/MergedMaster_BOEMmonitoringDB_13Nov2017_cajUPDATE16Nov2017.csv',
#          header = TRUE) -> dat
# 
# #verify that StudyLocation labels match
# levels(map$StudyLocation)
# levels(dat$StudyLocation)
# diff <- setdiff(map$StudyLocation, dat$StudyLocation)
# 
# #Merge into master data set based on shared identifier "StudyLocation"
# masterWmap <- merge(dat, map, "StudyLocation")
# 
# #merging with map has reduced the number of cells, suggesting data loss (rows lost during linkage)
# #   same number of StudyLocation levels in data and merged data
# #   manual check also shows that they all match (names)
# levels(masterWmap$StudyLocation)
# 
# write.csv(masterWmap, file = "BOEMmonitoringDatabase_masterWithMaps.csv",row.names=FALSE, na="")


