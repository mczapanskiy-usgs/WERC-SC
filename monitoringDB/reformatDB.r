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

## reformat study contents data so that theres a separate row for every method/spp combination
studyContents <- gather(studyContents_old, Method, Species, ColonyCount:OtherDataTypeMethod) %>% 
  cSplit('Species', sep=", ", type.convert=FALSE) %>% 
  gather(SppCount, Species, Species_01:Species_24) %>% 
  select(-SppCount, -CreatedDate)


test <- metadata_old %>% 
  select(Affiliation:Jurisdiction)

metaData <- test %>% 
  cSplit('StudyRegion', sep=", ", type.convert=FALSE) %>% 
  gather(State, StudyRegion, StudyRegion_1:StudyRegion_9) %>% 
  group_by(Affiliation)

