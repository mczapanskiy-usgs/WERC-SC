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


# densities_long <- gather(densities, species, dens, ASSP:XAMU) %>% # change to long format (tidyr)
#   mutate(densityRank = percent_rank(dens)) %>% # add percent rank of all survey densities
#   merge(vulnRanks, by.x = "species", by.y = "SurveySpp") %>%  # merge with vulnRanks table
#   mutate(rankDensPCV = densityRank*PCVrank,
#          rankDensPDV = densityRank*PDVrank)
# 
# studyContents <- studyContents_old %>% 
#   mutate(ColonyCount = strsplit(as.character(ColonyCount), ","),
#          NestCount = strsplit(as.character(NestCount), ","),
#          RoostingHaulOutCount = strsplit(as.character(RoostingHaulOutCount), ","),
#          AtseaCountFromShore = strsplit(as.character(AtseaCountFromShore), ","),
#          AtseaDistributionAbundanceVessel = strsplit(as.character(AtseaDistributionAbundanceVessel), ","),
#          AtseaBehavior = strsplit(as.character(AtseaBehavior), ","),
#          ColonyBehavior = strsplit(as.character(ColonyBehavior), ","),
#          TelemetrySensors = strsplit(as.character(TelemetrySensors), ","),
#          NestBurrowOccupancy = strsplit(as.character(NestBurrowOccupancy), ","),
#          HatchingSuccess = strsplit(as.character(HatchingSuccess), ","),
#          FledgingSuccess = strsplit(as.character(FledgingSuccess), ","),
#          BreedingSuccess = strsplit(as.character(BreedingSuccess), ","),
#          ChickGrowthMorphometrics = strsplit(as.character(ChickGrowthMorphometrics), ","),
#          Phenology = strsplit(as.character(Phenology), ","),
#          SubadultAdultSurvival = strsplit(as.character(SubadultAdultSurvival), ","),
#          AdultMorphometrics = strsplit(as.character(AdultMorphometrics), ","),
#          BloodFeatherTissueSampling = strsplit(as.character(BloodFeatherTissueSampling), ","),
#          ChickDiet = strsplit(as.character(ChickDiet), ","),
#          AdultDiet = strsplit(as.character(AdultDiet), ","),
#          Contaminants = strsplit(as.character(Contaminants), ","),
#          BeachCarcassSurveys = strsplit(as.character(BeachCarcassSurveys), ","),
#          NecropsyTissueArchival = strsplit(as.character(NecropsyTissueArchival), ","),
#          Disturbance = strsplit(as.character(Disturbance), ","),
#          Predation = strsplit(as.character(Predation), ","),
#          Misnetting = strsplit(as.character(Misnetting), ","),
#          Acoustics = strsplit(as.character(Acoustics), ","),
#          OtherDataType = strsplit(as.character(OtherDataType), ","),
#          OtherDataTypeMethod = strsplit(as.character(OtherDataTypeMethod), ",")) %>% 
#   unnest(ColonyCount, NestCount, RoostingHaulOutCount, AtseaCountFromShore, AtseaDistributionAbundanceVessel, AtseaBehavior, ColonyBehavior, TelemetrySensors, NestBurrowOccupancy, 
#          HatchingSuccess, FledgingSuccess, BreedingSuccess, ChickGrowthMorphometrics, Phenology, SubadultAdultSurvival, AdultMorphometrics, BloodFeatherTissueSampling,
#          ChickDiet, AdultDiet, Contaminants, BeachCarcassSurveys, NecropsyTissueArchival, Disturbance, Predation, Misnetting, Acoustics, OtherDataType, OtherDataTypeMethod)
