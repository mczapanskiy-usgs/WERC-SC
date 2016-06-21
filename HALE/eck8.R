## this code is used to classify bait types for HALE pred control data
## cat food, dog food, cat&dog food, cat/dog food + scent, other

library("data.table", lib.loc="~/R/win-library/3.2")
library("dplyr", lib.loc="~/R/win-library/3.2")
library(mosaic)

read.csv('~/WERC-SC/HALEcatch_traploc_weeks_baitTypes.csv',
         stringsAsFactors = FALSE)
  
## test for duplicates in catchID for catch_traploc_weeks_baitType
unique <- !catch_traploc_weeks_baitType$catchID %in% catch_traploc_weeks_baitType$catchID[duplicated(catch_traploc_weeks_baitType$catchID)]
summary(unique) ## no duplicate IDs now
