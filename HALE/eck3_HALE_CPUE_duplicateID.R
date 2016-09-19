# this script is used to remove duplicate data from the "catch" database and add them back in with the correct duplicate codes
# previous: run data from TRAPS database through eck1 script,
#          in excel, extracted and sorted duplicates and added duplicate codes

setwd("~/WERC-SC/HALE") ## setwd("~/PredControl/analysis")

## import .csv data
catch <- read.csv("catch.csv")   

## load trap data
load("traps.RData")

## remove duplicate catchIDs 
# catch_unique <- subset(catch, !duplicated(catch[,24])) # this only removes one of the duplicate pair
catch_unique <- catch[!(duplicated(catch$catchID) | duplicated(catch$catchID, fromLast = TRUE)),]

## add in duplicate data with IDs from 2000-2014
duplicates <- read.csv("duplicates_sorted.csv")
# change duplicate dates to MM/DD/YYYY format


catch_duplicateID <- rbind(catch_unique, duplicates)

## summary statistics of duplicates
describe(catch_duplicateID$duplicate)
stats <- summary(duplicates)
write.csv(stats, file = "duplicateStats.csv")
write.csv(catch_duplicateID, file = "catch_duplicateID.csv")  # save all data to run pivot tables on duplicate stats

## save data
save(traps, file = "catch_3_duplicateID.RData")