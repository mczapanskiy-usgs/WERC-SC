## this code is used to classify bait types for HALE pred control data
## cat food, dog food, cat&dog food, cat/dog food + scent, other

library(data.table)
library(dplyr)
library(mosaic)

read.csv('~/WERC-SC/HALE/catch_6_traploc_weekChecks_w14_20181226.csv',
         stringsAsFactors = FALSE) %>%
  
# read.csv('~/WERC-SC/HALE/catch_6_traploc_weekChecks_20161209.csv',
#          stringsAsFactors = FALSE) %>%

  # remove "not rebaited" data (traps where BaitStatus = N and Comments said not rebaited)
  mutate(baitType = mosaic::derivedFactor(
    "Lure" = grepl("lure", BaitPrev, ignore.case = TRUE),
    "CannedCat+CannedDog" = grepl("^CD$", BaitPrev, ignore.case = TRUE),
    "CannedCat+CannedDog+DryDog+Oil" = grepl("CDDO$", BaitPrev, ignore.case = TRUE),
    "CannedCat+CannedDog+Other" = grepl("CD", BaitPrev, ignore.case = TRUE),
    "CannedCat"= grepl("^C$", BaitPrev, ignore.case = TRUE),
    "CannedDog" = grepl("^D$", BaitPrev, ignore.case = TRUE),
    "DryDog+Oil(+Other)" = grepl("DO+", BaitPrev, ignore.case = TRUE),
    "DryDog+Oil" = grepl("^DO$", BaitPrev, ignore.case = TRUE),
    "ProfessionalBait"= grepl("cave$|coll$", BaitPrev, ignore.case = TRUE),
    "CannedCat+Other"= grepl("^C+", BaitPrev, ignore.case = TRUE), 
    "CannedDog+Other" = grepl("^D+", BaitPrev, ignore.case = TRUE),
    "Not recorded" = grepl("NR" , BaitPrev, ignore.case = TRUE),
    "None" = grepl("none", BaitPrev, ignore.case = TRUE),
    .method = "first",
    .default = "Other")) -> catch_traploc_weeks_baitType

write.csv(catch_traploc_weeks_baitType, file = '~/WERC-SC/HALE/catch_7_traploc_weeks_baitTypes_w14_20190220.csv',
          row.names = FALSE)
# write.csv(catch_traploc_weeks_baitType, file = '~/WERC-SC/HALE/catch_7_traploc_weeks_baitTypes_20161209_edited.csv',
#           row.names = FALSE)  
