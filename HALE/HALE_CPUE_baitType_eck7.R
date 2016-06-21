## this code is used to classify bait types for HALE pred control data
## cat food, dog food, cat&dog food, cat/dog food + scent, other

library("data.table", lib.loc="~/R/win-library/3.2")
library("dplyr", lib.loc="~/R/win-library/3.2")
library(mosaic)

read.csv('~/WERC-SC/HALE/catch_traploc_weekChecks.csv',
         stringsAsFactors = FALSE) %>%

  # remove "not rebaited" data (traps where BaitStatus = N and Comments said not rebaited)
  mutate(baitType = derivedFactor(
         "Lure" = grepl("lure", BaitPrev, ignore.case = TRUE),
         "None" = grepl("none", BaitPrev, ignore.case = TRUE),
         "NR" = grepl("NR" , BaitPrev, ignore.case = TRUE),
         "UNK" = grepl("UNK" , BaitPrev, ignore.case = TRUE),
         "cannedCat+cannedDog" = grepl("^CD$", BaitPrev, ignore.case = TRUE),
         "cannedCat+cannedDog+dryDog+oil" = grepl("CDDO$", BaitPrev, ignore.case = TRUE),
         "cannedCat+cannedDog+other" = grepl("CD", BaitPrev, ignore.case = TRUE),
         "cannedCat"= grepl("^C$", BaitPrev, ignore.case = TRUE),
         "cannedDog" = grepl("^D$", BaitPrev, ignore.case = TRUE),
         "dryDog+oil+other" = grepl("DO+", BaitPrev, ignore.case = TRUE),
         "dryDog+oil" = grepl("^DO$", BaitPrev, ignore.case = TRUE),
         "Other" = grepl("cast", BaitPrev, ignore.case = TRUE),
         "ProfessionalBait"= grepl("cave$|coll$", BaitPrev, ignore.case = TRUE),
         "cannedCat+other"= grepl("^C+", BaitPrev, ignore.case = TRUE), 
         "cannedDog+other" = grepl("^D+", BaitPrev, ignore.case = TRUE),
  .method = "first",
  .default = " ")) -> catch_traploc_weeks_baitType

write.csv(catch_traploc_weeks_baitType, file = '~/WERC-SC/HALE/catch_traploc_weeks_baitTypes.csv',
          row.names = FALSE)  