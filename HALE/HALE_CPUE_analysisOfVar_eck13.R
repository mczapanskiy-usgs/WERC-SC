## this script statistically analyzes the effect of year, trap location, trap type, and bait type on predator event

library("stats", lib.loc="C:/Program Files/R/R-3.2.3/library")
library("data.table", lib.loc="~/R/win-library/3.2")
library("dplyr", lib.loc="~/R/win-library/3.2")
library("nnet", lib.loc="C:/Program Files/R/R-3.2.3/library")

read.csv('~/WERC-SC/HALE/TraplineWeeklyCatches.csv',
         stringsAsFactors = FALSE) -> weeklyCatches

# ## convert data to the correct data types
# weeklyCatches$Year = as.numeric(weeklyCatches$Year)
# weeklyCatches$week = as.numeric(weeklyCatches$week)

## get a feel for the data
with(weeklyCatches, table(predEvent, Year))
with(weeklyCatches, table(predEvent, baitType))

# test <- multinom(predEvent ~ baitType + Year, data = weeklyCatches)
  