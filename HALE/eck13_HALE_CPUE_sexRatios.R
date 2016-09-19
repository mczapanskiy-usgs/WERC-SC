### this script is used to test the sex ratios of the predators caught

library(plyr)
library(dplyr)
library(MASS)

read.csv('~/WERC-SC/HALE/catch_11_traploc_baitTypes_predEvent_weeklyCatches.csv',
         stringsAsFactors = FALSE) -> weeklyCatches

## filter data to relevant values
weeklyCatches$Sex <- revalue(weeklyCatches$Sex, c("m"="M"))

sexes <- c("F", "M", "NR", "UNK", "Unk")
preds <- c("catCaught", "ratCaught", "mouseCaught", "mongooseCaught")

catch_sexRatio <- weeklyCatches %>% 
  filter(Sex %in% sexes & predEvent %in% preds)

## create a table of sex ratios for analysis
sexRatio <- table(catch_sexRatio$predEvent, catch_sexRatio$Sex) #use select function instead?

# binomial
