## this script statistically analyzes the effect of year, trap location, trap type, and bait type on predator event


library("data.table", lib.loc="~/R/win-library/3.2")
library("dplyr", lib.loc="~/R/win-library/3.2")
library(mosaic)

read.csv('~/WERC-SC/HALE/predEventPUE.csv',
         stringsAsFactors = FALSE) -> predEventPUE

var <- lm(predEvent ~ Year * Trapline * baitType, data = predEventPUE)
anova(var)
  