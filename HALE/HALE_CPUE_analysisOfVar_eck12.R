## this script statistically analyzes the effect of year, trap location, trap type, and bait type on predator event

library("stats", lib.loc="C:/Program Files/R/R-3.2.3/library")
library("data.table", lib.loc="~/R/win-library/3.2")
library("dplyr", lib.loc="~/R/win-library/3.2")
library("nnet", lib.loc="C:/Program Files/R/R-3.2.3/library")
library(ggplot2)
library(Rcpp)

# read.csv('~/WERC-SC/HALE/TraplineWeeklyCatches.csv',
#          stringsAsFactors = FALSE) -> weeklyCatches
# 
# ## get a feel for the data
# with(weeklyCatches, table(predEvent, Year))
# with(weeklyCatches, table(predEvent, baitType))
# 
# mutate(weeklyCatches, catch = (predEvent %in% c('catCaught', 'ratCaught', 'mongooseCaught', 'mouseCaught'))) -> weeklyCatches
# 
# catchGLM <- lm(catch ~ Year + Month, data = weeklyCatches)
# summary(catchGLM)
# anova(catchGLM)
# 
# mutate(weeklySeasonalCatches, catch = (predEvent %in% c('catCaught', 'ratCaught', 'mongooseCaught', 'mouseCaught'))) -> weeklySeasonalCatches
# 
# seasonCatchGLM <- lm(catch ~ Year + season, data = weeklySeasonalCatches)
# summary(seasonCatchGLM)
# anova(seasonCatchGLM)

# ## convert data to the correct data types
# weeklyCatches$Year = as.numeric(weeklyCatches$Year)
# weeklyCatches$week = as.numeric(weeklyCatches$week)

# catch <- c("catCaught", "ratCaught", "mongooseCaught", "mouseCaught")
# noCatch <- c("baitLost", "none", "trapTriggered", "birdOtherCaught")
  
# test <- multinom(predEvent ~ baitType + Year, data = weeklyCatches)
  