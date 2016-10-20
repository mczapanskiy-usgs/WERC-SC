## this script statistically analyzes the effect of year, trap location, trap type, and bait type on predator event

library("stats", lib.loc="C:/Program Files/R/R-3.2.3/library")
library("data.table", lib.loc="~/R/win-library/3.2")
library("dplyr", lib.loc="~/R/win-library/3.2")
library(ggplot2)
library("ez", lib.loc="~/R/win-library/3.2")

read.csv('~/WERC-SC/HALE/TraplinePredEventPUE_11.csv',
          stringsAsFactors = FALSE) -> predEventPUE
predEventPUE$Year <- as.character(predEventPUE$Year)

## normal distribution? frequency histogram should be ~symetical, SD of most variable sample shoudl be <10x the SD of the least variable sample
hist(predEventPUE$CPUE)
sd(predEventPUE$CPUE)

poi <- glm(NEvents ~ offset(log(NTraps)) + Trapline + Year + Season, data = predEventPUE, family=poisson)

Qpoi <- glm(NEvents ~ offset(log(NTraps)) + Trapline + Year + Season, data = predEventPUE, family=quasipoisson)

negB <- glm.nb(NEvents ~ offset(log(NTraps)) + Trapline + Year + Season, data = predEventPUE)

# ## anova using the car library
# ratCaught <- filter(predEventPUE, predEvent == "ratCaught")
# rat <- ezANOVA(ratCaught, 
#                  dv = CPUE, 
#                  wid = Trapline, 
#                  within_full = c('Trapline'), 
#                  between = c('Year', 'Season'), 
#                  observed = ('Year'), 
#                  type = 2)
# 
# friedman.test(CPUE ~ year | Trapline, data = predEventPUE, subset = predEvent)

## anova using normal stats package
# ratCaught <- filter(predEvent == "ratCaught")
# rat <- aov(CPUE ~ Year * Trapline * Season, data = ratCaught)
# summary(aov(rt ~ color * shape + Error(subj/(color + shape)), data = Hays.df)) from http://www.psych.upenn.edu/~baron/rpsych/rpsych.html#htoc60