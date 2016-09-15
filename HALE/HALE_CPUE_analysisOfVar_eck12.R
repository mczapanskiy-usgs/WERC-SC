## this script statistically analyzes the effect of year, trap location, trap type, and bait type on predator event

library("stats", lib.loc="C:/Program Files/R/R-3.2.3/library")
library("data.table", lib.loc="~/R/win-library/3.2")
library("dplyr", lib.loc="~/R/win-library/3.2")
library(ggplot2)
library("ez", lib.loc="~/R/win-library/3.2")

read.csv('~/WERC-SC/HALE/TraplinePredEventPUE.csv',
          stringsAsFactors = FALSE) -> predEventPUE

## normal distribution? frequency histogram should be ~symetical, SD of most variable sample shoudl be <10x the SD of the least variable sample
hist(predEventPUE$CPUE)
sd(predEventPUE$CPUE)

## anova using the car library
ratCaught <- filter(predEventPUE, predEvent == "ratCaught")
rat <- ezANOVA(ratCaught, dv = CPUE, wid = trap, with = c('Year', 'Trapline', 'Season'), type = 2)

anova <- ezANOVA(predEventPUE, 
                 dv = CPUE, 
                 wid = trap, 
                 within_full = c('Year', 'Season'), 
                 between = c('Year', 'Trapline', 'Season'), 
                 observed = ('Year'), 
                 type = 2)
# friedman.test


## anova using normal stats package
# ratCaught <- filter(predEvent == "ratCaught")
# rat <- aov(CPUE ~ Year * Trapline * Season, data = ratCaught)
# summary(aov(rt ~ color * shape + Error(subj/(color + shape)), data = Hays.df)) from http://www.psych.upenn.edu/~baron/rpsych/rpsych.html#htoc60