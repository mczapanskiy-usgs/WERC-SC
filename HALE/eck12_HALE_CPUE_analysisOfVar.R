## this script statistically analyzes the effect of year, trap location, trap type, and bait type on predator event

library(stats)
library(data.table)
library(dplyr)
library(ggplot2)
library(ez)
library(mlogit)

read.csv('~/WERC-SC/HALE/TraplinePredEventPUE_11.csv',
          stringsAsFactors = FALSE) -> predEventPUE
predEvents <- mlogit.data(predEventPUE, choice = "predEvent", shape = "wide")

## normal distribution? frequency histogram should be ~symetical, SD of most variable sample shoudl be <10x the SD of the least variable sample
hist(predEventPUE$CPUE)
sd(predEventPUE$CPUE)

## Poisson log-linear model
predEvents %>% 
  mutate(chid = as.factor(chid),
         Year = as.factor(Year)) -> predEvents

glm.test <- system.time(                                     # how much time does it take to run the model
  cpue1 <- glm(predEvent ~ chid + Year + Season + Trapline,
               family = poisson,
               data = predEvents)
)
tail(coefficients(summary(cpue1)), 5) # no random effects in this model

## mlogit model
cpue2 <- mlogit(predEvent ~ Year + Season + Trapline, data = predEvents,
                rpar = c(Year = "n",
                         Trapline = "n"),
                panel = TRUE)
summary(cpue2)



# poi <- glm(NEvents ~ offset(log(NTraps)) + Trapline + Year + Season, data = predEventPUE, family=poisson)
# 
# Qpoi <- glm(NEvents ~ offset(log(NTraps)) + Trapline + Year + Season, data = predEventPUE, family=quasipoisson)
# 
# negB <- glm.nb(NEvents ~ offset(log(NTraps)) + Trapline + Year + Season, data = predEventPUE)

# ## anova using the car library
# ratCaught <- filter(predEventPUE, predEvent == "ratCaught")
# rat <- ezANOVA(ratCaught, 
#                  dv = CPUE, 
#                  wid = Trapline, 
#                  within_full = c('Trapline'), 
#                  between = c('Year', 'Season'), 
#                  observed = ('Year'), 
#                  type = 2)

## anova using normal stats package
# ratCaught <- filter(predEvent == "ratCaught")
# rat <- aov(CPUE ~ Year * Trapline * Season, data = ratCaught)
# summary(aov(rt ~ color * shape + Error(subj/(color + shape)), data = Hays.df)) from http://www.psych.upenn.edu/~baron/rpsych/rpsych.html#htoc60