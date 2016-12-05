## this script statistically analyzes the effect of year, trap location, trap type, and bait type on predator event

library(stats)
library(data.table)
library(plyr)
library(dplyr)
library(ggplot2)
library(ez)
library(mlogit)

read.csv('~/WERC-SC/HALE/TraplinePredEventPUE_11.csv',
          stringsAsFactors = FALSE) -> predEventPUE
## normal distribution? freq hist should be ~symetical, SD of most variable sample should be <10x the SD of least variable sample
hist(predEventPUE$CPUE)
sd(predEventPUE$CPUE)
## look at data
summary(predEventPUE)
dim(predEventPUE)
with(predEventPUE, table(predEvent))
with(predEventPUE, table(Trapline, predEvent))
with(predEventPUE, table(Year, predEvent))
with(predEventPUE, table(Season, predEvent))
with(predEventPUE, table(Trapline, predEvent, Season))

## There are `r length(predEvents)` possible outcomes every time trap was set, & trap was set `r nEvents` times.  
predEvents <- unique(predEventPUE$predEvent)
nEvents <- sum(predEventPUE$NEvents)
## Total rows in data would be the product `r nEvents*length(predEvents)`.  Perform in 2 steps.  
# First, expand the rows so that each choice situation is on its own unique row.
data2 <- predEventPUE[rep(row.names(predEventPUE), predEventPUE$NEvents),]
data2 <- data2 %>%
  mutate(chid = row.names(data2))
# Then expand ea. choice situation so that ea. alternative is on its own row. Alternative names stored in column `x`.
expanded_data <- merge(predEvents, data2)
expanded_data <- expanded_data %>% 
  mutate(choice = ifelse(x==predEvent, TRUE, FALSE)) 
# Now apply the `mlogit.data` function.  
cpue <- mlogit.data(expanded_data, choice="choice",
                     alt.var ="x", 
                     shape="long", chid.var="chid")

## create model list
cpue.models <- list()

# Simple `mlogit` model where Trapline & Season are specific to the choice situation (i.e. "individual"-specific, not alternative-specific).
cpue.models[[1]] <- mlogit(choice ~ 0 | Trapline + Season , data=cpue)

# Possible problem 0-valued cells?  What happens when data are restricted to traplines where every outcome occurred >= once.
cpue2 <- mlogit.data(expanded_data %>% 
                       filter(Trapline %in% c('A','B','C','D','E','F')), 
                     choice="choice",
                     alt.var ="x", 
                     shape="long", chid.var="chid")
cpue.models[[2]] <- mlogit(choice ~ 0 | Season, data=cpue2)


## remove the mouse data and rerun the mlogit analysis
predEventPUE2 <- predEventPUE %>% 
  filter(predEvent != 'mouseCaught')

predEvents2 <- unique(predEventPUE2$predEvent)
nEvents2 <- sum(predEventPUE2$NEvents)
# First, expand the rows so that each choice situation is on its own unique row.
data3 <- predEventPUE2[rep(row.names(predEventPUE2), predEventPUE2$NEvents),]
data3 <- data3 %>%
  mutate(chid = row.names(data3))
# Then expand ea. choice situation so that ea. alternative is on its own row. Alternative names stored in column `x`.
expanded_data2 <- merge(predEvents2, data3)
expanded_data2 <- expanded_data2 %>% 
  mutate(choice = ifelse(x==predEvent, TRUE, FALSE)) 
# mlogit model with season specific to the choice situation. 
cpue3 <- mlogit.data(expanded_data2, 
                     choice="choice",
                     alt.var ="x", 
                     shape="long", chid.var="chid")
cpue.models[[3]] <- mlogit(choice ~ 0 | Season, data=cpue3)
# ## Poisson log-linear model
# predEvents %>% 
#   mutate(chid = as.factor(chid),
#          Year = as.factor(Year)) -> predEvents
# 
# glm.test <- system.time(                                     # how much time does it take to run the model
#   cpue1 <- glm(predEvent ~ chid + Year + Season + Trapline,
#                family = poisson,
#                data = predEvents)
# )
# tail(coefficients(summary(cpue1)), 5) # no random effects in this model

# poi <- glm(NEvents ~ offset(log(NTraps)) + Trapline + Year + Season, data = predEventPUE, family=poisson)
# Qpoi <- glm(NEvents ~ offset(log(NTraps)) + Trapline + Year + Season, data = predEventPUE, family=quasipoisson)
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