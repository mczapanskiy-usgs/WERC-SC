## this script statistically analyzes the effect of year, trap location, trap type, and bait type on predator event

library(stats)
library(data.table)
library(plyr)
library(dplyr)
library(ggplot2)
library(ez)
library(mlogit)
library(mosaic)

read.csv('~/WERC-SC/HALE/TraplinePredEventPUE_11_20161209.csv',
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

#### RESTRUCTURE DATA
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

## In addition, remove the mouse data and group predator events, for rerun of mlogit analysis
predEventPUE2 <- predEventPUE %>% 
  filter(predEvent != 'mouseCaught') %>% 
  mutate(event = mosaic::derivedFactor(
    "predatorEvent" = predEvent %in% c('ratCaught', 'catCaught', 'mongooseCaught'),
    "otherEvent" = predEvent %in% c('birdOtherCaught', 'trapTriggered', 'baitLost'),
    "noEvent" = predEvent =="none",
    .default = "noEvent"))
  # mutate(predator = (predEvent %in% c('ratCaught', 'catCaught', 'mongooseCaught'))) 
predEvents2 <- unique(predEventPUE2$predEvent)
nEvents2 <- sum(predEventPUE2$NEvents)
# Next, re-expand the rows so that each choice situation is on its own unique row.
data3 <- predEventPUE2[rep(row.names(predEventPUE2), predEventPUE2$NEvents),]
data3 <- data3 %>%
  mutate(chid = row.names(data3))
# Then expand ea. choice situation so that ea. alternative is on its own row. Alternative names stored in column `x`.
expanded_data2 <- merge(predEvents2, data3)
expanded_data2 <- expanded_data2 %>% 
  mutate(choice = ifelse(x==predEvent, TRUE, FALSE)) 
expanded_data2$Year <- as.factor(expanded_data2$Year)

## Create yet another version of expanded_data (expanded_data3) using grouped 'event' (predator, other, no) variable.
# First, aggregate the data by summing the NEvents within each event group.
predEventPUE3 <- predEventPUE2 %>% 
  group_by(Trapline, Week, Year, Season, Month, NTraps, event) %>%
  summarise(NEvents=sum(NEvents)) %>%
  mutate(CPUE = NEvents/NTraps) %>% # this line optional
  as.data.frame()
# There are `r length(event)` possible outcomes every time trap was set, & trap was set `r nEvents` times.
predEvents3 <- unique(predEventPUE3$event)
nEvents3 <- sum(predEventPUE3$NEvents)
# Next, re-expand the rows so that each choice situation is on its own unique row.
data3 <- predEventPUE3[rep(row.names(predEventPUE3), predEventPUE3$NEvents),]
data3 <- data3 %>%
  mutate(chid = row.names(data3))
# Then expand ea. choice situation so that ea. alternative is on its own row. Alternative names stored in column `x`.
expanded_data3 <- merge(predEvents3, data3)
expanded_data3 <- expanded_data3 %>% 
  mutate(choice = ifelse(x==event, TRUE, FALSE)) 
expanded_data3$Year <- as.factor(expanded_data3$Year)


#### RUN MODELS
# Now apply the `mlogit.data` function.  
cpue <- mlogit.data(expanded_data, choice="choice",
                     alt.var ="x", 
                     shape="long", chid.var="chid")
## create model list
cpue.models <- list()
# Simple `mlogit` model where Trapline & Season are specific to the choice situation (i.e. "individual"-specific, not alternative-specific).
cpue.models[[1]] <- mlogit(choice ~ 0 | Trapline + Season , data=cpue)

# # Possible problem 0-valued cells?  What happens when data are restricted to traplines where every outcome occurred >= once.
# cpue2 <- mlogit.data(expanded_data %>% 
#                        filter(Trapline %in% c('A','B','C','D','E','F')), 
#                      choice="choice",
#                      alt.var ="x", 
#                      shape="long", chid.var="chid")
# cpue.models[[2]] <- mlogit(choice ~ 0 | Season, data=cpue2)
# mlogit model with season specific to the choice situation. 
cpue3 <- mlogit.data(expanded_data2, 
                     choice="choice",
                     alt.var ="x", 
                     shape="long", 
                     chid.var="chid")
cpue.models[[3]] <- mlogit(choice ~ 0 | Season, data=cpue3)

# mlogit model with season specific to the choice situation 
# and Year and Trapline as random variables (although mlogit is choking on this?)
cpue4 <- mlogit.data(expanded_data2, 
                     choice="choice",
                     alt.var ="x", # predEvent (7 options)
                     shape="long", 
                     chid.var="chid")  # reflevel = "none"
cpue.models[[4]] <- mlogit(choice ~ 0 | Season + Year, 
                           # rpar = c(Year = 'n'), 
                           data=cpue4)

# mlogit model with season specific to the choice situation 
# and choice simplified to 'event'
cpue5 <- mlogit.data(expanded_data3, 
                     choice="choice",
                     alt.var ="x", # event: predator, other, or no
                     shape="long", 
                     chid.var="chid") # reflevel = "noEvent"
cpue.models[[5]] <- mlogit(choice ~ 0 | Season  + Year + Trapline, 
                           rpar = c(Year = 'n'), 
                           R=100, halton=NA, # no random effects for now...
                           data=cpue5)

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