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
          stringsAsFactors = FALSE) -> CPUEdata

## normal distribution? freq hist should be ~symetical, SD of most variable sample should be <10x the SD of least variable sample
hist(CPUEdata$CPUE)
sd(CPUEdata$CPUE)
## look at data
summary(CPUEdata)
dim(CPUEdata)
with(CPUEdata, table(predEvent))
with(CPUEdata, table(Trapline, predEvent))
with(CPUEdata, table(Year, predEvent))
with(CPUEdata, table(Season, predEvent))
with(CPUEdata, table(Trapline, predEvent, Season))

#### EDIT DATA: remove the mouse events, separate front and backcountry traps, & group predator events (for rerun of mlogit analysis)
data_rev <- CPUEdata %>% 
  filter(predEvent != 'mouseCaught') %>% 
  mutate(eventType = mosaic::derivedFactor(
    "predatorEvent" = predEvent %in% c('ratCaught', 'catCaught', 'mongooseCaught'),
    "otherEvent" = predEvent %in% c('birdOtherCaught', 'trapTriggered', 'baitLost'),
    "noEvent" = predEvent =="none",
    .default = "noEvent"),
    loc = mosaic::derivedFactor(
      front = Trapline %in% c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'),
      back = Trapline %in% c('HAL', 'KAP', 'KAU', 'KW', 'LAI', 'LAU', 'NAM', 'PAL', 'PUU', 'SS', 'WAI'),
      .default = "back")) # unique(data$Trapline)

#### RESTRUCTURE DATA FUNCTION
  formatData <- function(data){
    # Reshape data so that there is one row for every option, for every choice situation. 
    # Here are the possible outcomes every time the trap is set:
    events <- unique(data$predEvent) # events <- unique(data$e) # 
    # And here are the number of choice situations:
    nEvents <- sum(data$NEvents)
    
    # Replicate the rows according to number of events:
    data2 <- data[rep(row.names(data), data$NEvents),]
    data2 <- data2 %>%
      mutate(chid = row.names(data2))
    
    # Expand each choice situation so that each alternative is on its own row.  
    # Do this with the merge function.  The alternative names will be stored in column `x`.
    expanded_data <- merge(events, data2)
    expanded_data <- expanded_data %>% 
      mutate(choice = ifelse(x==predEvent, TRUE, FALSE)) # mutate(choice = ifelse(x==e, TRUE, FALSE))
    expanded_data$Year <- as.factor(expanded_data$Year)
    return(expanded_data)
    remove(e)
  }

#### CREATE DATA TABLES
# e <- "predEvent"
expanded_data <- formatData(data_rev)

# filter out predators caught
data.Caughts_only <- data_rev %>% 
  filter(eventType == "predatorEvent")
# e <- "predEvent" 
expanded_data.Caughts_only <- formatData(data.Caughts_only)
with(expanded_data.Caughts_only %>% 
       filter(choice==TRUE), # filter(choice==TRUE & Trapline=='A'),
     table(predEvent, Year))

# # run function using grouped 'event' (predator, other, no) variable.
# # First, aggregate the data by summing the NEvents within each event group.
# data_events <- data_rev %>% 
#   group_by(Trapline, Week, Year, Season, Month, NTraps, loc, eventType) %>%
#   summarise(NEvents=sum(NEvents)) %>%
#   mutate(CPUE = NEvents/NTraps) %>% # this line optional
#   as.data.frame()
# e = "eventType"
# expanded_data.events <- formatData(data_events)
# with(expanded_data.events %>% 
#        filter(choice==TRUE),
#      table(predEvent, Year))

#### RUN mlogt MODELS
## create model list
cpue.models <- list()
# Simple `mlogit` model where Trapline & Season = individual-specific variables (i.e. specific to the choice situation, not alternative-specific).
# first apply the `mlogit.data` function.  
cpue <- mlogit.data(expanded_data, 
                    choice="choice",
                    alt.var ="x", 
                    shape="long", 
                    chid.var="chid")
cpue.models[[1]] <- mlogit(choice ~ 0 | Trapline + Season , data=cpue) # individual specific variables go in "part 2"; can't be random effects

# Possible problem 0-valued cells?  Restrict data to traplines where every outcome occurred >= once (e.g. only front country traps).
cpue2 <- mlogit.data(expanded_data %>%
                       filter(loc == "front"),
                     choice="choice",
                     alt.var ="x",
                     shape="long", 
                     chid.var="chid")
cpue.models[[2]] <- mlogit(choice ~ 0 | Trapline + Season, data=cpue2)

# season = individual-specific variable (simplified model with only season)
cpue3 <- mlogit.data(expanded_data2, # %>% filter(loc == "front"),  
                     choice="choice",
                     alt.var ="x", 
                     shape="long", 
                     chid.var="chid")
cpue.models[[3]] <- mlogit(choice ~ 0 | Season, data=cpue3) # 

# season = individual-specific variable. Year = ??
cpue4 <- mlogit.data(expanded_data2 %>% 
                       filter(loc == "front"),  
                     choice="choice",
                     alt.var ="x", # predEvent (7 options)
                     # id.var = "Year",
                     shape="long", 
                     chid.var="chid")  # reflevel = "none"
cpue.models[[4]] <- mlogit(choice ~ Year | Season, 
                           # rpar = c(Year = 'n') # rpar = c(Year = 'n', Trapline = 'n'), 
                           data=cpue4)

# season = individual-specific variable; Year = alternative-specific variable 
# and choice simplified to 'event'
cpue5 <- mlogit.data(expanded_data.events %>% 
                       filter(loc == "front"),  
                     choice="choice",
                     alt.var ="x", # event: predator, other, or no
                     id.var = "Year",
                     shape="long", 
                     chid.var="chid") # reflevel = "noEvent"
cpue.models[[5]] <- mlogit(choice ~  Year + Trapline | Season, 
                           rpar = c(Year = 'n'), 
                           R=100, 
                           halton=NA, # no random effects for now...
                           data=cpue5) # error: system is exactly singular

# Season, Year and Trapline = alternative-specific variables 
cpue6 <- mlogit.data(expanded_data %>% 
                       filter(loc == "front"), 
                     choice="choice",
                     alt.var ="x", 
                     shape="long", 
                     chid.var="chid")
cpue.models[[6]] <- mlogit(choice ~ Season + Trapline + Year,
                           # rpar=c(Year='n', Trapline='n'), 
                           # halton=NA,
                           data=cpue6) # error: system is compuatationally singular

# season = individual-specific variable, year = random variable
cpue.Year <- mlogit.data(expanded_data.Caughts_only, # %>% filter(Trapline %in% c('A')) [ = too many zeros]
              choice="choice",
              alt.var ="x", 
              id.var = "Year",
              shape="long", 
              chid.var="chid")
cpue.models[[7]] <- mlogit(choice ~ 0 | Season,
         # rpar=c('birdOtherCaught:(intercept)'='n'), R=50, halton=NA,
         # panel=TRUE,
         iterlim=1, print.level=1,
         data=cpue.Year)
summary(cpue.models[[7]])

cpue.models[[8]] <- mlogit(choice ~ 0 | Season,
         rpar=c('catCaught:(intercept)'='n',
                'mongooseCaught:(intercept)'='n'), R=50, halton=NA,
         panel=TRUE,
         #iterlim=1, print.level=1,
         data=cpue.Year)
summary(cpue.models[[8]])
AIC(cpue.models[[8]])

# season = individual-specific variable. Year and Trapline = random variable (combined into one variable)
cpue.trap <- mlogit.data(expanded_data %>% 
                filter(loc == "front"), 
              choice="choice",
              alt.var ="x", 
              id.var = "Trapline",
              shape="long", 
              chid.var="chid")
cpue.trapyr <- mlogit.data(expanded_data %>% 
                filter(loc == "front") %>%
                mutate(trapyr=paste0(Trapline,'-',Year)),
              choice="choice",
              alt.var ="x", 
              id.var = "trapyr",
              shape="long", 
              chid.var="chid")
cpue.models[[9]] <- mlogit(choice ~ 0 | Season,
                           rpar=c('catCaught:(intercept)'='n',
                                  'mongooseCaught:(intercept)'='n'), R=50, halton=NA,
                           panel=TRUE,
                           #iterlim=1, print.level=1,
                           data=cpue.Year)
summary(cpue.models[[9]])
AIC(cpue.models[[9]])


# ## There are `r length(predEvents)` possible outcomes every time trap was set, & trap was set `r nEvents` times.  
# predEvents <- unique(predEventPUE$predEvent)
# nEvents <- sum(predEventPUE$NEvents)
# ## Total rows in data would be the product `r nEvents*length(predEvents)`.  Perform in 2 steps.  
# # First, expand the rows so that each choice situation is on its own unique row.
# data2 <- predEventPUE[rep(row.names(predEventPUE), predEventPUE$NEvents),]
# data2 <- data2 %>%
#   mutate(chid = row.names(data2))
# # Then expand ea. choice situation so that ea. alternative is on its own row. Alternative names stored in column `x`
# expanded_data <- merge(predEvents, data2)
# expanded_data <- expanded_data %>% 
#   mutate(choice = ifelse(x==predEvent, TRUE, FALSE)) 

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