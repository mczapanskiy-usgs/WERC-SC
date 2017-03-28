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
      .default = "back")) 

#### RESTRUCTURE DATA FUNCTION
  formatData <- function(data){
    # Reshape data so that there is one row for every option, for every choice situation. 
    # Here are the possible outcomes every time the trap is set:
    events <- unique(data$eventType) # events <- unique(data$predEvent) # 
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
      mutate(choice = ifelse(x==eventType, TRUE, FALSE)) # mutate(choice = ifelse(x==predEvent, TRUE, FALSE)) # 
    # expanded_data$Year <- as.factor(expanded_data$Year)
    return(expanded_data)
  }

#### CREATE LONG DATA TABLES
# e <- "predEvent"
expanded_data <- formatData(data_rev)

## with only predator data
data.Caughts_only <- data_rev %>% 
  filter(eventType == "predatorEvent")
# e <- "predEvent" 
expanded_data.Caughts_only <- formatData(data.Caughts_only)
with(expanded_data.Caughts_only %>% 
       filter(choice==TRUE), # filter(choice==TRUE & Trapline=='A'),
     table(predEvent, Year))

## run function using grouped 'event' (predator, other, no) variable.
# First, aggregate the data by summing the NEvents within each event group.
data_events <- data_rev %>%
  group_by(Trapline, Week, Year, Season, Month, NTraps, loc, eventType) %>%
  summarise(NEvents=sum(NEvents)) %>%
  mutate(CPUE = NEvents/NTraps) %>% # this line optional
  as.data.frame()
# e = "eventType"
expanded_data.events <- formatData(data_events)
expanded_data.events$Year <- as.numeric(expanded_data.events$Year)

#### RUN mlogt MODELS
## create model list
cpue.models <- list()
## dependent var = predator, other, none (events)
## Trapline & Season = individual-specific variables
cpue3events <- mlogit.data(expanded_data.events %>% 
                             # filter(loc == "front") %>%
                             mutate(trapyr=paste0(Trapline,'-',Year)), 
                    choice="choice",
                    alt.var ="x", 
                    id.var = "trapyr",
                    shape="long", 
                    chid.var="chid")
cpue.models[[10]] <- mlogit(choice ~ 0 | Season + Trapline + Year,
                            rpar=c('noEvent:(intercept)'='n',
                            'otherEvent:(intercept)'='n'),
                            R=50, halton=NA,
                            panel=TRUE, # correlation = TRUE,
                            iterlim=1, print.level=1,
                            data=cpue3events)
summary(cpue.models[[10]])
AIC(cpue.models[[10]])

## Trapline & Season = individual-specific variables (i.e. specific to the choice situation, not alternative-specific).
# first apply the `mlogit.data` function.  
cpue <- mlogit.data(expanded_data, # %>% filter(loc == "front"),
                    choice="choice",
                    alt.var ="x", 
                    shape="long", 
                    chid.var="chid")
cpue.models[[1]] <- mlogit(choice ~ 0 | Trapline + Season + Year, data=cpue) # individual specific variables go in "part 2"; can't be random effects

## season = individual-specific variable (simplified model with only season)
cpue2 <- mlogit.data(expanded_data, # %>% filter(loc == "front"),  
                     choice="choice",
                     alt.var ="x", 
                     shape="long", 
                     chid.var="chid")
cpue.models[[2]] <- mlogit(choice ~ 0 | Season, data=cpue2) 

## Year = alternative-specific season = individual-specific variable.
cpue3 <- mlogit.data(expanded_data %>% 
                       filter(loc == "front"),  
                     choice="choice",
                     alt.var ="x", # predEvent (7 options)
                     id.var = "Year",
                     shape="long", 
                     chid.var="chid")  # reflevel = "none"
cpue.models[[3]] <- mlogit(choice ~ Year | Season, 
                           rpar = c(Year = 'n'), # rpar = c(Year = 'n', Trapline = 'n'),
                           data=cpue3) # Error in solve.default(H, g[!fixed]): Lapack routine dgesv: system is exactly singular: U[3,3] = 0

# season = individual-specific variable; Year = alternative-specific variable 
# and choice simplified to 'event'
cpue4 <- mlogit.data(expanded_data.events %>% 
                       filter(loc == "front"),  
                     choice="choice",
                     alt.var ="x", # event: predator, other, or no
                     id.var = "Year",
                     shape="long", 
                     chid.var="chid") # reflevel = "noEvent"
cpue.models[[4]] <- mlogit(choice ~  Year + Trapline | Season, 
                           rpar = c(Year = 'n'), 
                           R=100, 
                           halton=NA, # no random effects for now...
                           data=cpue4) # Error in solve.default(H, g[!fixed]): Lapack routine dgesv: system is exactly singular: U[3,3] = 0

## Season, Year and Trapline = alternative-specific variables 
cpue5 <- mlogit.data(expanded_data %>% 
                       filter(loc == "front"), 
                     choice="choice",
                     alt.var ="x", 
                     shape="long", 
                     chid.var="chid")
cpue.models[[5]] <- mlogit(choice ~ Season + Trapline + Year,
                           # rpar=c(Year='n', Trapline='n'), 
                           # halton=NA,
                           data=cpue5) # Error in solve.default(H, g[!fixed]): system is computationally singular: reciprocal condition number = 8.832e-34

## season = individual-specific variable, year = random variable
cpue.year <- mlogit.data(expanded_data%>% 
                           filter(loc == "front"), # %>% filter(Trapline %in% c('A')) # >>> too many zeros
              choice="choice",
              alt.var ="x", 
              id.var = "Year",
              shape="long", 
              chid.var="chid")
# only predators caught data in front country
cpue.year.caughts <- mlogit.data(expanded_data.Caughts_only %>% 
                                   filter(loc == "front"), # %>% filter(Trapline %in% c('A')) # >>> too many zeros
             choice="choice",
             alt.var ="x", 
             id.var = "Year",
             shape="long", 
             chid.var="chid")

# season = indiv-sp, year = random variable; front country traps
cpue.models[[6]] <- mlogit(choice ~ 0 | Season,
         rpar=c('ratCaught:(intercept)'='n',
                'mongooseCaught:(intercept)'='n'), R=50, halton=NA,
         panel=TRUE,
         iterlim=1, print.level=1,
         data=cpue.year.caughts) # all data is too big for model
# summary(cpue.models[[6]])
AIC(cpue.models[[6]])

# season and year = indiv-sp, year = random; front country traps
cpue.models[[7]] <- 
  mlogit(choice ~ 0 | Season + Year,
         rpar=c('ratCaught:(intercept)'='n',
                'mongooseCaught:(intercept)'='n'), R=50, halton=NA,
         panel=TRUE,
         #iterlim=1, print.level=1,
         data=cpue.year.caughts) # all data is too big for model
# summary(cpue.models[[7]])
AIC(cpue.models[[7]])


## season = individual-specific variable. Trapline = random variable
cpue.trap <- mlogit.data(expanded_data %>% 
                         filter(loc == "front"), 
                         choice="choice",
                         alt.var ="x", 
                         id.var = "Trapline",
                         shape="long", 
                         chid.var="chid")
# only predators caught data in front country
cpue.trap.caughts <- mlogit.data(expanded_data.Caughts_only %>% 
                filter(loc == "front"), 
              choice="choice",
              alt.var ="x", 
              id.var = "Trapline",
              shape="long", 
              chid.var="chid")
# season and year = indiv-sp, year = random; front country traps
cpue.models[[8]] <- mlogit(choice ~ 0 | Season + Year +Trapline,
                           rpar=c('ratCaught:(intercept)'='n',
                                  'mongooseCaught:(intercept)'='n',
                                  'mongooseCaught:SeasonNestling' ='n',
                                  'ratCaught:SeasonNestling' ='n',
                                  'mongooseCaught:SeasonoffSeason' ='n',
                                  'ratCaught:SeasonoffSeason' ='n',
                                  'mongooseCaught:SeasonPre-laying' ='n',
                                  'ratCaught:SeasonPre-laying' ='n'), R=50, halton=NA,
                           panel=TRUE,
                           iterlim=1, print.level=1,
                           data=cpue.trap.caughts) # all data creates an error
summary(cpue.models[[8]])
AIC(cpue.models[[8]])

## season = individual-specific variable. Year and Trapline = random variable (combined into one variable)
cpue.trapyr <- mlogit.data(expanded_data.Caughts_only %>% 
                             # filter(loc == "front") %>%
                             mutate(trapyr=paste0(Trapline,'-',Year)),
              choice="choice",
              alt.var ="x", 
              id.var = "trapyr",
              shape="long", 
              chid.var="chid")
cpue.models[[9]] <- mlogit(choice ~ 0 | Season + Year + Trapline,
                           rpar=c('ratCaught:(intercept)'='n',
                                  'mongooseCaught:(intercept)'='n'),
                           #        # 'mongooseCaught:SeasonNestling' ='n',
                           #        # 'ratCaught:SeasonNestling' ='n',
                           #        # 'mongooseCaught:SeasonoffSeason' ='n',
                           #        # 'ratCaught:SeasonoffSeason' ='n',
                           #        # 'mongooseCaught:SeasonPre-laying' ='n',
                           #        # 'ratCaught:SeasonPre-laying' ='n'), 
                           R=50, halton=NA,
                           panel=TRUE,
                           iterlim=1, print.level=1,
                           data=cpue.trapyr)
summary(cpue.models[[9]])
AIC(cpue.models[[9]])




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