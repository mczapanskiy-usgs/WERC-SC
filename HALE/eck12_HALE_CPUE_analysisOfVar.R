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

read.csv('~/WERC-SC/HALE/predEventPUE_climate_11.5.csv',
         stringsAsFactors = FALSE) -> CPUEdata_WL



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
data_rev <- CPUEdata_WL %>% 
  filter(predEvent != 'mouseCaught') %>% 
  mutate(eventType = mosaic::derivedFactor(
    "predatorEvent" = predEvent %in% c('ratCaught', 'catCaught', 'mongooseCaught'),
    "otherEvent" = predEvent %in% c('birdOtherCaught', 'trapTriggered', 'baitLost'),
    "noEvent" = predEvent =="none",
    .default = "noEvent"),
    loc = mosaic::derivedFactor(
      front = Trapline %in% c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'),
      back = Trapline %in% c('HAL', 'KAP', 'KAU', 'KW', 'LAI', 'LAU', 'NAM', 'PAL', 'PUU', 'SS', 'WAI'),
      .default = "back"),
    eventCnoC = mosaic::derivedFactor(
      "event" = predEvent %in% c('ratCaught', 'catCaught', 'mongooseCaught', 'birdOtherCaught', 'trapTriggered', 'baitLost'),
      "none" = predEvent == "none", 
      .default = "none")
    ) 

#### RESTRUCTURE DATA FUNCTION
  formatData <- function(data){
    # Reshape data so that there is one row for every option, for every choice situation. 
    # Here are the possible outcomes every time the trap is set:
    events <- unique(data$predEvent) # events <- unique(data$eventType) # events <- unique(data$eventCnoC) # 
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
      mutate(choice = ifelse(x==predEvent, TRUE, FALSE),  # mutate(choice = ifelse(x==eventType, TRUE, FALSE), # mutate(choice = ifelse(x==eventCnoC, TRUE, FALSE),  
             YearCat = as.factor(Year),
             YearCts = as.numeric(Year))
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
     table(predEvent, Trapline))

## run function using grouped 'event' (predator, other, no) variable.
# First, aggregate the data by summing the NEvents within each event group.
data_events <- data_rev %>%
  group_by(Trapline, Week, Year, Season, Month, NTraps, loc, eventType) %>%
  dplyr::summarise(NEvents=sum(NEvents)) %>%
  mutate(CPUE = NEvents/NTraps) %>% # this line optional
  as.data.frame()
expanded_data.events <- formatData(data_events)
# also run for 2 event types: event, no event
data_CnoC <- data_rev %>%
  group_by(Trapline, Week, Year, Season, Month, NTraps, loc, eventCnoC) %>%
  dplyr::summarise(NEvents=sum(NEvents)) %>%
  as.data.frame()
expanded_data.CnoC<- formatData(data_CnoC)

#### RUN mlogt MODELS
## create model list
cpue.models <- list()
## Trapline, Season, Year = individual-specific variables (i.e. specific to the choice situation, not alternative-specific).
# models 1 and 2: separated by front and back country (all trapliness together choked: "Error in solve.default(H, g[!fixed]): system is computationally singular: reciprocal condition number = 1.57643e-16")
# front country traps
cpue <- mlogit.data(expanded_data %>% 
                      # filter(loc == "front") %>% 
                      filter(!(Trapline %in% c('KAU', 'KW', 'LAU', 'PUU', 'SS'))),
                    choice="choice",
                    alt.var ="x", 
                    shape="long", 
                    chid.var="chid")
cpue.models[[1]] <- mlogit(choice ~ 0 | Trapline + Season + Year, # (individual specific variables go in "part 2"; can't be random effects)
                           reflevel = "none",
                           iterlim=1, print.level=1,
                           data=cpue) 
summary(cpue.models[[1]])
AIC(cpue.models[[1]])
# back country traps
cpue2 <- mlogit.data(expanded_data %>% 
                       filter(loc == "back"),  
                     choice="choice",
                     alt.var ="x", 
                     shape="long", 
                     chid.var="chid")
cpue.models[[2]] <- mlogit(choice ~ 0 | Trapline + Season + Year, 
                           reflevel = "none",
                           data=cpue2) 
summary(cpue.models[[2]])
AIC(cpue.models[[2]])

## Year = alternative-specific season = individual-specific variable.
cpue3 <- mlogit.data(expanded_data %>% 
                       filter(loc == "front"),  
                     choice="choice",
                     alt.var ="x", 
                     id.var = "Year",
                     shape="long", 
                     chid.var="chid") 
cpue.models[[3]] <- mlogit(choice ~ Year | Season,
                           reflevel = "none",
                           data=cpue3) # "Error in solve.default(H, g[!fixed]): Lapack routine dgesv: system is exactly singular: U[3,3] = 0"
# If Season, Year and Trapline = alternative-specific variables: "Error in solve.default(H, g[!fixed]): system is computationally singular: reciprocal condition number = 8.832e-34"
 
#### RANDOM EFFECTS
## year = random variable
cpue.year <- mlogit.data(expanded_data
                         %>% filter(loc == "front"), # %>% filter(Trapline %in% c('A')) # >>> too many zeros
              choice="choice",
              alt.var ="x", 
              id.var = "Year",
              shape="long", 
              chid.var="chid")
cpue.models[[4]] <- mlogit(choice ~ 0 | Season + Year + Trapline,
                           rpar=c('ratCaught:(intercept)'='n',
                                  'mongooseCaught:(intercept)'='n'),
                           R=50, halton=NA,
                           panel=TRUE,
                           iterlim=1, print.level=1,
                           data=cpue.year) 
## Trapline = random variable 
cpue.trap <- mlogit.data(expanded_data, # %>% filter(loc == "front"), 
                         choice="choice",
                         alt.var ="x", 
                         id.var = "Trapline",
                         shape="long", 
                         chid.var="chid")
cpue.models[[5]] <- mlogit(choice ~ 0 | Season + Year + Trapline,
                           rpar=c('ratCaught:(intercept)'='n',
                                  'mongooseCaught:(intercept)'='n'),
                           R=50, halton=NA,
                           panel=TRUE,
                           iterlim=1, print.level=1,
                           data=cpue.trap) 
## Trapline and Year = random variable 
cpue.trapyr <- mlogit.data(expanded_data %>% 
                             filter(loc == "back") %>% # filter(!(Trapline %in% c('KAU', 'KW', 'LAU', 'PUU', 'SS'))) %>% # 
                                     mutate(trapyr=paste0(Trapline,'-',Year)),
                                   choice="choice",
                                   alt.var ="x", 
                                   id.var = "trapyr",
                                   shape="long", 
                                   chid.var="chid")
cpue.models[[6]] <- mlogit(choice ~ 0 | Season + Year + Trapline,
                           rpar=c('ratCaught:(intercept)'='n',
                                  'mongooseCaught:(intercept)'='n'),
                           R=50, halton=NA,
                           panel=TRUE,
                           iterlim=1, print.level=1,
                           data=cpue.trapyr) 

### PREDS ONLY ANALYSIS: compare 3 random effect options
# Year = random variable, Trapline, Season, Year = individual-specific variables
cpue.year.caughts <- mlogit.data(expanded_data.Caughts_only
                                 %>% filter(loc == "front"), # %>% filter(Trapline %in% c('A')) # >>> too many zeros
                              choice="choice",
                              alt.var ="x", 
                              id.var = "Year",
                              shape="long", 
                              chid.var="chid")
cpue.models[[7]] <- mlogit(choice ~ 0 | Season + Year + Trapline,
                           rpar=c('ratCaught:(intercept)'='n',
                                  'mongooseCaught:(intercept)'='n'),
                           R=50, halton=NA,
                           panel=TRUE,
                           iterlim=1, print.level=1,
                           data=cpue.year.caughts) 
summary(cpue.models[[7]])
# Trapline = random variable, Trapline, Season, Year = individual-specific variables
cpue.trap.caughts <- mlogit.data(expanded_data.Caughts_only %>%
                                   filter(loc == "front"), 
                                 choice="choice",
                                 alt.var ="x", 
                                 id.var = "Trapline",
                                 shape="long", 
                                 chid.var="chid")
cpue.models[[8]] <- mlogit(choice ~ 0 | Season + Year + Trapline,
                           rpar=c('ratCaught:(intercept)'='n',
                                  'mongooseCaught:(intercept)'='n'),
                           R=50, halton=NA,
                           panel=TRUE,
                           iterlim=1, print.level=1,
                           data=cpue.trap.caughts)
summary(cpue.models[[8]])
## Year and Trapline = random variable (combined into one variable), Trapline, Season, Year = individual-specific variables
cpue.trapyr.caughts <- mlogit.data(expanded_data.Caughts_only %>% 
                             filter(loc == "front") %>%
                             mutate(trapyr=paste0(Trapline,'-',Year)),
                           choice="choice",
                           alt.var ="x", 
                           id.var = "trapyr",
                           shape="long", 
                           chid.var="chid")
cpue.models[[9]] <- mlogit(choice ~ 0 | Season + Year + Trapline,
                           rpar=c('ratCaught:(intercept)'='n',
                                  'mongooseCaught:(intercept)'='n'),
                           R=50, halton=NA,
                           panel=TRUE,
                           iterlim=1, print.level=1,
                           data=cpue.trapyr.caughts)
summary(cpue.models[[9]])
# without any random effects, Trapline, Season, Year = individual-specific variables
cpue.caughts <- mlogit.data(expanded_data.Caughts_only
                            %>% filter(loc == "front"),  
                     choice="choice",
                     alt.var ="x", 
                     shape="long", 
                     chid.var="chid")
cpue.models[[10]] <- mlogit(choice ~ 0 | Season + Year + Trapline,
         iterlim=1, print.level=1,
         data=cpue.caughts) 
summary(cpue.models[[10]])

# now compare AIC models between models 4 - 7
AIC(cpue.models[[7]])
AIC(cpue.models[[8]])
AIC(cpue.models[[9]])
AIC(cpue.models[[10]])


### EVENTS ANALYSIS: dependent var = predator, other, none (events)
## Year + Trapline + Season = individual-specific variables
cpue3events <- mlogit.data(expanded_data.events %>% 
                             filter(loc == "front") %>%
                             mutate(trapyr=paste0(Trapline,'-',YearCat)), # mutate(trapyr=paste0(Trapline,'-',Year)), 
                           choice="choice",
                           alt.var ="x", 
                           id.var = "trapyr",
                           shape="long", 
                           chid.var="chid")
                 
cpue.models[[11]] <- mlogit(choice ~ 0 | Season + Trapline + Year,
                            reflevel = "noEvent",
                            iterlim=1, print.level=1,
                            data=cpue3events)
summary(cpue.models[[11]])
# and random effects
cpue.models[[12]] <- mlogit(choice ~ 1 | Season + Trapline + Year,
                            rpar=c('predatorEvent:(intercept)'='n',
                                   'otherEvent:(intercept)'='n'),
                            R=50, halton=NA,
                            panel=TRUE, # correlation = TRUE,
                            reflevel = "noEvent",
                            iterlim=1, print.level=1,
                            data=cpue3events)
summary(cpue.models[[12]])

# compare AIC
AIC(cpue.models[[11]])
AIC(cpue.models[[12]])
## REMOVED TRAPLINE AS INDIV-SPP EFFECT
# Year & Season = individual-specific variables
cpue.models[[13]] <- mlogit(choice ~ 0 | Season + YearCts,
                            reflevel = "noEvent",
                            iterlim=1, print.level=1,
                            data=cpue3events)
summary(cpue.models[[13]]) # runs
## run on 2 events: event, no event
cpue2events <- mlogit.data(expanded_data.CnoC %>% 
                             filter(!(Trapline %in% c('KAU', 'KW', 'LAU', 'PUU', 'SS'))) %>%# filter(loc == "front") %>%
                             mutate(trapyr=paste0(Trapline,'-',YearCat)), # mutate(trapyr=paste0(Trapline,'-',Year)), 
                           choice="choice",
                           alt.var ="x", 
                           id.var = "trapyr",
                           shape="long", 
                           chid.var="chid")
cpue.models[[13.1]] <- mlogit(choice ~ 0 | Season + YearCts,
                            reflevel = "none",
                            iterlim=1, print.level=1,
                            data=cpue2events)
summary(cpue.models[[13.1]]) # runs
# and random effects
cpue.models[[14]] <- mlogit(choice ~ 1 | Season + YearCts,
                            rpar=c('predatorEvent:(intercept)'='n',
                                   'otherEvent:(intercept)'='n'),
                            R=50, halton=NA,
                            panel=TRUE, # correlation = TRUE,
                            reflevel = "noEvent",
                            iterlim=1, print.level=1,
                            data=cpue3events)
summary(cpue.models[[14]]) # "missing value where TRUE/FALSE needed"
## run on 2 events: event, no event
cpue.models[[14.1]] <- mlogit(choice ~ 1 | Season + YearCts,
                            rpar=c('event:(intercept)'='n'),
                            R=50, halton=NA,
                            panel=TRUE, # correlation = TRUE,
                            reflevel = "none",
                            iterlim=1, print.level=1,
                            data=cpue2events)
summary(cpue.models[[14.1]]) # "missing value where TRUE/FALSE needed"

#### RANDOM EFFECTS 
## year = random variable, year + season = indiv. spp.
cpue.year2 <- mlogit.data(expanded_data,
                         # %>% filter(loc == "front"), # %>% filter(Trapline %in% c('A')) # >>> too many zeros
                         choice="choice",
                         alt.var ="x", 
                         id.var = "YearCat",
                         shape="long", 
                         chid.var="chid")
cpue.models[[15]] <- mlogit(choice ~ 0 | Season + YearCts,
                           rpar=c('ratCaught:(intercept)'='n',
                                  'mongooseCaught:(intercept)'='n'),
                           R=50, halton=NA,
                           panel=TRUE,
                           iterlim=1, print.level=1,
                           data=cpue.year2) # "missing value where TRUE/FALSE needed"
## Trapline = random variable, year + season = indiv. spp. 
cpue.trap2 <- mlogit.data(expanded_data, # %>% filter(loc == "front"), 
                         choice="choice",
                         alt.var ="x", 
                         id.var = "Trapline",
                         shape="long", 
                         chid.var="chid")
cpue.models[[16]] <- mlogit(choice ~ 0 | Season + YearCts,
                           rpar=c('ratCaught:(intercept)'='n',
                                  'mongooseCaught:(intercept)'='n'),
                           R=50, halton=NA,
                           panel=TRUE,
                           iterlim=1, print.level=1,
                           data=cpue.trap2) # "missing value where TRUE/FALSE needed"
## Trapline and Year = random variable 
cpue.trapyr2 <- mlogit.data(expanded_data %>% 
                             # %>% filter(loc == "back") %>% # filter(!(Trapline %in% c('KAU', 'KW', 'LAU', 'PUU', 'SS'))) %>% # 
                             mutate(trapyr=paste0(Trapline,'-',YearCat)),
                           choice="choice",
                           alt.var ="x", 
                           id.var = "trapyr",
                           shape="long", 
                           chid.var="chid")
cpue.models[[17]] <- mlogit(choice ~ 0 | Season + YearCts,
                           rpar=c('ratCaught:(intercept)'='n',
                                  'mongooseCaught:(intercept)'='n'),
                           R=50, halton=NA,
                           panel=TRUE,
                           iterlim=1, print.level=1,
                           data=cpue.trapyr2) # "missing value where TRUE/FALSE needed"

### PREDS ONLY ANALYSIS: compare 3 random effect options
# Year = random variable, Trapline, Season, Year = individual-specific variables
cpue.year.caughts2 <- mlogit.data(expanded_data.Caughts_only,
                                 # %>% filter(loc == "front"), # %>% filter(Trapline %in% c('A')) # >>> too many zeros
                                 choice="choice",
                                 alt.var ="x", 
                                 id.var = "YearCat",
                                 shape="long", 
                                 chid.var="chid")
cpue.models[[18]] <- mlogit(choice ~ 0 | Season + Year,
                           rpar=c('ratCaught:(intercept)'='n',
                                  'mongooseCaught:(intercept)'='n'),
                           R=50, halton=NA,
                           panel=TRUE,
                           iterlim=1, print.level=1,
                           data=cpue.year.caughts2) 
summary(cpue.models[[18]])
# Trapline = random variable, Trapline, Season, Year = individual-specific variables
cpue.trap.caughts2 <- mlogit.data(expanded_data.Caughts_only, # %>%
                                   # filter(loc == "front"), 
                                 choice="choice",
                                 alt.var ="x", 
                                 id.var = "Trapline",
                                 shape="long", 
                                 chid.var="chid")
cpue.models[[19]] <- mlogit(choice ~ 0 | Season + Year,
                           rpar=c('ratCaught:(intercept)'='n',
                                  'mongooseCaught:(intercept)'='n'),
                           R=50, halton=NA,
                           panel=TRUE,
                           iterlim=1, print.level=1,
                           data=cpue.trap.caughts2)
summary(cpue.models[[19]])
## Year and Trapline = random variable (combined into one variable), Trapline, Season, Year = individual-specific variables
cpue.trapyr.caughts2 <- mlogit.data(expanded_data.Caughts_only %>% 
                                     # filter(loc == "front") %>%
                                     mutate(trapyr=paste0(Trapline,'-',YearCat)),
                                   choice="choice",
                                   alt.var ="x", 
                                   id.var = "trapyr",
                                   shape="long", 
                                   chid.var="chid")
cpue.models[[20]] <- mlogit(choice ~ 0 | Season + Year,
                           rpar=c('ratCaught:(intercept)'='n',
                                  'mongooseCaught:(intercept)'='n'),
                           R=50, halton=NA,
                           panel=TRUE,
                           iterlim=1, print.level=1,
                           data=cpue.trapyr.caughts2)
summary(cpue.models[[20]])
# without any random effects, Trapline, Season, Year = individual-specific variables
cpue.caughts2 <- mlogit.data(expanded_data.Caughts_only,
                            # %>% filter(loc == "front"),  
                            choice="choice",
                            alt.var ="x", 
                            shape="long", 
                            chid.var="chid")
cpue.models[[21]] <- mlogit(choice ~ 0 | Season + Year + Trapline,
                            iterlim=1, print.level=1,
                            data=cpue.caughts2) 
summary(cpue.models[[21]])

# now compare AIC models between models 4 - 7
AIC(cpue.models[[18]])
AIC(cpue.models[[19]])
AIC(cpue.models[[20]])
AIC(cpue.models[[21]])
