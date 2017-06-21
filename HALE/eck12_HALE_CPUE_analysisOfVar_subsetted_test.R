## this script statistically tests the consistency of resutls
## when subsetting data to run through the model
## by comparing AIC values btwn models of predator events: RAT, CAT, MONGOOSE
## using the mlogit model
## THIS SCRIPT IS CARRIED OVER FROM eck12_HALE_CPUE_analysisOfVar.r

library(stats)
library(plyr)
library(data.table)
library(dplyr)
library(ggplot2)
library(ez)
library(mlogit)
library(mosaic)

setwd("~/WERC-SC/HALE")

read.csv('~/WERC-SC/HALE/TraplinePredEventPUE_11_20161209.csv',
         stringsAsFactors = FALSE) -> CPUEdata

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
      .default = "back"),
    eventCnoC = mosaic::derivedFactor(
      "event" = predEvent %in% c('ratCaught', 'catCaught', 'mongooseCaught', 'birdOtherCaught', 'trapTriggered', 'baitLost'),
      "none" = predEvent == "none", 
      .default = "none")
  ) 

#### RESTRUCTURE DATA FUNCTION
formatData <- function(data, var, subset = NA){
  if(!(var %in% colnames(data)))
    stop(sprintf('var [%s] not found in data columns', var))
  data$var <- getElement(data, var)
  # Reshape data so that there is one row for every option, for every choice situation. 
  # Here are the possible outcomes every time the trap is set:
  events <- unique(data$var) 
  # And here are the number of choice situations:
  nEvents <- sum(data$NEvents)
  
  # Replicate the rows according to number of events:
  data2 <- data[rep(row.names(data), data$NEvents),]
  data2 <- data2 %>%
    mutate(chid = row.names(data2))
  
  if (!is.na(subset)){
    data2 <- data2[sample(1:nrow(data2), subset),]
  }
  
  # Expand each choice situation so that each alternative is on its own row.  
  # Do this with the merge function.  The alternative names will be stored in column `x`.
  expanded_data <- merge(events, data2)
  expanded_data <- expanded_data %>% 
    mutate(choice = ifelse(x==var, TRUE, FALSE), 
           YearCat = as.factor(Year),
           YearCts = as.numeric(Year))
  return(expanded_data)
}

## with only predator data
data.Caughts_only <- data_rev %>% 
  filter(eventType == "predatorEvent")
expanded_data.Caughts_only <- formatData(data.Caughts_only, 'predEvent')
## subset to test validity of subset btwn models
set.seed(20170621)
expanded_data.Caughts_only_subset <- formatData(data.Caughts_only, 'predEvent', subset=500)
expanded_data.Caughts_only_subset2 <- formatData(data.Caughts_only, 'predEvent', subset=500)

### PREDS ONLY ANALYSIS
## Year = random variable
cpue.year.caughts2 <- mlogit.data(expanded_data.Caughts_only %>% 
                                    filter(!(Trapline %in% c('KAU', 'KW', 'LAU', 'PUU', 'SS'))),
                                  choice="choice",
                                  alt.var ="x", 
                                  id.var = "YearCat",
                                  shape="long", 
                                  chid.var="chid")
# subsetted
cpue.year.caughts2_sub1 <- mlogit.data(expanded_data.Caughts_only_subset %>% 
                                         filter(!(Trapline %in% c('KAU', 'KW', 'LAU', 'PUU', 'SS'))),
                                       choice="choice",
                                       alt.var ="x", 
                                       id.var = "YearCat",
                                       shape="long", 
                                       chid.var="chid")
cpue.year.caughts2_sub2 <- mlogit.data(expanded_data.Caughts_only_subset2 %>% 
                                         filter(!(Trapline %in% c('KAU', 'KW', 'LAU', 'PUU', 'SS'))),
                                       choice="choice",
                                       alt.var ="x", 
                                       id.var = "YearCat",
                                       shape="long", 
                                       chid.var="chid")

## Trapline = random variable
cpue.trap.caughts2 <- mlogit.data(expanded_data.Caughts_only %>% 
                                    filter(!(Trapline %in% c('KAU', 'KW', 'LAU', 'PUU', 'SS'))), 
                                  choice="choice",
                                  alt.var ="x", 
                                  id.var = "Trapline",
                                  shape="long", 
                                  chid.var="chid")
#subsetted
cpue.trap.caughts2_sub1 <- mlogit.data(expanded_data.Caughts_only_subset %>% 
                                         filter(!(Trapline %in% c('KAU', 'KW', 'LAU', 'PUU', 'SS'))), 
                                       choice="choice",
                                       alt.var ="x", 
                                       id.var = "Trapline",
                                       shape="long", 
                                       chid.var="chid")
cpue.trap.caughts2_sub2 <- mlogit.data(expanded_data.Caughts_only_subset2 %>% 
                                         filter(!(Trapline %in% c('KAU', 'KW', 'LAU', 'PUU', 'SS'))), 
                                       choice="choice",
                                       alt.var ="x", 
                                       id.var = "Trapline",
                                       shape="long", 
                                       chid.var="chid")

## Year + Trapline = random variable (combined into one variable)
cpue.trapyr.caughts2 <- mlogit.data(expanded_data.Caughts_only %>% 
                                      mutate(trapyr=paste0(Trapline,'-',YearCat)) %>% 
                                      filter(!(Trapline %in% c('KAU', 'KW', 'LAU', 'PUU', 'SS'))),
                                    choice="choice",
                                    alt.var ="x", 
                                    id.var = "trapyr",
                                    shape="long", 
                                    chid.var="chid")
#subsetted
cpue.trapyr.caughts2_sub1 <- mlogit.data(expanded_data.Caughts_only_subset %>% 
                                      mutate(trapyr=paste0(Trapline,'-',YearCat)) %>% 
                                        filter(!(Trapline %in% c('KAU', 'KW', 'LAU', 'PUU', 'SS'))),
                                    choice="choice",
                                    alt.var ="x", 
                                    id.var = "trapyr",
                                    shape="long", 
                                    chid.var="chid")
cpue.trapyr.caughts2_sub2 <- mlogit.data(expanded_data.Caughts_only_subset2 %>% 
                                      mutate(trapyr=paste0(Trapline,'-',YearCat)) %>% 
                                        filter(!(Trapline %in% c('KAU', 'KW', 'LAU', 'PUU', 'SS'))),
                                    choice="choice",
                                    alt.var ="x", 
                                    id.var = "trapyr",
                                    shape="long", 
                                    chid.var="chid")

## without any random effects
cpue.caughts2 <- mlogit.data(expanded_data.Caughts_only %>% 
                               filter(!(Trapline %in% c('KAU', 'KW', 'LAU', 'PUU', 'SS'))),
                             choice="choice",
                             alt.var ="x", 
                             shape="long", 
                             chid.var="chid")
#subsetted
cpue.caughts2_sub1 <- mlogit.data(expanded_data.Caughts_only_subset %>% 
                                    filter(!(Trapline %in% c('KAU', 'KW', 'LAU', 'PUU', 'SS'))),
                             choice="choice",
                             alt.var ="x", 
                             shape="long", 
                             chid.var="chid")
cpue.caughts2_sub2 <- mlogit.data(expanded_data.Caughts_only_subset2 %>% 
                                    filter(!(Trapline %in% c('KAU', 'KW', 'LAU', 'PUU', 'SS'))),
                             choice="choice",
                             alt.var ="x", 
                             shape="long", 
                             chid.var="chid")
#### RUN MODELS
cpue.models <- list()
### ANALYSIS: original (full) datasets
# Year = random variable, Season + Year = individual-specific variables
cpue.models[[22]] <- mlogit(choice ~ 0 | Season + YearCts,
                            rpar=c('ratCaught:(intercept)'='n',
                                   'mongooseCaught:(intercept)'='n'),
                            R=50, halton=NA,
                            panel=TRUE,
                            iterlim=1, print.level=1,
                            data=cpue.year.caughts2) 
# Trapline = random variable, Season + Year = individual-specific variables
cpue.models[[23]] <- mlogit(choice ~ 0 | Season + YearCts,
                            rpar=c('ratCaught:(intercept)'='n',
                                   'mongooseCaught:(intercept)'='n'),
                            R=50, halton=NA,
                            panel=TRUE,
                            # reflevel = "ratCaught",
                            iterlim=1, print.level=1,
                            data=cpue.trap.caughts2)
# Year and Trapline = random variable (combined into one variable), Season + Year = individual-specific variables
cpue.models[[24]] <- mlogit(choice ~ 0 | Season + YearCts,
                            rpar=c('ratCaught:(intercept)'='n',
                                   'mongooseCaught:(intercept)'='n'),
                            R=50, halton=NA,
                            panel=TRUE,
                            iterlim=1, print.level=1,
                            data=cpue.trapyr.caughts2)
# no random effects, Trapline, Season, Year = individual-specific variables
cpue.models[[25]] <- mlogit(choice ~ 0 | Season + YearCts + Trapline,
                            iterlim=1, print.level=1,
                            data=cpue.caughts2) 
# no random effects, Season + Year = individual-specific variables
cpue.models[[26]] <- mlogit(choice ~ 0 | Season + YearCts,
                            iterlim=1, print.level=1,
                            data=cpue.caughts2) 

### SUBSET 1 ANALYSIS
# Year = random variable, Season + Year  = individual-specific variables
cpue.models[[29]] <- mlogit(choice ~ 0 | Season + YearCts,
                            rpar=c('ratCaught:(intercept)'='n',
                                   'mongooseCaught:(intercept)'='n'),
                            R=50, halton=NA,
                            panel=TRUE,
                            iterlim=1, print.level=1,
                            data=cpue.year.caughts2_sub1) 
# Trapline = random variable, Season + Year = individual-specific variables
cpue.models[[30]] <- mlogit(choice ~ 0 | Season + YearCts,
                            rpar=c('ratCaught:(intercept)'='n',
                                   'mongooseCaught:(intercept)'='n'),
                            R=50, halton=NA,
                            panel=TRUE,
                            iterlim=1, print.level=1,
                            data=cpue.trap.caughts2_sub1)
# Year and Trapline = random variable (combined into one variable), Season + Year  = individual-specific variables
cpue.models[[31]] <- mlogit(choice ~ 0 | Season + YearCts,
                            rpar=c('ratCaught:(intercept)'='n',
                                   'mongooseCaught:(intercept)'='n'),
                            R=50, halton=NA,
                            panel=TRUE,
                            iterlim=1, print.level=1,
                            data=cpue.trapyr.caughts2_sub1)
# No random effects, Trapline, Season, Year = individual-specific variables
cpue.models[[32]] <- mlogit(choice ~ 0 | Season + YearCts + Trapline,
                            iterlim=1, print.level=1,
                            data=cpue.caughts2_sub1) 
# No random effects, Season + Year= individual-specific variables
cpue.models[[33]] <- mlogit(choice ~ 0 | Season + YearCts,
                            iterlim=1, print.level=1,
                            data=cpue.caughts2_sub1) 

### SUBSET 2 ANALYSIS
# Year = random variable, Season + Year = individual-specific variables
cpue.models[[34]] <- mlogit(choice ~ 0 | Season + YearCts,
                            rpar=c('ratCaught:(intercept)'='n',
                                   'mongooseCaught:(intercept)'='n'),
                            R=50, halton=NA,
                            panel=TRUE,
                            iterlim=1, print.level=1,
                            data=cpue.year.caughts2_sub2) 
# Trapline = random variable, Season + Year = individual-specific variables
cpue.models[[35]] <- mlogit(choice ~ 0 | Season + YearCts,
                            rpar=c('ratCaught:(intercept)'='n',
                                   'mongooseCaught:(intercept)'='n'),
                            R=50, halton=NA,
                            panel=TRUE,
                            # reflevel = "ratCaught",
                            iterlim=1, print.level=1,
                            data=cpue.trap.caughts2_sub2)
# Year and Trapline = random variable (combined into one variable), Season + Year = individual-specific variables
cpue.models[[36]] <- mlogit(choice ~ 0 | Season + YearCts,
                            rpar=c('ratCaught:(intercept)'='n',
                                   'mongooseCaught:(intercept)'='n'),
                            R=50, halton=NA,
                            panel=TRUE,
                            iterlim=1, print.level=1,
                            data=cpue.trapyr.caughts2_sub2)
# No random effects, Trapline + Season + Year = individual-specific variables
cpue.models[[37]] <- mlogit(choice ~ 0 | Season + YearCts + Trapline,
                            iterlim=1, print.level=1,
                            data=cpue.caughts2_sub2) 
# No random effects, Season + Year= individual-specific variables
cpue.models[[38]] <- mlogit(choice ~ 0 | Season + YearCts,
                            iterlim=1, print.level=1,
                            data=cpue.caughts2_sub2) 

AIC(cpue.models[[22]], cpue.models[[23]], cpue.models[[24]], cpue.models[[25]], cpue.models[[26]])
AIC(cpue.models[[29]], cpue.models[[30]], cpue.models[[31]], cpue.models[[32]], cpue.models[[33]])
AIC(cpue.models[[34]], cpue.models[[35]], cpue.models[[36]], cpue.models[[37]], cpue.models[[38]])

