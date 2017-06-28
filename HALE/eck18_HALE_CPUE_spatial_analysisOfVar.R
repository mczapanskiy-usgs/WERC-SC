## This script statistically analyzes spatial data
## (effect of elevation, slope, proximity to roads/trails/fences/structures, vegetation, etc. on trap events)
## using mlogit model

library(stats)
library(data.table)
library(plyr)
library(dplyr)
library(ggplot2)
library(ez)
library(mlogit)
library(mosaic)
library(AICcmodavg)

setwd("~/WERC-SC/HALE")

read.csv('~/WERC-SC/HALE/TrapsGrid.csv', # catch data processed by Ben (elev, slope, prox to roads/trails/fences/structures, veg, etc.) & Jon (grid cells)
         stringsAsFactors = FALSE) -> trapData

## normal distribution? freq hist should be ~symetical, SD of most variable sample should be <10x the SD of least variable sample
# hist(trapData$)
# sd(trapData$)
## look at data
summary(trapData)
dim(trapData)
with(trapData, table(predEvent))

#### EDIT DATA: remove the mouse events, separate front and backcountry traps, & group predator events (for rerun of mlogit analysis)
data_rev <- trapData %>% 
  filter(predEvent != 'mouseCaught') %>%
  mutate(Date = as.Date(Date, "%m/%d/%Y"),
         eventType = mosaic::derivedFactor(
           "predatorEvent" = predEvent %in% c('ratCaught', 'catCaught', 'mongooseCaught'),
           "otherEvent" = predEvent %in% c('birdOtherCaught', 'trapTriggered', 'baitLost'),
           "noEvent" = predEvent =="none",
            .default = "noEvent"),
         loc = mosaic::derivedFactor(
           front = Trapline %in% c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'),
           back = Trapline %in% c('HAL', 'KAP', 'KAU', 'KW', 'LAI', 'LAU', 'NAM', 'PAL', 'PUU', 'SS', 'WAI'),
            .default = "back"))

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
# formatData_old <- function(data, subset = NA){
#   # Reshape data so that there is one row for every option, for every choice situation.
#   # Here are the possible outcomes every time the trap is set:
#   events <- unique(data$eventType) # events <- unique(data$eventCnoC) # events <- unique(data$predEvent) # 
#   # And here are the number of choice situations:
#   nEvents <- sum(data$NEvents)
#   
#   # Replicate the rows according to number of events:
#   data2 <- data[rep(row.names(data), data$NEvents),]
#   data2 <- data2 %>%
#     mutate(chid = row.names(data2))
#   
#   if (!is.na(subset)){
#     data2 <- data2[sample(1:nrow(data2), subset),]
#   }
#   
#   # Expand each choice situation so that each alternative is on its own row.
#   # Do this with the merge function.  The alternative names will be stored in column `x`.
#   expanded_data <- merge(events, data2)
#   expanded_data <- expanded_data %>%
#     mutate(choice = ifelse(x==eventType, TRUE, FALSE), # mutate(choice = ifelse(x==eventCnoC, TRUE, FALSE), # mutate(choice = ifelse(x==predEvent, TRUE, FALSE),  # 
#            YearCat = as.factor(Year),
#            YearCts = as.numeric(Year))
#   return(expanded_data)
# }
# 

# 
# #### CREATE LONG DATA TABLES
# expanded_data <- formatData(data_rev, 'predEvent')