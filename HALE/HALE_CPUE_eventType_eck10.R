### this script identifies the different "predator event" types

library("data.table", lib.loc="~/R/win-library/3.2")
library("dplyr", lib.loc="~/R/win-library/3.2")
library(mosaic)

read.csv('~/WERC-SC/HALE/catch_traploc_weeks_baitTypes_edited.csv',
         stringsAsFactors = FALSE) -> catch

# function that does a sequental test to classifly each row by event type
is.predEvent <- function(predCaught, birdCaught, otherCaught, trapStatus, baitStatus) {
  ifelse(predCaught == 'FC', 'catCaught', NA)
    ifelse(predCaught == 'HA', 'mongooseCaught', NA)
      ifelse(predCaught == 'MM', 'mouseCaught', NA)
        ifelse(predCaught %in% c('RR', 'RN', 'RB'), 'ratCaught', NA)
          ifelse(!is.na(birdCaught) | !is.na(otherCaught), 'BirdOtherCaught', NA)
            ifelse(trapStatus == 'O' & baitStatus == 'N', 'baitStolen', NA)
              ifelse(is.na(predCaught) & is.na(birdCaught) & trapStatus == "C", 'trapTriggered', NA) ## this includes events when predator escaped
}
  
# predEvent <- function(predCaught, birdCaught, otherCaught, trapStatus, baitStatus) {  
#   if(predCaught == 'FC') return('catCaught')
#   if(predCaught == 'HA') return('mongooseCaught')
#   if(predCaught == 'MM') return('mouseCaught')
#   if(predCaught == '^R$') return('ratCaught')
#   if(!is.na(birdCaught) || !is.na(otherCaught)) return('BirdOtherCaught')
#   if(trapStatus == 'O' && baitStatus == 'N') return('baitStolen')
#   if(is.na(predCaught) && is.na(birdCaught) && trapStatus == "C") return('trapTriggered')
#   return(NA)
# }

catch <- mutate(catch, predEvent = is.predEvent(predCaught, birdCaught, otherCaught, TrapStatus, BaitStatus))

catch %>%
  group_by(Trapline) %>%
  count(predCaught)


  
  