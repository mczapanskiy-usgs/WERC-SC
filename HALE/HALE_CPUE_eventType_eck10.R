### this script identifies the different "predator event" types

library("data.table", lib.loc="~/R/win-library/3.2")
library("dplyr", lib.loc="~/R/win-library/3.2")
library(mosaic)
library(ggplot2)
library(grid)
library(gridExtra)
library(lubridate)

read.csv('~/WERC-SC/HALE/catch_traploc_weeks_baitTypes_edited.csv',
         stringsAsFactors = FALSE) -> catch

catch$otherCaught <- as.character(catch$otherCaught)
catch$otherCaught[is.na(catch$otherCaught)] <- ""

# create function to identify different predator event types
is.predEvent <- function(predCaught, birdCaught, otherCaught, trapStatus, baitStatus) {
  case_when(
    predCaught == "FC" ~ "catCaught",
    predCaught == "HA" ~ "mongooseCaught",
    predCaught == "MM" ~ "mouseCaught",
    predCaught %in% c("RR", "RN", "RB") ~ "ratCaught",
    birdCaught != "" | otherCaught != "" ~ "birdOtherCaught",
    trapStatus == 'O' & baitStatus == "N" ~ "baitStolen",
    predCaught == "" & birdCaught == "" & trapStatus == "C" ~ "trapTriggered", ## this includes events when predator escaped
    TRUE ~ "none")
}

# create predEvent column based on is.predEvent function
catch <- mutate(catch, predEvent = is.predEvent(predCaught, birdCaught, otherCaught, TrapStatus, BaitStatus))


# save new catch data file with predEvents to GitHub file
write.csv(catch, file = '~/WERC-SC/HALE/catch_traploc_weeks_baitTypes_edited_predEvent.csv',
          row.names = FALSE) 


### summary stats and graphs of predEvent data
# different pred event counts
catch_summary <-
  catch %>%
  group_by(predEvent) %>%
  summarize(name_count = n())
catch_summary

# histogram of pred events per trapline
ggplot(catch, aes(predEvent)) +
  geom_bar() +
  labs(x = 'Predator event type', y = 'frequency') +
  facet_wrap(~ Trapline, nrow = 4) +
  theme(axis.text.x = element_text(angle=60, hjust=1))

# count of predEvents per trapline per year
traplinePredEvent <- 
  catch %>%
  group_by(Trapline, Year, predEvent) %>%
  tally

# plot of predEvents per trapline per year
ggplot(traplinePredEvent, aes(Year, n, color=predEvent)) +
  geom_point() +
  labs(x = 'Year', y = 'Frequency') +
  facet_wrap(~ Trapline, nrow = 4) +
  theme(axis.text.x = element_text(angle=60, hjust=1))




# function that does a sequental test to classifly each row by event type
# is.predEvent <- function(predCaught, birdCaught, otherCaught, trapStatus, baitStatus) {
#   ifelse(predCaught == 'FC', 'catCaught', NA)
#     ifelse(predCaught == 'HA', 'mongooseCaught', NA)
#       ifelse(predCaught == 'MM', 'mouseCaught', NA)
#         ifelse(predCaught %in% c('RR', 'RN', 'RB'), 'ratCaught', NA)
#           ifelse(!is.na(birdCaught) | !is.na(otherCaught), 'BirdOtherCaught', NA)
#             ifelse(trapStatus == 'O' & baitStatus == 'N', 'baitStolen', NA)
#               ifelse(is.na(predCaught) & is.na(birdCaught) & trapStatus == "C", 'trapTriggered', NA) ## this includes events when predator escaped
# }

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
  