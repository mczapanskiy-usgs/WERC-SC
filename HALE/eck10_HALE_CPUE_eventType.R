### this script identifies the different "predator event" types

library("data.table", lib.loc="~/R/win-library/3.2")
library("dplyr", lib.loc="~/R/win-library/3.2")
library(mosaic)
library(ggplot2)
library(grid)
library(gridExtra)
library(lubridate)

## set wd
setwd("~/WERC-SC/HALE")

read.csv('~/WERC-SC/HALE/catch_7_traploc_weeks_baitTypes_20161209_edited.csv',
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
    trapStatus == 'O' & baitStatus == "N" ~ "baitLost",
    predCaught == "" & birdCaught == "" & trapStatus == "C" ~ "trapTriggered", ## this includes events when predator escaped
    TRUE ~ "none")
}

# create predEvent column based on is.predEvent function (and remove mouse caught and NA events)
catch <- mutate(catch, predEvent = is.predEvent(predCaught, birdCaught, otherCaught, TrapStatus, BaitStatus)) %>% 
  filter(predEvent != 'mouseCaught',
         predEvent != 'NA')

# save new catch data file with predEvents to GitHub file
write.csv(catch, file = '~/WERC-SC/HALE/catch_10_traploc_weeks_baitTypes_edited_predEvent_20161209.csv',
          row.names = FALSE) 

### summary stats and graphs of predEvent data
# first remove dates when trap hadn't been checked in >14 days
catch %>% 
  filter(!TrapChecked)

# order predEvents for graphing
catch$predEvent <- factor(catch$predEvent, 
                          levels = c("catCaught", "mongooseCaught", "ratCaught", "mouseCaught", "birdOtherCaught", "baitLost", "trapTriggered", "none"))


# different pred event counts
catch_summary <-
  catch %>%
  group_by(predEvent) %>%
  summarize(name_count = n())
catch_summary

# histogram of pred events per trapline
ggplot(catch, aes(predEvent)) +
  geom_bar() +
  labs(x = 'Trap Event Type', y = 'Number of Events (years 2000-2014)') +
  # scale_y_continuous(trans='log10') + 
  # facet_wrap(~ Trapline, nrow = 4) +
  # theme(axis.text.x = element_text(angle=60, hjust=1)) +
  theme_bw()
ggsave(width = 8.5, height = 5, dpi=300, filename = "~/WERC-SC/HALE/outputs/predEvent_hist_eck10.pdf")
#stats
summary(catch$predEvent) # 259,021 obs.
# (catch$predEvent)

# count of predEvents per trapline per year
traplinePredEvent <- 
  catch %>%
  group_by(Trapline, Year, predEvent) %>%
  tally

# plot of predEvents per trapline per year
ggplot(traplinePredEvent, aes(Year, n, color=predEvent)) +
  geom_point() +
  labs(x = 'Year', y = 'Numeber of Events') +
  facet_wrap(~ Trapline, nrow = 4) +
  theme_bw() +
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
  