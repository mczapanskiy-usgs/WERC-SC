## This script looks at amounts of bait types lost
## for all events (not just <13 check days)

setwd("~/WERC-SC/HALE")

## load packages
library(stats)
library(data.table)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(mosaic)

#### load data and separate baitTypes
read.csv('~/WERC-SC/HALE/allCatch_25_20171120.csv',
         stringsAsFactors = FALSE) -> allCatch

allCatch_baitType <- allCatch %>% 
  select(-Filename, -StartDate, -Day, -NumCaught, -Sex, -Age, -Condition, -Disposition, -TrapStatus, -BaitStatus, -Trapline, -TrapNum, -Trap_Brand, 
         -Trap_size, -Trap_numDoors, -BaitPresent, -predCaught, -birdCaught, -otherCaught, -BaitPrevOld, -BaitSet, -Observer, -Recorder, -Dummy, -dateStr, -RatInvalid) 
  

#### Separate by bait type and quantify bait lost
baitLostAmounts <- allCatch_baitType %>%
  # filter(predEvent == "baitLost") %>%
  ## remove unneccesary variables
  group_by(predEvent, baitType) %>%
  ## number of each bait type events:
  summarise(count = n()) %>%
  spread(predEvent, count) %>% 
  ## multiply each event by the amount of bait lost (estimated 3oz per trap)
  mutate(baitLost = ifelse(is.na(baitLost),0,baitLost),
         birdOtherCaught = ifelse(is.na(birdOtherCaught),0,birdOtherCaught),
         catCaught = ifelse(is.na(catCaught),0,catCaught),
         mongooseCaught = ifelse(is.na(mongooseCaught),0,mongooseCaught),
         none = ifelse(is.na(none),0,none),
         ratCaught = ifelse(is.na(ratCaught),0,ratCaught),
         trapTriggered = ifelse(is.na(trapTriggered),0,trapTriggered),
         Total = baitLost + birdOtherCaught + catCaught + mongooseCaught + none + ratCaught + trapTriggered,
         baitLostOz = baitLost*3,
         baitLostProp = baitLost/Total)

### graph amount of bait lost
ggplot(baitLostAmounts, aes(baitType, baitLostOz)) +
  geom_bar(stat = "identity") +
  labs(x = 'Type of Bait Lost (2000 - 2014)', 
       y = 'Total Amount (OZ)') +
  geom_hline(yintercept = 208000) +
  theme_bw()
ggsave(width = 10, height = 5, dpi=300, filename = "~/WERC-SC/HALE/outputs/baitLostAmounts_eck27.pdf")

ggplot(baitLostAmounts, aes(baitType, baitLostProp)) +
  geom_bar(stat = "identity") +
  labs(x = 'Type of Bait Lost (2000 - 2014)', 
       y = 'Proportion of Bait Used (oz)') +
  ylim(c(0,1)) +
  theme_bw()
ggsave(width = 10, height = 5, dpi=300, filename = "~/WERC-SC/HALE/outputs/baitLostProp_eck27.pdf")

write.csv(allCatch_baitType, file = '~/WERC-SC/HALE/allCatch_27_baitType.csv',
          row.names = FALSE) 

