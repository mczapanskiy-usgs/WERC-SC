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
read.csv('~/WERC-SC/HALE/allCatch_25_20171120.csv.csv',
         stringsAsFactors = FALSE) -> allCatch

allCatch_baitType <- allCatch %>% 
  select(-Filename, -StartDate, -Day, -NumCaught, -Sex, -Age, -Condition, -Disposition, -TrapStatus, -BaitStatus, -Trapline, -TrapNum, -Trap_Brand, -Trap_size, 
       -Trap_numDoors, -BaitPresent, -predCaught, -birdCaught, -otherCaught, -BaitPrevOld, -BaitSet, -Observer, -Recorder, -Dummy, -dateStr, -RatInvalid) 
  

### Separate by bait type and quantify bait lost
baitLostAmounts <- allCatch_baitType %>% 
  filter(predEvent == "baitLost") %>% 
  ## remove unneccesary variables
  group_by(baitType) %>% 
  ## number of each bait type events:
  summarise(count = n()) %>% 
  ## multiply each event by the amount of bait lost (estimated 3oz per trap)
  mutate(baitOz = count*3)
### graph amount of bait lost
ggplot(baitLostAmounts, aes(baitType, baitOz)) +
  geom_bar(stat = "identity") +
  labs(x = 'Type of Bait Lost (2000 - 2014)', 
       y = 'Total Amount (OZ)') +
  geom_hline(yintercept = 208000) +
  theme_bw()
ggsave(width = 10, height = 5, dpi=300, filename = "~/WERC-SC/HALE/outputs/baitLostAmounts_eck27.pdf")

write.csv(allCatch_baitType, file = '~/WERC-SC/HALE/allCatch_27_baitType.csv',
          row.names = FALSE) 

