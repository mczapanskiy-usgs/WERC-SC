# this is code Max wrote to help me with looking for ideas on how to "bin" time frames in R
## bin "catch weeks" to link 'baitPrev' and 'baitUsed'

setwd("~/WERC-SC/HALE")

library(data.table)
library(dplyr)

# combine original data with revised duplicate data (also add 2014toappend and 2014toappend data)
read.csv('~/WERC-SC/HALE/catch_1_addDuplicates.csv',
         stringsAsFactors = FALSE) -> catch_1_addDuplicates
read.csv('~/WERC-SC/HALE/catch_3_duplicateID.csv',
         stringsAsFactors = FALSE) -> catch_3_duplicateID
catch <- read.csv("catch_3_2011toappend.csv") -> catch_3_2011toappend 
catch <- read.csv("catch_3_2014toappend.csv") -> catch_3_2014toappend
rbind(catch_1_addDuplicates, catch_3_duplicateID, catch_3_2011toappend, catch_3_2014toappend) -> catch_duplicateID_all 

# test for duplicates
unique <- !catch_duplicateID_all$catchID %in% catch_duplicateID_all$catchID[duplicated(catch_duplicateID_all$catchID)]
summary(unique)

# read.csv('~/WERC-SC/HALE/catch_addDuplicates.csv',  ##catch_duplicateID.csv',
#          stringsAsFactors = FALSE) %>%

catch_duplicateID_all %>%
  data.table %>%  # can use column names as variables directly (no $ needed)
  filter(!is.na(TrapNum)) %>% # remove all rows that don't have a trap number (28 entires)
  mutate(date = as.Date(as.character(date), format = '%Y%m%d')) -> catch_duplicateID_all

catch_duplicateID_all %>%
  data.table %>%  # can use column names as variables directly (no $ needed)
#   filter(!is.na(TrapNum)) %>% # remove all rows that don't have a trap number (28 entires)
#   mutate(ObsDate = as.Date(as.character(dateStr), format = '%Y%m%d')) %>% # adds new column "ObsDate"
#   select(Trapline, TrapNum, ObsDate, BaitPrev, BaitSet) %>%
  setkeyv(c('Trapline', 'TrapNum', 'date')) %>% # sort data by these values, in this order
  group_by(Trapline, TrapNum) %>% # distinct trap groups, so that lag(BaitSet) isn't accidentally associated with any other traplines/numbers
  mutate(BaitPrevOld = BaitPrev, 
         BaitPrev = as.character(ifelse(BaitPrevOld != '', BaitPrevOld, lag(BaitSet)))) %>% ## recreated BaitPrev to include all values and renamed BaitPrevOld
  ungroup %>%
  write.csv('~/WERC-SC/HALE/catch_4_duplicateID_20161209.csv',  ##catch_addDuplicates2.csv',
            row.names = FALSE)
# incorporate BaitPrev2 into catch_duplicateID.csv
