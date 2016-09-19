# this is code Max wrote to help me with looking for ideas on how to "bin" time frames in R
## bin "catch weeks" to link 'baitPrev' and 'baitUsed'

library("data.table", lib.loc="~/R/win-library/3.2")
library("dplyr", lib.loc="~/R/win-library/3.2")

# combine original data with revised duplicate data
read.csv('~/WERC-SC/HALE/catch_addDuplicates.csv',
         stringsAsFactors = FALSE) -> catch_1_addDuplicates
read.csv('~/WERC-SC/HALE/catch_duplicateID.csv',
         stringsAsFactors = FALSE) -> catch_3_duplicateID
rbind(catch_addDuplicates, catch_duplicateID) -> catch_duplicateID_all 

# read.csv('~/WERC-SC/HALE/catch_addDuplicates.csv',  ##catch_duplicateID.csv',
#          stringsAsFactors = FALSE) %>%
catch_duplicateID_all %>%
  data.table %>%  # can use column names as variables directly (no $ needed)
  filter(!is.na(TrapNum)) %>% # remove all rows that don't have a trap number (28 entires)
  mutate(ObsDate = as.Date(as.character(dateStr), format = '%Y%m%d')) %>% # adds new column "ObsDate"
#   select(Trapline, TrapNum, ObsDate, BaitPrev, BaitSet) %>%
  setkeyv(c('Trapline', 'TrapNum', 'ObsDate')) %>% # sort data by these values, in this order
  group_by(Trapline, TrapNum) %>% # distinct trap groups, so that lag(BaitSet) isn't accidentally associated with any other traplines/numbers
  mutate(BaitPrevOld = BaitPrev, 
         BaitPrev = as.character(ifelse(BaitPrevOld != '', BaitPrevOld, lag(BaitSet)))) %>% ## recreated BaitPrev to include all values and renamed BaitPrevOld
  ungroup %>%
  write.csv('~/WERC-SC/HALE/catch_4_duplicateID2.csv',  ##catch_addDuplicates2.csv',
            row.names = FALSE)
# incorporate BaitPrev2 into catch_duplicateID.csv
