# this is code Max wrote to help me with looking for ideas on how to "bin" time frames in R
## bin "catch weeks" to link 'baitPrev' and 'baitUsed'

library("data.table", lib.loc="~/R/win-library/3.2")
library("dplyr", lib.loc="~/R/win-library/3.2")

read.csv('~/PredControl/analysis/catch_duplicateID.csv',
         stringsAsFactors = FALSE) %>%
  data.table %>%  # can use column names as variables directly (no $ needed)
  filter(!is.na(TrapNum)) %>% # remove all rows that don't have a trap number (28 entires)
  # mutate(ObsDate = as.Date(as.character(dateStr), format = '%Y%m%d')) %>% # adds new column "ObsDate"
  # select(Trapline, TrapNum, ObsDate, BaitPrev, BaitSet) %>%
  setkeyv(c('Trapline', 'TrapNum', 'date')) %>% # sort data by these values, in this order
  group_by(Trapline, TrapNum) %>% # distinct trap groups, so that lag(BaitSet) isn't accidentally associated with any other traplines/numbers
  mutate(BaitPrevOld = BaitPrev, 
         BaitPrev = as.character(ifelse(BaitPrevOld != '', BaitPrevOld, lag(BaitSet)))) %>% ## recreated BaitPrev to include all values and renamed BaitPrevOld
  ungroup %>%
  write.csv('~/PredControl/analysis/catch_duplicateID2.csv',
            row.names = FALSE)
# incorporate BaitPrev2 into catch_duplicateID.csv
