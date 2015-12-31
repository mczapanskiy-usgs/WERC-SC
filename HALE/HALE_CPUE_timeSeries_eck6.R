## this code is to analyze the time series' of the HALE pred control data
## how frequently are traps checked?  How often are they skipped?  Can we analyze effort /week?

library("data.table", lib.loc="~/R/win-library/3.2")
library("dplyr", lib.loc="~/R/win-library/3.2")

read.csv('~/PredControl/analysis/catch_duplicateID_withtraploc.csv',
         stringsAsFactors = FALSE) %>% # 293714 obs.
  data.table %>%
  filter(!is.na(TrapNum),  # remove entries w/o trap number (shouldn't be any left though)
         duplicate %in% 0:1)
## also need to remove all TrapStatus = M
         