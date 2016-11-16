## this code is to analyze the time series' of the HALE pred control data
## how frequently are traps checked?  How often are they skipped?  Can we analyze effort /week?

library("data.table", lib.loc="~/R/win-library/3.2")
library("dplyr", lib.loc="~/R/win-library/3.2")

# write RatInvalid loop function to call to later
is.RatInvalid <- function(predCaught, TrapStatus) {
  RatInvalid <- rep(FALSE, length(predCaught))
  flag <- FALSE
  for(i in seq(predCaught)) {
    if(flag && TrapStatus[i] == 'C')
      RatInvalid[i] <- TRUE
    if(predCaught[i] == 'RR')
      flag <- TRUE
    else if(TrapStatus[i] == 'O')
      flag <- FALSE
  }
  RatInvalid
}

read.csv('~/WERC-SC/HALE/catch_5_duplicateID_withtraploc.csv',
         stringsAsFactors = FALSE) %>%
  
  # remove entries for traps that are not present/inactive
  filter(TrapStatus != "M") %>% # TrapStatus = M = missing = trap not present
  
  # flag trap checks with closed rat
  mutate(RatInvalid = is.RatInvalid(predCaught, TrapStatus)) %>% 
  filter(!RatInvalid) %>%
  
  # remove "not rebaited" data (traps where BaitStatus = N and Comments said not rebaited)
  mutate(NotRebaited = grepl('not rebaited', 
                             Comments, 
                             ignore.case = TRUE) | 
                       grepl("didn't rebait", 
                             Comments, 
                             ignore.case = TRUE), 
         BaitPresent = !(BaitStatus %in% c('N', 'NR', 'NI') & NotRebaited)) %>% 
  filter(BaitPresent) %>% 
  
  # count check interval days, flag intervals >14 days
  mutate(date = as.POSIXct(date, format = '%Y-%m-%d')) %>%
  arrange(Trapline, TrapNum, date) %>%
  group_by(Trapline, TrapNum, StartDate) %>%
  mutate(prevCheckInterval = difftime(date, lag(date), units = 'days') %>% 
           as.numeric %>% 
           floor) %>% # mutate(prevCheckInterval = difftime(lead(date), date, units = 'days') %>% as.numeric %>% floor) %>% 
  ungroup %>%
  mutate(TrapChecked = !is.na(prevCheckInterval) & prevCheckInterval < 14) -> catch_traploc_weekChecks # change the number to adjust the "effort" interval

## remove entries with unknown BaitStatus (for Raina to review)
catch_traploc_weekChecks %>%
  filter(BaitStatus != "N" & BaitStatus != "Y" & BaitStatus != "NR"
         & BaitStatus != "y" & BaitStatus != "n" & BaitStatus != "") -> catch_unkBaitStatus
write.csv(catch_unkBaitStatus, '~/WERC-SC/HALE/catch_6_unk_BaitStatus.csv',
            row.names = FALSE)
## save catch output
write.csv(catch_traploc_weekChecks, file = '~/WERC-SC/HALE/catch_6_traploc_weekChecks.csv',
          row.names = FALSE) 

## stats
# average interval between checks
  catch_traploc_weekChecks %>%
  filter(TrapChecked) %>%
  summarize(mean(CheckInterval), sd(CheckInterval))
# number of checks that exceed the check interval
  catch_traploc_weekChecks %>%
  group_by(TrapChecked) %>%
  summarize(n())
# distribution of check intervals
ggplot(filter(catch_traploc_weekChecks, TrapChecked), 
       aes(x = CheckInterval)) + 
  geom_bar()

#   setkeyv(c('Trapline', 'TrapNum', 'date')) %>% # sort data by these values, in this order
#   group_by(Trapline, TrapNum) %>% # distinct trap groups, so that function isn't accidentally associated with any other traplines/numbers
#   mutate(nobait = (ifelse(BaitPrev = '', BaitPrevOld, lag(BaitSet)))) %>%