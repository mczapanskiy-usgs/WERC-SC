## this code is to analyze the time series' of the HALE pred control data
## how frequently are traps checked?  How often are they skipped?  Can we analyze effort /week?

library("data.table", lib.loc="~/R/win-library/3.2")
library("dplyr", lib.loc="~/R/win-library/3.2")

read.csv('~/WERC-SC/HALE/catch_duplicateID_withtraploc.csv',
         stringsAsFactors = FALSE) %>%
  filter(TrapStatus != "M") %>% # TrapStatus = M = missing = trap not present
  mutate(date = as.POSIXct(date, format = '%Y-%m-%d')) %>%
  arrange(Trapline, TrapNum, date) %>%
  group_by(Trapline, TrapNum, StartDate) %>%
  mutate(CheckInterval = difftime(lead(date), date, units = 'days') %>% as.numeric %>% floor) %>% 
  ungroup %>%
  mutate(TrapChecked = !is.na(CheckInterval) & CheckInterval < 14) -> catch_traploc_weekChecks # change the number to adjust the "effort" interval


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

write.csv(catch_traploc_weekChecks, file = '~/WERC-SC/HALE/catch_traploc_weekChecks.csv',
          row.names = FALSE)  
