## this is code Max wrote to help me with looking for ideas on how to "bin" time frames in R
## link trap locations to the trapline/trap number for that date

library("data.table", lib.loc="~/R/win-library/3.2")
library("dplyr", lib.loc="~/R/win-library/3.2")

# load trap metadata: 
# add "filler" start and end dates where there were "NA"s, pull out just neccessary data: trap type and location
read.csv('~/WERC-SC/HALE/trap_metadata_20161012.csv',
         stringsAsFactors = FALSE) %>%
  mutate(StartDate = as.Date(ifelse(is.na(StartYear), 
                                    '1988-01-01', # this is the earliest date that trap locations were recorded (trapline E)
                                    paste(StartYear, StartMonth, StartDay, sep = '-')),
                             format = '%Y-%m-%d'),
         EndDate = as.Date(ifelse(is.na(EndYear), 
                                  '2100-01-01', 
                                  paste(EndYear, EndMonth, EndDay, sep = '-')), 
                           format = '%Y-%m-%d')) %>% ## adds new columns "StartDate" and "EndDate" in correct formats
  select(Trapline, TrapNum, StartDate, EndDate, Trap_Brand, Trap_size, Trap_numDoors, point_X, point_Y) %>% ## Easting, Northing
  data.table %>%
  setkeyv(c('Trapline', 'TrapNum', 'StartDate', 'EndDate')) -> trap_metadata ## created datatable of columns selected above, ordered by these variables

trap_metadata$TrapNum <- as.character(trap_metadata$TrapNum)

# remove duplicates, create duplicate for "join" command
read.csv('~/WERC-SC/HALE/catch_4_duplicateID2.csv',
         stringsAsFactors = FALSE) %>%
  data.table %>%
  filter(!is.na(TrapNum),  # remove entries w/o trap number (shouldn't be any left though)
         duplicate %in% 0:1) %>% # select duplicate codes 0 & 1 (leave out unresolved duplicates)
  mutate(date = as.Date(as.character(dateStr), format = '%Y%m%d'), Dummy = date) %>% 
  setkeyv(c('Trapline', 'TrapNum', 'date', 'Dummy')) -> trap_catches
group_by(trap_catches, Trapline, TrapNum, date) %>% summarize(N = n()) %>% ungroup %>% arrange(-N) %>% nrow # 291,995 obs, 4,451 duplicates removed (290,614 obs, which makes sense because there are 3100 duplicate IDs that are not 0 or 1)

# merge Catches without corresponding trap location
trap_catches %>%
  left_join(trap_metadata, by = c('Trapline', 'TrapNum')) %>%
  mutate(CaughtAtLocation = !is.na(StartDate) & date >= StartDate & date <= EndDate) %>% 
  group_by(Trapline, TrapNum, date) %>%
  summarize(missing_metadata = !any(CaughtAtLocation)) %>% 
  filter(missing_metadata) -> missing_metadata
  write.csv('~/WERC-SC/HALE/missing_metadata_5.csv',
            row.names = FALSE) ## %>% nrow # 6408 (was 6698)

# add data from trap_metadata (trap type, trap size, easting and northing) to trap_catches
foverlaps(x = trap_catches, 
          y = trap_metadata,
          by.x = c('Trapline', 'TrapNum', 'date'),  #  'Dummy'), # foverlaps {data.table} - vector of column names to compute the overlap joins. last 2 columns in both by.x & by.y should each correspond to the start and end interval columns in x and y respectively. Start column should always be <= end column. If x is keyed, by.x = key(x), else key(y). by.y defaults to key(y)
          by.y = c('Trapline', 'TrapNum', 'StartDate', 'EndDate'),
          type = 'within') %>% 
  filter(StartDate != date) -> catches_with_traploc_partial

anti_join(trap_catches, ## anti_join - return all rows from x where there are not matching values in y, keeping just columns from x
          catches_with_traploc_partial,
          by = c('Trapline', 'TrapNum', 'date')) %>% 
  list(select(catches_with_traploc_partial, date:catchID, StartDate:EndDate, Trapline:TrapNum, Trap_Brand:point_Y, Year:dateStr, duplicate:date), .) %>%  ## StartDate:EndDate, Trapline:TrapNum, Year:Dummy, Trap_Brand:Northing), .) %>% 
  rbindlist(fill = TRUE) %>% #  -> catches_with_traploc
  write.csv(.,file = '~/WERC-SC/HALE/catch_5_duplicateID_withtraploc.csv',
            row.names = FALSE)  


