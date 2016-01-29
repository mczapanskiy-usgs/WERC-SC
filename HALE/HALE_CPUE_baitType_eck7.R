## this code is used to classify bait types for HALE pred control data
## cat food, dog food, cat&dog food, cat/dog food + scent, other

library("data.table", lib.loc="~/R/win-library/3.2")
library("dplyr", lib.loc="~/R/win-library/3.2")

# remove "not rebaited" data (traps where BaitStatus = N and Comments said not rebaited)
mutate(BaitType = grepl('', 
                           BaitPrev, 
                           ignore.case = TRUE) | 
                  grepl("", 
                          BaitPrev, 
                          ignore.case = TRUE)) %>% 

  