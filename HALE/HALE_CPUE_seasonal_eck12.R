## this script is used to further analyze CPUE by season

library("data.table", lib.loc="~/R/win-library/3.2")
library("dplyr", lib.loc="~/R/win-library/3.2")
library(mosaic)

read.csv('~/WERC-SC/HALE/catch_traploc_weeks_baitTypes_edited_predEvent.csv',
         stringsAsFactors = FALSE) %>% 
  mutate(season = derivedFactor(
    "offSeason" = Month == 1,
    "Pre-laying" = Month >= 2,
    "Incubation" = Month >= 5,
    "Nestling" = Month >= 7,
    "offSeason" = Month >= 11,
    .method = "last", 
    .defualt = "offSeason")) -> catch_seasons



# ## determine monthly frequency of trap checks
# months  <- data.frame(table(catch$Month)) 
# sum(months$Freq) # 283192
# mutate(months, percent=(Freq/283192)*100) -> months
# 
# ## make table of HAPE seasons
# season <- data.frame(
#   season = c("offSeason", "Pre-laying", "Incubation", "Nestling"),
#   startMonth = c(11, 2, 5, 7), ## as.Date(c('2000-01-01', ...))
#   startDay = c(1, 23, 1, 1),
#   endMonth = c(2, 4, 6, 10),
#   endDay = c(23, 30, 30, 31)
# )
# 


