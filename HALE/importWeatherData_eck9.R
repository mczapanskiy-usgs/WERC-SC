## import and organize HALEnet files

library(tidyr)
library(dplyr)
setwd("~/PredControl/HaleNetFiles")

rainfall <- read.table("HaleNet_Raifall_Daily", header=TRUE) %>%
  gather(key=date, value=rainfall, -(parameter:elev)) %>%
  mutate(date=as.Date(date, format="X%Y.%m.%d"))
  
relHumidity <- read.table("HaleNet_RelativeHumidity_Daily", header=TRUE) %>%
  gather(key=date, value=relHumidity, -(parameter:elev)) %>%
  mutate(date=as.Date(date, format="X%Y.%m.%d"))

weather <- merge(rainfall, relHumidity)