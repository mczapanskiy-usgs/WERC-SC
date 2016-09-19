## import and organize HALEnet files

library(tidyr)
library(dplyr)

setwd("~/PredControl/HaleNetFiles")

# create file list
files <- list.files(path="~/PredControl/HaleNetFiles", full.names=T, recursive=FALSE)

# function to import each HaleNet weather file
read.halenet.data <- function(file) {
  varName <- sub(".*HaleNet_(.*)_Daily", "\\1", file)
  read.table(file, header=TRUE) %>%
    select(-parameter) %>%
    gather(key=date, value=temp, -(Sta_ID:elev)) %>%
    mutate(date=as.Date(date, format="X%Y.%m.%d")) %>%
    rename_(.dots=setNames(list("temp"),varName))
}

# use funtion to read in and merge all weather files into one worksheet 
haleNet_data <- lapply(files, read.halenet.data) %>%
  Reduce(merge, .)

# save combined HaleNet weather worksheet to GitHub file
write.csv(haleNet_data, file = '~/WERC-SC/HALE/haleNet_data_9.csv',
          row.names = FALSE) 

# rainfall <- read.table("HaleNet_Rainfall_Daily", header=TRUE) %>%
#   gather(key=date, value=rainfall, -(parameter:elev)) %>%
#   mutate(date=as.Date(date, format="X%Y.%m.%d"))