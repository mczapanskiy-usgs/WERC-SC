### import HALE pred control data 2000-2015 and make catchID, remove duplicates

setwd("~/WERC-SC/HALE") ## setwd("~/PredControl/analysis")

## import .csv data
traps <- read.csv("Traps_2014needtoappend.csv")   ##("Traps_AllYears.csv")  _2000-2013 "traps_addDuplicates.csv"
## traps$trapID <- ([,c(Year, Month, Day, "_", Trapline, TrapNum)])

## make all catchID values 2-digits (month, day, and trap number)
month <- sprintf("%02d", traps$Month)
month <- as.matrix(month)
traps$Month <- month

day <- sprintf("%02d", traps$Day)
day <- as.matrix(day)
traps$Day <- day

## front-country traps on have 1 or 2-digit numbers, these could be standardized to 2-digits using the following 
# trap <- sprintf("%02d", traps$TrapNum)
# trap <- as.matrix(trap)
# traps$TrapNum <- trap

traps$dateStr <- paste( traps$Year,traps$Month,traps$Day, sep = "") ## combine year, month, and day
traps$date <- as.Date(traps$dateStr, format="%Y%m%d")
traps$trap <- paste(traps$Trapline,traps$TrapNum, sep = "")  ## combine the trap line and trap number, trap type is not consistent within this value
traps$catchID <- paste(traps$dateStr,traps$trap, sep = "_")  ## unique trapID based on the date the trap was set and the location

write.table(traps, file = "catch_1_2014toappend.csv", sep=",",
            row.names = FALSE)

### clean up data
## test for duplicates in catchID
unique <- !traps$catchID %in% traps$catchID[duplicated(traps$catchID)]
summary(unique)

## analyze duplicate catchIDs 
duplicates <- rbind(traps[ duplicated(traps$catchID, fromLast=TRUE),], 
                    traps[ duplicated(traps$catchID),])
duplicateSum <- summary(duplicates)
## remove duplicate catchIDs 
# traps2 <- subset(traps, !duplicated(traps[,24])) #this only removes on set of the duplicate

# # blank values were converted to "NA"s when importing from excel, changed them all to "0"
# traps[is.na(traps)] <- 0 

## save data
save(traps, file = "traps.RData")