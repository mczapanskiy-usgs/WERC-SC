#CHIS ASSP Mistnet Database
#Banding QA/QC
#25-26 March 2019
#Amelia DuVall

#============================ 
library(tidyverse)
library(lubridate)
#============================ 

#Read in Banding data sheet (copied/pasted from database version 3/24/2020)
#Change NDs to be read as NAs so morph data interpreted as integers
banding <- read.csv("banding.csv", header = TRUE, na.strings = c("NA", "ND"))

#Filter out ASSP species only and banded individuals only
ASSP <- group_by(.data = banding) %>%
  filter(species == "ASSP") %>%
  filter(band_no != "notbanded") %>%
  ungroup()

#============================ 
##Exploring unique band no's

#Create summary of band no's and no of captures per unique band no
summary <-  group_by(.data = ASSP, band_no) %>% 
  summarise(no_captures = n()) %>% 
  ungroup()

#3644 results
#Is this the number of unique bands?
NROW(unique(ASSP$band_no))
#Yes

#Summarize capture rates
summarycaptures <- group_by(summary, no_captures) %>%
  summarise(count = n()) %>%
  ungroup()

#============================ 
##Exploring recapture field

#How does unique band no's does this match recapture field data?
recap <- group_by(.data = ASSP, recapture) %>%
  summarise(no_captures = n()) %>%
  ungroup()
#3607 new captures
#41 SNR
#196 recaptures

#How to proof these data?
#Unique band numbers are not necessarily new captures 

#============================ 
##Capture time

#Change data type of time stamp and pull out hour as new field
ASSP$capture_time <- mdy_hm(ASSP$capture_time, tz="US/Pacific")
ASSP$cap_hour <- hour(ASSP$capture_time)

unique(ASSP$cap_hour)
#Some problematic data, unique values at 11, 10, 12
#4 also seems like a really late morning capture time, but plausible, double-check. 

summary(ASSP$cap_hour)
#No NA's in capture time. Double-checked database, this is correct. 
#They were filtered out (destroyed/lost bands).

ggplot(data = ASSP) +
  geom_histogram(mapping = aes(x = cap_hour), binwidth = 1)
#Appears that some time stamps were not entered in military time
#Reference raw data and manually fix? Does this need to be flagged?

captimesite <- arrange(ASSP, site_code, capture_time)
#Unsure how to check min/sec of capture time without preservation of data entry order

#============================      
##Release time

#Change data type of release time stamp and pull out hour as new field
ASSP$release_time <- mdy_hm(ASSP$release_time, tz="US/Pacific")
ASSP$rel_hour <- hour(ASSP$release_time)

unique(ASSP$rel_hour)
#Problematic data: 12
#Double-check 4am also

summary(ASSP$rel_hour)
#1920 NA's
#Min and max make sense

ggplot(data = ASSP) +
  geom_histogram(mapping = aes(x = rel_hour), binwidth = 1)
#Also appears to be data not entered in military time

#Plan to reference raw data

#============================                 
##BP

unique(ASSP$BP)
#Fix lowercase values
#All in range of values included in data dictionary
#Not sure how else to QAQC?

#============================ 
##Mass (uncorrected)

unique(ASSP$uncorr_mass)
summary(ASSP$uncorr_mass)
#Large range between 26 - 157  but it's uncorrected data
#493 NAs

ggplot(data = ASSP) +
  geom_bar(mapping = aes(x = uncorr_mass), binwidth =1)
#Bimodal! Could be due to two different weighing containers used.
#Also really high outlier (157)

#============================ 
##Mass tare
unique(ASSP$mass_tare)
summary(ASSP$mass_tare)
#494 NAs
#1 more NA in mass tar than uncorr_mass. Why?

#============================ 
##Mass (corrected)

unique(ASSP$mass_corr)
summary(ASSP$mass_corr)
#494 NAs
#large range between 6 - 135

ggplot(data = ASSP) +
  geom_histogram(mapping = aes(x = as.numeric(mass_corr)), binwidth = 1)

ggplot(data = ASSP) +
  geom_point(mapping = aes(x = mass_corr, y = year, color = as.factor(year)))
#Need to double-check outliers

#============================ 
##Culmen

summary(ASSP$culmen)
#Large range between 10.50 - 24.80
#Mean at 14.51
#Medium at 14.50
#Larger values seen incongruous

ggplot(data = ASSP) +
  geom_bar(mapping = aes(x = culmen))

#============================ 
##Skull length

summary(ASSP$skull_length)
#Range between 26 - 46.90
#Mean at 37.93
#Median at 38
#Lower values seen incongruous 

ggplot(data = ASSP) +
  geom_bar(mapping = aes(x = skull_length))

#============================ 
##Tarsus

summary(ASSP$tarsus)
#Range between 12.60 - 235
#Mean at 23.55
#Median at 23.45

ggplot(data = ASSP) +
  geom_bar(mapping = aes(x = tarsus))

#============================ 
##Wing

summary(ASSP$wing)
#Range between 123 - 1425
#Mean at 141
#Median at 141

ggplot(data = ASSP) +
  geom_bar(mapping = aes(x = wing))

#============================ 
##Tail

summary(ASSP$tail)
#Range between 9 - 83
#Mean 72.10
#Median 78

ggplot(data = ASSP) +
  geom_bar(mapping = aes(x = tail))
             