
# clear all
rm(list=ls())


library('dplyr')
library('reshape2')

####  meta.in for .csv files
meta.in <- "D:/Share_Data/GitHub/WERC-SC/trackcode/gps/"

# Read metadata and combine year, subcolony, and deployment session into a single variable
metadata <- read.csv(paste(meta.in,'metadata_all_GPS.csv',sep='')) %>% 
  mutate(`Deployment Session` = paste(Year, SubCol_Code, DeplSess, sep = '_'))

# Summarize tagging event types
tagging_summary <- dcast(metadata, `Deployment Session` ~ Tagging_Event) %>% 
  select(`Deployment Session`, 'Bird Handled Not Tagged' = N, Recovered = R, Deployed = D)

# Summarize gps deployments
gps_summary <- dcast(filter(metadata, GPS_TagRecov < 6), `Deployment Session` ~ GPS_TagRecov) %>% 
  rename('GPS_Deployments' = `0`,
         'GPS_Good_File' = `1`,
         'GPS_Tag_Lost' = `2`,
         'GPS_Tag_Problems_File_OK' = `3`,
         'GPS_Tag Problems_File_Not_Useful' = `4`,
         'GPS_Tag_Problems_No_File' = `5`) %>% 
  merge(dcast(filter(metadata, Tagging_Event %in% c('D', 'N'), GPS_TagRecov == 6), `Deployment Session` ~ GPS_TagRecov) %>% 
          rename('GPS_No_Tag_Deployed' = `6`),
        all = TRUE) %>% View


gps_summary$Percent_GPS_Tag_Recov<-(rowSums(gps_summary[,c(3,5,6,7)])/gps_summary[,2])*100

gps_summary$Percent_GPS_Tag_Data_Issue<-(rowSums(gps_summary[,c(6,7)])/gps_summary[,2])*100

gps_summary$Percent_GPS_Bird_Resighted<-(rowSums(gps_summary[,c(3,4,5,6,7)])/gps_summary$GPS_Deployments)*100

gps_summary$Percent_GPS_Bird_Resighted[is.nan(gps_summary$Percent_GPS_Bird_Resighted)]<-0

# Summarize tdr deployments
tdr_summary <- dcast(metadata, `Deployment Session` ~ TDR_TagRecov) %>%
  rename('TDR_Deployments' = `0`,
         'TDR_Good_File' = `1`,
         'TDR_Tag_Lost' = `2`,
         'TDR_Tag_Problems_File_OK' = `3`,
         'TDR_Tag_Problems_File_Not_Useful' = `4`,
         'TDR_Tag_Problems_No File' = `5`,
         'TDR_No_Tag_Deployed' = `6`)

tdr_summary$Percent_TDR_Tag_Recov<-(rowSums(tdr_summary[,c(3,5,6,7)])/tdr_summary[,2])*100

tdr_summary$Percent_TDR_Tag_Data_Issue<-(rowSums(tdr_summary[,c(6,7)])/tdr_summary[,2])*100

tdr_summary$Percent_TDR_Bird_Resighted<-(rowSums(tdr_summary[,c(3,4,5,6,7)])/tdr_summary$TDR_Deployments)*100

tdr_summary$Percent_TDR_Bird_Resighted[is.nan(tdr_summary$Percent_TDR_Bird_Resighted)]<-0

# Merge the three summaries together
deployment_summary <- merge(tagging_summary, gps_summary, all = TRUE) %>%
  merge(tdr_summary, all = TRUE)