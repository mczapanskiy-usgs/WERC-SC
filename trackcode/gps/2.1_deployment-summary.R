
# clear all
rm(list=ls())


library('dplyr')
library('reshape2')

#### dir.out for .csv files
dir.out <- "D:/Share_Data/Tracking_Data/GPS/Stats_out/"

####  meta.in for .csv files
meta.in <- "D:/Share_Data/GitHub/WERC-SC/trackcode/gps/"

# Read metadata and combine year, subcolony, and deployment session into a single variable
metadata <- read.csv(paste(meta.in,'metadata_all_GPS.csv',sep='')) %>% 
  mutate(`Deployment Session` = paste(Species, Year, SubCol_Code, DeplSess, sep = '_'))

# Summarize tagging event types
tagging_summary <- dcast(metadata, `Deployment Session` ~ Tagging_Event) %>% 
  select(`Deployment Session`, 'Bird Handled Not Tagged' = N, Recovered = R, Deployed = D) # %>% 

tagging_summary <- merge(unique(metadata[,c('Deployment Session', 'Species', 'Year', 'SubCol_Code', 'DeplSess')]),tagging_summary)

# Summarize gps deployments
gps_summary <- dcast(filter(metadata, GPS_TagRecov < 6), `Deployment Session` ~ GPS_TagRecov) %>% 
  rename('GPS Deployments' = `0`,
         'GPS Good File Tag OK' = `1`,
         'GPS Tag Lost' = `2`,
         'GPS Good File Tag Problems' = `3`,
         'GPS Tag Problems File Not Useful' = `4`,
         'GPS Tag Problems No File' = `5`) %>% 
  merge(dcast(filter(metadata, Tagging_Event %in% c('D', 'N'), GPS_TagRecov == 6), `Deployment Session` ~ GPS_TagRecov) %>% 
          rename('GP No Tag Deployed' = `6`),
        all = TRUE) 

gps_summary$'Percent GPS Tag Recov'<-(rowSums(gps_summary[,c(3,5,6,7)])/gps_summary[,2])*100

gps_summary$'Percent GPS Tag Data Issue'<-(rowSums(gps_summary[,c(6,7)])/gps_summary[,2])*100

gps_summary$'Percent GPS Bird Resighted'<-(rowSums(gps_summary[,c(3,4,5,6,7)])/gps_summary$'GPS Deployments')*100

gps_summary$'Percent GPS Bird Resighted'[is.nan(gps_summary$'Percent GPS Bird Resighted')]<-0

# Summarize tdr deployments
tdr_summary <- dcast(filter(metadata, TDR_TagRecov < 6), `Deployment Session` ~ TDR_TagRecov) %>%
  rename('TDR Deployments' = `0`,
         'TDR Good File Tag OK' = `1`,
         'TDR Tag Lost' = `2`,
         'TDR Good File Tag Problems' = `3`,
         'TDR Tag Problems File Not Useful' = `4`,
         'TDR Tag Problems No File' = `5`) %>% 
merge(dcast(filter(metadata, Tagging_Event %in% c('D', 'N'), TDR_TagRecov == 6), `Deployment Session` ~ TDR_TagRecov) %>% 
                   rename('TDR No Tag Deployed' = `6`),
                 all = TRUE)

tdr_summary$'Percent TDR Tag Recov'<-(rowSums(tdr_summary[,c(3,5,6,7)])/tdr_summary[,2])*100

tdr_summary$'Percent TDR Tag Data Issue'<-(rowSums(tdr_summary[,c(6,7)])/tdr_summary[,2])*100

tdr_summary$'Percent TDR Bird Resighted'<-(rowSums(tdr_summary[,c(3,4,5,6,7)])/tdr_summary$'TDR Deployments')*100

tdr_summary$'Percent TDR Bird Resighted'[is.nan(tdr_summary$'Percent TDR Bird Resighted')]<-0

# Merge the three summaries together
deployment_summary <- merge(tagging_summary, gps_summary, all = TRUE) %>%
  merge(tdr_summary, all = TRUE)

write.table(deployment_summary, paste(dir.out,"GPS_Deployment_Summary.csv",sep = ""),sep=",",quote=FALSE,col.names=TRUE,row.names=FALSE)
