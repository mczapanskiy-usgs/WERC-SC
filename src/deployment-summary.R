library('dplyr')
library('reshape2')

# Read metadata and combine year, subcolony, and deployment session into a single variable
metadata <- read.csv('trackcode/gps/metadata_all_GPS.csv') %>% 
  mutate(`Deployment Session` = paste(Year, SubCol_Code, DeplSess, sep = '_'))

# Summarize tagging event types
tagging_summary <- dcast(metadata, `Deployment Session` ~ Tagging_Event) %>% 
  select(`Deployment Session`, 'Bird Handled Not Tagged' = N, Recovered = R, Deployed = D)

# Summarize gps deployments
gps_summary <- dcast(metadata, `Deployment Session` ~ GPS_TagRecov) %>% 
  rename('GPS Deployments' = `0`,
         'GPS Good File' = `1`,
         'GPS Tag Lost' = `2`,
         'GPS Tag Problems, File OK' = `3`,
         'GPS Tag Problems, File Not Useful' = `4`,
         'GPS Tag Problems, No File' = `5`,
         'GPS No Tag Deployed' = `6`)

# Summarize tdr deployments
tdr_summary <- dcast(metadata, `Deployment Session` ~ TDR_TagRecov) %>%
  rename('TDR Deployment' = `0`,
         'TDR Good File' = `1`,
         'TDR Tag Lost' = `2`,
         'TDR Tag Problems, File OK' = `3`,
         'TDR Tag Problems, File Not Useful' = `4`,
         'TDR Tag Problems, No File' = `5`,
         'TDR No Tag Deployed' = `6`)

# Merge the three summaries together
deployment_summary <- merge(tagging_summary, gps_summary, all = TRUE) %>%
  merge(tdr_summary, all = TRUE)