### To read in effort-related observations (ON/OFF, Beaufort, ViewCond), expand them into time series and join with GPS track
####### no replicates of ON_OFF from two observers (clean up input file to have only one record for each ON_OFF change)
####### Make sure each observers "End" row (last row of each survey) has a final Beaufort, ViewCond, NO_OBS value (fill in from their most previous update if necessary)
library(openxlsx)
library(dplyr)
library(lubridate)
library(zoo)
library(Distance)
library(ggplot2)
library(rgdal)

#put all GPS track CSV files in one folder and concatenate them
Tracks.dir <- "C:/JJF/MAMU_Region6_Surveys/2018Data/Tracks/"
GPS_filenames <- list.files(Tracks.dir)
setwd(Tracks.dir)
Tracks <- do.call("rbind", lapply(GPS_filenames, read.csv, header=TRUE, stringsAsFactors=FALSE))
Tracks <- transmute(Tracks, datetime=as.POSIXct(ltime, format="%Y/%m/%d %H:%M:%S", tz="Etc/GMT+7"), Latitude=Latitude, Longitude=Longitude)
Tracks <- distinct(Tracks) #remove duplicates

#define input Excel file with Effort Obs, Animal Obs, and Survey Metadata
input_file <- "C:/JJF/MAMU_Region6_Surveys/2018Data/MAMU2018_MASTER_DATA_09_12_2018.xlsx"

#read Survey metadata worksheet
SurveyMeta <- read.xlsx(input_file, sheet = "SurveyMetadata", startRow = 1, colNames = TRUE, 
                        rowNames = FALSE, detectDates = TRUE, na.strings = c("NA", ""), skipEmptyRows = FALSE)

#read Animal Observations worksheet, convert datetime to proper format
Obs <- read.xlsx(input_file, sheet = "Observations", startRow = 1, colNames = TRUE, 
                    rowNames = FALSE, detectDates = FALSE, na.strings = c("NA", ""), skipEmptyRows = FALSE)
Obs$datetime <- as.POSIXct(paste(paste(Obs$YEAR, Obs$MONTH, Obs$DAY, sep="/"), 
                                 paste(Obs$HOUR, Obs$MIN, Obs$SEC, sep=":"), sep=" "), 
                           format="%Y/%m/%d %H:%M:%S", tz="Etc/GMT+7")
# Obs$datetime <- convertToDateTime(Obs$datetime, origin = "1900-01-01", tz="Etc/GMT+7")

#read Effort worksheet, convert datetime to proper format
Effort <- read.xlsx(input_file, sheet = "EffortConditions", startRow = 1, colNames = TRUE, 
          rowNames = FALSE, detectDates = TRUE, na.strings = c("NA", ""), skipEmptyRows = FALSE)
Effort$datetime <- as.POSIXct(paste(paste(Effort$YEAR, Effort$MONTH, Effort$DAY, sep="/"), 
                                 paste(Effort$HOUR, Effort$MIN, Effort$SEC, sep=":"), sep=" "), 
                           format="%Y/%m/%d %H:%M:%S", tz="Etc/GMT+7")
# Effort$datetime <- convertToDateTime(Effort$datetime, origin = "1900-01-01", tz="Etc/GMT+7")

#build OnOff start and end times
EffortONOFF <- filter(Effort, !is.na(ON_OFF)) %>% #extract all records of ON_OFF updates and arrange in temporal order
  arrange(datetime)
EffortONOFF$datetime2 <- lead(EffortONOFF$datetime) - 1 #create end time for ON_OFF update by getting the start time of the next update and subtracting one second
EffortONOFF <- filter(EffortONOFF, as.Date(datetime)==as.Date(datetime2)) #remove rows where the dates do not match (Effort updates between end of one survey and start of next are not valid)

#repeat above for VIEWCOND_L, VIEWCOND_R, BEAUFORT, NO_OBS
# EffortViewL <- filter(Effort, !is.na(VIEWCOND_L)) %>%
#   arrange(datetime)
# EffortViewL$datetime2 <- lead(EffortViewL$datetime) - 1 
# EffortViewL <- filter(EffortViewL, as.Date(datetime)==as.Date(datetime2))
# 
# EffortViewR <- filter(Effort, !is.na(VIEWCOND_R)) %>%
#   arrange(datetime)
# EffortViewR$datetime2 <- lead(EffortViewR$datetime) - 1 
# EffortViewR <- filter(EffortViewR, as.Date(datetime)==as.Date(datetime2))
# 
# EffortBeaufort <- filter(Effort, !is.na(BEAUFORT)) %>%
#   arrange(datetime) %>%
#   distinct(datetime, .keep_all=TRUE) #discard duplicate datetime beaufort updates
# EffortBeaufort$datetime2 <- lead(EffortBeaufort$datetime) - 1 
# EffortBeaufort <- filter(EffortBeaufort, as.Date(datetime)==as.Date(datetime2))
# 
# EffortNOOBS <- filter(Effort, !is.na(NO_OBS)) %>%
#   arrange(datetime) %>%
#   distinct(datetime, .keep_all=TRUE) #discard duplicate datetime NO_OBS updates
# EffortNOOBS$datetime2 <- lead(EffortNOOBS$datetime) - 1 
# EffortNOOBS <- filter(EffortNOOBS, as.Date(datetime)==as.Date(datetime2))

#create empty data frame for ON_OFF Effort time series to live in
EffortExpanded <- data.frame(datetime=as.POSIXct(character()), ONOFF=factor(), stringsAsFactors=FALSE) 

#loop through each ONOFF Effort update row
for (k in 1:NROW(EffortONOFF)) {
  seq <- as.data.frame(seq.POSIXt(EffortONOFF$datetime[k], EffortONOFF$datetime2[k], by="1 sec")) #create a sequence of rows for every second from start to end of Effort update
  colnames(seq) <- c("datetime") #name column
  seq$Surv_No <- EffortONOFF$Surv_No[k] #add Survey_No field from Effort table
  seq$ONOFF <- EffortONOFF$ON_OFF[k] #add ON_OFF value from Effort table
  EffortExpanded <- rbind(EffortExpanded, seq) #add these rows to the Effort data frame
}

#repeat above for VIEWCOND_L, VIEWCOND_R, BEAUFORT, NO_OBS 
# ViewLExpanded <- data.frame(datetime=as.POSIXct(character()), VIEWCOND_L=factor(), stringsAsFactors=FALSE) 
# for (i in 1:NROW(EffortViewL)) {  
#   seq <- as.data.frame(seq.POSIXt(EffortViewL$datetime[i], EffortViewL$datetime2[i], by="1 sec"))
#   colnames(seq) <- c("datetime")
#   seq$VIEWCOND_L <- EffortViewL$VIEWCOND_L[i]
#   ViewLExpanded <- rbind(ViewLExpanded, seq)
# }
# 
# ViewRExpanded <- data.frame(datetime=as.POSIXct(character()), VIEWCOND_R=factor(), stringsAsFactors=FALSE) 
# for (i in 1:NROW(EffortViewR)) {  
#   seq <- as.data.frame(seq.POSIXt(EffortViewR$datetime[i], EffortViewR$datetime2[i], by="1 sec"))
#   colnames(seq) <- c("datetime")
#   seq$VIEWCOND_R <- EffortViewR$VIEWCOND_R[i]
#   ViewRExpanded <- rbind(ViewRExpanded, seq)
# }
# 
# BeaufortExpanded <- data.frame(datetime=as.POSIXct(character()), BEAUFORT=factor(), stringsAsFactors=FALSE) 
# for (j in 1:NROW(EffortBeaufort)) {  
#   seq <- as.data.frame(seq.POSIXt(EffortBeaufort$datetime[j], EffortBeaufort$datetime2[j], by="1 sec"))
#   colnames(seq) <- c("datetime")
#   seq$BEAUFORT <- EffortBeaufort$BEAUFORT[j]
#   BeaufortExpanded <- rbind(BeaufortExpanded, seq)
# }
# 
# NOOBSExpanded <- data.frame(datetime=as.POSIXct(character()), NOOBS=factor(), stringsAsFactors=FALSE) 
# for (j in 1:NROW(EffortNOOBS)) {  
#   seq <- as.data.frame(seq.POSIXt(EffortNOOBS$datetime[j], EffortNOOBS$datetime2[j], by="1 sec"))
#   colnames(seq) <- c("datetime")
#   seq$NOOBS <- EffortNOOBS$NO_OBS[j]
#   NOOBSExpanded <- rbind(NOOBSExpanded, seq)
# }

#merge it all together by joining based on datetime
EffortExpanded <- left_join(EffortExpanded, BeaufortExpanded, by="datetime") %>% 
  left_join(ViewLExpanded, by="datetime") %>% 
  left_join(ViewRExpanded, by="datetime") %>%
  left_join(NOOBSExpanded, by="datetime") %>%
  left_join(Tracks, by="datetime") %>%
  left_join(SurveyMeta, by="Surv_No") %>%
  distinct() #make sure there are no duplicates

#Join effort to observations
# Obs <- left_join(Obs, EffortExpanded, by="datetime") %>%
#   transmute(Surv_No=Surv_No.x,
#             Survey.Route=Survey.Route,
#             Survey.Type=Survey.Type,
#             Transect.Direction=Transect.Direction,
#             Travel.Direction=Travel.Direction,
#             Latitude=Latitude,
#             Longitude=Longitude,
#             YEAR=YEAR,
#             MONTH=MONTH,
#             DAY=DAY,
#             HOUR=HOUR,
#             MIN=MIN,
#             SEC=SEC,
#             datetime=datetime,
#             ONOFF_Eff = ONOFF,
#             BEAUFORT=BEAUFORT,
#             VIEWCOND=if_else(SIDE=="R", VIEWCOND_R, VIEWCOND_L),
#             OBSERVER=OBSERVER,
#             SIDE=SIDE,
#             ON_OFF_Obs=ON_OFF_Obs,
#             SPP=SPP,
#             TOTAL.COUNT=TOTAL.COUNT,
#             BEHAVIOR=BEHAVIOR,
#             AHY=AHY, IMM=IMM, HY=HY, UNK=UNK,
#             DIST=DIST,
#             ANGLE=ANGLE,
#             distance=as.numeric(DIST)*sin(as.numeric((ANGLE*pi)/180))/1000,
#             FISHHOLDER = FISHHOLDER,
#             FISH.SPECIES.CARRIED=FISH.SPECIES.CARRIED,
#             NOTES=NOTES)
# 
# ### write a CSV output to go use in ArcGIS, where you will create a new field ("Stratum") and spatially identify which observations are "Nearshore" (<1350m) or "Offshore" (>1350m)
# write.csv(Obs, "C:/JJF/MAMU_Region6_Surveys/2018Data/MAMU_Obs_AllSpecies2018.csv", row.names=FALSE)
# write.csv(EffortExpanded, "C:/JJF/MAMU_Region6_Surveys/2018Data/MAMU_GPS_Effort_Clean_2018.csv", row.names=FALSE)
# 
# ########################################################################################################################
# ###### GO TO ARCGIS WITH THIS CSV AND RUN THE SPATIAL ANALYSIS, THEN EXPORT A CSV FROM ARCGIS TO REIMPORT HERE ######
# ##For Linear Effort
# #1. Import route GPX files (or tables of route waypoints with an order field), convert to lines, create a Transect field and use the route name, Merge all routes into a single feature.
# #2. Create a 1350m buffer around "coastn83.shp". First select only the portion of coastline that is relevant. Use Dissolve All, rounded end type.
# #3. Split route lines by the buffered coastline polygon (XTools). Add a new text field called "Stratum" to table of attributes
# #4. Select by location the new line segments that fall within the coastline buffer polygon. In the table of attributes, calculate Stratum field ="Nearshore". Select the inverse of the current selection and calculate Stratum field ="Offshore". De-select all features. 
# #5. Calculate geometry (XTools) of new line segments to get length in km for each segment.
# #6. Export table of attributes as CSV and open in Excel. Pivot the table to reduce to the sum of length for each Transect-Stratum combo. Assemble a new table with Surv_No, Stratum, Length, Direction (N or S) fields and save as CSV.
# 
# ##For Obs
# #1. Import Obs data that you just processed above into ArcGIS and convert to a feature class in a geodatabase.
# #2. Add a new text field called "Stratum" to table of attributes
# #3. Select by location the Obs that intersect the coastline buffer polygon. In the table of attributes, calculate Stratum field ="Nearshore". Select the inverse of the current selection and calculate Stratum field ="Offshore". De-select all features.
# #4. Export the Obs table of attributes as a CSV.
# #######################################################################################################################
# 
# ###### FINAL PREPARATION FOR DISTANCE ANALYSIS #################
# ## read in observation data that was just processed in ArcGIS and filter to only MAMU; 
# # required fields: 
# # "Stratum" (Nearshore or Offshore), 
# # "Surv_No" (survey number), 
# # "TOTAL_COUNT" (total count of individuals for each sighting), 
# # "distance" (perpendicular distance of each sighting from the vessel),
# # "BEHAVIOR" (animal behavior),
# # "VIEWCOND" (viewing conditions at time of observation)
# # "Transect_Direction" (Direction from which transect was drawn)
# Obs <- read.csv("C:/JJF/MAMU_Region6_Surveys/2018Data/MAMU_Obs_2018_Distance.csv") %>%
#   select(-OBJECTID) %>% #remove fields from ArcGIS
#   distinct() %>% #make sure there are no duplicates
#   filter(SPP=="MAMU") %>% #filter to only MAMU
#   filter(!is.na(distance)) #remove rows with NA for distance
# 
# ## read in Effort data; required fields: 
# # "Surv_No" (survey number), 
# # "Stratum" (Nearshore or Offshore), 
# # "LENGTH" (linear effort in km for that survey in that stratum)
# # "Direction" (Direction from which transect was drawn)
# Effort <- read.csv("C:/JJF/MAMU_Region6_Surveys/2018Data/MAMU_Linear_Effort_2018.csv")
# 
# ## join Effort information to observations; use full join beacuse you want record of survey/stratum effort even if no MAMU were seen in that survey/stratum
# Obs <- full_join(Obs, Effort, by=c("Surv_No"="Surv_No", "Stratum"="Stratum"))
# 
# ## Transmute to desired fields and field names that will be needed for DISTANCE analysis
# MAMU_dist_all <- transmute(Obs, 
#                            Stratum=Stratum, #nearshore vs offshore stratum
#                            Area=104.65, #area of each stratum is the same (104.65)
#                            Transect=Surv_No, #the indendent survey is the sample
#                            Effort=LENGTH, #the linear effort in km for each survey in each stratum
#                            Distance=distance, #perpendicular distance of observation to vessel
#                            Cluster.Size=TOTAL_COUNT, #group size of individuals in the observation
#                            Behavior=BEHAVIOR, #beahvior of individuals in the observation
#                            ViewCond=VIEWCOND, #viewing conditions of the observation
#                            Direction=Direction) %>% #direction from which the transect was drawn
#   arrange(Stratum, Transect) #arrange by Stratum, then by Transect (required by DISTANCE software)
#   
# ## Export tab-delimited text file for use in DISTANCE software ##
# write.table(MAMU_dist_all, "C:/JJF/MAMU_Region6_Surveys/2018Data/MAMU_Obs_2018_DistanceInput.txt", sep="\t", row.names=FALSE, na="")
# 
# #####Re-create detection curve plot from DISTANCE#####
# #(1) Paste the data from the clipboard to a text file. Let's say the file is "plot.txt"
# #this reads in the file just created
# forplot<-read.table(file="C:/JJF/MAMU_Region6_Surveys/2018Data/plot.txt", header=T, sep="\t", dec=".")
# #note, depending on your language, dec might be "," rather than "."
# #this plots the detection function or pdf (if point transects)
# plot(forplot$C1, forplot$C2, type="l", ylim=c(0,1),
#      xlab="Distance (km)", ylab="Detection probability")
# #Define labels as you wish
# #this adds in the data bars
# lines(c(0,0), c(forplot$C3[1], forplot$C4[1]))
# lines(forplot$C3, forplot$C4)
# 
# 
# ####Plotting maps
# # First read in the shapefile, using the path to the shapefile and the shapefile name minus the
# # extension as arguments
# coast_shp <- readOGR("C:/JJF/coastline/CA/Coastn83", "coastnWGS84")
# 
# # Next the shapefile has to be converted to a dataframe for use in ggplot2
# coast_df <- filter(fortify(coast_shp), lat<37.5, lat>36.9, long<(-121.9))
# 
# # Now the shapefile can be plotted as either a geom_path or a geom_polygon.
# # Paths handle clipping better. Polygons can be filled.
# # You need the aesthetics long, lat, and group.
# ggplot(filter(Obs, SPP=="MAMU"), aes(x=Longitude, y=Latitude)) +
#   geom_path(data = coast_df,
#             aes(x = long, y = lat, group = group)) +
#   geom_point(data = EffortExpanded,
#              aes(x=Longitude, y=Latitude), color="blue", size=0.15) +
#   geom_point(color="red") +
#   facet_grid(.~Surv_No)

library(leaflet)
library(sp)

EffTest <- filter(EffortExpanded, !is.na(Longitude), Surv_No=="2018-03")
# MAMU <- filter(Obs, SPP=="MAMU", Surv_No=="2018-03")
coordinates(EffTest) <- ~Longitude+Latitude
# coordinates(MAMU) <- ~Longitude+Latitude
pal <- colorFactor(c("white", "black"), domain = c("OFF", "ON"))
leaflet(data = EffTest) %>%
  addTiles() %>%
  addCircleMarkers(radius=1, popup = ~as.character(datetime), color = ~pal(ONOFF))

# 
# EffTest <- filter(EffortExpanded, !is.na(Longitude), Surv_No=="2018-09")
# MAMU <- filter(Obs, SPP=="MAMU", Surv_No=="2018-09")
# coordinates(EffTest) <- ~Longitude+Latitude
# coordinates(MAMU) <- ~Longitude+Latitude
# pal <- colorFactor(c("white", "black"), domain = c("OFF", "ON"))
# leaflet() %>%
#   addTiles() %>%
#   addCircleMarkers(data = EffTest, radius=0.5, popup = ~as.character(datetime), color = ~pal(ONOFF)) %>%
#   addCircleMarkers(data = MAMU, radius=~5*TOTAL.COUNT, color = "red")
# 
# # 
# # # write.csv(EffortExpanded, "EffortExpanded.csv")
