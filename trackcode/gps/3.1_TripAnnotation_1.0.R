####
## June 10, 2015 Bill Henry USGS
## July 8 2015 Bill Henry USGS (added capacity to accomodate/filter duplicated date/times)
## Feb 8 2015 Max Czapanskiy USGS (vectorized)
####

####
# Annotates tracks with trip number
# also replaces any interpolated start times with a colony or nest location.
#

# clear all
rm(list=ls())

library('dplyr')

# select a spp
spp="PFSH"

# 1=WTSH
# 1.5=RTTR, BRBO, RFBO
# 2.5 = PFSH

radius="2.5"

filename=paste(spp,radius,sep='_')

####  meta.in for meta.csv file
meta.in <- 'D:/Share_Data/GitHub/WERC-SC/trackcode/gps/'

####  dir.in for tracks.csv file
dir.in <- 'D:/Share_Data/Tracking_Data/GPS/All_tracks/'

####  trips.in for trips.csv file
trips.in<-'D:/Share_Data/Tracking_Data/GPS/3_Trips/'

#### dir.out for .csv files
dir.out <- 'D:/Share_Data/Tracking_Data/GPS/All_tracks/'

# Read data, select relevant data and columns
filt.tracks <- data.frame(read.csv(paste(dir.in,'All_Species_GPS_allTracks.csv',sep='')))

filt.tracks <- filt.tracks[filt.tracks$species==spp & filt.tracks$SDAfilter!="removed",c(1,2,3,21,20)]

filt.tracks <- transform(filt.tracks, UTC = as.POSIXct(UTC, tz = "GMT", format = "%Y-%m-%d %H:%M:%S"))
# filt.tracks$SDAfilter<-as.character(filt.tracks$SDAfilter)

trip.info <- data.frame(read.csv(paste(trips.in,filename,'_tripInfo.csv',sep='')))

# Each Deploy_ID has one entry for deployment and retrieval. They're functionally equivalent for
# this purpose so we only want unique Deploy_ID/lat/lon combos.
metadata <- unique(data.frame(read.csv(paste(meta.in,'metadata_all_GPS.csv',sep='')))[,c(3,9,48:52)]) 
metadata <- metadata[metadata$Species==spp,]

# transform dates
filt.tracks <- transform(filt.tracks, UTC = as.POSIXct(UTC, tz = "GMT", format = "%Y-%m-%d %H:%M:%S"))
trip.info <- transform(trip.info, tripSt = as.POSIXct(tripSt, tz = "GMT", format = "%Y-%m-%d %H:%M:%S"))
trip.info <- transform(trip.info, tripEnd = as.POSIXct(tripEnd, tz = "GMT", format = "%Y-%m-%d %H:%M:%S"))

# Merge tracks with trip.info on deploy id and time in trip boundaries (conditional join)
annotated.tracks<- filter(merge(filt.tracks, trip.info, 'Deploy_ID'), UTC >= tripSt & UTC <= tripEnd)

# Add track points for interpolated start/end at time = trip start/end and lat/lon = nest lat/lon
start.interpolated <- mutate(filter(trip.info, tripStComp == 3), UTC = tripSt)
end.interpolated <- mutate(filter(trip.info, tripEndComp == 3), UTC = tripEnd)
at.nest <- merge(rbind(start.interpolated, end.interpolated), metadata, 'Deploy_ID')

if (length(at.nest[,1])>0) {
#  i=1
for (i in 1:length(at.nest[,1])) {               
if (at.nest$Nest_loc_use[i]==0) { 
  at.nest$Latitude[i]<-at.nest$Col_Lat_DD[i]
  at.nest$Longitude[i]<-at.nest$Col_Long_DD[i]
} else { 
  at.nest$Latitude[i]<-at.nest$Nest_Lat_DD[i]
  at.nest$Longitude[i]<-at.nest$Nest_Long_DD[i]
  }
}
  interpolated.points<-at.nest[,c(1,8,15,16,9,2:7)]
  interpolated.points<-rename(interpolated.points,species=Species)
  # Append interpolated track points and sort
  annotated.tracks <- arrange(rbind(annotated.tracks, interpolated.points), Deploy_ID, trip_no, UTC)
}

# check for and remove duplicates c(UTC,deploy_id,trip_no) from annotated.tracks (trip breaker may put these in under certain conditions)
check<-paste(as.character(annotated.tracks$UTC),as.numeric(annotated.tracks$Deploy_ID+(annotated.tracks$trip_no*.01)),sep="_")
annotated.tracks<-annotated.tracks[duplicated(check)==0,]

# ### adjust any duplicate UTC, deploy_ID, trip_no (which may have resulted from rounding UTC to seconds)
# annotated.tracks$UTC[duplicated(paste(as.character(annotated.tracks$Deploy_ID+(annotated.tracks$trip_no*.01)),as.character(annotated.tracks$UTC),sep="_"))==0,]=annotated.tracks$UTC[(duplicated(paste(as.character(annotated.tracks$Deploy_ID+(annotated.tracks$trip_no*.01)),as.character(annotated.tracks$UTC),sep="_")))]+1

write.table(annotated.tracks, paste(dir.out,filename,'_trips_annotated.csv',sep = ""),sep=",",quote=FALSE,col.names=TRUE,row.names=FALSE)

# END