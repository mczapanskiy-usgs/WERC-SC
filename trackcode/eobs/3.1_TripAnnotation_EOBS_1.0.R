## Feb 8 Max Czapanskiy USGS
## July 8 2015 Bill Henry USGS
# Annotates tracks with trip number
# also replaces any interpolated start times with a colony or nest location.

# clear all
rm(list=ls())

library('plyr')
library('dplyr')

# select a species
spp="LAAL"

filename="LAAL_5"

####  dir.in for .csv files
dir.in <- 'D:/Share_Data/Tracking_Data/EOBS/'

#### dir.out for .csv files
dir.out <- 'D:/Share_Data/Tracking_Data/EOBS/All_tracks/'

# Read data, select relevant data and columns
filt.tracks <- data.frame(read.csv(paste(dir.in,'All_tracks/All_Species_EOBS_allTracks.csv',sep="")))[,c(2:4,15,14)]
filt.tracks <- filt.tracks[filt.tracks$species==spp,]
trip.info <- data.frame(read.csv(paste(dir.in,spp,'/3_Trips/',filename,'_tripInfo.csv',sep='')))
# Each Deploy_ID has one entry for deployment and retrieval. They're functionally equivalent for
# this purpose so we only want unique Deploy_ID/lat/lon combos.

#################you ARE HERE!!!!!!!!!############
metadata <- data.frame(read.csv(paste(dir.in,'metadata_EOBS.csv',sep='')))

#### get metadata for Species omitting any metadata records without deployment or recovery
metadata<-(subset(metadata, Species==spp & Tagging_Event=='D' & loc.data==1))

# Parse dates
filt.tracks <- transform(filt.tracks, UTC = as.POSIXct(UTC, tz = "GMT", format = "%Y-%m-%d %H:%M:%S"))
trip.info <- transform(trip.info, tripSt = as.POSIXct(tripSt, tz = "GMT", format = "%Y-%m-%d %H:%M:%S"), tripEnd = as.POSIXct(tripEnd, tz = "GMT", format = "%Y-%m-%d %H:%M:%S"))
metadata <- transform(metadata, UTC = as.POSIXct(UTC, tz = "GMT", format = "%m/%d/%Y %H:%M" ))

# Merge tracks with trip.info on deploy id and time in trip boundaries (conditional join)
annotated.tracks<- filter(merge(filt.tracks, trip.info, 'Deploy_ID'), UTC >= tripSt & UTC <= tripEnd)

# Parse dates
filt.tracks <- transform(filt.tracks, UTC = as.character(UTC))
trip.info <- transform(trip.info, tripSt = as.character(tripSt), tripEnd = as.character(tripEnd))
metadata <- transform(metadata, UTC = as.character(UTC))

# Add track points for interpolated start/end at time = trip start/end and lat/lon = nest lat/lon
start.interpolated <- mutate(filter(trip.info, tripStComp == 3), UTC = tripSt)
end.interpolated <- mutate(filter(trip.info, tripEndComp == 3), UTC = tripEnd)

interp.start.ends<-rbind(start.interpolated, end.interpolated)
at.nest <- merge(interp.start.ends, metadata[, c("Deploy_ID",setdiff(colnames(metadata),colnames(interp.start.ends)))], by="Deploy_ID")
# at.nest <- merge(rbind(start.interpolated, end.interpolated), metadata, "Deploy_ID")

for (i in 1:length(at.nest[,1])) {               
if (at.nest$Nest_loc_use[i]==0) { 
  at.nest$Latitude[i]<-at.nest$Col_Lat_DD[i]
  at.nest$Longitude[i]<-at.nest$Col_Long_DD[i]
} else { 
  at.nest$Latitude[i]<-at.nest$Nest_Lat_DD[i]
  at.nest$Longitude[i]<-at.nest$Nest_Long_DD[i]
  }
}

interpolated.points<-at.nest[,c(1,8,91,92,22,2:7)]
 
interpolated.points<-rename(interpolated.points,species=Species,latitude=Latitude,longitude=Longitude)

# Append interpolated track points and sort
annotated.tracks <- arrange(rbind(annotated.tracks, interpolated.points), Deploy_ID, UTC)[,c(1,2,3,4,5,10,11)]

write.table(annotated.tracks, paste(dir.out,filename,'_trips_annotated.csv',sep = ""),sep=",",quote=FALSE,col.names=TRUE,row.names=FALSE)

# END