## Feb 8 Max Czapanskiy USGS
## July 8 2015 Bill Henry USGS
# Annotates tracks with trip number
# also replaces any interpolated start times with a colony or nest location.

# clear all
rm(list=ls())

library('dplyr')

# select a species
spp<-"BFAL"

radius<-"20"

filename=paste(spp,radius,sep='_')

####  dir.in for .csv files
# dir.in <- "D:/Share_Data/Tracking_Data/"
dir.in <- '/Users/henry/Documents/Work/Projects/USGS/USGS 08.10.15/Share 8.11.2015/Tracking_Data/EOBS/'

#### dir.out for .csv files
# dir.out <- "D:/Share_Data/Tracking_Data/EOBS/"
dir.out <- '/Users/henry/Documents/Work/Projects/USGS/USGS 08.10.15/Share 8.11.2015/Tracking_Data/EOBS/All_tracks/'

# open metadata file
metadata<-read.table('/Users/henry/Documents/Work/Projects/USGS/USGS 08.10.15/Share 8.11.2015/GitHub/WERC-SC/trackcode/EOBS/metadata_EOBS.csv',header=T, sep=",", strip.white=T)
#### get metadata for Species omitting any metadata records without loc.data
metadata <- metadata[metadata$Species==spp & metadata$loc.data==1,]

# Read data, select relevant data and columns
filt.tracks <- data.frame(read.csv(paste(dir.in,'All_tracks/All_Species_EOBS_allTracks.csv',sep="")))[,c(2:4,15,14)]
filt.tracks <- filt.tracks[filt.tracks$species==spp,]
trip.info <- data.frame(read.csv(paste(dir.in,spp,'/3_Trips/',filename,'_tripInfo.csv',sep='')))
# Each Deploy_ID has one entry for deployment and retrieval. They're functionally equivalent for
# this purpose so we only want unique Deploy_ID/lat/lon combos.

# Parse dates
filt.tracks <- transform(filt.tracks, UTC = as.POSIXct(UTC, tz = "GMT", format = "%Y-%m-%d %H:%M:%S"))
trip.info <- transform(trip.info, tripSt = as.POSIXct(tripSt, tz = "GMT", format = "%Y-%m-%d %H:%M:%S"), tripEnd = as.POSIXct(tripEnd, tz = "GMT", format = "%Y-%m-%d %H:%M:%S"))
metadata <- transform(metadata, UTC = as.POSIXct(UTC, tz = "GMT", format = "%m/%d/%Y %H:%M" ))

# Merge tracks with trip.info on deploy id and time in trip boundaries (conditional join)
annotated.tracks<- filter(merge(filt.tracks, trip.info, 'Deploy_ID'), UTC >= tripSt & UTC <= tripEnd)

####?????####
# ???? Check this - I've commented this out? but may need it
# # Parse dates
# filt.tracks <- transform(filt.tracks, UTC = as.character(UTC))
# trip.info <- transform(trip.info, tripSt = as.character(tripSt), tripEnd = as.character(tripEnd))
# metadata <- transform(metadata, UTC = as.character(UTC))
####?????####

# Add track points for interpolated start/end at time = trip start/end and lat/lon = nest lat/lon
start.interpolated <- mutate(filter(trip.info, tripStComp == 3), UTC = tripSt)
end.interpolated <- mutate(filter(trip.info, tripEndComp == 3), UTC = tripEnd)
at.nest <- merge(rbind(start.interpolated, end.interpolated), metadata, 'Deploy_ID')
at.nest<-rename(at.nest,UTC=UTC.x)

# ####?????####
# # ???? Check this - I've commented this out? but may need it
# interp.start.ends<-rbind(start.interpolated, end.interpolated)
# at.nest <- merge(interp.start.ends, metadata[, c("Deploy_ID",setdiff(colnames(metadata),colnames(interp.start.ends)))], by="Deploy_ID")
# # at.nest <- merge(rbind(start.interpolated, end.interpolated), metadata, "Deploy_ID")
# ####?????####

if (length(at.nest[,1])>0) {
  #  i=1
  for (i in 1:length(at.nest[,1])) {               
    if (at.nest$Nest_loc_use[i]==0) { 
      at.nest$latitude[i]<-at.nest$Col_Lat_DD[i]
      at.nest$longitude[i]<-at.nest$Col_Long_DD[i]
    } else { 
      at.nest$latitude[i]<-at.nest$Nest_Lat_DD[i]
      at.nest$longitude[i]<-at.nest$Nest_Long_DD[i]
    }
  }
  interpolated.points<-at.nest[,c(1,8,92,93,22,2:7)]
  interpolated.points<-rename(interpolated.points,species=Species)
  # Append interpolated track points and sort
  annotated.tracks <- arrange(rbind(annotated.tracks, interpolated.points), Deploy_ID, UTC)
}

# check for and remove duplicates c(UTC,deploy_id,trip_no) from annotated.tracks (trip breaker may put these in under certain conditions)
check<-paste(as.character(annotated.tracks$UTC),as.numeric(annotated.tracks$Deploy_ID+(annotated.tracks$trip_no*.01)),sep="_")
annotated.tracks<-annotated.tracks[duplicated(check)==0,]

# join in colony name
annotated.tracks <- merge(annotated.tracks, metadata[,c(4,7)], 'Deploy_ID')

write.table(annotated.tracks, paste(dir.out,filename,'_trips_annotated.csv',sep = ""),sep=",",quote=FALSE,col.names=TRUE,row.names=FALSE)

# END