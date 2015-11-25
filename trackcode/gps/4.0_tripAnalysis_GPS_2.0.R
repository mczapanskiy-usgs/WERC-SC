#############################################
# EBOS Trip analyses
# Bill Henry USGS
# Sept 10, 15
#
# uses broken GPS trip data to quantify trips
# 
#############################################

#### clear all
rm(list=ls())

library(sp)
library(plyr)
library(proj4)
library(ggplot2)
library(rgdal)
library(geosphere)

library(data.table)

library(trip)

library(adehabitatLT)

# library(rgeos)
# library(raster)

# library(SDMTools)
# library(adehabitatHR)
# library(adehabitatLT)
# library(adehabitatMA)

library(CircStats)
library(ade4)
library(stringr)
library(maptools)


#### select species
species="RTTR"

#### enter radius of data (in km)
rad=1.5

#### enter degrees for size of raster grid cells
# cell.size.dd<-5

#### directory in
# dir.in <- 'D:/Share_Data/Tracking_Data/EOBS/All_tracks/'
# dir.in <- '/Users/henry/Documents/Work/Projects/USGS/USGS 08.10.15/Share 8.11.2015/Tracking_Data/EOBS/All_tracks/'
dir.in <- 'D:/Share_Data/Tracking_Data/GPS/All_tracks/'

####
# dir.out <- 'D:/Share_Data/Tracking_Data/EOBS/All_tracks/'
# dir.out <- '/Users/henry/Documents/Work/Projects/USGS/USGS 08.10.15/Share 8.11.2015/Tracking_Data/EOBS/Analyses/'
dir.out <- 'D:/Share_Data/Tracking_Data/GPS/All_tracks/Analyses/'

#### directory for world background to read in
#dir.in.poly <- 'D:/Share_Data/ARCMAP/World'

# Get metadata
# meta<-read.table (paste('D:/Share_Data/GitHub/WERC-SC/trackcode/eobs/','metadata_EOBS.csv',sep = ""),header=T, sep=",", strip.white=T)
# meta<-read.table (paste('/Users/henry/Documents/Work/Projects/USGS/USGS 08.10.15/Share 8.11.2015/GitHub/WERC-SC/trackcode/eobs/','metadata_EOBS.csv',sep = ""),header=T, sep=",", strip.white=T)
metadata<-read.table(paste("D:/Share_Data/GitHub/WERC-SC/trackcode/gps/","metadata_all_GPS.csv",sep=""),header=T, sep=",", strip.white=T)

# subset metadata
metadata<-metadata[metadata$Species==species & metadata$Tagging_Event=="R" & metadata$GPS_TagRecov %in% c(1,3),]

# get trip.info summary
# trip.info <- data.frame(read.csv(paste("/Users/henry/Documents/Work/Projects/USGS/USGS 08.10.15/Share 8.11.2015/Tracking_Data/EOBS/",species,"/3_trips/",species,"_",rad,"_","tripInfo.csv",sep="")))
trip.info <- data.frame(read.csv(paste("D:/Share_Data/Tracking_Data/GPS/3_trips/",species,"_",rad,"_","tripInfo.csv",sep="")))

trip.info$unique_trip<-as.numeric(trip.info$Deploy_ID+(trip.info$trip_no*.01))

# get broken track data
tracks<-read.table (paste(dir.in,species,'_',rad,'_trips_annotated.csv',sep = ""),header=T, sep=",", strip.white=T)
# subset track data
# tracks<-tracks[tracks$species==species,]

tracks <- transform(tracks, UTC = as.POSIXct(UTC, tz = "GMT", format = "%Y-%m-%d %H:%M:%S"))

# get raw track data
tracks_filt<-read.table (paste(dir.in,'All_Species_GPS_allTracks.csv',sep = ""),header=T, sep=",", strip.white=T)
# subset track data
tracks_filt<-transform(tracks_filt[tracks_filt$species==species,], UTC = as.POSIXct(UTC, tz = "GMT", format = "%Y-%m-%d %H:%M:%S"))

tracks<- merge(tracks, tracks_filt, all.x=TRUE)[,c(1:12,28:30)]

tracks$unique_trip<-as.numeric(tracks$Deploy_ID+(tracks$trip_no*.01))

#2*sd(tracks_filt$speed.pckTrip.SPD,na.rm=TRUE)
# convert to km/hr = ((60*60)/1000)

# tag.serial.number,UTC,latitude,longitude,height.above.ellipsoid,SPDfilter,temperature,dt,dist.pckArgos.SPD,speed.pckArgos.SPD,dist.pckTrip.SPD,speed.pckTrip.SPD,azimuth.SPD,Deploy_ID,species,year,Site

tracks<-rename(tracks, c("Longitude"="longitude", "Latitude"="latitude"))

# convert tracks into a spatial data frame
tracks$longitude360<-tracks$longitude
tracks$longitude360[(tracks$longitude360 < 0)] <- (tracks$longitude360[(tracks$longitude360 < 0)])+360
tracks.sp <- SpatialPointsDataFrame(coords = tracks[c("longitude360","latitude")], data = tracks)
# define projection, use the WGS84 projection that Argos Data is delivered in
tracks.sp@proj4string <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
plot(tracks.sp, pch=3,cex=.01)

# get grid point data
# point.grid<-read.table ('D:/Share_Data/ARCMAP/Fisheries/Longline/LONGLINE_00.csv',header=T, sep=,", strip.white=T)
# point.grid<-read.table ('/Users/henry/Documents/Work/Projects/USGS/USGS 08.10.15/Share 8.11.2015/ARCMAP/Fisheries/Longline/LONGLINE_00.csv',header=T, sep=",", strip.white=T)

# performs an analysis of trips

###### transform coordinates to desired projection for speed calculations
# dir.in.poly <- "/Users/henry/Documents/Work/Projects/USGS/USGS 08.10.15/Share 8.11.2015/Clip Polygons/"
dir.in.poly <- "D:/Share_Data/Clip Polygons/"

  # read in list of potential clipper files
  clipPolyList<-read.csv (paste(dir.in.poly,"clipPolyList.csv", sep=""), header=T, sep=",", strip.white=T)
  print(clipPolyList) # show a list of the clipper files
  
  #### select clipperfile
  #### ui
  rno<-14 # row number of file list
  
  # read in polygon
  ##  clipper <- readOGR(dir.in.poly,clipper) # clipper comes in as unprojected WGS84
  
  ### get projection want
  # read in projection best for selected polygon (contained in table clipPolyList)
  projWant<-as.character(clipPolyList$Proj4.specs[rno])
  
#   track.sp <- SpatialPointsDataFrame(coords = tracks_all[c("Longitude","Latitude")], data = data.frame(utc = tracks_all$UTC))
#   # define projection, use the WGS84 projection that Argos Data is delivered in
#   track.sp@proj4string <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")

  # transform the track to projection defined for the polygon
  track.sp_trans<-spTransform(tracks.sp, CRS(paste("+",projWant,sep='')))
  ######
  plot(track.sp_trans, pch=3,cex=.01)

  ## compare the coords
  # cbind(track.sp_trans@coords,tracks_all[c("Longitude","Latitude")])
  
  ## get transformed tracks for to calc speed and distance calculations
  tracks_trans<-as.data.frame(track.sp_trans@coords)
  tracks_trans<-rename(tracks_trans, c("longitude360"="longitude_trans", "latitude"="latitude_trans"))
  
  tracks<-cbind(tracks, tracks_trans)
  
  # set logical for trips that are complete
  tracks["trip_comp"] <- 0
  tracks$trip_comp[which(tracks$tripStComp>=1 & tracks$tripEndComp>=1)]=1 
  
  # set burst id (unique trips)
  tracks["unique_trip"] <- 0
  tracks$unique_trip<-as.numeric(tracks$Deploy_ID+(tracks$trip_no*.01))
  
  # tracks_comp <- tracks[which(tracks$trip_comp==1 & tracks$trip_no>0),]
  
  # head(tracks_comp)
  
  # xy<-tracks[c("longitude_trans","latitude_trans")]
  # head(xy)

# fix any duplicate dates by adding 1 second to 2nd duplicate time value (duplicates could be artifact of rounding datetime to seconds in during previous processing)
trip.date<- paste(tracks$unique_trip,as.character(tracks$UTC),sep="_")

tracks$UTC[tracks$UTCc %in% as.character(tracks$UTCc[duplicated(trip.date)==1])][1:length(tracks$UTC[tracks$UTCc %in% as.character(tracks$UTCc[duplicated(trip.date)==1])]) %% 2 == 0] = (tracks$UTC[tracks$UTCc %in% as.character(tracks$UTCc[duplicated(trip.date)==1])][1:length(tracks$UTC[tracks$UTCc %in% as.character(tracks$UTCc[duplicated(trip.date)==1])]) %% 2 == 0]+1)

# test<-tracks[tracks$UTCc %in% as.character(tracks$UTCc[duplicated(trip.date)==1]),]

# length(tracks[,1])-length(unique(test))
# 
# test1<-(c(test[1],test)==c(test,test[length(test)]))
# sum(test1)
# head(test1)
# tail(test1)
# test[test1==TRUE]
# 
# lvect<-(c(tracks$UTC[1],tracks$UTC)==c(tracks$UTC,tracks$UTC[length(tracks$UTC)]))
# sum(lvect)
# head(lvect)
# tail(lvect)

# create track object for AdehabitatLT
  
trackObjs<-as.ltraj(tracks[c("longitude_trans","latitude_trans")], tracks$UTC, id=tracks$unique_trip, burst = as.character(tracks$unique_trip), typeII = TRUE, infolocs =data.frame(tracks$Altitude,tracks$trip_no,tracks$tripStComp,tracks$duration.hrs,tracks$tripEndComp,tracks$trip_comp))

#   #### temp
#   trip.info <- data.frame(read.csv(paste("/Users/henry/Documents/Work/Projects/USGS/USGS 08.10.15/Share 8.11.2015/Tracking_Data/EOBS/",species,"/3_trips/",species,"_",rad,"_","tripInfo.csv",sep="")))
#   
#   trip.info$unique_trip<-as.numeric(trip.info$Deploy_ID+(trip.info$trip_no*.01))
#   ### temp
#   
unique_trip <- unique(tracks$unique_trip)

# form datatable of burst data and order by trip_no and data
    DT<-cbind(data.table(ld(trackObjs)))
    DT<- DT[order(as.numeric(as.character(DT$burst)),DT$date),]

# calculate  variables that need to be calculated by c(Deploy_ID and trip) "burst"
    DT$Deploy_ID<-floor(as.numeric(as.character(DT$burst)))
    DT$latitude<-tracks$latitude
    DT$longitude<-tracks$longitude
    DT$longitude360<-tracks$longitude360
    # DT$temperature<-as.numeric(tracks$temperature)
    relocs<-as.vector(summary(trackObjs)[,3])

    # get speed between locations and distance to colony
    DT$vel<-NA
    DT$dist2col<-NA
    # i=71
    for (i in 1:length(unique(DT$burst))) {
      DTsub<-DT[DT$burst==unique_trip[i],]
      
      ########### you are here shift DTsub down and add bottom to top
      
      
      DTsub[,c(dy,dy,dist,dt,abs.angle,rel.angle)]
      
      # get dist to col
      # get colony locs and project
      Deploy_ID.want<-unique(as.integer(as.character(DTsub$id)))
      col.loc<-cbind(metadata$Col_Long_DD[metadata$Deploy_ID==Deploy_ID.want],metadata$Col_Lat_DD[metadata$Deploy_ID==Deploy_ID.want])
      
      #### calculate distance to colony (uses wgs84 ellipsoid) package geosphere
      DTsub$dist2col<-distGeo(cbind(DTsub$longitude,DTsub$latitude), col.loc)
      # (this uses the default ellipsoid a=6378137, f=1/298.257223563)
      
      DTsub$dist[1]<-
      # get velocity for between each location    
      DTsub$vel<-DTsub$dist[1/DTsub$dt
      
      
      
      DT$vel[DT$burst==unique_trip[i]]<-DTsub$vel
      DT$dist2col[DT$burst==unique_trip[i]]<-DTsub$dist2col
      }
    
#########    uncomment to export annotated trips
#     #### export example annotated trip
#     want<-unique(DT$Deploy_ID[which(DT$tracks.duration.hrs==max(DT$tracks.duration.hrs, na.rm=TRUE))])
#     DT1<-as.data.frame(DT)
#     trip_annonate<-as.data.frame(DT1[DT1$Deploy_ID==want,])
#     trip_annonate<-merge(trip_annonate, meta[meta$Deploy_ID==want,], all.x=TRUE,all.y=TRUE)
#     ####     
    
#     write.table(trip_annonate, paste(dir.out,species,"_rad_",rad,"_","sample_trip_annonate.csv",sep = ""),sep=",",quote=TRUE,col.names=TRUE,row.names=FALSE)
        
    
    ############### YOU ARE HERE FIGURE OUT WHY RETURNING FUNKY DIST!!!)
    
    # Summary stats
    agg1 <- as.data.frame(DT[, j=list(
      Deploy_ID = Deploy_ID,
      mean_dt = mean(dt, na.rm = TRUE), med_dt = median(dt, na.rm = TRUE), sd_dt = sd(dt, na.rm = TRUE), max_dt = max(dt, na.rm = TRUE), cum_dt = sum(dt, na.rm = TRUE),
      mean_dist = mean(dist, na.rm = TRUE), med_dist = median(dist, na.rm = TRUE), sd_dist = sd(dist, na.rm = TRUE), max_dist = max(dist, na.rm = TRUE), cum_dist = sum(dist, na.rm = TRUE),
      mean_vel = mean(vel, na.rm = TRUE), med_vel = median(vel, na.rm = TRUE), sd_vel = sd(vel, na.rm = TRUE), max_vel = max(vel, na.rm = TRUE), 
      mean_abs.angle = mean(abs.angle, na.rm = TRUE), med_abs.angle = median(abs.angle, na.rm = TRUE), sd_abs.angle = sd(abs.angle, na.rm = TRUE), max_abs.angle = max(abs.angle, na.rm = TRUE), min_abs.angle = min(abs.angle, na.rm = TRUE),
      mean_rel.angle = mean(rel.angle, na.rm = TRUE), med_rel.angle = median(rel.angle, na.rm = TRUE), sd_rel.angle = sd(rel.angle, na.rm = TRUE), max_rel.angle = max(rel.angle, na.rm = TRUE), min_rel.angle = min(rel.angle, na.rm = TRUE),
      mean_Altitude = mean(tracks.Altitude, na.rm = TRUE), med_Altitude = median(tracks.Altitude, na.rm = TRUE), sd_Altitude = sd(tracks.Altitude, na.rm = TRUE), max_Altitude = max(tracks.Altitude, na.rm = TRUE), Altitude = min(tracks.Altitude, na.rm = TRUE),
      tracks.duration.hrs = mean(tracks.duration.hrs, na.rm = TRUE),
      mean_R2n = mean(R2n, na.rm = TRUE),med_R2n = median(R2n, na.rm = TRUE),sd_R2n = sd(R2n, na.rm = TRUE),max_R2n = max(R2n, na.rm = TRUE),
      # mean_temp = mean(temperature, na.rm = TRUE),med_temp = median(temperature, na.rm = TRUE),sd_temp = sd(temperature, na.rm = TRUE),max_temp = max(temperature, na.rm = TRUE), min_temp = min(temperature, na.rm = TRUE),
      mean_latitude = mean(latitude, na.rm = TRUE),med_latitude = median(latitude, na.rm = TRUE),sd_latitude = sd(latitude, na.rm = TRUE),max_latitude = max(latitude, na.rm = TRUE), min_latitude = min(latitude, na.rm = TRUE),
      mean_longitude360 = mean(longitude360, na.rm = TRUE),med_longitude360 = median(longitude360, na.rm = TRUE),sd_longitude360 = sd(longitude360, na.rm = TRUE),max_longitude360 = max(longitude360, na.rm = TRUE), min_longitude360 = min(longitude360, na.rm = TRUE),
      mean_dist2col = mean(dist2col, na.rm = TRUE),med_dist2col = median(dist2col, na.rm = TRUE),sd_dist2col = sd(dist2col, na.rm = TRUE),max_dist2col = max(dist2col, na.rm = TRUE), min_dist2col = min(dist2col, na.rm = TRUE)
      ),
      by = id])
    
    agg<-unique(agg1)
    agg<-cbind(agg,relocs)
    agg<-merge(agg, metadata, all.x=TRUE,all.y=TRUE)
    sub.for.merge<-unique(tracks[,c("unique_trip","tripStComp","tripEndComp")])
    sub.for.merge$unique_trip<-as.factor(sub.for.merge$unique_trip)
    out<-merge(agg,sub.for.merge,by.x="id",by.y="unique_trip",all.x=TRUE,all.y=FALSE)
    out$id<-as.numeric(as.character(out$id), digits=2)
    out<-out[order(out$id),]


write.table(out, paste(dir.out,species,"_rad_",rad,"_","trip_summarys.csv",sep = ""),sep=",",quote=TRUE,col.names=TRUE,row.names=FALSE)
  

# some plotting
library(ggplot2)
library(reshape2)
library(gridExtra)

#### plot 1
legend_title <- "Median Velocity by Sex"
p <- ggplot(out, aes(x=med_vel))
p <- p + geom_density(aes(fill=factor(Sex)), alpha=.4)
p + scale_fill_manual(legend_title,values = c("pink","blue","yellow"))

#### plot 2
legend_title <- "Max Dist2Col by Sex"
p <- ggplot(out, aes(x=max_dist2col))
            p <- p + geom_density(aes(fill=factor(Sex)), alpha=.4)
            p + scale_fill_manual(legend_title,values = c("pink","blue","yellow"))
            
#### plot 3
legend_title <- "Trip duration hrs by Sex"
p <- ggplot(out, aes(x=tracks.duration.hrs))
            p <- p + geom_density(aes(fill=factor(Sex)), alpha=.4)
            p + scale_fill_manual(legend_title,values = c("pink","blue","yellow"))