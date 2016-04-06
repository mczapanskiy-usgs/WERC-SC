#############################################
# EBOS Trip analyses
# Bill Henry USGS
# Sept 10, 15
#
# uses broken EOBS trip data to quantify trips
# 
#############################################

#### clear all
rm(list=ls())

library(sp)
library(dplyr)
library(proj4)
library(ggplot2)
library(rgdal)
library(geosphere)
library (oce)

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
species="BFAL"

#### enter radius of data (in km)
rad=20

#### enter degrees for size of raster grid cells
# cell.size.dd<-5

#### directory in
# dir.in <- 'D:/Share_Data/Tracking_Data/EOBS/All_tracks/'
dir.in <- '/Users/henry/Documents/Work/Projects/USGS/USGS 08.10.15/Share 8.11.2015/Tracking_Data/EOBS/All_tracks/'

####
# dir.out <- 'D:/Share_Data/Tracking_Data/EOBS/All_tracks/'
dir.out <- '/Users/henry/Documents/Work/Projects/USGS/USGS 08.10.15/Share 8.11.2015/Tracking_Data/EOBS/Analyses/'

#### directory for world background to read in
#dir.in.poly <- 'D:/Share_Data/ARCMAP/World'

# Get metadata
# meta<-read.table (paste('D:/Share_Data/GitHub/WERC-SC/trackcode/eobs/','metadata_EOBS.csv',sep = ""),header=T, sep=",", strip.white=T)
meta<-read.table (paste('/Users/henry/Documents/Work/Projects/USGS/USGS 08.10.15/Share 8.11.2015/GitHub/WERC-SC/trackcode/eobs/','metadata_EOBS.csv',sep = ""),header=T, sep=",", strip.white=T)

# subset metadata
meta<-meta[meta$Species==species & meta$loc.data==1,]

# get trip.info summary
trip.info <- data.frame(read.csv(paste("/Users/henry/Documents/Work/Projects/USGS/USGS 08.10.15/Share 8.11.2015/Tracking_Data/EOBS/",species,"/3_trips/",species,"_",rad,"_","tripInfo.csv",sep="")))

trip.info$unique_trip<-as.numeric(trip.info$Deploy_ID+(trip.info$trip_no*.01))

# get broken track data
tracks<-read.table (paste(dir.in,species,'_',rad,'_trips_annotated.csv',sep = ""),header=T, sep=",", strip.white=T)
# subset track data
# tracks<-tracks[tracks$species==species,]

tracks <- transform(tracks, UTC = as.POSIXct(UTC, tz = "GMT", format = "%Y-%m-%d %H:%M:%S"))

# get raw track data
tracks_filt<-read.table (paste(dir.in,'All_Species_EOBS_allTracks.csv',sep = ""),header=T, sep=",", strip.white=T)
# subset track data
tracks_filt<-transform(tracks_filt[tracks_filt$species==species,], UTC = as.POSIXct(UTC, tz = "GMT", format = "%Y-%m-%d %H:%M:%S"))

tracks<- merge(tracks, tracks_filt, all.x=TRUE)[,c(1:11,12,14,16,22,23)]

tracks$unique_trip<-as.numeric(tracks$Deploy_ID+(tracks$trip_no*.01))


#2*sd(tracks_filt$speed.pckTrip.SPD,na.rm=TRUE)
# convert to km/hr = ((60*60)/1000)

# tag.serial.number,UTC,latitude,longitude,height.above.ellipsoid,SPDfilter,temperature,dt,dist.pckArgos.SPD,speed.pckArgos.SPD,dist.pckTrip.SPD,speed.pckTrip.SPD,azimuth.SPD,Deploy_ID,species,year,Site

# convert tracks into a spatial data frame
tracks$longitude360<-tracks$longitude
tracks$longitude360[(tracks$longitude360 < 0)] <- (tracks$longitude360[(tracks$longitude360 < 0)])+360
tracks.sp <- SpatialPointsDataFrame(coords = tracks[c("longitude360","latitude")], data = tracks)
# define projection, use the WGS84 projection that Argos Data is delivered in
tracks.sp@proj4string <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
plot(tracks.sp, pch=3,cex=.01)


# get grid point data
# point.grid<-read.table ('D:/Share_Data/ARCMAP/Fisheries/Longline/LONGLINE_00.csv',header=T, sep=",", strip.white=T)
# point.grid<-read.table ('/Users/henry/Documents/Work/Projects/USGS/USGS 08.10.15/Share 8.11.2015/ARCMAP/Fisheries/Longline/LONGLINE_00.csv',header=T, sep=",", strip.white=T)

# performs an analysis of trips

###### transform coordinates to desired projection for speed calculations
dir.in.poly <- "/Users/henry/Documents/Work/Projects/USGS/USGS 08.10.15/Share 8.11.2015/Clip Polygons/"
    
  # read in list of potential clipper files
  clipPolyList<-read.csv (paste(dir.in.poly,"clipPolyList.csv", sep=""), header=T, sep=",", strip.white=T)
#  print(clipPolyList) # show a list of the clipper files
  
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
  tracks_trans<-rename(tracks_trans, longitude_trans=longitude360, latitude_trans=latitude)
  
  tracks<-cbind(tracks, tracks_trans)
  
  # set logical for trips that are complete
  tracks["trip_comp"] <- 0
  tracks$trip_comp[which(tracks$tripStComp>=1 & tracks$tripEndComp>=1)]=1 
  
  # set burst id (unique trips)
  tracks["unique_trip"] <- 0
  tracks$unique_trip<-as.numeric(tracks$Deploy_ID+(tracks$trip_no*.01))
  
  #tracks_comp <- tracks[which(tracks$trip_comp==1 & tracks$trip_no>0),]
  
  # head(tracks_comp)
  
  # xy<-tracks[c("longitude_trans","latitude_trans")]
  # head(xy)

# fix any duplicate dates by adding 1 second to 2nd duplicate time value (these were possible creating by rounding datetime to seconds along the line...)
# tracks$UTCc<-as.character(tracks$UTC)
# 
# trip.date<- paste(tracks$unique_trip,tracks$UTCc,sep="_")

trip.date<- paste(tracks$unique_trip,as.character(tracks$UTC),sep="_")


tracks$UTC[tracks$UTCc %in% as.character(tracks$UTCc[duplicated(trip.date)==1])][1:length(tracks$UTC[tracks$UTCc %in% as.character(tracks$UTCc[duplicated(trip.date)==1])]) %% 2 == 0] = (tracks$UTC[tracks$UTCc %in% as.character(tracks$UTCc[duplicated(trip.date)==1])][1:length(tracks$UTC[tracks$UTCc %in% as.character(tracks$UTCc[duplicated(trip.date)==1])]) %% 2 == 0]+1)

# 
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

trackObjs<-as.ltraj(tracks[c("longitude_trans","latitude_trans")], tracks$UTC, id=tracks$unique_trip, burst = as.character(tracks$unique_trip), typeII = TRUE, infolocs =data.frame(tracks$height.above.ellipsoid,tracks$temperature,tracks$trip_no,tracks$tripStComp,tracks$tripEndComp,tracks$trip_comp,tracks$duration.hrs,tracks$Site))

#   #### temp
#   trip.info <- data.frame(read.csv(paste("/Users/henry/Documents/Work/Projects/USGS/USGS 08.10.15/Share 8.11.2015/Tracking_Data/EOBS/",species,"/3_trips/",species,"_",rad,"_","tripInfo.csv",sep="")))
#   
#   trip.info$unique_trip<-as.numeric(trip.info$Deploy_ID+(trip.info$trip_no*.01))
#   ### temp
#   
unique_trip <- unique(tracks$unique_trip)

# form datatable of burst data
    DT<-cbind(data.table(ld(trackObjs)))
    DT<- DT[order(as.numeric(as.character(DT$burst)),DT$date),]

# calculate  variables that need to be calculated by c(Deploy_ID and trip) "burst"
    DT$Deploy_ID<-floor(as.numeric(as.character(DT$burst)))
    DT$latitude<-tracks$latitude
    DT$longitude<-tracks$longitude
    DT$longitude360<-tracks$longitude360
    DT$temperature<-as.numeric(tracks$temperature)
    #relocs<-as.numeric(as.vector(summary(trackObjs)[,3]))

    # get speed between locations and distance to colony
    DT$vel<-NA
    DT$dist2col<-
    DT$diel.naut<-NA
    
    # i=1
    for (i in 1:length(unique(DT$burst))) {
      DTsub<-DT[DT$burst==unique_trip[i],]
      
      DTsub[,c(dy,dy,dist,dt,abs.angle,rel.angle)]
      
      # get velocity for between each location    
      # DTsub$vel<-DTsub$dist/DTsub$dt
      
      # get dist to col
      # get colony locs and project
      Deploy_ID.want<-unique(as.integer(as.character(DTsub$id)))
      col.loc.sp<-SpatialPointsDataFrame(coords = cbind(meta$Col_Long_DD[meta$Deploy_ID==Deploy_ID.want],meta$Col_Lat_DD[meta$Deploy_ID==Deploy_ID.want]),data=as.data.frame(1))
      col.loc.sp@proj4string <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
      # col.loc.sp_trans<-spTransform(col.loc.sp, CRS(paste("+",projWant,sep='')))
      
      DTsub.sp<-SpatialPointsDataFrame(coords = cbind(DTsub$longitude,DTsub$latitude),data=as.data.frame(cbind(DTsub$longitude,DTsub$latitude)))
      DTsub.sp@proj4string <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
      # DTsub.sp_trans<-spTransform(DTsub.sp, CRS(paste("+",projWant,sep='')))
      
      #### calculate distance to colony (uses wgs84 ellipsoid) package geosphere
      DTsub$dist2col<-distGeo(DTsub.sp, col.loc.sp) # (this uses the default ellipsoid a=6378137, f=1/298.257223563)
      # DTsub$dist2col<-distVincentyEllipsoid(DTsub.sp, col.loc.sp, a=6378137, b=6356752.3142, f=1/298.257223563)
      
      # get velocity for between each location    
      DTsub$vel<-DTsub$dist/DTsub$dt
      
      DT$vel[DT$burst==unique_trip[i]]<-DTsub$vel
      DT$dist2col[DT$burst==unique_trip[i]]<-DTsub$dist2col
      
      # Nautical dawn/dusk
      # library (oce)
      utc<-as.POSIXct(format(DTsub$date), tz="UTC")
      lat<-DTsub.sp@coords[,1]
      lon<-DTsub.sp@coords[,2]
      
      # diel function (Max Czapazanskiy, Jan 12, 2016)
      calc.diel.naut <- function(utc, lat, lon) {
        sunElevationNow <- sunAngle(utc, lat, lon)$altitude
        sunElevation1hr <- sunAngle(utc + 3600, lat, lon)$altitude
        ifelse(sunElevationNow <= 0 & sunElevationNow > -12, # -12 = nautical twilight, -6 = civil twilight
               ifelse(sunElevationNow < sunElevation1hr, 'dawn.naut', 'dusk.naut'),
               ifelse(sunElevationNow > 0, 'day', 'night'))
      }
      #DTsub <- mutate(DTsub, diel.naut = calc.diel.naut(utc, lat, lon))
      DT$diel.naut[DT$burst==unique_trip[i]]<-calc.diel.naut(utc, lat, lon)
    }
    
#########    uncomment to export annotated trips
#     #### export example annotated trip
#     want<-unique(DT$Deploy_ID[which(DT$tracks.duration.hrs==max(DT$tracks.duration.hrs, na.rm=TRUE))])
#     DT1<-as.data.frame(DT)
#     trip_annonate<-as.data.frame(DT1[DT1$Deploy_ID==want,])
#     trip_annonate<-merge(trip_annonate, meta[meta$Deploy_ID==want,], all.x=TRUE,all.y=TRUE)
#     ####     
    
#     write.table(trip_annonate, paste(dir.out,species,"_rad_",rad,"_","sample_trip_annonate.csv",sep = ""),sep=",",quote=TRUE,col.names=TRUE,row.names=FALSE)
        
    # Summary stats
    agg1 <- as.data.frame(DT[, j=list(
      Deploy_ID = Deploy_ID,
      mean_dt = mean(dt, na.rm = TRUE), med_dt = median(dt, na.rm = TRUE), sd_dt = sd(dt, na.rm = TRUE), max_dt = max(dt, na.rm = TRUE), cum_dt = sum(dt, na.rm = TRUE),
      mean_dist = mean(dist, na.rm = TRUE), med_dist = median(dist, na.rm = TRUE), sd_dist = sd(dist, na.rm = TRUE), max_dist = max(dist, na.rm = TRUE), cum_dist = sum(dist, na.rm = TRUE),
      mean_vel = mean(vel, na.rm = TRUE), med_vel = median(vel, na.rm = TRUE), sd_vel = sd(vel, na.rm = TRUE), max_vel = max(vel, na.rm = TRUE), 
      mean_abs.angle = mean(abs.angle, na.rm = TRUE), med_abs.angle = median(abs.angle, na.rm = TRUE), sd_abs.angle = sd(abs.angle, na.rm = TRUE), max_abs.angle = max(abs.angle, na.rm = TRUE), min_abs.angle = min(abs.angle, na.rm = TRUE),
      mean_rel.angle = mean(rel.angle, na.rm = TRUE), med_rel.angle = median(rel.angle, na.rm = TRUE), sd_rel.angle = sd(rel.angle, na.rm = TRUE), max_rel.angle = max(rel.angle, na.rm = TRUE), min_rel.angle = min(rel.angle, na.rm = TRUE),
      mean_tracks.height.above.ellipsoid = mean(tracks.height.above.ellipsoid, na.rm = TRUE), med_tracks.height.above.ellipsoid = median(tracks.height.above.ellipsoid, na.rm = TRUE), sd_tracks.height.above.ellipsoid = sd(tracks.height.above.ellipsoid, na.rm = TRUE), max_tracks.height.above.ellipsoid = max(tracks.height.above.ellipsoid, na.rm = TRUE), min_tracks.height.above.ellipsoid = min(tracks.height.above.ellipsoid, na.rm = TRUE),
      tracks.duration.hrs = mean(tracks.duration.hrs, na.rm = TRUE),
      mean_R2n = mean(R2n, na.rm = TRUE),med_R2n = median(R2n, na.rm = TRUE),sd_R2n = sd(R2n, na.rm = TRUE),max_R2n = max(R2n, na.rm = TRUE),
      mean_temp = mean(temperature, na.rm = TRUE),med_temp = median(temperature, na.rm = TRUE),sd_temp = sd(temperature, na.rm = TRUE),max_temp = max(temperature, na.rm = TRUE), min_temp = min(temperature, na.rm = TRUE),
      mean_latitude = mean(latitude, na.rm = TRUE),med_latitude = median(latitude, na.rm = TRUE),sd_latitude = sd(latitude, na.rm = TRUE),max_latitude = max(latitude, na.rm = TRUE), min_latitude = min(latitude, na.rm = TRUE),
      mean_longitude360 = mean(longitude360, na.rm = TRUE),med_longitude360 = median(longitude360, na.rm = TRUE),sd_longitude360 = sd(longitude360, na.rm = TRUE),max_longitude360 = max(longitude360, na.rm = TRUE), min_longitude360 = min(longitude360, na.rm = TRUE),
      mean_dist2col = mean(dist2col, na.rm = TRUE),med_dist2col = median(dist2col, na.rm = TRUE),sd_dist2col = sd(dist2col, na.rm = TRUE),max_dist2col = max(dist2col, na.rm = TRUE), min_dist2col = min(dist2col, na.rm = TRUE)
      ),
      by = id])
    
    agg<-unique(agg1)
    #agg<-cbind(agg,as.data.frame(relocs))
    agg<-merge(agg, meta, all.x=TRUE,all.y=TRUE)
    sub.for.merge<-unique(tracks[,c("trip_no","unique_trip","tripStComp","tripEndComp")])
    sub.for.merge$unique_trip<-as.character(sub.for.merge$unique_trip)
    sub.for.merge$unique_trip = as.character(sub.for.merge$unique_trip)
    agg$id = as.character(agg$id)
    out<-merge(agg,sub.for.merge,by.x="id",by.y="unique_trip",all.x=TRUE,all.y=TRUE)
    out$burst<-as.numeric(as.character(out$id), digits=2)
    out<-out[order(out$id),]

    sum.trackObjs<-summary(trackObjs)
    sum.trackObjs<-sum.trackObjs[order(sum.trackObjs$id),]
    sum.trackObjs <- sum.trackObjs[,c('id','nb.reloc','date.begin','date.end')]
    
    out<-merge(out,sum.trackObjs,by.x="id",by.y="id",all.x=TRUE,all.y=FALSE)
    
    out[out==-Inf]="NA"

write.table(DT,paste(dir.out,species,"_rad_",rad,"_","locs.behav.annotation.csv",sep = ""),sep=",",quote=TRUE,col.names=TRUE,row.names=FALSE)

write.table(out,paste(dir.out,species,"_rad_",rad,"_","trip_summarys.csv",sep = ""),sep=",",quote=TRUE,col.names=TRUE,row.names=FALSE)


##############################################################################################################
##############################################################################################################
# Rediscretize if needed
DT<-as.data.frame(DT)

# convert tracks into a spatial data frame
DT.sp <- SpatialPointsDataFrame(coords = DT[c("longitude360","latitude")], data = DT)
# define projection, use the WGS84 projection that Argos Data is delivered in
DT.sp@proj4string <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
# plot(DT.sp, pch=3,cex=.01)

DT.sp_trans<-spTransform(DT.sp, CRS(paste("+",projWant,sep='')))
######
# plot(DT.sp_trans, pch=3,cex=.01) # plot transforms
# plot(DT$longitude360,DT$latitude, pch=3,cex=.01) # plot untransformed

## compare the coords
# cbind(track.sp_trans@coords,tracks_all[c("Longitude","Latitude")])

## get transformed tracks for to calc speed and distance calculations
DT.sp_trans<-as.data.frame(DT.sp_trans@coords)
DT.sp_trans<-rename(DT.sp_trans, longitude_trans=longitude360, latitude_trans=latitude)

DT<-cbind(DT, DT.sp_trans)

trackObjs<-as.ltraj(DT[c("longitude_trans","latitude_trans")], DT$date, id=DT$id, burst = DT$id, typeII = TRUE, infolocs =data.frame(DT$tracks.height.above.ellipsoid,DT$tracks.temperature,DT$tracks.trip_no,DT$tracks.tripStComp,DT$tracks.tripEndComp,DT$tracks.trip_comp,DT$tracks.duration.hrs,DT$tracks.Site,DT$Deploy_ID,DT$longitude360,DT$longitude,DT$latitude))

# Rediscretize to x seconds
seconds<-20*60
Redis<-redisltraj(trackObjs, seconds, burst = burst, samplex0 = FALSE, addbit = FALSE,
           nnew = 10, type = "time")

Redis<-cbind(data.table(ld(Redis)))
Redis<- as.data.frame(Redis[order(as.numeric(as.character(Redis$burst)),Redis$date),])
Redis$id<-as.character(Redis$burst)
out$Deploy_ID<-as.character(out$Deploy_ID)
out$Site<-as.character(out$Site)
outwant<-out[,c("id","Site","Deploy_ID")]
Redis<- merge(Redis,outwant,by='id')

write.table(Redis,paste(dir.out,species,"_rad_",rad,"_","Redis_",seconds,"_sec.csv",sep = ""),sep=",",quote=TRUE,col.names=TRUE,row.names=FALSE)






##### plotting untested as of 2.5.2016

# some plotting
library(ggplot2)
library(reshape2)
library(gridExtra)

#### plot 1
legend_title <- "Median Velocity by Sex"
p <- ggplot(out, aes(x=med_vel))
p <- p + geom_density(aes(fill=factor(Sex)), alpha=.4)
p + scale_fill_manual(legend_title,values = c("pink","blue"))

#### plot 2
legend_title <- "Max Dist2Col by Sex"
p <- ggplot(out, aes(x=max_dist2col))
            p <- p + geom_density(aes(fill=factor(Sex)), alpha=.4)
            p + scale_fill_manual(legend_title,values = c("pink","blue"))
            
#### plot 3
legend_title <- "Trip duration hrs by Sex"
p <- ggplot(out, aes(x=tracks.duration.hrs))
            p <- p + geom_density(aes(fill=factor(Sex)), alpha=.4)
            p + scale_fill_manual(legend_title,values = c("pink","blue"))