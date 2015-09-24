

################################################################
#
# Filtering EOBS GPS TAG DATA USING THE ARGOSFILTER PACKAGE
#
# (Using location data compiled from the EOBS tags)
#
################################################################
# Bill Henry USGS July 23, 2015
#
#
# supporting inputs (.csv files)
# 1. table for parameters to base filter on (spp  vmaxSA_Tab2_W5ms_x_3SD  ang1	ang2	distlim1	distlim2	colrad	type)
# 2. metadata table with Deployment and Recovery information
# 3. track data
# outputs
# .csv of time lat long filtered
# FilterParametersUsed_ - species,vmax,ang1,ang2,distlim1,distlim2
# .pdf of track and histograms of distance, time lag (between locations), and speed for  - summary of points filtered
#
# note
# unique_id input = GPS_ID
# unique_id output = deploy_id
# 

# clear all
rm(list=ls())

ticall<-as.numeric(proc.time()[3])

# install required packages
library(argosfilter)
library(plyr)
library(CircStats)          
library(plotrix)
library(spatstat)
library(trip)
library(ggplot2)

# set plot option to review plots TRUE or FALSE
plot<-TRUE

####  dir.in and loanding for track .csv files for each year (get them in one file)
dir.in <- "D:/Share_Data/Tracking_Data/EOBS/BFAL/Kure 2012/tag_data/"
tracks <- read.table (paste(dir.in,"all.locs.csv",sep = ""),header=T, sep=",", strip.white=T)
dir.in <- "D:/Share_Data/Tracking_Data/EOBS/BFAL/Kure 2013/tag_data/"
tracks <- rbind(tracks,read.table (paste(dir.in,"all.locs.csv",sep = ""),header=T, sep=",", strip.white=T))
dir.in <- "D:/Share_Data/Tracking_Data/EOBS/LAAL/Kaena 2014/tag_data/"
tracks <- read.table (paste(dir.in,"all.locs.csv",sep = ""),header=T, sep=",", strip.white=T)
dir.in <- "D:/Share_Data/Tracking_Data/EOBS/LAAL/NAK 2014/tag_data/"
tracks <- rbind(tracks,read.table (paste(dir.in,"all.locs.csv",sep = ""),header=T, sep=",", strip.white=T))


####  get metadata
dir.meta <- "D:/Share_Data/Tracking_Data/EOBS/"
metadata<-read.table(paste(dir.meta,"metadata_EOBS.csv",sep=""),header=T, sep=",", strip.white=T)

#### dir.out for .csv files
dir.out <- "D:/Share_Data/Tracking_Data/EOBS/"

####  in parameters to use for analyses
dir.in.params <- "D:/Share_Data/Tracking_Data/Support_Files/" 

#### filter the data? say no if you just want to summarize data
filter.data = "Y"

# initialize the parameters from parameters table for speed, distance, and angle filtering
#vmax=max velocity in m/s,  vmaxSA_Tab2_W5ms_x_3SD = values from Spear and Ainley, note: 22.22 = 80kph; 19.44 = 70 kph; 16.66 = 60 kph
#ang=angle or spikes to filter out at successive distlim: ang <- c(15, 25) the angle of the spike that is removed, this is Freitas default
#distlim= distance between successive points to use ang1 vs ang2: ang <- c(2500, 5000) this is Freitas default

### parameters 
# parameters <- read.csv ("D:/Share_Data/Tracking_Data/GPS/Support_Files/parameters.csv", header=T, sep=",", strip.white=T)

# get speed value used in Frietas SDA filter
parameters <- read.table(paste(dir.in.params,"parameters.csv",sep=""),header=T,sep=",",strip.white=T)

# get species from AUO Code in metadata
species<-"BFAL"

  #### get filter parameters
  paramwant<-subset(parameters,spp==as.character(species) & tag=="eobs")
  
  vmax <- (paramwant$gps_vmax)                                     
  ang <- c(paramwant$ang1,paramwant$ang2)
  distlim <-  c(paramwant$distlim1,paramwant$distlim2)
  type<- (paramwant$type)    
  #### initialize error
  lcerr<-paramwant$error_GPS ## GET igotu wakefield booby paper, working on our own error
  print(paramwant)
  
  #### will use this to loop through years 
  # Get years from metadata.csv
  year.ids<-(unique((subset(metadata, Species==species))$Year))
  
  #### Get sites from metadata.csv
  site.ids<-unique(subset(metadata, Species==species)$Site)
  
  #### get metadata for Species omitting any metadata records without deployment or recovery
  metaWants<-(subset(metadata, Species==species & Tagging_Event=='D' & loc.data==1))

  ### get Deploy_IDs of files that have good data
  deploy.ids<-metaWants$Deploy_ID
  print(c("number of birds with recoveries and good data",length(deploy.ids)))
  
  tag.id<-metaWants$GPS_ID

  #### subset tracking data you want
  tracks<-tracks[tracks$tag.serial.number %in% tag.id,]
  
  #### reorder metadata$UTC format
  metaWants$UTC<-format(strptime(as.character(metaWants$UTC), "%m/%d/%Y %H:%M"), "%Y-%m-%d %H:%M:%S")
  
  #### select deployments
  deploys<-metaWants
  
  #### select recoveries
  # recovs<-subset(metaWants,Tagging_Event=='R' & (Deploy_ID %in% deploy.ids))

  if (filter.data == "Y") {
  
  # get list of tags that have data for
  length(unique(tracks$tag.serial.number))  
    
  #### Loop to read in tracks for each track.csv 
  # j=1
  for (j in 1:length(deploy.ids)) {
    
    deploy.id<-deploy.ids[j]
    file.id <- deploys$GPS_ID[j]
    print(file.id)
    
    #### get track deployment metadata
    deplwant<-deploys[deploys$Deploy_ID==deploy.id,]
    
    #### get track
    track <- tracks[tracks$tag.serial.number==file.id,]
    # head(track)
    
    #### create date time field
    track$UTC<- as.POSIXct(track$timestamp.of.fix, tz = "GMT", format = "%Y-%m-%d %H:%M:%S")
  
    # double check track order by year, yday and time
    track <- track[order(track$UTC),]
        
    #### check if depl and recover times exist in metadata, if not set to beginning and end of track
    if (is.na(deplwant$UTC)) {
      deplwant$UTC<-track$UTC[1]
    }
    
    ####subset track for points between deployment and recovery times (UTC)
    trackwant<-subset(track,(UTC)>=as.POSIXct(as.character(deplwant$UTC), tz = "GMT"))
    # head(trackwant)
    
    #### check all lat-long values = 0 
    trackwant <- trackwant[trackwant$latitude != 0,]
    trackwant <- trackwant[trackwant$longitude != 0,]
    print ('Total rows after 0 locations removed')
    print (length(trackwant[,1]))

    #### set up vectors for Argos filter (Freitas)
    dtime<- trackwant$UTC
    
    lc<-rep.int(3, length(trackwant$latitude))
    
    # tic1=as.numeric(proc.time()[3])
    
    #### this is just a speed filter - not used, use SDA
    # filter by speed only [set max speed to equivalent to vmax]
    if(length(trackwant$latitude)>1){
      mfilter<- vmask(trackwant$latitude, trackwant$longitude, dtime, vmax)
      trackwant$SPDfilter<-mfilter
      
      #
      # lines(track$longitude[which(mfilter=="not")],track$latitude[which(mfilter=="not")],col="red")
      ####	

##### just use the speed filter, sda filter not working with dateline anyhow      
#       #### filter data using sdafilter [max speed to equivalent to vmax]
#       trackwant$longitude360<-trackwant$longitude
#       trackwant$longitude360[trackwant$longitude360<0]=((trackwant$longitude360[trackwant$longitude360<0]+180)+180)
#     
#       cfilter<- sdafilter(trackwant$latitude, trackwant$longitude, dtime, lc, vmax, ang, distlim)
#       head(cfilter)
#       trackwant$SDAfilter<-cfilter
      
      # get time elapsed
      #time_elapsed.1=as.numeric(proc.time()[3]) - tic1
      #print (time_elapsed.1)
      
      # summarize cfilter data
      original<-length (trackwant[,1])
      retained_SPD<-length(trackwant$SPDfilter[which(mfilter!="removed")])
      omitted_SPD<-length(trackwant$SPDfilter[which(mfilter=="removed")])          
#      retained_SDA<-length(trackwant$SDAfilter[which(cfilter!="removed")])
#      omitted_SDA<-length(trackwant$SDAfilter[which(cfilter=="removed")])
      print (c("original", original))
      print (c("retained_SPD", retained_SPD))
      print (c("omitted_SPD", omitted_SPD))
#       print (c("retained_SDA", retained_SDA))
#       print (c("omitted_SDA", omitted_SDA))
      
      ################################################################################
      #### NOTE first set of calculations based on the SPEED Filter Data
      ################################################################################
      
      trackkeeps.spd<-trackwant[which(trackwant$SPDfilter!="removed"),]
      
      #### great cirlce distance between successive locations (argosfilter package)
      trackkeeps.spd$dist.pckArgos<-append(0,distanceTrack(trackkeeps.spd$latitude,trackkeeps.spd$longitude))
      trackwant$dist.pckArgos.SPD[which(trackwant$SPDfilter!="removed")]<-trackkeeps.spd$dist.pckArgos
      
      #### great cirlce distance between successive locations (trip package)
      trackkeeps.spd$dist.pckTrip<-append(0,trackDistance(cbind(trackkeeps.spd$longitude,trackkeeps.spd$latitude), longlat = TRUE, prev = FALSE))
      trackwant$dist.pckTrip.SPD[which(trackwant$SPDfilter!="removed")]<-trackkeeps.spd$dist.pckTrip
      
      #### time differential between accepted filter locations
      trackkeeps.spd$dt<-append(0,(as.POSIXct(trackkeeps.spd$UTC[2:(nrow(trackkeeps.spd))] ,format='%Y/%m/%d %H:%M:%S', tz = "GMT")-
                                     as.POSIXct(trackkeeps.spd$UTC[1:(nrow(trackkeeps.spd)-1)] ,format='%Y/%m/%d %H:%M:%S', tz = "GMT")))
      trackwant$dt[which(trackwant$SPDfilter!="removed")]<-trackkeeps.spd$dt
      
      #### speed between locations
      trackkeeps.spd$speed.pckArgos<-((trackkeeps.spd$dist.pckArgos*1000)/((trackkeeps.spd$dt)*60))
      trackwant$speed.pckArgos.SPD[which(trackwant$SPDfilter!="removed")]<-trackkeeps.spd$speed.pckArgos
      
      #### speed between locations
      trackkeeps.spd$speed.pckTrip<-((trackkeeps.spd$dist.pckTrip*1000)/((trackkeeps.spd$dt)*60))
      trackwant$speed.pckTrip.SPD[which(trackwant$SPDfilter!="removed")]<-trackkeeps.spd$speed.pckTrip
      
      #### azimuth bearing between successive locations
      trackkeeps.spd$azimuth<-append(NA,distanceTrack(trackkeeps.spd$latitude,trackkeeps.spd$latitude))
      trackwant$azimuth.SPD[which(trackwant$SPDfilter!="removed")]<-trackkeeps.spd$azimuth
      
#       ################################################################################
#       #### NOTE this second set of calculations based on the SDA Filter Data
#       ################################################################################
#       
#       trackkeeps.sda<-trackwant[which(trackwant$SDAfilter!="removed"),]
#       
#       #### great cirlce distance between successive locations (argosfilter package in km)
#       trackkeeps.sda$dist.pckArgos<-append(0,distanceTrack(trackkeeps.sda$latitude,trackkeeps.sda$longitude))
#       trackwant$dist.pckArgos.SDA[which(trackwant$SDAfilter!="removed")]<-trackkeeps.sda$dist.pckArgos
#       
#       #### great cirlce distance between successive locations (trip package in km)
#       trackkeeps.sda$dist.pckTrip<-append(0,trackDistance(cbind(trackkeeps.sda$longitude,trackkeeps.sda$latitude), longlat = TRUE, prev = FALSE))
#       trackwant$dist.pckTrip.SDA[which(trackwant$SDAfilter!="removed")]<-trackkeeps.sda$dist.pckTrip
#       
#       #### time differential between accepted filter locations (in s)
#       trackkeeps.sda$dt<-append(0,(as.POSIXct(trackkeeps.sda$UTC[2:(nrow(trackkeeps.sda))] ,format='%Y/%m/%d %H:%M:%S', tz = "GMT")-
#                                      as.POSIXct(trackkeeps.sda$UTC[1:(nrow(trackkeeps.sda)-1)] ,format='%Y/%m/%d %H:%M:%S', tz = "GMT")))
#       trackwant$dt.SDA[which(trackwant$SDAfilter!="removed")]<-trackkeeps.sda$dt
#       
#       #### speed between locations (in m/s)
#       trackkeeps.sda$speed.pckArgos<-((trackkeeps.sda$dist.pckArgos*1000)/((trackkeeps.sda$dt)*60))
#       trackwant$speed.pckArgos.SDA[which(trackwant$SDAfilter!="removed")]<-trackkeeps.sda$speed.pckArgos
#       
#       #### speed between locations (in m/s)
#       trackkeeps.sda$speed.pckTrip<-((trackkeeps.sda$dist.pckTrip*1000)/((trackkeeps.sda$dt)*60))
#       trackwant$speed.pckTrip.SDA[which(trackwant$SDAfilter!="removed")]<-trackkeeps.sda$speed.pckTrip
#       
#       #### azimuth bearing between successive locations
#       trackkeeps.sda$azimuth<-append(NA,distanceTrack(trackkeeps.sda$latitude,trackkeeps.sda$latitude))
#       trackwant$azimuth.SDA[which(trackwant$SDAfilter!="removed")]<-trackkeeps.sda$azimuth
#       
      #### get max speeds  (in m/s)
      max_SPD<-max(trackwant$speed.pckArgos.SPD,na.rm=TRUE)
      # max_SDA<-max(trackwant$speed.pckTrip.SDA,na.rm=TRUE)

      #### convert trackwant$UTC back to character
      trackwant$UTC<-as.character(trackwant$UTC)
      
      
      trackwant[trackwant=="NaN"]=NA
      head(trackwant)
      rm(trackkeeps.spd)
      # rm(trackkeeps.sda)
      
      Deploy_ID<-deploy.id
      
      #### make table for number original and number removed for each bird
      if(j==1){
        filter_sum<-as.data.frame(cbind(as.character(file.id),original,retained_SPD,omitted_SPD,max_SPD))
      }else{
        filter_sum<-rbind(filter_sum,(as.data.frame(cbind(as.character(file.id),original,retained_SPD,omitted_SPD,max_SPD))))
      }
      rm(original,retained_SPD,omitted_SPD)
      
      #       #### make table for number original and number removed for each bird
#       if(j==1){
#         filter_sum<-as.data.frame(cbind(as.character(file.id),original,retained_SPD,omitted_SPD,max_SPD,retained_SDA,omitted_SDA,max_SDA,time_elapsed.1))
#       }else{
#         filter_sum<-rbind(filter_sum,(as.data.frame(cbind(as.character(file.id),original,retained_SPD,omitted_SPD,max_SPD,retained_SDA,omitted_SDA,max_SDA,time_elapsed.1))))
#       }
#       rm(original,retained_SPD,omitted_SPD,retained_SDA,omitted_SDA,time_elapsed.1)
      
      trackfiltout <- trackwant[,c("tag.serial.number","UTC", "latitude", "longitude", "height.above.ellipsoid", "SPDfilter","temperature", "dt", "dist.pckArgos.SPD", "speed.pckArgos.SPD", "dist.pckTrip.SPD", "speed.pckTrip.SPD", "azimuth.SPD")]
      trackfiltout$Deploy_ID <- rep(Deploy_ID,length(trackwant[,1]))
      # head(tracksdaout)
 
      rm(trackwant,track)
      
      #### output filtered data for each bird
      
      # create output directory, will not replace if already exists
      dir.create(paste(dir.out,species,"/2_EOBS_Freitas_out/",sep = ""),showWarnings=TRUE)
      dir.create(paste(dir.out,species,"/2_EOBS_Freitas_out/Indiv_Files",sep = ""),showWarnings=TRUE)
      
      #### output filtered data    
      write.table (trackfiltout, paste(dir.out,species,"/2_EOBS_Freitas_out/Indiv_Files/",species,"_",file.id,"_",deploy.id,"_SPD.csv", sep =""), sep=",", quote=FALSE,col.names=TRUE, row.names=F,na="")
    }
  }
  }


#### END #### 
  
    
    
    # #       
# #       if(plot== TRUE){
# #         
# #         require(maps)
# #         world_map = data.frame(map(plot=FALSE)[c("x","y")])
# #         names(world_map) = c("lon","lat")
# #         world_map = within(world_map, {
# #           lon = ifelse(lon < 0, lon + 360, lon)
# #         })
# #         
# #         mp1 <- fortify(map(fill=TRUE, plot=FALSE))
# #         mp2 <- mp1
# #         mp2$long <- mp2$long + 360
# #         mp2$group <- mp2$group + max(mp2$group) + 1
# #         mp <- rbind(mp1, mp2)
# #         ggplot(aes(x = long, y = lat), data = mp) + 
# #           geom_path() + 
# #           scale_x_continuous(limits = c(0, 360)) +
# #           geom_point(data=trackfiltout,aes(longitude360, latitude)) 
# #         
# #         trackfiltout$longitude360<-trackfiltout$longitude
# #         trackfiltout$longitude360[trackfiltout$longitude360<0]=((trackfiltout$longitude360[trackfiltout$longitude360<0]+180)+180)
# #         
# #         
# #         mp1 <- fortify(map(fill=TRUE, plot=FALSE))
# #         mp2 <- mp1
# #         mp2$long <- mp2$long + 360
# #         mp2$group <- mp2$group + max(mp2$group) + 1
# #         mp <- rbind(mp1, mp2)
# #         ggplot(aes(x = longitude, y = latitude), data = trackfiltout) + 
# #           geom_path() + 
# #           scale_x_continuous(limits = c(0, 360))
# # 
# #                 
# #         
# #         pdf(paste(dir.out,"2_GPS_Freitas_out/",file.id,"_",deploy.id,"_SPD_SDA.pdf",sep = ""),15,15)
# #         par(mfrow=c(3,4)) 
# #         
# #         plot(trackfiltout$longitude,trackfiltout$latitude,col="red",type="l", xlab="longitude_SPD",ylab="latitude_SPD",main = "SPD_filter")
# #         points(trackfiltout$longitude[which(mfilter=="removed")],trackfiltout$latitude[which(mfilter=="removed")],col="red",pch = 20,cex =2)
# #         lines(trackfiltout$longitude[which(mfilter!="removed")],trackfiltout$latitude[which(mfilter!="removed")],col="blue",pch = 20,cex =1.5)
# #         points(trackfiltout$longitude[which(mfilter!="removed")],trackfiltout$latitude[which(mfilter!="removed")],col="blue",pch = 20,cex =2)
# #         
# #         plot(trackfiltout$longitude[which(mfilter!="removed")],trackfiltout$latitude[which(mfilter!="removed")],col="blue",type="l", xlab="longitude_SDA",ylab="latitude_SDA",main = "SDA_filter_points_removed")
# #         points(trackfiltout$longitude[which(mfilter!="removed")],trackfiltout$latitude[which(mfilter!="removed")],col="blue",pch = 20,cex =2)
# #         
# #         #### histogram of times
# #         hist(na.omit(trackfiltout$dt[which(trackfiltout$SPDfilter!="removed")]),100,xlab ='time_lag (m)', main="interval btwn locs.SPD")
# #         
# #         #### SPD data
# #         #### histogram of distance.pckArgos
# #         hist(na.omit(trackfiltout$dist.pckArgos.SPD[which(trackfiltout$SPDfilter!="removed")]),100,xlab='distance (km)', main = "pckArgos dist btwn locs.SPD")
# #         
# #         #### histogram of distance.pckTrip
# #         hist(na.omit(trackfiltout$dist.pckTrip.SPD[which(trackfiltout$SPDfilter!="removed")]),100,xlab='distance (km)', main = "pckTrip dist btwn locs.SPD")
# #         
# #         #### histogram of speed.pckArgos
# #         hist(na.omit(trackfiltout$speed.pckArgos.SPD[which(trackfiltout$SPDfilter!="removed")]),100,xlab ='speed (m/s)',main="speed.pckArgos.SPD")
# #         
# #         #### histogram of speed.pckTrip
# #         hist(na.omit(trackfiltout$speed.pckTrip.SPD[which(trackfiltout$SPDfilter!="removed")]),100,xlab ='speed (m/s)',main="speed.pckTrip.SPD")
# #         
# #         #### SDA data
# #         #### histogram of distance.pckArgos
# #         hist(na.omit(trackfiltout$dist.pckArgos.SDA[which(trackfiltout$SDAfilter!="removed")]),100,xlab='distance (km)', main = "pckArgos dist btwn locs.SDA")
# #         
# #         #### histogram of distance.pckTrip
# #         hist(na.omit(trackfiltout$dist.pckTrip.SDA[which(trackfiltout$SDAfilter!="removed")]),100,xlab='distance (km)', main = "pckTrip dist btwn locs.SDA")
# #         
# #         #### histogram of speed.pckArgos
# #         hist(na.omit(trackfiltout$speed.pckArgos.SDA[which(trackfiltout$SDAfilter!="removed")]),100,xlab ='speed (m/s)',main="speed.pckArgos.SDA")
# #         
# #         #### histogram of speed.pckTrip
# #         hist(na.omit(trackfiltout$speed.pckTrip.SDA[which(trackfiltout$SDAfilter!="removed")]),100,xlab ='speed (m/s)',main="speed.pckTrip.SDA")
# #         
# #         #### roseplot of bearings
# #         # rose.diag(na.omit(rad(bearingTrack(trackfiltout$latitude, trackfiltout$longitude))),bins=36, main="azimuth", prop=3.5, pts=FALSE, cex=2.5, pch=5, dotsep=40, shrink=1) 
# #         dev.off()
# #       }
# #       rm(trackfiltout)
#       
#     } else {
#       original<-NA
#       retained_SPD<-NA
#       omitted_SPD<-NA
#        max_SPD<-NA
#       if(j==1){
#         filter_sum<-as.data.frame(cbind(as.character(file.id),original,retained_SPD,omitted_SPD,max_SPD))
#       }else{
#         filter_sum<-rbind(filter_sum,(as.data.frame(cbind(as.character(file.id),original,retained_SPD,omitted_SPD,max_SPD))))
#       }
#       rm(original,retained_SPD,omitted_SPD,retained_SDA)
#     }
#   }   # file.id close loop
#   
#   } # end if filter.data
#   
#   #### summarize deployments and recoveries
#   
# #   #head(recovs)
# #   metaWants$DeplEvents<-paste(metaWants$Year,metaWants$SubCol_Code,metaWants$DeplSess, sep="_")
# #   deps_summary<-data.frame(unique(metaWants$DeplEvents))
# #   deps_summary_out <- data.frame(matrix(ncol = 11, nrow = length(deps_summary[,1])))
# #   colnames(deps_summary_out)<-c("DeplEvents", "D", "N", "R", "0", "1", "2", "3", "4", "5", "6")
#   
#   #   GPS Fields Recovery Fields
#   #   0-deployment
#   #   1-good file
#   #   2-tag lost
#   #   3-tag problems but data file ok
#   #   4-tag problems file has some data but not useful
#   #   5-tag problems no file recovered
#   
#   library(plyr)
#   
#   # k=12
#   for (k in 1:length(deps_summary_out[,1])) {
#     deps_summary_out[k,1]<-as.character(deps_summary[k,1])
#     metaWants.sub<-subset(metaWants,DeplEvents==deps_summary[k,])
#     y=t(c(table(metaWants.sub$Tagging_Event), table(metaWants.sub$GPS_TagRecov)))
#     y<-cbind(as.data.frame(deps_summary_out[k,1]),y)
#     colnames(y)[1]<-("DeplEvents") 
#     
#     new<-join(deps_summary_out[k,], y, by = NULL, type = "full", match = "all")
#     
#     deps_summary_out[k,]<-new[2,]
#     }
#   
#   
#   colnames(deps_summary_out)<-c("deploy_events","deployed_GPS_or_TDR","handled_not_tagged", "recovered","deployed_GPS","good_file","tag_lost","tag_problem data ok","tag_problems_data_bad","tag_problems_no_data","tdr_only")
#   
#   deps_summary_out[is.na(deps_summary_out)]<-0
#   deps_summary_out$Percent_GPS_Tag_Recov<-(rowSums(deps_summary_out[,c(6,8,9,10)], na.rm = FALSE, dims = 1)/(deps_summary_out$deployed_GPS_or_TDR-deps_summary_out$tdr_only))*100
#   deps_summary_out$Percent_GPS_Bird_Resighted<-(rowSums(deps_summary_out[,c(6,7,8,9,10)], na.rm = FALSE, dims = 1)/(deps_summary_out$deployed_GPS_or_TDR-deps_summary_out$tdr_only))*100
#   deps_summary_out$Percent_GPS_Bird_not_Resighted<-100-(deps_summary_out$Percent_GPS_Bird_Resighted)
#   deps_summary_out$Percent_GPS_Tag_Data_Issue<-(rowSums(deps_summary_out[,c(9,10)], na.rm = FALSE, dims = 1)/(deps_summary_out$deployed_GPS_or_TDR-deps_summary_out$tdr_only))*100
#   
#   if (filter.data=="Y") {
#   ####output the number of points filtered for each bird (gps error source from lcerrors input table ex: for GPS from wakefield 2013 scienc (as set in beginning of code)) 
#   write.table(filter_sum,paste(dir.out,"2_GPS_Freitas_out/Summary_GPS_Filtered",species,"_SPD_ref.csv",sep = ""),sep=",",quote=FALSE,col.names=TRUE,row.names=FALSE)
#   
#   ####output FilterParametersUsed
#   FilterParametersUsed<-as.data.frame(t(c(species,vmax,ang,distlim,lc[1],lcerr)))
#   colnames(FilterParametersUsed)<-c("species","gps_vmax","ang1","ang2","distlim1","distlim2","lc","lcerror")
#   write.table(FilterParametersUsed,paste(dir.out,"2_GPS_Freitas_out/FilterParametersUsed_",species,"_SPD_ref.csv",sep = ""),sep=",",quote=FALSE,col.names=TRUE,row.names=FALSE)
#   }
#   
#   ####output summary of deployments for each year session location
#   write.table(deps_summary_out,paste(dir.out,"Stats_out/Deps_Sum_All_",species,"_GPS.csv",sep = ""),sep=",",quote=FALSE,col.names=TRUE,row.names=FALSE)
#   
#   
#   rm(filter_sum,FilterParametersUsed,deps_summary_out)
#   
#   
# } # species
# 
# 
# 
# tocall=as.numeric(proc.time()[3]) - as.numeric(ticall)
# print("time elapsed")
# print(tocall)
# 
# #### END ####
# 
# # For exporting plots in .png
# #           if(plot== TRUE){
# #             png(paste("D:/Share_Data/Tracking_Data/GPS/2_GPS_Freitas_out/",file.id,"_SPD_SDA.png",sep = ""),10000,12000)
# #             par(mfrow=c(2,4)) 
# #             
# #             plot(trackfiltout$longitude,trackfiltout$latitude,col="red",type="l", xlab="longitude_SPD",ylab="latitude_SPD", cex =14)
# #             points(trackfiltout$longitude[which(mfilter=="removed")],trackfiltout$latitude[which(mfilter=="removed")],col="red",pch = 20, cex =19)
# #             lines(trackfiltout$longitude[which(mfilter!="removed")],trackfiltout$latitude[which(mfilter!="removed")],col="blue",pch = 20, cex =15)
# #             points(trackfiltout$longitude[which(mfilter!="removed")],trackfiltout$latitude[which(mfilter!="removed")],col="blue",pch = 20, cex =20)
# #             
# #             plot(trackfiltout$longitude,trackfiltout$latitude,col="red",type="l", xlab="longitude_SDA",ylab="latitude_SDA", cex =14)
# #             points(trackfiltout$longitude[which(cfilter=="removed")],trackfiltout$latitude[which(cfilter=="removed")],col="red",pch = 20, cex =19)
# #             lines(trackfiltout$longitude[which(cfilter!="removed")],trackfiltout$latitude[which(cfilter!="removed")],col="blue",pch = 20, cex =15)
# #             points(trackfiltout$longitude[which(cfilter!="removed")],trackfiltout$latitude[which(cfilter!="removed")],col="blue",pch = 20, cex =20)
# # 
# #             plot(trackfiltout$longitude[which(cfilter!="removed")],trackfiltout$latitude[which(cfilter!="removed")],col="blue",type="l", xlab="longitude_SDA",ylab="latitude_SDA")
# #             points(trackfiltout$longitude[which(cfilter!="removed")],trackfiltout$latitude[which(cfilter!="removed")],col="blue",pch = 20,cex =2)
# #             
# #             #### histogram of distance.pckArgos
# #             hist(na.omit(trackfiltout$dist.pckArgos[2:nrow(trackfiltout)]),100,xlab='distance (km)')
# #             
# #             #### histogram of distance.pckTrip
# #             hist(na.omit(trackfiltout$dist.pckTrip[2:nrow(trackfiltout)]),100,xlab='distance (km)')
# #             
# #             #### histogram of times
# #             hist(na.omit(trackfiltout$dt[which(trackfiltout$SPDfilter!="removed")]),100,xlab ='time_lag (m)')
# #               
# #             #### histogram of speed.pckArgos
# #             hist(na.omit(trackfiltout$speed.pckTrip[which(trackfiltout$SPDfilter!="removed")]),100,xlab ='speed (m/s)')
# #             
# #             #### histogram of speed.pckTrip
# #             hist(na.omit(trackfiltout$speed.pckTrip[which(trackfiltout$SPDfilter!="removed")]),100,xlab ='speed (m/s)')
# #             
# #             #### roseplot of bearings
# #             # rose.diag(na.omit(rad(bearingTrack(trackfiltout$latitude, trackfiltout$longitude))),bins=36, main="azimuth", prop=3.5, pts=FALSE, cex=2.5, pch=5, dotsep=40, shrink=1) 
# #             
# #             dev.off()
# #           }
# 
# 
# #### Extra
# library(ggplot2)
# gg <- ggplot(trackkeeps.spd)
# gg <- gg + geom_density(aes(speed.pckTrip), alpha=1) + 
#   scale_color_brewer(palette = "Set1")
# gg <- gg + scale_x_continuous(limits=c(0,30)) + xlab("speed") 
# gg
# 

