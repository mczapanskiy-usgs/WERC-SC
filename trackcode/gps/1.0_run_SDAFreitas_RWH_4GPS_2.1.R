

################################################################
#
# EDITING GPS TAG DATA USING THE ARGOSFILTER PACKAGE
#
# (Using location data direct from the iGOTU platform)
#
################################################################
# Bill Henry USGS April 2014
# updated Dec 18, 2014 to refine speed filters and graphical outputs
#
#
# for SDA filtering of GPS TAG DATA where:
# originating file is from igotu tag
#
# supporting inputs (.csv files)
# 1. table for parameters to base filter on (spp  vmaxSA_Tab2_W5ms_x_3SD  ang1	ang2	distlim1	distlim2	colrad	type)
# 2. metadata table with Deployment and Recovery information
#
# outputs
# .csv of time lat long filtered
# FilterParametersUsed_ - species,vmax,ang1,ang2,distlim1,distlim2
# .pdf of track and histograms of distance, time lag (between locations), and speed for  - summary of points filtered
#
# note: file name = unique id unless otherwise noted

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

# set plot option to review plots TRUE or FALSE
plot<-TRUE

####  dir.in for .csv files
dir.in <- "D:/Share_Data/Tracking_Data/GPS/"

####  meta.in for .csv files
meta.in <- "D:/Share_Data/GitHub/WERC-SC/trackcode/gps/"

#### dir.out for .csv files
dir.out <- "D:/Share_Data/Tracking_Data/GPS/"

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

# open metadata file
# metadata<-read.table ("/Users/henry/Documents/Work/Projects/USGS/Latest/Tracking Data/GPS/metadata_all_GPS_12.19.14_2.0.csv",header=T, sep=",", strip.white=T)
metadata<-read.table(paste(meta.in,"metadata_all_GPS.csv",sep=""),header=T, sep=",", strip.white=T)

# get species from AUO Code in metadata
species.tagged<-unique(metadata$Species)
# i=5
for (i in 1:length(species.tagged)) {
  species<-species.tagged[i]

  #### get filter parameters
  paramwant<-subset(parameters,spp==as.character(species))
  
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
  
  #### Get deployment session (depl_sess) from from metadata.csv
  depl_sess.ids<-unique(subset(metadata, Species%in%species & (Site%in%site.ids) & Year%in%year.ids)$DeplSess)
  
  #### get metadata for Species omitting any metadata records without deployment or recovery
  metaWants<-(subset(metadata, Species==species & (Tagging_Event=='D' | Tagging_Event=='R')))
  
  ### get Deploy_IDs of files that have good data
  deploy.ids<-(subset(metaWants, Tagging_Event=="R" & (GPS_TagRecov==1 | GPS_TagRecov==3)))$Deploy_ID
  print(c("number of birds with recoveries and good data",length(deploy.ids)))
    
  #### reorder metadata$UTC format
  metaWants$UTC<-format(strptime(as.character(metaWants$UTC), "%m/%d/%Y %H:%M"), "%Y/%m/%d %H:%M:%S")
  
  #### select deployments
  deploys<-subset(metaWants,Tagging_Event=='D' & (Deploy_ID %in% deploy.ids))
  
  #### select recoveries
  recovs<-subset(metaWants,Tagging_Event=='R' & (Deploy_ID %in% deploy.ids))

  if (filter.data == "Y") {
  
  #### Loop to read in tracks for each track.csv 
  # j=27
  for (j in 1:length(deploy.ids)) {
    
    deploy.id<-deploy.ids[j]
    file.id <- recovs$GPS_Track_File[j]
    print(file.id)
    
    #### get track recovery metadata
    recwant<-recovs[recovs$Deploy_ID==deploy.id,]

    #### get track deployment metadata
    deplwant<-deploys[deploys$Deploy_ID==deploy.id,]
    
    #### get track
    track <- read.table (paste(dir.in,"1_RawGPS/",file.id,"_RawGPS.csv",sep = ""),header=T, sep=",", strip.white=T)
    # head(track)
    
    #### create date time field
    track$UTC<-(as.POSIXct(paste(as.character(track$Date), as.character(track$Time, sep=" ")), tz = "GMT"))
      
    # double check track order by year, yday and time
    track <- track[order(track$UTC),]
        
    #### check if depl and recover times exist in metadata, if not set to beginning and end of track
    if (is.na(deplwant$UTC)) {
      deplwant$UTC<-track$UTC[1]
    }
    
    if (is.na(recwant$UTC)) {
      recwant$UTC<-track$UTC[length(track[,1])]
    }
    
    ####subset track for points between deployment and recovery times (UTC)
    trackwant<-subset(track,(UTC)>=as.POSIXct(as.character(deplwant$UTC), tz = "GMT") & (UTC)<=as.POSIXct(as.character(recwant$UTC), tz = "GMT"))
    # head(trackwant)
    
    #### check all lat-long values = 0 
    trackwant <- trackwant[trackwant$Latitude != 0,]
    trackwant <- trackwant[trackwant$Longitude != 0,]
    print ('Total rows after 0 locations removed')
    print (length(trackwant[,1]))

    #### set up vectors for Argos filter (Freitas)
    dtime<- trackwant$UTC
    
    lc<-rep.int(3, length(trackwant$Latitude))
    
    tic1=as.numeric(proc.time()[3])
    
    #### this is just a speed filter - not used, use SDA
    # filter by speed only [set max speed to equivalent to vmax]
    if(length(trackwant$Latitude)>1){
      mfilter<- vmask(trackwant$Latitude, trackwant$Longitude, dtime, vmax)
      trackwant$SPDfilter<-mfilter
      
      #
      # lines(track$Longitude[which(mfilter=="not")],track$Latitude[which(mfilter=="not")],col="red")
      ####	
      
      #### filter data using sdafilter [max speed to equivalent to vmax]
      cfilter<- sdafilter(trackwant$Latitude, trackwant$Longitude, dtime, lc, vmax, ang, distlim)
      head(cfilter)
      trackwant$SDAfilter<-cfilter
      
      # get time elapsed
      time_elapsed.1=as.numeric(proc.time()[3]) - tic1
      print (time_elapsed.1)
      
      # summarize cfilter data
      original<-length (trackwant[,1])
      retained_SPD<-length(trackwant$SPDfilter[which(mfilter!="removed")])
      omitted_SPD<-length(trackwant$SPDfilter[which(mfilter=="removed")])          
      retained_SDA<-length(trackwant$SDAfilter[which(cfilter!="removed")])
      omitted_SDA<-length(trackwant$SDAfilter[which(cfilter=="removed")])
      print (c("original", original))
      print (c("retained_SPD", retained_SPD))
      print (c("omitted_SPD", omitted_SPD))
      print (c("retained_SDA", retained_SDA))
      print (c("omitted_SDA", omitted_SDA))
      
      ################################################################################
      #### NOTE first set of calculations based on the SPEED Filter Data
      ################################################################################
      
      trackkeeps.spd<-trackwant[which(trackwant$SPDfilter!="removed"),]
      
      #### great cirlce distance between successive locations (argosfilter package)
      trackkeeps.spd$dist.pckArgos<-append(0,distanceTrack(trackkeeps.spd$Latitude,trackkeeps.spd$Longitude))
      trackwant$dist.pckArgos.SPD[which(trackwant$SPDfilter!="removed")]<-trackkeeps.spd$dist.pckArgos
      
      #### great cirlce distance between successive locations (trip package)
      trackkeeps.spd$dist.pckTrip<-append(0,trackDistance(cbind(trackkeeps.spd$Longitude,trackkeeps.spd$Latitude), longlat = TRUE, prev = FALSE))
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
      trackkeeps.spd$azimuth<-append(NA,distanceTrack(trackkeeps.spd$Latitude,trackkeeps.spd$Latitude))
      trackwant$azimuth.SPD[which(trackwant$SPDfilter!="removed")]<-trackkeeps.spd$azimuth
      
      ################################################################################
      #### NOTE first set of calculations based on the SDA Filter Data
      ################################################################################
      
      trackkeeps.sda<-trackwant[which(trackwant$SDAfilter!="removed"),]
      
      #### great cirlce distance between successive locations (argosfilter package in km)
      trackkeeps.sda$dist.pckArgos<-append(0,distanceTrack(trackkeeps.sda$Latitude,trackkeeps.sda$Longitude))
      trackwant$dist.pckArgos.SDA[which(trackwant$SDAfilter!="removed")]<-trackkeeps.sda$dist.pckArgos
      
      #### great cirlce distance between successive locations (trip package in km)
      trackkeeps.sda$dist.pckTrip<-append(0,trackDistance(cbind(trackkeeps.sda$Longitude,trackkeeps.sda$Latitude), longlat = TRUE, prev = FALSE))
      trackwant$dist.pckTrip.SDA[which(trackwant$SDAfilter!="removed")]<-trackkeeps.sda$dist.pckTrip
      
      #### time differential between accepted filter locations (in s)
      trackkeeps.sda$dt<-append(0,(as.POSIXct(trackkeeps.sda$UTC[2:(nrow(trackkeeps.sda))] ,format='%Y/%m/%d %H:%M:%S', tz = "GMT")-
                                     as.POSIXct(trackkeeps.sda$UTC[1:(nrow(trackkeeps.sda)-1)] ,format='%Y/%m/%d %H:%M:%S', tz = "GMT")))
      trackwant$dt.SDA[which(trackwant$SDAfilter!="removed")]<-trackkeeps.sda$dt
      
      #### speed between locations (in m/s)
      trackkeeps.sda$speed.pckArgos<-((trackkeeps.sda$dist.pckArgos*1000)/((trackkeeps.sda$dt)*60))
      trackwant$speed.pckArgos.SDA[which(trackwant$SDAfilter!="removed")]<-trackkeeps.sda$speed.pckArgos
      
      #### speed between locations (in m/s)
      trackkeeps.sda$speed.pckTrip<-((trackkeeps.sda$dist.pckTrip*1000)/((trackkeeps.sda$dt)*60))
      trackwant$speed.pckTrip.SDA[which(trackwant$SDAfilter!="removed")]<-trackkeeps.sda$speed.pckTrip
      
      #### azimuth bearing between successive locations
      trackkeeps.sda$azimuth<-append(NA,distanceTrack(trackkeeps.sda$Latitude,trackkeeps.sda$Latitude))
      trackwant$azimuth.SDA[which(trackwant$SDAfilter!="removed")]<-trackkeeps.sda$azimuth
      
      #### get max speeds  (in m/s)
      max_SPD<-max(trackwant$speed.pckTrip.SPD,na.rm=TRUE)
      max_SDA<-max(trackwant$speed.pckTrip.SDA,na.rm=TRUE)
      
  #    trackwant[trackwant=="NaN"]=NA
      head(trackwant)
      rm(trackkeeps.spd)
      rm(trackkeeps.sda)
      
      #### make table for number original and number removed for each bird
      if(j==1){
        filter_sum<-as.data.frame(cbind(as.character(file.id),original,retained_SPD,omitted_SPD,max_SPD,retained_SDA,omitted_SDA,max_SDA,time_elapsed.1))
      }else{
        filter_sum<-rbind(filter_sum,(as.data.frame(cbind(as.character(file.id),original,retained_SPD,omitted_SPD,max_SPD,retained_SDA,omitted_SDA,max_SDA,time_elapsed.1))))
      }
      rm(original,retained_SPD,omitted_SPD,retained_SDA,omitted_SDA,time_elapsed.1)
      
      
      #### convert trackwant$UTC back to character
      trackwant$UTC<-as.character(trackwant$UTC)
      
      trackfiltout <-  trackwant[,c("UTC", "Latitude", "Longitude", "Altitude", "SPDfilter", "SDAfilter", "dt", "dist.pckArgos.SPD", "speed.pckArgos.SPD", "dist.pckTrip.SPD", "speed.pckTrip.SPD", "azimuth.SPD", "dist.pckArgos.SDA", "speed.pckArgos.SDA", "dist.pckTrip.SDA", "speed.pckTrip.SDA", "azimuth.SDA")]
      # head(tracksdaout)

      rm(trackwant,track)
      
      #### output filtered data for each bird
      
      # create output directory, will not replace if already exists
      dir.create(paste(dir.out,"2_GPS_Freitas_out/",sep = ""),showWarnings=TRUE)
      
      #### output filtered data    
      write.table (trackfiltout, paste(dir.out,"2_GPS_Freitas_out/",file.id,"_",deploy.id,"_SPD_SDA.csv", sep =""), sep=",", quote=FALSE,col.names=TRUE, row.names=F,na="")
      
      if(plot== TRUE){
        pdf(paste(dir.out,"2_GPS_Freitas_out/",file.id,"_",deploy.id,"_SPD_SDA.pdf",sep = ""),15,15)
        par(mfrow=c(3,4)) 
        
        plot(trackfiltout$Longitude,trackfiltout$Latitude,col="red",type="l", xlab="Longitude_SPD",ylab="Latitude_SPD",main = "SPD_filter")
        points(trackfiltout$Longitude[which(mfilter=="removed")],trackfiltout$Latitude[which(mfilter=="removed")],col="red",pch = 20,cex =2)
        lines(trackfiltout$Longitude[which(mfilter!="removed")],trackfiltout$Latitude[which(mfilter!="removed")],col="blue",pch = 20,cex =1.5)
        points(trackfiltout$Longitude[which(mfilter!="removed")],trackfiltout$Latitude[which(mfilter!="removed")],col="blue",pch = 20,cex =2)
        
        plot(trackfiltout$Longitude,trackfiltout$Latitude,col="red",type="l", xlab="Longitude_SDA",ylab="Latitude_SDA",main = "SDA_filter")
        points(trackfiltout$Longitude[which(cfilter=="removed")],trackfiltout$Latitude[which(cfilter=="removed")],col="red",pch = 20,cex =2)
        lines(trackfiltout$Longitude[which(cfilter!="removed")],trackfiltout$Latitude[which(cfilter!="removed")],col="blue",pch = 20,cex =1.5)
        points(trackfiltout$Longitude[which(cfilter!="removed")],trackfiltout$Latitude[which(cfilter!="removed")],col="blue",pch = 20,cex =2)
        
        plot(trackfiltout$Longitude[which(cfilter!="removed")],trackfiltout$Latitude[which(cfilter!="removed")],col="blue",type="l", xlab="Longitude_SDA",ylab="Latitude_SDA",main = "SDA_filter_points_removed")
        points(trackfiltout$Longitude[which(cfilter!="removed")],trackfiltout$Latitude[which(cfilter!="removed")],col="blue",pch = 20,cex =2)
        
        #### histogram of times
        hist(na.omit(trackfiltout$dt[which(trackfiltout$SPDfilter!="removed")]),100,xlab ='time_lag (m)', main="interval btwn locs.SPD")
        
        #### SPD data
        #### histogram of distance.pckArgos
        hist(na.omit(trackfiltout$dist.pckArgos.SPD[which(trackfiltout$SPDfilter!="removed")]),100,xlab='distance (km)', main = "pckArgos dist btwn locs.SPD")
        
        #### histogram of distance.pckTrip
        hist(na.omit(trackfiltout$dist.pckTrip.SPD[which(trackfiltout$SPDfilter!="removed")]),100,xlab='distance (km)', main = "pckTrip dist btwn locs.SPD")
        
        #### histogram of speed.pckArgos
        hist(na.omit(trackfiltout$speed.pckArgos.SPD[which(trackfiltout$SPDfilter!="removed")]),100,xlab ='speed (m/s)',main="speed.pckArgos.SPD")
        
        #### histogram of speed.pckTrip
        hist(na.omit(trackfiltout$speed.pckTrip.SPD[which(trackfiltout$SPDfilter!="removed")]),100,xlab ='speed (m/s)',main="speed.pckTrip.SPD")
        
        #### SDA data
        #### histogram of distance.pckArgos
        hist(na.omit(trackfiltout$dist.pckArgos.SDA[which(trackfiltout$SDAfilter!="removed")]),100,xlab='distance (km)', main = "pckArgos dist btwn locs.SDA")
        
        #### histogram of distance.pckTrip
        hist(na.omit(trackfiltout$dist.pckTrip.SDA[which(trackfiltout$SDAfilter!="removed")]),100,xlab='distance (km)', main = "pckTrip dist btwn locs.SDA")
        
        #### histogram of speed.pckArgos
        hist(na.omit(trackfiltout$speed.pckArgos.SDA[which(trackfiltout$SDAfilter!="removed")]),100,xlab ='speed (m/s)',main="speed.pckArgos.SDA")
        
        #### histogram of speed.pckTrip
        hist(na.omit(trackfiltout$speed.pckTrip.SDA[which(trackfiltout$SDAfilter!="removed")]),100,xlab ='speed (m/s)',main="speed.pckTrip.SDA")
        
        #### roseplot of bearings
        # rose.diag(na.omit(rad(bearingTrack(trackfiltout$Latitude, trackfiltout$Longitude))),bins=36, main="azimuth", prop=3.5, pts=FALSE, cex=2.5, pch=5, dotsep=40, shrink=1) 
        dev.off()
      }
      rm(trackfiltout)
      
    } else {
      original<-NA
      retained_SPD<-NA
      omitted_SPD<-NA
      retained_SDA<-NA
      omitted_SDA<-NA
      time_elapsed.1<-NA
      max_SPD<-NA
      max_SDA<-NA
      if(j==1){
        filter_sum<-as.data.frame(cbind(as.character(file.id),original,retained_SPD,omitted_SPD,max_SPD,retained_SDA,omitted_SDA,max_SDA,time_elapsed.1))
      }else{
        filter_sum<-rbind(filter_sum,(as.data.frame(cbind(as.character(file.id),original,retained_SPD,omitted_SPD,max_SPD,retained_SDA,omitted_SDA,max_SDA,time_elapsed.1))))
      }
      rm(original,retained_SPD,omitted_SPD,retained_SDA,omitted_SDA,time_elapsed.1)
    }
  }   # file.id close loop
  
  } # end if filter.data
  
  #### summarize deployments and recoveries
  
  #head(recovs)
#   metaWants$DeplEvents<-paste(metaWants$Year,metaWants$SubCol_Code,metaWants$DeplSess, sep="_")
#   deps_summary<-data.frame(unique(metaWants$DeplEvents))
#   deps_summary_out <- data.frame(matrix(ncol = 17, nrow = length(deps_summary[,1])))
#   colnames(deps_summary_out)<-c("DeplEvents", "D", "N", "R", "GPS_0", "GPS_1", "GPS_2", "GPS_3", "GPS_4", "GPS_5", "GPS_6",...
#                                 "TDR_0", "TDR_1", "TDR_2", "TDR_3", "TDR_4", "TDR_5", "TDR_6")
#   
#   #   GPS Fields Recovery Fields
#   #   0-deployment
#   #   1-good file
#   #   2-tag lost
#   #   3-tag problems but data file ok
#   #   4-tag problems file has some data but not useful
#   #   5-tag problems no file recovered
#   #   6-no GPS tag deployed (or no other tag deployed)
#   
#   library(plyr)
#   
#   # k=12
#   for (k in 1:length(deps_summary_out[,1])) {
#     deps_summary_out[k,1]<-as.character(deps_summary[k,1])
#     metaWants.sub<-subset(metaWants,DeplEvents==deps_summary[k,])
#     y=t(c(table(metaWants.sub$Tagging_Event), table(metaWants.sub$GPS_TagRecov),table(metaWants.sub$TDR_TagRecov)))
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
  if (filter.data=="Y") {
  ####output the number of points filtered for each bird (gps error source from lcerrors input table ex: for GPS from wakefield 2013 scienc (as set in beginning of code)) 
  write.table(filter_sum,paste(dir.out,"2_GPS_Freitas_out/Summary_GPS_Filtered",species,"_SPD_ref.csv",sep = ""),sep=",",quote=FALSE,col.names=TRUE,row.names=FALSE)
  
  ####output FilterParametersUsed
  FilterParametersUsed<-as.data.frame(t(c(as.character(species),vmax,ang,distlim,lc[1],lcerr)))
  colnames(FilterParametersUsed)<-c("species","gps_vmax","ang1","ang2","distlim1","distlim2","lc","lcerror")
  write.table(FilterParametersUsed,paste(dir.out,"2_GPS_Freitas_out/FilterParametersUsed_",species,"_SPD_ref.csv",sep = ""),sep=",",quote=FALSE,col.names=TRUE,row.names=FALSE)
  }
  
#   ####output summary of deployments for each year session location
#   write.table(deps_summary_out,paste(dir.out,"Stats_out/Deps_Sum_All_",species,"_GPS.csv",sep = ""),sep=",",quote=FALSE,col.names=TRUE,row.names=FALSE)
  
  
  rm(filter_sum,FilterParametersUsed,deps_summary_out)
  
  
} # species



tocall=as.numeric(proc.time()[3]) - as.numeric(ticall)
print("time elapsed")
print(tocall)

#### END ####

# For exporting plots in .png
#           if(plot== TRUE){
#             png(paste("D:/Share_Data/Tracking_Data/GPS/2_GPS_Freitas_out/",file.id,"_SPD_SDA.png",sep = ""),10000,12000)
#             par(mfrow=c(2,4)) 
#             
#             plot(trackfiltout$Longitude,trackfiltout$Latitude,col="red",type="l", xlab="Longitude_SPD",ylab="Latitude_SPD", cex =14)
#             points(trackfiltout$Longitude[which(mfilter=="removed")],trackfiltout$Latitude[which(mfilter=="removed")],col="red",pch = 20, cex =19)
#             lines(trackfiltout$Longitude[which(mfilter!="removed")],trackfiltout$Latitude[which(mfilter!="removed")],col="blue",pch = 20, cex =15)
#             points(trackfiltout$Longitude[which(mfilter!="removed")],trackfiltout$Latitude[which(mfilter!="removed")],col="blue",pch = 20, cex =20)
#             
#             plot(trackfiltout$Longitude,trackfiltout$Latitude,col="red",type="l", xlab="Longitude_SDA",ylab="Latitude_SDA", cex =14)
#             points(trackfiltout$Longitude[which(cfilter=="removed")],trackfiltout$Latitude[which(cfilter=="removed")],col="red",pch = 20, cex =19)
#             lines(trackfiltout$Longitude[which(cfilter!="removed")],trackfiltout$Latitude[which(cfilter!="removed")],col="blue",pch = 20, cex =15)
#             points(trackfiltout$Longitude[which(cfilter!="removed")],trackfiltout$Latitude[which(cfilter!="removed")],col="blue",pch = 20, cex =20)
# 
#             plot(trackfiltout$Longitude[which(cfilter!="removed")],trackfiltout$Latitude[which(cfilter!="removed")],col="blue",type="l", xlab="Longitude_SDA",ylab="Latitude_SDA")
#             points(trackfiltout$Longitude[which(cfilter!="removed")],trackfiltout$Latitude[which(cfilter!="removed")],col="blue",pch = 20,cex =2)
#             
#             #### histogram of distance.pckArgos
#             hist(na.omit(trackfiltout$dist.pckArgos[2:nrow(trackfiltout)]),100,xlab='distance (km)')
#             
#             #### histogram of distance.pckTrip
#             hist(na.omit(trackfiltout$dist.pckTrip[2:nrow(trackfiltout)]),100,xlab='distance (km)')
#             
#             #### histogram of times
#             hist(na.omit(trackfiltout$dt[which(trackfiltout$SPDfilter!="removed")]),100,xlab ='time_lag (m)')
#               
#             #### histogram of speed.pckArgos
#             hist(na.omit(trackfiltout$speed.pckTrip[which(trackfiltout$SPDfilter!="removed")]),100,xlab ='speed (m/s)')
#             
#             #### histogram of speed.pckTrip
#             hist(na.omit(trackfiltout$speed.pckTrip[which(trackfiltout$SPDfilter!="removed")]),100,xlab ='speed (m/s)')
#             
#             #### roseplot of bearings
#             # rose.diag(na.omit(rad(bearingTrack(trackfiltout$Latitude, trackfiltout$Longitude))),bins=36, main="azimuth", prop=3.5, pts=FALSE, cex=2.5, pch=5, dotsep=40, shrink=1) 
#             
#             dev.off()
#           }
