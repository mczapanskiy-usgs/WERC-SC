################################################################
#
# FILTERING SATELLITE TAG DATA USING THE ARGOSFILTER PACKAGE
#
# (Using location data edited by the swap filter in the STAT package)
#
################################################################

# for SDA filtering of Argos data where:
# doppler mirrors have been filtered (switched) via Seaturle.org's Algorithm
# inputs
# folder containing .csv's of bird movements with time(UTC, 6/19/2008 8:20),lat (dd),lon (dd),lc,nb_mess
# where file name is unique id
# table of filter parameters
# table of location errors, (default is lcerrors from Costa et al 2010)
# outputs
# .csv of time lat long
# FilterParametersUsed_ - species,vmax,ang1,ang2,distlim1,distlim2
# SummaryFriedasErrors_ - summary of individual errors, animal.id,meanerr
# SummaryFriedasFiltered_ -animal.ids1,lc.1,end_location,not,removed

# clear all
rm(list=ls())

# install the argosfilter package
library(argosfilter)
library(plyr)

# set species AUO Code
species<-"HAPE"

# set year (optional)
year=NA

# set plot option to review plots TRUE or FALSE
plot<-TRUE

dir.in <- "D:/Share_Data/Tracking_Data/PTT/"
dir.out <- "D:/Share_Data/Tracking_Data/PTT/"
dir.in.meta <- "D:/Share_Data/GitHub/WERC-SC/trackcode/ptt/"

#### initialize the parameters from parameters table for speed, distance, and angle filtering
#### NOTE: if parameters vary by species be sure to annotate the output tablenames to reflect this!!!
# vmax=max velocity in m/s,  vmaxSA_Tab2_W5ms_x_3SD = values from Spear and Ainley, note: 22.22 = 80kph; 19.44 = 70 kph; 16.66 = 60 kph
# ang=angle or spikes to filter out at successive distlim: ang <- c(15, 25) the angle of the spike that is removed, this is Freitas default
# distlim= distance between successive points to use ang1 vs ang2: ang <- c(2500, 5000) this is Freitas default
parameters <- read.csv ("D:/Share_Data/Tracking_Data/Support_Files/parameters.csv", header=T, sep=",", strip.white=T)
paramwant<-subset(parameters,(spp==species & tag=="ptt"))

vmax <- (paramwant$vmaxSA_Tab2_W5ms_x_3SD)                                     
ang <- c(paramwant$ang1,paramwant$ang2)
distlim <-  c(paramwant$distlim1,paramwant$distlim2)
print (paramwant)

#### get location class error reference (default = Costa)
lcerrref<-"costa"
# lcerrors
lcerrors <- read.csv ('D:/Share_Data/Tracking_Data/Support_Files/lcerrors.csv', header=T, sep=",", strip.white=T)

if(lcerrref=="costa"){
  lcerr<-lcerrors$X68errCosta
}else{
  if(lcerrref=="douglas"){
    lcerr<-lcerrors$X68errDougMaxredun10
  }}

#### read in metadata
meta<-read.table (paste(dir.in.meta,"PTT_metadata_all.csv",sep = ""),header=T, sep=",", strip.white=T,na.strings = "")

#### select metadata want
if (is.na(year)) {
  meta<-meta[meta$species==species & meta$loc_data==1,]
} else{
  meta<-meta[meta$species==species & meta$year==year & meta$loc_data==1,]
}

meta<-transform(meta, 
                datetime_deploy_UTC = as.POSIXct(datetime_deploy_UTC, tz = "GMT", format = "%m/%d/%Y %H:%M"),
                datetime_recover_UTC = as.POSIXct(datetime_recover_UTC, tz = "GMT", format = "%m/%d/%Y %H:%M"),
                datetime_start_track_UTC = as.POSIXct(datetime_start_track_UTC, tz = "GMT", format = "%m/%d/%Y %H:%M"),
                datetime_end_track_UTC = as.POSIXct(datetime_end_track_UTC, tz = "GMT", format = "%m/%d/%Y %H:%M"))

# loop through birds want
# i=145
for (i in 1:length(meta[,1])) {			  
  
  animal.id <- meta$animal_id[i]
  file_name <- meta$file_name[i]
  ptt_deploy_id <- meta$ptt_deploy_id[i]
  print(animal.id)
  
  #### read in track
  track <- read.table(paste(dir.in,species,"/1_SeaTurtle_out/",file_name,".csv",sep = ""),header=T, sep=",",strip.white=T)
  print ('Total rows')
  track$utc<-as.character(track$utc)
  TrackLengthOrig<-(length (track[,1]))
  print (length (track[,1]))
  track<-transform(track, utc= as.POSIXct(utc, tz = "GMT", format = "%m/%d/%Y %H:%M"))
  
  #### cut start/end track based on metadata and insert deployment/recovery locations if specified in metadata
  
  #### cut beginning of track
  ## check if there is a deployment time
  if (!is.na(meta$datetime_deploy_UTC[i])) {
    # cut track to those locations made >= time of deployment 
    track<-track[which(track$utc>=meta$datetime_deploy_UTC[i]),]
  } else if (!is.na(meta$datetime_start_track_UTC[i])) {
    # cut track to those locations made >= time of predefined start time 
    track<-track[which(track$utc>=meta$datetime_start_track_UTC[i]),]
  }
  
  #### cut end of track
  ## check if there is a Recovery time
  if (!is.na(meta$datetime_recover_UTC[i])) {
    # cut track to those locations made <= time of recovery
    track<-track[which(track$utc <= meta$datetime_recover_UTC[i]),]
  } else if (!is.na(meta$datetime_end_track_UTC[i])) {
    # cut track to those locations made <= time of predefined end time 
    track<-track[(track$utc<=meta$datetime_end_track_UTC[i]),]
  }
  
  track$year<-rep(meta$year[i],length(track[,1]))
  track$ptt<-rep(meta$tag_id[i],length(track[,1]))
  
  Tracklength_clipped<-length(track[,1])
  TrackLength_ends_added<-0
  
  #### insert rows in track if metadata indicates insert deployment location, inserted rows labelled with lc<-4 and are not filtered
  if ((!is.na(meta$incl_deploy_loc[i])) & (meta$incl_deploy_loc[i]!=0)) {
    t1=track[1,]
    t1[1,]<-NA
    t1$lc<-4
    t1$program<-track$program[1]
    t1$tag_id<-track$tag_id[1]
    t1$year<-track$year[1]
    t1$ptt<-track$ptt[1]
    t1$uid<-track$uid[1]-.1
    t1$utc<-meta$datetime_deploy_UTC[i]
    t1$lat1<-meta$lat_deploc[i]
    t1$lon1<-meta$lon_deploc[i]
    track<-rbind(t1,track)
    rm(t1)
    TrackLength_ends_added<-TrackLength_ends_added+1
  }
  
  #### insert rows in track if metadata indicates insert recovery location, inserted rows labelled with lc<-4 and are not filtered
  if ((!is.na(meta$incl_recovery_loc[i])) & (meta$incl_recovery_loc[i]!=0)) {
    t2=track[length(track[,1]),]
    t2[1,]<-NA
    t2$lc<-4
    t2$year<-track$year[length(track[,1])]
    t2$ptt<-track$ptt[length(track[,1])]
    t2$uid<-track$uid[length(track[,1])]+.1
    t2$utc<-meta$datetime_end_track_UTC[i]
    t2$lat1<-meta$lat_end[i]
    t2$lon1<-meta$lon_end[i]
    track<-rbind(track,t2)
    rm(t2)
    TrackLength_ends_added<-TrackLength_ends_added+1
  }
  
  # head(track)
  
  # initiate track filtered vector
  track$filtered<-rep(0,length(track[,1]))
  
  #### remove all lat-long values = 0
  latlon0=sum(as.numeric(track$lat1 == 0 | track$lon1 == 0))
  track<-track[(track$lat1 != 0 | track$lon1 != 0),]
  print (c('Total rows after latlon=0 removed',length(track[,1])))
  
  #### edit lc levels
  # argos classes are 
  # G - GPS (<100m, >=1messages)
  # 3 (<250m, >=4 messages)
  # 2 (250-500m, >=4 messages)
  # 1 (500m-1500, >=4 messages)
  # 0 (>1500m, >=4 messages)
  # A (no accuracy estimation, =3 messages)
  # B (no accuracy estimation, =1 or 2 messages)
  # Z (invalid location)
  
  # replace lc with numbers
  # ("Z", "B", "A") with (-9, -2, -1)
  track$lc <- as.character(track$lc) # change to characters
  
  track$lc[track$lc == "A"] <- -1
  track$lc[track$lc == "B"] <- -2
  track$lc[track$lc == "Z"] <- -9
  
  track$lc <- as.numeric(track$lc) # change to numbers
  
  # new order of accuracy
  # 3,2,1,0,-1,-2,-9
  
  #### delete duplicate points retaining based on 1. >lc & 2. >nb_mes, or 3. first duplicate, trash others
  track$utc <- factor(track$utc)
  dups<-  track[track$utc %in% track$utc[duplicated(track$utc)],]
  dups$utc<-factor(dups$utc)
  
  if (length(dups[,1])!=0) {
    # identify duplicate records
    dup.freq <- as.data.frame.table (tapply(dups$utc, dups$utc, length))
    # k<-1
    for (k in 1:length(dup.freq[,1])) {
      dups.test<-dups[dups$utc %in% dup.freq$Var1[k],]
      if ((length(unique(dups.test$lc))!=1) & ("lc" %in% names(dups.test))) {
        # first check and retain location higher lc
        track$filtered[which(track$uid==dups.test$uid[dups.test$lc==min(dups.test$lc)])]="dup" 
        # second if same lc, check and retain location higher num.mes 
      } else if ((length(unique(dups.test$nb_mes))!=1) & ("nb_mes" %in% names(dups.test))) {
        track$filtered[which(track$uid==dups.test$uid[dups.test$nb_mes==min(dups.test$nb_mes)])]="dup" 
        # third if same nb.mess, retain the first duplicate
      } else {
        track$filtered[which(track$uid==dups.test$uid[2:(length(dups.test$uid))])]="dup"
      }
    }
  }
  dupremoved=length(track$filtered[track$filtered=="dup"])
  
  # plot unfiltered data, for QAQC pre SDA
  # for windows machine use window() for mac use quartz()
  # windows() 
  if(plot== TRUE){
    #quartz()
    plot(track$lon1[track$filtered!="dup"],track$lat1[track$filtered!="dup"],col="lightgrey",type="l", xlab="Longitude",ylab="Latitude")
  }	
  
  # remove dups
  track<-track[track$filtered!="dup",]
  
  #### for speed filter only use this - however not used in Atlas, use SDA
  #
  # # filter by speed only [set max speed to equivalent to vmax]
  # mfilter<- vmask (lat, lon, dtime, vmax)
  # mfilter[1:10]
  # table(lc,mfilter)
  #
  # lines(lon[which(mfilter=="not")],lat[which(mfilter=="not")],col="red")
  ####	
  
  #### filter data using sdafilter [set max speed to equivalent to vmax]
  track$cfilter<- sdafilter (track$lat1, track$lon1, track$utc, track$lc, vmax, ang, distlim)
  #	cfilter[1:20]
  
  #### plots retained SDA points on plot in blue	
  if(plot== TRUE){
    lines(track$lon1[which(track$cfilter=="not")],track$lat1[which(track$cfilter=="not")],col="blue")
  }	
  
  #### remove low quality endlocation records accepted by Fritas filter
  track$filtered[(track$cfilter == "end_location" & track$lc <= -2)]="end_location_rem" ## -2 = LC B
  track$ptt_deploy_id <- rep(meta$ptt_deploy_id[i], length(track[,1]))
  
  #### create vector for points kept
  track$keeps <- (as.numeric((track$cfilter=="not") | (track$cfilter=="end_location")) * (1-as.numeric(track$filtered=="end_location_rem")))
  
  retained <- sum(track$keeps)
  filtered<- (length(track[,1]) - sum(track$keeps))
  
  #### screen output		
  print(c("TrackLengthOrig",TrackLengthOrig,"Tracklength_clipped",Tracklength_clipped,"TrackLength_ends_added",TrackLength_ends_added,"latlong=0",latlon0,"dup removed", dupremoved,"filtered",filtered,"retained",retained))
  
  #### output filtered data for each birds
  # create output directory, will not replace if already exists
  dir.create(file.path(paste(dir.out,species,"/",sep=""),"2_SDA_Freitas_out"),showWarnings=TRUE)
  dir.create(file.path(paste(dir.out,species,"/",sep=""),"2_SDA_Freitas_out/indiv_tracks"),showWarnings=TRUE)
  
  # write the track
  write.table (track, paste(dir.out,species,"/2_SDA_Freitas_out/indiv_tracks/",file_name,"_FreitasFilt.csv", sep =""), sep=",", quote=FALSE,col.names=TRUE, row.names=F,na="")
  
  # bind all filtered tracks
  if (i==1) {
    tracks<-track
  } else {
    tracks<-rbind.fill(tracks,track)
  }
  
  rm(track)
} 

tracks$uid<-1:length(tracks[,1])

####output FilterParametersUse
FilterParametersUsed<-as.data.frame(t(c(species,vmax,ang,distlim)))
colnames(FilterParametersUsed)<-c("species","vmax","ang1","ang2","distlim1","distlim2")
write.table(FilterParametersUsed,paste(dir.out,species,"/2_SDA_Freitas_out/","FilterParametersUsed_",species,"_ref.csv",sep = ""),sep=",",quote=FALSE,col.names=TRUE,row.names=FALSE) 	

#### output all tracks concatenated
write.table (tracks, paste(dir.out,species,"/2_SDA_Freitas_out/","All_tracks_and_data_",species,".csv", sep =""), sep=",", quote=FALSE,col.names=TRUE, row.names=F,na="")

library(ggplot2)
ptt_deploy_id<-unique(tracks$ptt_deploy_id)
#i=9
if(plot== TRUE){
  for (i in 1:length(ptt_deploy_id)) {
    track<-tracks[tracks$ptt_deploy_id==ptt_deploy_id[i]&tracks$keeps==1,]
    
    pdf(paste(dir.out,species,"/2_SDA_Freitas_out/indiv_tracks/",ptt_deploy_id[i],"_SDA.pdf",sep = ""),15,15)
    
    plot(track$lon1,track$lat1,col="grey",type="l", xlab="Longitude_SDA",ylab="Latitude_SDA",main = "SDA_points_kept")
    #lines(track$lon1,track$lat1,col="grey",pch = 20,cex =1.5)
    points(track$lon1[1],track$lat1[1],col="blue",pch = 20,cex =2)
    points(track$lon1[length(track$lon1)],track$lat1[length(track$lon1)],col="red",pch = 20,cex =2)
    
    #qplot(lon1, lat1, data=track, colour=as.numeric(as.POSIXct(track$utc,tz="GMT"))) + scale_colour_gradient(low="red", high="black")
    
    dev.off()
  }
}


####
####  Could reenter start end locations here
#### 
##### 
##### Project coordinates into appopriate projection - depends on area that will be projected in final image
##### geodalbers {spsurvey}
##### convert lat lon to meters mapproj
#####   
##### 
##### Once loop done Concatenate all files and output ID labeled master .csv
#####