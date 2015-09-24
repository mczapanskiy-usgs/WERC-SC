

#### used to break trips for GPS (or other) tracks that have multiple trips
#
#  RWH  -April 30, 2014
#       -Dec 19, 2014 update to accomodate new version of metadate and out track annotation with trips
#      
# 
# input .csv files
# 1. with Colony Locations: Species (or other grouping ID),  Island (Location),	Site (Sublocation),	SubCol_Code (Sublocation code),	Year (YYYY),	lon (dd.ddd),	lat (dd.ddd),	colBuffer (m)
# 2. distance of ring around colony that when a location is outside of dilineates that begining of a trip (m)
# 3. Track_File_in contains at min: UTC,  Latitud (dd.ddd),	longitude (dd.ddd),	SPDfilter (where locations kept !="removed"))
# 4. metadata file containing: Deploy_ID (unique tag identifier), UTC (YYYY/MM/DD HH:MM:SS of either deploy "D" or recover "R" as desingated in next field), Tagging_Event ("D" or "R") GPS_TagRecov(1=good file,3=problems with tag but ok), SubCol_Code (matches that in Colony Location), GPS_Track_File (file name of track)
# 
# outputs
# 1. tripInfoSort: Deploy_ID (unique tag identifier), tripST (UTC start trip), tripStComp (-2=never left colony, 0=no, 1=yes, 3=yes time interpolated),
#   tripEND (UTC start trip), tripEndComp (-2=never left colony, 0=no, 1=yes, 3=yes time interpolated), tripNum (number of this trip nested in track), 
#   species, radCol, minDepAdj, minRecAdj, noLocsSpeed, timeStLim (s), timeEndLim (s), minTripDur (s)
# 
# 2. Track_File_out (adds column to Track_File_in that has (0=bird on colony, 1=trip_1, 2=trip_2, 3=trip_3, etc)
# 3. trip summaries (bird_id, )
#                   ist of on colony times interval between arrival and departure on subsequent trip
# 3.
#####


# clear all
rm(list=ls())

ticall<-as.numeric(proc.time()[3])


library(fields) # contains a great circle distance function that uses radius at equator - suitable for HI tracking data
library(ggplot2)
library(trip)
library(plyr)

####  dir.in for .csv files
dir.in <- "D:/Share_Data/Tracking_Data/"

#### dir.out for .csv files
dir.out <- "D:/Share_Data/Tracking_Data/EOBS/"

# open metadata file
metadata<-read.table(paste(dir.in,"EOBS/metadata_EOBS.csv",sep=""),header=T, sep=",", strip.white=T)

#### enter species to process
species<-"LAAL"

#### get metadata for Species omitting any metadata records without deployment or recovery
metaWants<-(subset(metadata, Species==species & Tagging_Event=='D' & loc.data==1))

#### Plot or not?
plot<-"Y"

##########
#### read in parameters
# parameters <- read.csv ("D:/Share_Data/Tracking_Data/GPS/Support_Files/parameters.csv", header=T, sep=",", strip.white=T)
p <- read.csv(paste(dir.in,"Support_Files/parameters.csv",sep=""), header=T, sep=",", strip.white=T)

p <- p[p$spp==species & p$tag=="eobs",]

## enter radius from colony in km you want consider a trip in km
radCol<-p$radCol.km # in km
## enter time limits that are used to characterize trips
minDepAdj<-60*p$minDepAdj.min # p$minDepAdj.min = time in minutes to accept a difference in recorded deployment release time if release time < minDepAdj
minRecAdj<-60*p$minRecAdj.min # p$minRecAdj.min = time in minutes to accept a difference in recorded deployment recovery time if recovery time < minRecAdj
noLocsSpeed<-p$noLocsSpeed # number of locations to use to generate an average speed for back/forward calculating a trip start or end time using the time of first/last location at sea
timeStLim<-60*p$timeStLim.min # p$timeStLim.min = time in minutes allowed to designate a start as complete if the tag begins logging when at sea
  #NOTE THIS NUMBER IS SENSITIVE AT LARGE VALUES IF BIRD IS TRAVELING ABNORMALLY SLOW DURING FIRST LOCATIONS
timeEndLim<-60*p$timeEndLim.min # p$timeEndLim.min = time in minutes allowed to designate a end as complete if the tag ends logging when at sea
  #NOTE THIS NUMBER IS SENSITIVE AT LARGE VALUES IF BIRD IS TRAVELING ABNORMALLY SLOW DURING LAST LOCATIONS
minTripDur<-60*60*p$minTripDur.h # min trip duration in seconds (sec,min,hr)
##########

#### Create specs table
Trip_Characterization_Specs<-data.frame(species,minDepAdj,minRecAdj,noLocsSpeed,timeStLim,timeEndLim,minTripDur)

# ColLocs<-read.table (paste("/Users/henry/Documents/Work/Projects/USGS/Latest/Tracking Data/GPS/All_Tracks/","GPS_Colony_locs.csv",sep = ""),header=T, sep=",", strip.white=T, stringsAsFactors=FALSE)
# 
# # subset for species
# ColLocs<-subset(ColLocs,Species==species)

# open all tracks
tracks_all<-read.table (paste(dir.in,"EOBS/All_tracks/","All_Species_EOBS_allTracks.csv",sep = ""),header=T, sep=",", strip.white=T, stringsAsFactors=FALSE)

# loop through colonies (based on unique SubCol_Code)
# i<-1
# for (i in 1:length(SubCol_Code_ColLocs)) { 

  # tracksWant<-(subset(tracks_all,SubCol_Code==SubCol_Code[i]))
  # metaWant<-(subset(metaWants,SubCol_Code==SubCol_Code_ColLocs[i] & Species==species))
  # ColLocWant<-(subset(ColLocs,SubCol_Code==SubCol_Code_ColLocs[i]))
  
  
#  colBuf<-ColLocWant$colBuffer # note this is no longer used in below code but may be resurected if each colony has a different buffer used to define a trip
  # head(metaWants)
  
  # loop through tracks
  # j<-12145
  for (j in 1:length(metaWants$GPS_ID)) {

    file.idWant<-metaWants$GPS_ID[j]
    Deploy_ID<-metaWants$Deploy_ID[j]
    metaTrack<-metaWants[j,]
    track<-(subset(tracks_all,tag.serial.number == file.idWant & SPDfilter != "removed"))
    print(c("Deploy_iD", Deploy_ID))
    # head(tracks_all)
    # head(track)
    # track[1:100,]    
     
    # label col rad for breaking
    track$radCol4break_km<-rep(radCol,length(track[,1]))
    
    # get what is deemed "start location" or "central place location)
    if (metaTrack$Nest_loc_use==1) {
      start.lon<-metaTrack$Nest_Long_DD
      start.lat<-metaTrack$Nest_Lat_DD
    } else if (metaTrack$Nest_loc_use==0) {
      start.lon<-metaTrack$Col_Long_DD
      start.lat<-metaTrack$Col_Lat_DD
    }

    # add time to track start time to make think bird came back to colony albeit much later
    track.start1<-track[1,]
    track.start1[1,c(2:13)]<-NA
    track.start1[1,c(2,3,4)]<-c(format(strptime(as.character(metaTrack$UTC), "%m/%d/%Y %H:%M"), "%Y-%m-%d %H:%M:%S"),start.lat,start.lon)
    
    # add time to track end time to make think bird came back to colony albeit much later
    track.end.time<-(as.POSIXct(as.character(track[length(track[,1]),2]),tz = "GMT")+(10*60*60*24))
    track.end.time<-as.character(track.end.time)
    
    track.end1<-track[length(track[,1]),]
    track.end1[1,c(2:13)]<-NA
    track.end1[1,c(2,3,4)]<-c(track.end.time,start.lat,start.lon)
    
    track<-rbind(track.start1,track,track.end1)

    rm(track.start1,track.end1)
    
    track$longitude<-as.numeric(track$longitude)
    track$latitude<-as.numeric(track$latitude)
  
    track$longitude360<-track$longitude
    track$longitude360[track$longitude360<0]=((track$longitude360[track$longitude360<0]+180)+180)
    
    # distance to colony in km
    dist2Col<-(t(rdist.earth((matrix(c(start.lon,start.lat), ncol=2)),(matrix(c(track$longitude,track$latitude),ncol=2)),miles=FALSE)))
    track$dist2Nest<-(t(rdist.earth((matrix(c(metaTrack$Col_Long_DD,metaTrack$Col_Lat_DD), ncol=2)),(matrix(c(track$longitude,track$latitude),ncol=2)),miles=FALSE)))
    onCol<-ifelse(dist2Col>=radCol, 1, 0)
    
    #### convert to using logicals not if statements then decide on minimum time or dist from colony to dilineate true trip
    lOnCol<-as.logical(rbind(0,onCol)!=rbind(onCol,0))
     # convert UTC to proper format before reincorporating
    deployUTC<-format(strptime(as.character(metadata$UTC[(which(metadata$Deploy_ID==Deploy_ID & metadata$Tagging_Event=="D"))]), "%m/%d/%Y %H:%M"), "%Y-%m-%d %H:%M:%S")
    recoverUTC<-format(strptime(track.end.time, "%Y-%m-%d %H:%M:%S"), "%Y-%m-%d %H:%M:%S")
    departsArrives<-t(track$UTC[lOnCol[1:nrow(onCol)]])
    
    # create data frame for times and label it
    times<-t(t(c(deployUTC[1],departsArrives,recoverUTC[1])))
    timeLabels<-t(t(rep("departsArrives",length(times))))
    timeLabels[1]<-"deployUTC"
    timeLabels[length(timeLabels)]<-"recoverUTC"
    timesInd<-as.data.frame(cbind(timeLabels,times))
    timesInd[order(as.Date(timesInd$V2, format="%Y-%m-%d %H:%M:%S")),]
    
    ##############################
    ##### Outputs
    # GPS point files with:
    #     Deploy_ID, trip1St (UTC "%Y/%m/%d %H:%M:%S")
    #     trip_no (integer, number of trip)
    #     tripStComp (0=no, 1=yes, -2=never left colony, 3=estimated based on speed)
    #     tripEndComp (0=no, 1=yes, -2=never left colony, 3=estimated based on speed)
    #     status (0=onCol, 1=atSea)
    # summary file with:
    #     Deploy_ID, trip1St (UTC "%Y/%m/%d %H:%M:%S")
    #     tripStart (UTC "%Y/%m/%d %H:%M:%S")
    #     tripEnd (UTC "%Y/%m/%d %H:%M:%S")
    #     trip_no (integer, number of trip)
    #     tripStComp (0=no, 1=yes, -2=never left colony, 3=estimated based on speed)
    #     tripEndComp (0=no, 1=yes, -2=never left colony, 3=estimated based on speed)
    ##############################
    
    # Run through scenarios:
    
    # create logic vector for departsArrives
    kEven<-1:length(departsArrives) %% 2 == 0
    
    # k=1
    # k=2
    # k=3
    for (k in 1:length(departsArrives))  { # loop through number departs arrives    
    ######################### 
    ##### Trip Starts
    ######################### 
    if (sum(onCol)==0) {
      #X fA if bird did not leave colony
      #X fA SET do nothing for trips where bird never leaves colony
      tripSt<-deployUTC
      tripStComp<--2
      startTrue<-1
    } else if (k==1) { #X fB Bird left colony and it is trip 1
      startTrue<-1
      if (onCol[1]==1) { # f1.1 if first loc is at sea (outside radCol)
      if (as.POSIXlt(deployUTC,"GMT")>=(as.POSIXlt(departsArrives[1],"GMT")-minDepAdj)) { # f1.1.1 time between deployment and first loc at sea is less than or equal to minDepAdj of deployment UTC
        # f1.1.1 SET deployment$UTC and colony loc the first position and label trip start as complete
        tripSt<-deployUTC
        tripStComp<-1
        #         track<-rbind(track[1,],track)
        #         track[1,1:3]<-c(deployUTC,ColLocWant$lat,ColLocWant$lon)
        #         track[1,5:6]<-c("trip1St","trip1St")
        # head(track)
      } else if (as.POSIXlt(deployUTC,"GMT")>=(as.POSIXlt(departsArrives[1],"GMT")-timeStLim)) { # f1.1.2 time between deployment and first loc at sea is less than or equal to timeStLim of deployment UTC
        # f1.1.2 SET make colony loc the first position and recalculate departure time using mean speed of first 10 locations and label trip start as complete
        tripSt<-format(strptime(as.character(as.POSIXlt(track$UTC[1], tz="GMT")-(dist2Col[1]*1000/
          # trackDistance units in km
          mean(((trackDistance(cbind(track$longitude[1:(1+noLocsSpeed)],track$latitude[1:(1+noLocsSpeed)]), longlat = TRUE, prev = FALSE)*1000)/
                 (as.numeric(as.POSIXlt(track$UTC[2:(1+noLocsSpeed)],"GMT")-as.POSIXlt(track$UTC[1:(noLocsSpeed)],"GMT"))*60)), na.rm=TRUE))), "%Y-%m-%d %H:%M:%S", tz = ""), "%Y-%m-%d %H:%M:%S")
        
        if (as.POSIXlt(tripSt,"GMT")<=as.POSIXlt(deployUTC,"GMT")) { # check that corrected time is not before deployment time, if so set to release time
          tripSt=deployUTC
        }
        
        tripStComp<-3
        #           track<-rbind(track[1,],track)
        #           track[1,1:3]<-c(tripSt,ColLocWant$lat,ColLocWant$lon)
        #           track[1,5:6]<-c("trip1St","trip1St")   
        # head(track) 
      } else if (as.POSIXlt(deployUTC,"GMT")<(as.POSIXlt(departsArrives[1],"GMT")-timeStLim)) {  # f1.1.3 time between deployment and first loc at sea is after timeStLim of deployment UTC
        # f1.1.3 SET keep first position at sea and label trip start as incomplete
        tripSt<-track$UTC[1]
        tripStComp<-0
        #           track[1,5:6]<-c("trip1StInc","trip1StInc")   
        }
    } else { # f1.2 if first loc is on the colony (inside radCol)
       # f1.2 SET keep first position of the track and make the trip 1 start time at location before first location at sea (outside radCol)
      tripSt<-track$UTC[which(track$UTC==departsArrives[1])-1]
      tripStComp<-1
      #         track[which(track$UTC==departsArrives[1])-1,5:6]<-c("trip1St","trip1St")    
    }
  } else if ((k>2) && (kEven[k]==FALSE)) { # fC Bird left colony on trip greater equal to 2
    startTrue<-1 
      # f1.3 trip greater than or equal to 2 has location at sea
    if (as.POSIXlt(track$UTC[which(track$UTC==departsArrives[k])-1],"GMT")>=(as.POSIXlt(departsArrives[k],"GMT")-minDepAdj)) { # f1.3.1 if time of on col loc prior to first at sea loc of trip k is within minDepAdj of the at sea location
        # f1.3.1 SET make trip(k)St = the previous colony loc the first position and label trip start as complete
        tripSt<-track$UTC[which(track$UTC==departsArrives[k])-1]
        tripStComp<-1
        # track[which(track$UTC==departsArrives[k])-1,2:3]<-c(ColLocWant$lat,ColLocWant$lon)
        #           track[which(track$UTC==departsArrives[k])-1,5:6]<-c(paste("trip",k,"St",sep=""),paste("trip",k,"St",sep=""))
        # head(track)
      } else if (as.POSIXlt(track$UTC[which(track$UTC==departsArrives[k])-1],"GMT")>=(as.POSIXlt(departsArrives[k],"GMT")-timeStLim)) { # f1.3.2 if time of on col loc prior to first at sea loc of trip k is within timeStLim of deployment UTC
        # f1.3.2 SET insert colony loc as the prior position and recalculate departure time using mean speed of first noLocsSpeed locations and label trip start as complete
        tripSt<-format(strptime(as.character(as.POSIXlt(track$UTC[which(track$UTC==departsArrives[k])], tz="GMT")
        -(dist2Col[which(track$UTC==departsArrives[k])]*1000/
        # trackDistance units in km
        mean(((trackDistance(cbind(track$longitude[(which(track$UTC==departsArrives[k])):(which(track$UTC==departsArrives[k])+noLocsSpeed)],
        track$latitude[(which(track$UTC==departsArrives[k])):(which(track$UTC==departsArrives[k])+noLocsSpeed)]),longlat = TRUE, prev = FALSE)*1000)/
        (as.numeric(as.POSIXlt(track$UTC[(which(track$UTC==departsArrives[k])+1):(which(track$UTC==departsArrives[k])+noLocsSpeed)],"GMT")
        -as.POSIXlt(track$UTC[(which(track$UTC==departsArrives[k])):(which(track$UTC==departsArrives[k])+noLocsSpeed-1)],"GMT"))*60)),
        na.rm=TRUE))), "%Y-%m-%d %H:%M:%S", tz = ""), "%Y-%m-%d %H:%M:%S")        
        tripStComp<-3
        # track[which(track$UTC==departsArrives[k])-1,1:3]<-c(trip1St,ColLocWant$lat,ColLocWant$lon)
        #           rowIns<-track[which(track$UTC==departsArrives[k])-1,]
        #           rowIns[1,1:3]<-c(tripSt,ColLocWant$lat,ColLocWant$lon)
        #           rowIns[1,5:6]<-c(paste("trip",k,"St",sep=""),paste("trip",k,"St",sep=""))               
        #           track<-rbind(track,rowIns)
        #           track<-track[order(track$UTC),]
        track[which(track$UTC==departsArrives[k])-1,5:6]<-c(paste("trip",k,"StMOd",sep=""),paste("trip",k,"StMod",sep=""))    
        # head(track) 
      } else if (as.POSIXlt(track$UTC[which(track$UTC==departsArrives[k])-1],"GMT")<(as.POSIXlt(departsArrives[k],"GMT")-timeStLim)) {  # f1.3.3 if first loc is before timeStLim of deployment UTC
        # f1.3.3 SET keep first position at sea and label trip start as incomplete
        tripSt<-track$UTC[which(track$UTC==departsArrives[k])]
        tripStComp<-0
        #           track[which(track$UTC==departsArrives[k])-1,5:6]<-c("trip1StInc","trip1StInc")   
      }
    }
    
      # create tripStarts 
    if (exists("startTrue")) {
      if (exists("tripStarts")) {
        tripStarts<-rbind(tripStarts,data.frame(Deploy_ID,tripSt,tripStComp))
      } else {
        tripStarts<-data.frame(Deploy_ID,tripSt,tripStComp)
      }
    rm(startTrue)
    }
    
    ######################### 
    ####  TRIP ENDS
    #########################
    if (sum(onCol)==0) {
      # fA if bird did not leave colony
      # fA SET do nothing for trips where bird never leaves colony
      endTrue<-1
      tripEnd<-recoverUTC
      tripEndComp<--2
    } else if (length(departsArrives)<=1) { # fB if there is only one recorded transition from colony to at sea, bird is not relocated at colony
      endTrue<-1
      # f2.1 
      if (as.POSIXlt(recoverUTC,"GMT")<=(as.POSIXlt(track$UTC[length(track$UTC)],"GMT")+minRecAdj)) { # f2.1.1 if recover UTC within minRecAdj of UTC of last loc
        # f2.1.1 SET make recovery$UTC and colony loc the last position and label trip end as complete 
        tripEnd<-recoverUTC
        tripEndComp<-1
        #         track<-rbind(track,track[length(track[,1]),])
        #         track[length(track[,1]),1:3]<-c(trip1End,ColLocWant$lat,ColLocWant$lon)
        #         track[length(track[,1]),5:6]<-c("trip1End","trip1End")
        # tail(track)
      } else if (as.POSIXlt(recoverUTC,"GMT")<(as.POSIXlt(track$UTC[length(track$UTC)],"GMT")+timeEndLim)) { # f2.1.2 if recover UTC not within minRecAdj but within timeEndLim of UTC of last loc
        # f2.1.2 SET colony loc the last position and recalculate arrival time using mean speed of last noLocsSpeed locations and label trip end as complete
        tripEnd<-format(strptime(as.character(as.POSIXlt(track$UTC[length(track$UTC)], tz="GMT")+(dist2Col[length(dist2Col)]*1000/
          # trackDistance units in km
          mean(((trackDistance(cbind(track$longitude[(length(track$UTC)-(noLocsSpeed)):(length(track$UTC))],track$latitude[(length(track$UTC)-(noLocsSpeed)):(length(track$UTC))]),
          longlat = TRUE, prev = FALSE)*1000)/
          (as.numeric(as.POSIXlt(track$UTC[(length(track$UTC)-noLocsSpeed+1):length(track$UTC)])-as.POSIXlt(track$UTC[(length(track$UTC)-noLocsSpeed):(length(track$UTC)-1)]))*60)), 
          na.rm=TRUE))),
          "%Y-%m-%d %H:%M:%S", tz = ""), "%Y-%m-%d %H:%M:%S")
        
        if (as.POSIXlt(tripEnd,"GMT")>=as.POSIXlt(recoverUTC,"GMT")) { # check that corrected time is not after recovery time, if so set to release time
          tripEnd=recoverUTC
        }
        
        tripEndComp<-3
        #### To plot a track distance
#         doy=((trackDistance(cbind(track$longitude[(length(track$UTC)-(noLocsSpeed)):(length(track$UTC))],track$latitude[(length(track$UTC)-(noLocsSpeed)):(length(track$UTC))]),
#                             longlat = TRUE, prev = FALSE)*1000)/
#                (as.numeric(as.POSIXlt(track$UTC[(length(track$UTC)-noLocsSpeed+1):length(track$UTC)])-as.POSIXlt(track$UTC[(length(track$UTC)-noLocsSpeed):(length(track$UTC)-1)]))*60))
#         doy1=doy[doy>.01]
#         hist(doy,100)
#         hist(doy1,100)
        ###
        
        #### ***NOTE*** CACULATIONS FOR BIRDS THAT HAVE LAST LOC AT SEA WITHIN timeEndLim  IS YEILDING LONG TIMES TO REACH COLONY - think it is
        # because there are many times when bird moving slowly!!! may not be able to calculate unless bird is near colony and actively traveling!!!
        # may not be a valid technique especially for return of bird or birds that sit on water upon return or departure!
        
        #         track<-rbind(track,track[length(track[,1]),])
        #         track[length(track[,1]),1:3]<-c(trip1End,ColLocWant$lat,ColLocWant$lon)
        #         track[length(track[,1]),5:6]<-c("trip1End","trip1End")   
        # tail(track)
      } else if (as.POSIXlt(recoverUTC,"GMT")>(as.POSIXlt(track$UTC[length(track$UTC)],"GMT")+timeEndLim)) { # f2.1.3 if last loc is after timeEndLim of recover UTC 
        # f2.1.3 SET keep last position at sea and label trip end as incomplete
        tripEnd<-track$UTC[length(track$UTC)]
        tripEndComp<-0
        #         track[length(track[,1]),5:6]<-c("trip1EndInc","trip1EndInc")
        # tail(track)
      }
    } else if (length(departsArrives)>1) { # C if there is one or more trips where bird is relocated on colony
      
      if ((kEven[k]==FALSE) && (length(departsArrives)==k)) { # f2.2 this is greater than trip 1, the last trip, and last loc is at sea
      endTrue<-1
      if (as.POSIXlt(recoverUTC,"GMT")<=(as.POSIXlt(track$UTC[length(track$UTC)],"GMT")+minRecAdj)) { # f2.2.1 if recover UTC within minRecAdj of UTC of last loc
      # f2.2.1 SET make recovery$UTC and colony loc the last position and label trip end as complete
      tripEnd<-recoverUTC
      tripEndComp<-1
      #         track<-rbind(track,track[length(track[,1]),])
      #         track[length(track[,1]),1:3]<-c(trip1End,ColLocWant$lat,ColLocWant$lon)
      #         track[length(track[,1]),5:6]<-c("trip1End","trip1End")
      # tail(track)
      } else if (as.POSIXlt(recoverUTC,"GMT")<(as.POSIXlt(track$UTC[length(track$UTC)],"GMT")+timeEndLim)) { # 2.2.2 if recover UTC not within minRecAdj but within timeEndLim of UTC of last loc
        # f2.2.2 SET colony loc the last position and recalculate arrival time using mean speed of last noLocsSpeed locations and label trip end as complete
        tripEnd<-format(strptime(as.character(as.POSIXlt(track$UTC[length(track$UTC)], tz="GMT")+(dist2Col[length(dist2Col)]*1000/
        # trackDistance units in km
        mean(((trackDistance(cbind(track$longitude[(length(track$UTC)-(noLocsSpeed)):(length(track$UTC))],track$latitude[(length(track$UTC)-(noLocsSpeed)):(length(track$UTC))]),
        longlat = TRUE, prev = FALSE)*1000)/
        (as.numeric(as.POSIXlt(track$UTC[(length(track$UTC)-noLocsSpeed+1):length(track$UTC)])-as.POSIXlt(track$UTC[(length(track$UTC)-noLocsSpeed):(length(track$UTC)-1)]))*60)), 
        na.rm=TRUE))),
        "%Y-%m-%d %H:%M:%S", tz = ""), "%Y-%m-%d %H:%M:%S")
        
        if (as.POSIXlt(tripEnd,"GMT")>=as.POSIXlt(recoverUTC,"GMT")) { # check that corrected time is not after recovery time, if so set to recover capture time
          tripEnd=recoverUTC
        }
        
        tripEndComp<-3
        #         track<-rbind(track,track[length(track[,1]),])
        #         track[length(track[,1]),1:3]<-c(trip1End,ColLocWant$lat,ColLocWant$lon)
        #         track[length(track[,1]),5:6]<-c("trip1End","trip1End")   
        # tail(track)
      } else if (as.POSIXlt(recoverUTC,"GMT")>(as.POSIXlt(track$UTC[length(track$UTC)],"GMT")+timeEndLim)) { # f2.2.3 if last loc is after timeEndLim of recover UTC
        # f2.2.3 SET keep last position at sea and label trip end as incomplete
        tripEnd<-track$UTC[length(track$UTC)]
        tripEndComp<-0
        #         track[length(track[,1]),5:6]<-c("trip1EndInc","trip1EndInc")
        # tail(track)
      }
    } else if (kEven[k]==TRUE) { # 2.3 there are one or more trips and the bird arrives to colony on this trip (may or may not be the last trip)
      endTrue<-1
      if (which(track$UTC==departsArrives[k])==length(track$UTC) && 
        (as.POSIXlt(track$UTC[which(track$UTC==departsArrives[k])],"GMT")<=(as.POSIXlt(track$UTC[which(track$UTC==departsArrives[k])], "GMT")+minRecAdj)) || 
        (as.POSIXlt(track$UTC[which(track$UTC==departsArrives[k])],"GMT")<=(as.POSIXlt(track$UTC[which(track$UTC==departsArrives[k])+1], "GMT")+minRecAdj))) {
      # f2.3.1 if on colony within minRecAdj of UTC of last loc at sea
      # f2.3.1 SET make recovery$UTC and colony loc the last position and label trip end as complete 
      (departsArrives[k]==track$UTC[length(track$UTC)])
      tripEnd<-departsArrives[k]
      tripEndComp<-1
      #         track<-rbind(track,track[length(track[,1]),])
      #         track[length(track[,1]),1:3]<-c(trip1End,ColLocWant$lat,ColLocWant$lon)
      #         track[length(track[,1]),5:6]<-c("trip1End","trip1End")
      # tail(track)
      } else if (as.POSIXlt(track$UTC[which(track$UTC==departsArrives[k])],"GMT")<=(as.POSIXlt(track$UTC[which(track$UTC==departsArrives[k])-1],"GMT")+timeEndLim)) { # f2.3.2 if on colony not within minRecAdj but within timeEndLim of UTC of last loc
        # f2.3.2 SET colony loc the last position and recalculate arrival time using mean speed of last 10 locations and label trip end as complete
        tripEnd<-format(strptime(as.character(as.POSIXlt(track$UTC[which(track$UTC==departsArrives[k])-1], tz="GMT")+(dist2Col[which(track$UTC==departsArrives[k])-1]*1000/
        # trackDistance units in km        
        mean(((trackDistance(cbind(track$longitude[(which(track$UTC==departsArrives[k])-noLocsSpeed-1):(which(track$UTC==departsArrives[k])-1)],
        track$latitude[(which(track$UTC==departsArrives[k])-noLocsSpeed-1):(which(track$UTC==departsArrives[k])-1)]), 
        longlat = TRUE, prev = FALSE)*1000)/
        (as.numeric(as.POSIXlt(track$UTC[(which(track$UTC==departsArrives[k])-noLocsSpeed):(which(track$UTC==departsArrives[k])-1)])-as.POSIXlt(track$UTC[(which(track$UTC==departsArrives[k])-noLocsSpeed-1):(which(track$UTC==departsArrives[k])-2)]))*60)),
        na.rm=TRUE))), "%Y-%m-%d %H:%M:%S", tz = ""), "%Y-%m-%d %H:%M:%S")
        
        if (as.POSIXlt(tripEnd,"GMT")>=as.POSIXlt(recoverUTC,"GMT")) { # check that corrected time is not after recovery time, if so set to recover capture time
          tripEnd=recoverUTC
        }
        
        tripEndComp<-3        
        #         track<-rbind(track,track[length(track[,1]),])
        #         track[length(track[,1]),1:3]<-c(trip1End,ColLocWant$lat,ColLocWant$lon)
        #         track[length(track[,1]),5:6]<-c("trip1End","trip1End")   
        # tail(track)
      } else if (as.POSIXlt(track$UTC[which(track$UTC==departsArrives[k])],"GMT")>(as.POSIXlt(track$UTC[which(track$UTC==departsArrives[k])-1],"GMT")+timeEndLim)) { # f2.3.3 if last loc is after timeEndLim of recover UTC 
        # SET f2.3.3 keep last position at sea and label trip end as incomplete
        tripEnd<-track$UTC[which(track$UTC==departsArrives[k])-1]
        tripEndComp<-0
        #         track[length(track[,1]),5:6]<-c("trip1EndInc","trip1EndInc")
        # tail(track)
        }
      }
    }
        
    # create trip info  
    if (exists("endTrue")) {
      if (exists("tripEnds")) {
      tripEnds<-rbind(tripEnds,data.frame(Deploy_ID,tripEnd,tripEndComp))
    } else {
      tripEnds<-data.frame(Deploy_ID,tripEnd,tripEndComp)
    }
    rm(endTrue)
    }
  
    } # end trip loop
    
  # create trip info  
 
    if (exists("tripInfo")) {
    tripInfo<-rbind(tripInfo,cbind(tripStarts,tripEnds[,2:3]))
  } else {
    tripInfo<-cbind(tripStarts,tripEnds[,2:3])
  }
    rm(tripStarts,tripEnds)   

if (j==1) {
  tracks_all_broken<-track
  } else {
  tracks_all_broken<-rbind(tracks_all_broken,track)
  }

rm(track)

  } # end track loop

# } # end colony loop

rm(j,k)

tripInfoSort<-tripInfo[order(tripInfo$Deploy_ID),]
tripInfoSort$trip_no<-sequence(rle(tripInfoSort$Deploy_ID)$lengths)

## check that trips do not overlap, if they do ("should" be associated with an interpolated departure or arrival time)
# if this occurs the second departure is set to the same time as the first arrival

Deploy_IDs<-unique(tripInfoSort$Deploy_ID)

# i=2
for (i in 1:length(Deploy_IDs)) {
    tripInfoSort_wants=tripInfoSort[which(tripInfoSort$Deploy_ID==Deploy_IDs[i]),]
    tripInfoSort_wants$tripSt <- as.POSIXlt(as.character(tripInfoSort_wants$tripSt), tz = 'GMT')
    tripInfoSort_wants$tripEnd <- as.POSIXlt(as.character(tripInfoSort_wants$tripEnd), tz = 'GMT')
  # j=3
  for (j in 1:length(tripInfoSort_wants[,1])) {    
   if (j>1) {
      if ((tripInfoSort_wants$tripSt[j])<(tripInfoSort_wants$tripEnd[(j-1)])) { # the trip overlaps the end of the previous trip
          if (tripInfoSort_wants$tripEndComp[j-1]==3) { # 1. prev trip.no end IS interpolated
            if (tripInfoSort_wants$tripStComp[j]!=3) { # A) trip.no start NOT interpolated 
              tripInfoSort_wants$tripEnd[j-1]=tripInfoSort_wants$tripSt[j] # trip.no-1 end = trip.no start
            } else if (tripInfoSort_wants$tripStComp[j]==3)  { # B) trip.no start IS interpolated
              tripInfoSort_wants$tripSt[j]=tripInfoSort_wants$tripEnd[j-1] # trip.no start = trip.no-1 end
            }
          } else { # 2. prev trip.no NOT interpolated
            if (tripInfoSort_wants$tripStComp[j]==3) { # A) trip.no start IS interpolated 
            tripInfoSort_wants$tripSt[j]<-tripInfoSort_wants$tripEnd[j-1] # trip.no start = trip.no-1 end  
           }          
          }
        } 
    }
  if (i==1 & j==1) { # if this is the first trip of all trips
     tripInfoSort_checked = tripInfoSort_wants[j,]  # initialize tripInfoSort_check
   } else {
     tripInfoSort_checked = rbind(tripInfoSort_checked,tripInfoSort_wants[j,])
   }
  } # end length(length(tripInfoSort_wants) 
  } # end length(Deploy_ID) 
  
############

# create output directory, will not replace if already exists
dir.create(paste(dir.out,species,"/3_Trips/",sep=""),showWarnings=TRUE)

# write trip info out
tripInfoSort_checked$duration.hrs<-(tripInfoSort_checked$tripEnd-tripInfoSort_checked$tripSt)/60
write.table(tripInfoSort_checked, paste(dir.out,species,"/3_trips/",species,"_",radCol,"_tripInfo.csv",sep = ""),sep=",",quote=FALSE,col.names=TRUE,row.names=FALSE)

# Plot trips for QAQC

    ##############################
    #     Deploy_ID, trip1St (UTC "%Y/%m/%d %H:%M:%S"
    #     tripStComp (0=no, 1=yes, -1=never left colony, 3=estimated based on speed), 
    #     tripEnd (UTC "%Y/%m/%d %H:%M:%S")
    #     tripEndComp (0=no, 1=yes, -1=never left colony, 3=estimated based on speed)
    #     status (0=onCol, 1=atSea)
    #     tripComp (0=no, 1=yes, -1=never left colony)
    ##############################

#### plot dist2Col vs UTC
    Deploy_IDs<-sort(unique(tripInfoSort_checked$Deploy_ID))

if (plot=="Y") {
    # l<-1
    for (l in 1:length(Deploy_IDs)) { 
      # windows()
      Deploy_ID_Want<-Deploy_IDs[l]
      metaWant<-metaWants[metaWants$Deploy_ID==Deploy_ID_Want,]
      file.idWant<-metaWant$GPS_ID
      track<-(subset(tracks_all,tag.serial.number == file.idWant & SPDfilter != "removed"))
      tripInfoWant<-subset(tripInfoSort_checked,Deploy_ID == Deploy_ID_Want)
      ColWant<-metaWant$SubCol_Code
      # get what is deemed "start location" or "central place location)
      if (metaWant$Nest_loc_use==1) {
        start.lon<-metaWant$Nest_Long_DD
        start.lat<-metaWant$Nest_Lat_DD
      } else if (metaWant$Nest_loc_use==0) {
        start.lon<-metaWant$Col_Long_DD
        start.lat<-metaWant$Col_Lat_DD
      }
      track$dist2Col<-t(rdist.earth((matrix(c(start.lon,start.lat), ncol=2)),(matrix(c(track$longitude,track$latitude),ncol=2)),miles=FALSE))
      
      # n<-paste("D:/Share_Data/Tracking_Data/GPS/3_Trips/",Deploy_ID_Want,"_",species,"_trips.pdf",sep = "")
      p <- ggplot(track, aes(x=as.POSIXct(as.character(UTC), tz = "GMT"), y=dist2Col))
      p1 <- p + geom_line(colour='seashell3',size=1) + ggtitle(Deploy_ID_Want)
      p2 <- p1 + geom_point(colour='sienna',size=.5) + ggtitle(Deploy_ID_Want)
      p3 <- p2 + geom_point(data=tripInfoWant,aes((tripEnd),tripEndComp*6), colour= "red", shape="e", size = 4)
      p4 <- p3 +  geom_point(data=tripInfoWant,aes(x=(tripSt),y=tripStComp*4),  colour="blue", shape="s", size = 4) + labs(title=paste(species,file.idWant,"Deploy_ID",Deploy_ID_Want,sep=" "))
      pdf(paste(dir.out,species,"/3_Trips/",Deploy_ID_Want,"_",file.idWant,"_",species,"_",radCol,"_trips.pdf",sep = ""), onefile = TRUE)
      print(p4)
      dev.off()      
    }
  
  for (l in 1:length(Deploy_IDs)) { 
    # windows()
    Deploy_ID_Want<-Deploy_IDs[l]
    metaWant<-metaWants[metaWants$Deploy_ID==Deploy_ID_Want,]
    file.idWant<-metaWant$GPS_ID
    track<-(subset(tracks_all,tag.serial.number == file.idWant & SPDfilter != "removed"))
    tripInfoWant<-subset(tripInfoSort_checked,Deploy_ID == Deploy_ID_Want)
    ColWant<-metaWant$SubCol_Code
    # get what is deemed "start location" or "central place location)
    if (metaWant$Nest_loc_use==1) {
      start.lon<-metaWant$Nest_Long_DD
      start.lat<-metaWant$Nest_Lat_DD
    } else if (metaWant$Nest_loc_use==0) {
      start.lon<-metaWant$Col_Long_DD
      start.lat<-metaWant$Col_Lat_DD
    }
    track$dist2Col<-t(rdist.earth((matrix(c(start.lon,start.lat), ncol=2)),(matrix(c(track$longitude,track$latitude),ncol=2)),miles=FALSE))
    track.filt<-track[track$temperature<40 & track$temperature>-10,]
    
    # n<-paste("D:/Share_Data/Tracking_Data/GPS/3_Trips/",Deploy_ID_Want,"_",species,"_trips.pdf",sep = "")
    p <- ggplot(track, aes(x=as.POSIXct(as.character(UTC), tz = "GMT"), y=dist2Col))
    p1 <- p + geom_point(data=track.filt,aes(x=as.POSIXct(as.character(UTC), tz = "GMT"),y=temperature*10),  colour='darkslategrey', size=.5)
    p2 <- p1 + stat_smooth(data=track.filt,aes(x=as.POSIXct(as.character(UTC), tz = "GMT"),y=temperature*10), fill='lightblue', colour='darkturquoise', size=.5)
    p3 <- p2 + geom_line(colour='seashell3',size=1) + ggtitle(Deploy_ID_Want)
    p4 <- p3 + geom_point(colour='sienna',size=.5) + ggtitle(Deploy_ID_Want)
    p5 <- p4 + geom_point(data=tripInfoWant,aes((tripEnd),tripEndComp*6), colour= "red", shape="e", size = 4)
    p6 <- p5 +  geom_point(data=tripInfoWant,aes(x=(tripSt),y=tripStComp*4),  colour="blue", shape="s", size = 4) + labs(title=paste(species,file.idWant,"Deploy_ID",Deploy_ID_Want,sep=" "))
    pdf(paste(dir.out,species,"/3_Trips/",Deploy_ID_Want,"_",file.idWant,"_",species,"_",radCol,"_trips_temp.pdf",sep = ""), onefile = TRUE)
    print(p6)
    dev.off()      
  }
  # l=1
  for (l in 1:length(Deploy_IDs)) { 
    # windows()
    Deploy_ID_Want<-Deploy_IDs[l]
    metaWant<-metaWants[metaWants$Deploy_ID==Deploy_ID_Want,]
    file.idWant<-metaWant$GPS_ID
    track<-(subset(tracks_all,tag.serial.number == file.idWant & SPDfilter != "removed"))
    tripInfoWant<-subset(tripInfoSort_checked,Deploy_ID == Deploy_ID_Want)
    ColWant<-metaWant$SubCol_Code
    # get what is deemed "start location" or "central place location)
    if (metaWant$Nest_loc_use==1) {
      start.lon<-metaWant$Nest_Long_DD
      start.lat<-metaWant$Nest_Lat_DD
    } else if (metaWant$Nest_loc_use==0) {
      start.lon<-metaWant$Col_Long_DD
      start.lat<-metaWant$Col_Lat_DD
    }
    track$dist2Col<-t(rdist.earth((matrix(c(start.lon,start.lat), ncol=2)),(matrix(c(track$longitude,track$latitude),ncol=2)),miles=FALSE))
    
    # n<-paste("D:/Share_Data/Tracking_Data/GPS/3_Trips/",Deploy_ID_Want,"_",species,"_trips.pdf",sep = "")
    track.filt<-track[track$height.above.ellipsoid<200 & track$height.above.ellipsoid>-20,]

    p <- ggplot(track, aes(x=as.POSIXct(as.character(UTC), tz = "GMT"), ,y=dist2Col))
    p1 <- p + geom_point(data=track.filt,aes(x=as.POSIXct(as.character(UTC), tz = "GMT"),y=height.above.ellipsoid*10),  colour='darkslategrey', size=.5)
    p2 <- p1 + stat_smooth(data=track.filt,aes(x=as.POSIXct(as.character(UTC), tz = "GMT"),y=height.above.ellipsoid*10), fill='lightblue', colour='darkturquoise', size=.5)
    p3 <- p2 +geom_line(colour='seashell3',size=1)+ ggtitle(Deploy_ID_Want)
    p4 <- p3 + geom_point(colour='sienna',size=.5)
    p5 <- p4 + geom_point(data=tripInfoWant,aes((tripEnd),tripEndComp*6), colour= "red", shape="e", size = 4)
    p6 <- p5 +  geom_point(data=tripInfoWant,aes(x=(tripSt),y=tripStComp*4),  colour="blue", shape="s", size = 4) + labs(title=paste(species,file.idWant,"Deploy_ID",Deploy_ID_Want,sep=" "))
    pdf(paste(dir.out,species,"/3_Trips/",Deploy_ID_Want,"_",file.idWant,"_",species,"_",radCol,"_trips_height.pdf",sep = ""), onefile = TRUE)
    print(p6)
    dev.off()      
  }
    
}