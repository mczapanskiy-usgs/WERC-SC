

#### used to break trips for GPS (or other) tracks that have multiple trips
#
#  RWH April 30, 2014
#      
# 
# input .csv files
# 1. with Colony Locations: Species (or other grouping ID),  Island (Location),	Site (Sublocation),	SubCol_Code (Sublocation code),	Year (YYYY),	lon (dd.ddd),	lat (dd.ddd),	colBuffer (m)
# 2. enter variables used to characterize trips
#   species, radCol, minDepAdj, minRecAdj, noLocsSpeed, timeStLim, timeEndLim, minTripDur, tLocal
# 3. trackfile_in contains at min: UTC,  Latitud (dd.ddd),	Longitude (dd.ddd),	SPDfilter (where locations kept !="removed"))
# 4. metadata file containing: Deploy_ID (unique tag identifier), UTC (YYYY/MM/DD HH:MM:SS of either deploy "D" or recover "R" as desingated in next field), Deploy_recover_DR ("D" or "R") TagRecov(1=good file,3=problems with tag but ok), SubCol_Code (matches that in Colony Location), TrackFile (file name of track)
# 
# outputs
# 1. tripInfoSort: Deploy_ID (unique tag identifier), tripNum (number of this trip nested in track), tripSt_GMT (UTC start trip), tripStComp (-2=never left colony, 0=no, 1=yes, 3=yes time interpolated),
#   tripEnd_GMT (UTC start trip), tripEndComp (-2=never left colony, 0=no, 1=yes, 3=yes time interpolated), trip_no, tStart_loc, tEnd_loc, tLocal,  tripSt_loc,
#   tripEnd_loc,  TripDurHrs,	TripDurDays,	minTripDur,	PostTripIntHr,	radCol, minDepAdj, minRecAdj, noLocsSpeed, timeStLim, timeEndLim, minTripDur, tLocal,
#   Island,	Site,	SubCol,	SubCol_Code,	Species,	Year,	Phenology,	NestNo, GPS_I,D  GPS_on_date,  DeplSess,	GPS_Hz,	GPS_notes,	Deploy_recover_DR,	
#   Contents,	Capture_date,	Capture_time,	Release_time,	UTC,	BandNo,	R.L	offset,	TagRecov
#   Notes,  Capture_loc,	Blood_FTA,	Blood_vial,	Feathers,	TrackFile,	revised_sample_no,	TDR_ID,	TDR_File,	Sex,	Diet,	Mass,	Culmen,	Wing
#
#
# 2. trackfile_out (adds column to trackfile_in that has (0=bird on colony, 1=trip_1, 2=trip_2, 3=trip_3, etc)
# 3. trip summaries (bird_id, )
#                   hist of post trip on colony duration (interval between arrival and departure on subsequent trip)
#
#####


# clear all
rm(list=ls())

ticall<-as.numeric(proc.time()[3])


library(fields) # contains a great circle distance function that uses radius at equator - suitable for HI tracking data
library(ggplot2)
library(trip)
library(plyr)

##########
#### set
## enter species to process
species<-"WTSH"
## enter radius from colony in m you want consider a trip in m
radCol<-500
## enter time limits that are used to characterize trips
minDepAdj<-60*10 # time in seconds (sec,min) to accept a difference in recorded deployment release time if release time < minDepAdj
minRecAdj<-60*10 # time in seconds (sec,min) to accept a difference in recorded deployment recovery time if recovery time < minRecAdj
noLocsSpeed<-10 # number of locations to use to generate an average speed for back/forward calculating a trip start or end time using the time of first/last location at sea
timeStLim<-60*60*2 # time in seconds (sec,min,hr) allowed to designate a start as complete if the tag begins logging when at sea
  #NOTE THIS NUMBER IS SENSITIVE AT LARGE VALUES IF BIRD IS TRAVELING ABNORMALLY SLOW DURING FIRST LOCATIONS
timeEndLim<-60*60*6 # time in seconds (sec,min,hr) allowed to designate a end as complete if the tag ends logging when at sea
  #NOTE THIS NUMBER IS SENSITIVE AT LARGE VALUES IF BIRD IS TRAVELING ABNORMALLY SLOW DURING LAST LOCATIONS
minTripDur<-7*60 # min trip duration in hours (sec,min,hr)
tLocal<-"HST" # local time zone (look up online for acceptable R timezone character strings)
##########

#### Create specs table
Trip_Characterization_Specs<-data.frame(species,minDepAdj,minRecAdj,noLocsSpeed,timeStLim,timeEndLim,minTripDur)

# get colony locations file
ColLocs <- read.table (paste("D:/Share_Data/Tracking_Data/GPS/All_Tracks/","GPS_Colony_locs.csv",sep = ""),header=T, sep=",", strip.white=T, stringsAsFactors=FALSE)

# subset for species
ColLocs<-subset(ColLocs,Species==species)

# open metadata file
metadata<-read.table (paste("D:/Share_Data/Tracking_Data/GPS/metadata_all_GPS_2.0.csv",sep = ""),header=T, sep=",", strip.white=T, stringsAsFactors=FALSE)

# subset those track that have been processed up to this point and have good data
metaWants<-subset(metadata,TagRecov==1 | TagRecov==3)

# open all tracks
tracks_all<-read.table (paste("D:/Share_Data/Tracking_Data/GPS/All_tracks/",species,"_GPS_allTracks.csv",sep = ""),header=T, sep=",", strip.white=T, stringsAsFactors=FALSE)

# get subcolony codes
SubCol_Code_ColLocs<-unique(ColLocs$SubCol_Code)

# loop through colonies (based on unique SubCol_Code)
# i<-8
for (i in 1:length(SubCol_Code_ColLocs)) { 

  # tracksWant<-(subset(tracks_all,SubCol_Code==SubCol_Code[i]))
  metaWant<-(subset(metaWants,SubCol_Code==SubCol_Code_ColLocs[i]))
  ColLocWant<-(subset(ColLocs,SubCol_Code==SubCol_Code_ColLocs[i]))
  colBuf<-ColLocWant$colBuffer
  # head(metaWants)
  
  # loop through tracks
  # j<-12
  for (j in 1:length(metaWant$TrackFile)) {
    
    file.idWant<-metaWant$TrackFile[j]
    Deploy_ID<-metaWant$Deploy_ID[j]
    track<-(subset(tracks_all,file.id == file.idWant & SPDfilter != "removed"))
    print(c("Deploy_iD", Deploy_ID))
    # head(track)
    # track[1:100,]    
    
    # distance to colony in km
    dist2Col<-(t(rdist.earth((matrix(c(ColLocWant$lon,ColLocWant$lat), ncol=2)),(matrix(c(track$Longitude,track$Latitude),ncol=2)),miles=FALSE)))
    onCol<-ifelse(dist2Col>=colBuf, 1, 0)
    
    #### convert to using logicals not if statements then decide on minimum time or dist from colony to dilineate true trip
    lOnCol<-as.logical(rbind(0,onCol)!=rbind(onCol,0))
     # convert UTC to proper format before reincorporating
    deployUTC<-format(strptime(as.character(metadata$UTC[(which(metadata$Deploy_ID==Deploy_ID & metadata$Deploy_recover_DR=="D"))]), "%m/%d/%Y %H:%M"), "%Y/%m/%d %H:%M:%S")
    recoverUTC<-format(strptime(as.character(metadata$UTC[(which(metadata$Deploy_ID==Deploy_ID & metadata$Deploy_recover_DR=="R"))]), "%m/%d/%Y %H:%M"), "%Y/%m/%d %H:%M:%S")
    departsArrives<-t(track$UTC[lOnCol[1:nrow(onCol)]])
    
    # create data frame for times and label it
    times<-t(t(c(deployUTC[1],departsArrives,recoverUTC[1])))
    timeLabels<-t(t(rep("departsArrives",length(times))))
    timeLabels[1]<-"deployUTC"
    timeLabels[length(timeLabels)]<-"recoverUTC"
    timesInd<-as.data.frame(cbind(timeLabels,times))
    timesInd[order(as.Date(timesInd$V2, format="%Y/%m/%d %H:%M:%S")),]
    
    ##############################
    ##### Goal have data.frame with columns:
    #     Deploy_ID, trip1St (UTC "%Y/%m/%d %H:%M:%S"
    #     trip1StComp (0=no, 1=yes, 2=never left colony, 3=estimated based on speed), 
    #     trip1End (UTC "%Y/%m/%d %H:%M:%S")
    #     trip1EndComp (0=no, 1=yes, 2=never left colony, 3=estimated based on speed)
    #     status (0=onCol, 1=atSea)
    #     tripComp (0=no, 1=yes, 2=never left colony)
    ##############################
    
    # Run through scenarios:
    
    # create logic vector for departsArrives
    kEven<-1:length(departsArrives) %% 2 == 0
    
    # k=1
    # k=2
    # k=4
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
        tripSt<-format(strptime(as.character(as.POSIXlt(as.character(track$UTC[1]), "%Y/%m/%d %H:%M:%S", tz="GMT")-(dist2Col[1]*1000/
          # trackDistance units in km
          mean(((trackDistance(cbind(track$Longitude[1:(1+noLocsSpeed)],track$Latitude[1:(1+noLocsSpeed)]), longlat = TRUE, prev = FALSE)*1000)/
                 (as.numeric(as.POSIXlt(track$UTC[2:(1+noLocsSpeed)],"GMT")-as.POSIXlt(track$UTC[1:(noLocsSpeed)],"GMT"))*60)), na.rm=TRUE))), "%Y-%m-%d %H:%M:%S", tz = ""), "%Y/%m/%d %H:%M:%S")
        
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
        # f1.3.2 SET insert colony loc as the prior position and recalculate departure time using mean speed of first 10 locations and label trip start as complete
        tripSt<-format(strptime(as.character(as.POSIXlt(as.character(track$UTC[which(track$UTC==departsArrives[k])]), "%Y/%m/%d %H:%M:%S", tz="GMT")
        -(dist2Col[which(track$UTC==departsArrives[k])]*1000/
        # trackDistance units in km
        mean(((trackDistance(cbind(track$Longitude[(which(track$UTC==departsArrives[k])):(which(track$UTC==departsArrives[k])+noLocsSpeed)],
        track$Latitude[(which(track$UTC==departsArrives[k])):(which(track$UTC==departsArrives[k])+noLocsSpeed)]),longlat = TRUE, prev = FALSE)*1000)/
        (as.numeric(as.POSIXlt(track$UTC[(which(track$UTC==departsArrives[k])+1):(which(track$UTC==departsArrives[k])+noLocsSpeed)],"GMT")
        -as.POSIXlt(track$UTC[(which(track$UTC==departsArrives[k])):(which(track$UTC==departsArrives[k])+noLocsSpeed-1)],"GMT"))*60)),
        na.rm=TRUE))), "%Y-%m-%d %H:%M:%S", tz = ""), "%Y/%m/%d %H:%M:%S")        
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
        tripEnd<-format(strptime(as.character(as.POSIXlt(as.character(track$UTC[length(track$UTC)]), "%Y/%m/%d %H:%M:%S", tz="GMT")+(dist2Col[length(dist2Col)]*1000/
          # trackDistance units in km
          mean(((trackDistance(cbind(track$Longitude[(length(track$UTC)-(noLocsSpeed)):(length(track$UTC))],track$Latitude[(length(track$UTC)-(noLocsSpeed)):(length(track$UTC))]),
          longlat = TRUE, prev = FALSE)*1000)/
          (as.numeric(as.POSIXlt(track$UTC[(length(track$UTC)-noLocsSpeed+1):length(track$UTC)])-as.POSIXlt(track$UTC[(length(track$UTC)-noLocsSpeed):(length(track$UTC)-1)]))*60)), 
          na.rm=TRUE))),
          "%Y-%m-%d %H:%M:%S", tz = ""), "%Y/%m/%d %H:%M:%S")
        
        if (as.POSIXlt(tripEnd,"GMT")>=as.POSIXlt(recoverUTC,"GMT")) { # check that corrected time is not after recovery time, if so set to release time
          tripEnd=recoverUTC
        }
        
        tripEndComp<-3
        #### To plot a track distance
#         doy=((trackDistance(cbind(track$Longitude[(length(track$UTC)-(noLocsSpeed)):(length(track$UTC))],track$Latitude[(length(track$UTC)-(noLocsSpeed)):(length(track$UTC))]),
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
        tripEnd<-format(strptime(as.character(as.POSIXlt(as.character(track$UTC[length(track$UTC)]), "%Y/%m/%d %H:%M:%S", tz="GMT")+(dist2Col[length(dist2Col)]*1000/
        # trackDistance units in km
        mean(((trackDistance(cbind(track$Longitude[(length(track$UTC)-(noLocsSpeed)):(length(track$UTC))],track$Latitude[(length(track$UTC)-(noLocsSpeed)):(length(track$UTC))]),
        longlat = TRUE, prev = FALSE)*1000)/
        (as.numeric(as.POSIXlt(track$UTC[(length(track$UTC)-noLocsSpeed+1):length(track$UTC)])-as.POSIXlt(track$UTC[(length(track$UTC)-noLocsSpeed):(length(track$UTC)-1)]))*60)), 
        na.rm=TRUE))),
        "%Y-%m-%d %H:%M:%S", tz = ""), "%Y/%m/%d %H:%M:%S")
        
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
        tripEnd<-format(strptime(as.character(as.POSIXlt(as.character(track$UTC[which(track$UTC==departsArrives[k])-1]), "%Y/%m/%d %H:%M:%S", tz="GMT")+(dist2Col[which(track$UTC==departsArrives[k])-1]*1000/
        # trackDistance units in km        
        mean(((trackDistance(cbind(track$Longitude[(which(track$UTC==departsArrives[k])-noLocsSpeed-1):(which(track$UTC==departsArrives[k])-1)],
        track$Latitude[(which(track$UTC==departsArrives[k])-noLocsSpeed-1):(which(track$UTC==departsArrives[k])-1)]), 
        longlat = TRUE, prev = FALSE)*1000)/
        (as.numeric(as.POSIXlt(track$UTC[(which(track$UTC==departsArrives[k])-noLocsSpeed):(which(track$UTC==departsArrives[k])-1)])-as.POSIXlt(track$UTC[(which(track$UTC==departsArrives[k])-noLocsSpeed-1):(which(track$UTC==departsArrives[k])-2)]))*60)),
        na.rm=TRUE))),
        "%Y-%m-%d %H:%M:%S", tz = ""), "%Y/%m/%d %H:%M:%S")   
       
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
    
  } # end track loop
} # end colony loop


tripInfoSort<-tripInfo[order(tripInfo$Deploy_ID),]
trip_no<-sequence(rle(tripInfoSort$Deploy_ID)$lengths) ## GREAT FUNCTION for annotating sequence!!! ##
tripInfoSort<-cbind(tripInfoSort,trip_no)

names(tripInfoSort)[names(tripInfoSort)=="tripSt"] <- "tripSt_GMT"
names(tripInfoSort)[names(tripInfoSort)=="tripEnd"] <- "tripEnd_GMT"
# head(tripInfoSort)

tripInfoSort$tripSt_loc<-as.POSIXlt(format(as.POSIXct(tripInfoSort$tripSt_GMT,'GMT'), tz=tLocal,usetz=TRUE))
tripInfoSort$tripEnd_loc<-as.POSIXlt(format(as.POSIXct(tripInfoSort$tripEnd_GMT,'GMT'), tz=tLocal,usetz=TRUE))
tripInfoSort$tLocal<-rep(tLocal,length(tripInfoSort[,1]))

# determine trip metrics
tripInfoSort$TripDurHrs<-as.numeric(as.POSIXlt(tripInfoSort$tripEnd_GMT,"GMT")-as.POSIXlt(tripInfoSort$tripSt_GMT,"GMT"))/60
tripInfoSort$TripDurDays<-(as.numeric(as.POSIXlt(tripInfoSort$tripEnd_GMT,"GMT")-as.POSIXlt(tripInfoSort$tripSt_GMT,"GMT"))/60)/24

## Flag Trips with duration > minTripDur
tripInfoSort$minTripDur<-(as.numeric(as.POSIXlt(tripInfoSort$tripEnd_GMT,"GMT")-as.POSIXlt(tripInfoSort$tripSt_GMT,"GMT"))>t(t(rep(minTripDur/60,length(tripInfo[,1])))))
# head(tripInfo)

# colony layover duration
PostTripIntHr=NA*(1:length(tripInfoSort[,1]))
for (m in 1:(length(tripInfoSort[,1])-1)) { 
  if ((tripInfoSort$Deploy_ID[m]==tripInfoSort$Deploy_ID[m+1]) && (tripInfoSort$tripEndComp[m]==1) && (tripInfoSort$tripStComp[m+1]==1)) {
    PostTripIntHr[m]<-as.numeric(as.POSIXlt(tripInfoSort$tripSt_GMT[m+1],"GMT")-as.POSIXlt(tripInfoSort$tripEnd_GMT[m],"GMT"))
  }
}
tripInfoSort$PostTripIntHr<-PostTripIntHr

# add in parameters used to calculate trips
tripInfoSort$radCol<-radCol
tripInfoSort$minDepAdj<-minDepAdj 
tripInfoSort$minRecAdj<-minRecAdj 
tripInfoSort$noLocsSpeed<-noLocsSpeed 
tripInfoSort$timeStLim<-timeStLim 
tripInfoSort$timeEndLim<-timeEndLim 

# compile
tripInfoOut <- merge(tripInfoSort, metadata[which(metadata$Deploy_recover_DR=="R"),], by="Deploy_ID") 
head(metadata)
head(tripInfoOut)
head(tripInfoSort)

# export file of trip parameters
####output error for each bird
write.table(tripInfoOut,paste("D:/Share_Data/Tracking_Data/GPS/Stats_out/Trip_summary_",species,"_GPS.csv",sep = ""),sep=",",quote=FALSE,col.names=TRUE,row.names=FALSE)

# Plot trips for QAQC
    # note on plotting - if there is a time prior to first location at sea - the plot may show a line for distance from colony that is inconsistent with first start time which is acvtually an on colony location.

    ##############################
    #     Deploy_ID, trip1St (UTC "%Y/%m/%d %H:%M:%S"
    #     tripStComp (0=no, 1=yes, -1=never left colony, 3=estimated based on speed), 
    #     tripEnd (UTC "%Y/%m/%d %H:%M:%S")
    #     tripEndComp (0=no, 1=yes, -1=never left colony, 3=estimated based on speed)
    #     status (0=onCol, 1=atSea)
    #     tripComp (0=no, 1=yes, -1=never left colony)
    ##############################

  # to plot dist2Col vs UTC
    Deploy_IDs<-sort(unique(tripInfo$Deploy_ID))
    # l<-20
    for (l in 1:length(Deploy_IDs)) { 
      # windows()
      Deploy_ID_Want<-Deploy_IDs[l]
      file.idWant<-metadata$TrackFile[which((metadata$Deploy_ID==Deploy_ID_Want) & (metadata$Deploy_recover_DR=="R"))]
      track<-(subset(tracks_all,file.id == file.idWant & SPDfilter != "removed"))
      tripInfoWant<-subset(tripInfoSort,Deploy_ID == Deploy_ID_Want)
      ColWant<-metadata$SubCol_Code[which((metadata$Deploy_ID == Deploy_ID_Want) & (metadata$Deploy_recover_DR=="D"))]
      track$dist2Col<-t(rdist.earth((matrix(c(ColLocs$lon[which(ColLocs$SubCol_Code==ColWant)],ColLocs$lat[which(ColLocs$SubCol_Code==ColWant)]), ncol=2)),(matrix(c(track$Longitude,track$Latitude),ncol=2)),miles=FALSE))
      # head(track)
      # n<-paste("D:/Share_Data/Tracking_Data/GPS/3_Trips/",Deploy_ID_Want,"_",species,"_trips.pdf",sep = "")
      time_local<-as.POSIXlt(format(as.POSIXct(track$UTC,'GMT'), tz=tLocal,usetz=TRUE))
      
      p <- ggplot(track, aes(x=time_local,y=dist2Col))
      p1 <- p + geom_line(colour='grey',size=1)
      p2 <- p1 + geom_point(data=tripInfoWant,aes(x=tripEnd_loc,y=tripEndComp*4), colour= "blue", shape="*", size = 10)
      p3 <- p2 + geom_point(data=tripInfoWant,aes(x=tripSt_loc,y=tripStComp*4),  colour="red", shape="o", size = 10)
      pdf(paste("D:/Share_Data/Tracking_Data/GPS/3_Trips/",Deploy_ID_Want,"_",species,"_trips.pdf",sep = ""), onefile = TRUE)
      print(p3)
      dev.off()      
    }
# check and notate any plots that look funky -


tocall=as.numeric(proc.time()[3]) - as.numeric(ticall)
print("time elapsed")
print(tocall)



#####

# m<-21
for (m in 1:length(Deploy_IDs))
  tripInfoWants<-tripInfo[(tripInfo$Deploy_ID==Deploy_IDs[m]) & (tripInfo[,4]==1 | 3) & (tripInfo[,6]==1 | 3) & (tripInfo[,7]=='TRUE'),]
  metaWant<-metadata[which((metadata$Deploy_ID==tripInfoWants[1,2]) & (metadata$Deploy_recover_DR=="R")),]
  track<-(subset(tracks_all,file.id == metaWant$TrackFile & (SPDfilter != "removed")))
  # n<-1
  for (n in 1:length(tripInfoWants[,1]))
    tripInfoWants[n,8]<-n # label trip with trip number
    trackTrip<-subset(track,(as.POSIXlt(track$UTC,"GMT")>=as.POSIXlt(tripInfoWants[n,3],"GMT") & (as.POSIXlt(track$UTC,"GMT")<=as.POSIXlt(tripInfoWants[n,5],"GMT")))) # select track bounded by trip times
    ColLocsWant<-ColLocs[which(ColLocs$SubCol_Code==metaWant$SubCol_Code),6:7]
  # now use if statements to label beginning and end of each track with colony location
  # will add a location if the track start time was interpolated
  if ((tripInfoWants[n,4]==1) & (trackTrip[1,1]==tripInfoWants[n,3])) { # if trip is complete and first track time matches tripInfo start time
    trackTrip[1,2:3]<-c(ColLocsWant[1,2],ColLocsWant[1,2]) # set first locations to colony locs
  } else if (((tripInfoWants[n,4]==1) & (trackTrip[1,1]!=tripInfoWants[n,3])) | (tripInfoWants[n,4]==3)) { # if trip is complete and first track time is after tripInfo start time
    # or is trip start time was interpolated based on speed of x # of locations following first off colony location
    trackTrip[1,1:13]<-c(as.character(tripInfoWants[n,3]),ColLocsWant[1,2],ColLocsWant[1,2],NA,'end_location','end_location',NA,NA,NA,NA,NA,NA,NA) # add new first location where departure time = interpolated tripInfo Start Time and locs = colony locations
    trackTrip<-rbind(trackTrip[1,],trackTrip)
    # head(trackTrip)
    
    dist2Col<-(t(rdist.earth((matrix(c(ColLocWant$lon,ColLocWant$lat), ncol=2)),(matrix(c(trackTrip$Longitude,trackTrip$Latitude),ncol=2)),miles=FALSE)))
    
  }
  if (tripInfoWants[n,6]==1) {
  
  } else if (tripInfoWants[n,6]==3) {
  
  }

  if  # replace track ends with appropriate ends
    if (tripInfoWant[,3]==1 & as.POSIXlt(tripInfoWant[,2],"GMT")==as.POSIXlt(trackTrip[1,1],"GMT")) {
      # reset on colony locations to colony location if UTC's match if they don't - prepend and append with appropriate UTC and colony locs
      # at this point resave trakc with minimal data Deploy_ID, trip_no?, tripComp&Valid, UTC, lat, lon, ?startstop?
      
      
      
      trackTrip[1,2:3]<-c(ColLocsWant$lat,ColLocsWant$lon)
      trackTrip[length(trackTrip[,1]),2:3]<-c(ColLocsWant$lat,ColLocsWant$lon)
      } else if (tripInfo$tripInfoWant=1) {
      
    }
                      
  }
  
#### END