


################################################################
#
# adding offest to GPS TAG DATA 
#
# (Using location data direct from the iGOTU platform)
#
################################################################

####
# for SDA filtering of GPS TAG DATA where:
# originating file is from igotu tag
#
# inputs (.csv)
# 1. metadata file specifying the offset required to correct time in file to UTC (offset)
# 2. individual .csv from igotu platform
#
# outputs
# .csv containing corrected time (in UTC)
#
# note: file name = unique id unless otherwise noted
#
####

# clear all
rm(list=ls())

ticall<-as.numeric(proc.time()[3])

# set species AUO Code
species<-"RFBO"

####  dir.in for .csv files
dir.in <- "D:/Share_Data/Tracking_Data/GPS/"

#### dir.out for .csv files
dir.out <- "D:/Share_Data/Tracking_Data/GPS/"

# open metadata file
metadata<-read.table(paste(dir.in,"metadata_all_GPS_06.04.15_working.csv",sep=""),header=T, sep=",", strip.white=T)

# year=2014
#### will use this to loop through years 
# Get years from file names in the 1_RawGPS directory
year.id<-list(unique(metadata$Year))

#### Get sites from file names in the 1_RawGPS directory
site.id<-(unique(metadata$Site))

#####
## Fix offsets
#####

# subset those files for that species and with successful processing
metaWants<-subset(metadata, Species==species & (Year>=min(metadata$Year) & Year<=max(max(metadata$Year))) & (GPS_TagRecov==1 | GPS_TagRecov==3 | GPS_TagRecov==4))
    
    ### file.ids<-list.files(paste("/Users/henry/Documents/Work/Projects/CA_Seabird_ALTAS/Josh Method/",species,"/1_SeaTurtle_out",sep = ""), pattern = "\\.csv$", all.files = TRUE,full.names = FALSE, recursive = FALSE,ignore.case = FALSE, include.dirs = FALSE)
    file.ids<-metaWants$GPS_Track_File
    
    # create output directory, will not replace if already exists
    dir.create(paste(dir.out,"1_RawGPS/",sep = ""),showWarnings=TRUE)
    
    #### Loop to read in location data for each tag.csv file #### (from STAT???)
    # l=1
    for (l in 1:length(file.ids)) {
      
      file.id <- as.character(file.ids[l])
      print(file.id)
      
      #### get metadata for file.id
      metaWant<-metaWants[l,]
      
      #### get track
      track <- read.table(paste(dir.in,"0_noOffset_Corr/",file.id,".csv",sep = ""),header=T, sep=",", strip.white=T)
      head(track)
      
      #### refer to metadata to determine if offset needed
      if (is.na(metaWant$offset) | (metaWant$offset==0)) {
        write.table (track, paste(dir.out,"1_RawGPS/",file.id,"_RawGPS.csv",sep = ""), sep=",", quote=FALSE,col.names=TRUE, row.names=F,na="")      
      } else {
      if (metaWant$offset>0) {
        # put date and time together in GMT and correct by # hrs indicated in offset column (correction must be converted to seconds hr*min*sec)
        corrdtime<- (as.POSIXct(paste(as.character(track$Date), as.character(track$Time, sep=" ")), tz = "GMT")) + (metaWant$offset*60*60)
        
        track$Date<-format(corrdtime, "%Y/%m/%d")
        track$Time<-format(corrdtime, "%H:%M:%S")
        
        write.table (track, paste(dir.out,"1_RawGPS/",file.id,"_RawGPS.csv",sep = ""), sep=",", quote=FALSE,col.names=TRUE, row.names=F,na="")      
        }
      }
  
  } # file.ids loop

tocall=as.numeric(proc.time()[3]) - as.numeric(ticall)
print("time elapsed")
print(tocall)


# END