

################################################################
#
# Concatenate GPS Tag Data in one file for export to ARC and Visualization
#
#
################################################################

#  
# outputs
# .csv of concatenated data

# clear all
rm(list=ls())

ticall<-as.numeric(proc.time()[3])

# install the argosfilter package
library(argosfilter)
library(plyr)
library(CircStats)          
library(plotrix)     
library(geosphere) 

# firsts - set the number of first cells that you want to grab from each file to get points used to get colony locations
firsts<-1
  
# set plot option to review plots TRUE or FALSE
plot<-TRUE

####  dir.in for .csv files
dir.in <- "D:/Share_Data/Tracking_Data/GPS/"

#### dir.out for .csv files
dir.out <- "D:/Share_Data/Tracking_Data/GPS/"

# open metadata file
metadata<-read.table(paste(dir.in,"metadata_all_GPS_06.04.15_working.csv",sep=""),header=T, sep=",", strip.white=T)

# # get species specific data
# metadata<-subset(metadata,Species==species)

# get species from AUO Code in metadata
species.tagged<-unique(metadata$Species)
# i=15
for (i in 1:length(species.tagged)) {
  species<-species.tagged[i]
      
  #### get metadata for Species omitting any metadata records without deployment or recovery
  metaWants<-subset(metadata, Species==species & (GPS_TagRecov==1 | GPS_TagRecov==3))
  # head(metaWants)

  depl_sess.ids<-unique(paste(metaWants$Species, metaWants$DeplSess, metaWants$Year , sep="_"))

  
  ### get file file.ids of files that have good data
  print(c("number of birds",length(metaWants[,1])))
        
      # j<-1
      for (j in 1:length(metaWants$Deploy_ID)) {
        
        file.id <- metaWants$GPS_Track_File[j]
        print(file.id)
      
          #### get track
        track <- read.table (paste(dir.in,"2_GPS_Freitas_out/",file.id,"_",metaWants$Deploy_ID[j],"_SPD_SDA.csv",sep = ""),header=T, sep=",", strip.white=T)
          # head(track)
          # track[1:50,]
          
          track$firstLocs<-rep(0,length(track[,1]))
          track$firstLocs[1:firsts]<-1
          
          # enter 
          # dist.pckArgos.SPD < XXX (refers to dist between point not in include subsequent locs)
          # firstLocs = # of location to take mean of for the colony locs
          geomean.wants<-subset(track,dist.pckArgos.SPD<.1 & firstLocs==1)                                   
          
          if (length(geomean.wants[,1])==1) { 
            deploy.loc<-data.frame(geomean.wants$Longitude,geomean.wants$Latitude)
            names(deploy.loc)<-c("lon", "lat")
            } else {
          if (length(geomean.wants[,1])==2) {
              deploy.loc<-data.frame(mean(geomean.wants$Longitude),mean(geomean.wants$Latitude))
              names(deploy.loc)<-c("lon", "lat")
            } else {
              deploy.loc<-centroid(cbind(geomean.wants$Longitude,geomean.wants$Latitude))
            }
             }
            
          if (i==1 & j==1) {
            allTracks<-data.frame(track, file.id = file.id, Deploy_ID=metaWants$Deploy_ID[j] ,species = metaWants$Species[j], year =  metaWants$Year[j], SubCol_Code = metaWants$SubCol_Code[j], session = depl_sess.ids[j])
            deploy.locs<-data.frame(deploy.loc, file.id)
          } else {
            allTracks<-rbind(allTracks,data.frame(track, file.id = file.id, Deploy_ID=metaWants$Deploy_ID[j] ,species = metaWants$Species[j], year =  metaWants$Year[j], SubCol_Code = metaWants$SubCol_Code[j], session = depl_sess.ids[j]))   
            deploy.locs<-rbind(deploy.locs,data.frame(deploy.loc, file.id))
          }
          #  allTracks[1:30,]
          #  head(allTracks)
  } # track close loop
    
} # close species loop

# create output directory, will not replace if already exists
dir.create(dir.out,"All_Tracks/",showWarnings=TRUE)

allTracks <- merge(x=allTracks,y=metadata[,c('GPS_Track_File')],by.x="file.id",by.y="GPS_Track_File")
#### output filtered data    
write.table(allTracks, paste(dir.out,"All_Tracks/","All_Species_GPS_allTracks.csv", sep =""), sep=",", quote=FALSE,col.names=TRUE, row.names=F,na="")

deploy.locs <- merge(x=deploy.locs,y=metadata[,c('GPS_Track_File', 'Deploy_ID')],by.x="file.id",by.y="GPS_Track_File")
#### output central locs for each bird
write.table(deploy.locs, paste(dir.out,"All_Tracks/","All_Species_GPS_Nest_Locs.csv",sep = ""),sep=",",quote=FALSE,col.names=TRUE,row.names=FALSE)

tocall=as.numeric(proc.time()[3]) - as.numeric(ticall)
print("time elapsed")
print(tocall)
