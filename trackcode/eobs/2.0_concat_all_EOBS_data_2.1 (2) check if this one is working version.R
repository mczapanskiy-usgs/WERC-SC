

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


# set plot option to review plots TRUE or FALSE
plot<-FALSE

####  dir.in for .csv files
dir.in <- "D:/Share_Data/Tracking_Data/EOBS/"

#### dir.out for .csv files
dir.out <- "D:/Share_Data/Tracking_Data/EOBS/"

# open metadata file
metadata<-read.table(paste(dir.in,"metadata_EOBS.csv",sep=""),header=T, sep=",", strip.white=T)

# # get species specific data
# metadata<-subset(metadata,Species==species)

# get species from AUO Code in metadata
species.tagged<-unique(metadata$Species)
# i=15
for (i in 1:length(species.tagged)) {
  species<-species.tagged[i]
      
  #### get metadata for Species omitting any metadata records without deployment or recovery
  metaWants<-(subset(metadata, Species==species & Tagging_Event=='D' & loc.data==1))
  # head(metaWants)
  
  ### get file file.ids of files that have good data
  print(c("number of tracks",length(metaWants[,1])))
        
      # j<-1
      for (j in 1:length(metaWants$Deploy_ID)) {
        
        file.id <- metaWants$GPS_ID[j]
        Deploy_ID <- metaWants$Deploy_ID[j]
        print(file.id)
      
          #### get track
        track <- read.table (paste(dir.in,species,"/2_EOBS_Freitas_out/Indiv_Files/",species,"_",file.id,"_",metaWants$Deploy_ID[j],"_SPD.csv",sep = ""),header=T, sep=",", strip.white=T)

          if (i==1 & j==1) {
            allTracks<-data.frame(track, species = species, year =  metaWants$Year[j], Site = metaWants$Site[j])
          } else {
            allTracks<-rbind(allTracks,data.frame(track, species = species, year =  metaWants$Year[j], Site = metaWants$Site[j]))
            }
          #  allTracks[1:30,]
          #  head(allTracks)
  } # track close loop
    
} # close species loop

# create output directory, will not replace if already exists
dir.create(paste(dir.out,"All_Tracks/",sep=""),showWarnings=TRUE)

#### output filtered data    
write.table(allTracks, paste(dir.out,"All_Tracks/","All_Species_EOBS_allTracks.csv", sep =""), sep=",", quote=FALSE,col.names=TRUE, row.names=F,na="")

tocall=as.numeric(proc.time()[3]) - as.numeric(ticall)
print("time elapsed")
print(tocall)
