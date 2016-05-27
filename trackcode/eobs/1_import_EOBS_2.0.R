####################################
#### import_EOBS_1.0
#### Bill Henry 08/08/14
####
#### imports GPS data from multiple eobs folders
#### removes duplicate timestamps
#### exports csv of obs data
#### creates movebank stack object
#### 
#### remove locations from initial tag testing 
#### 
#### in: gps files exported in movebank format from e-obs decoder
####      - can be from multiple eobs downloads
####          - one folder per download, this folder name must end with "logger" contains gps files including: tag1884_gps.txt
####          - these folders in parent directory"  "tag_data" (e.g. Tracking_Data/EOBS/Kure 2013/tag_data/LAAL_KURE_5.26.2014_logger)
#### in: tLim = time limit in mins that you want to remove, 1 day = 1440, 10 day = 14400, 20 day = 28800
#### out: movestack object
#### 
####
####################################

# clear all
rm(list=ls())

library(move)
#Load your libraries for mapping (may be in move or not)
library(sp)
library(maptools)
library(plyr)

# ?temp 
library(rworldmap)

# dir.folders<-paste("D:/Share_Data/Tracking_Data/EOBS/LAAL/NAK 2014/tag_data",sep = "")
# dir.folders<-paste("D:/Share_Data/Tracking_Data/EOBS/LAAL/Kaena 2014/tag_data",sep = "")
# dir.folders<-paste("D:/Share_Data/Tracking_Data/EOBS/BFAL/Kure 2012/tag_data/",sep = "")
# dir.folders<-paste("D:/Share_Data/Tracking_Data/EOBS/BFAL/Kure 2013/tag_data/",sep = "")


folders<-list.files(dir.folders, pattern = "\\logger$" , all.files = TRUE,full.names = FALSE, recursive = FALSE,ignore.case = FALSE, include.dirs = TRUE)

tracks.all<-data.frame( )
for (i in 1:length(folders)) { 
# i=1
  
  ### dir.in parent file directory
# dir.in <-  ("D:/Share_Data/Tracking_Data/EOBS/BFAL Kure 2012 tag downloads")
# dir.in <-  ("D:/Share_Data/Tracking_Data/EOBS/BFAL Kure 2013 tag downloads")
# dir.in <-  ("D:/Share_Data/Tracking_Data/EOBS/LAAL Oahu 2014 tag downloads")
dir.in <-  paste(dir.folders,"/",folders[i],sep ="")

### file.ids<-list.files(paste("/Users/henry/Documents/Work/Projects/CA_Seabird_ALTAS/Josh Method/",species,"/1_SeaTurtle_out",sep = ""), pattern = "\\.csv$", all.files = TRUE,full.names = FALSE, recursive = FALSE,ignore.case = FALSE, include.dirs = FALSE)
file.ids<-list.files(dir.in, pattern = "\\_gps.txt$", all.files = TRUE,full.names = FALSE, recursive = FALSE,ignore.case = FALSE, include.dirs = FALSE)
bird.ids<-sub("^([^_]*).*", "\\1",  file.ids)
bird.ids<-sub("tag", "\\1",  bird.ids)
print(bird.ids)
print(c("number of birds",length(bird.ids)))

# Lst=list()

# note movestack object does not work if there are no records for a tag

for (j in 1:length(file.ids)) {  		  
# j=1
  
  print(paste(j,i," ",folders[i],bird.ids[j]))
  
  if (j==1 & i==1){
tin=read.csv(paste(dir.in,"/",file.ids[j], sep=""), header=T, sep=",", strip.white=T)
tin$timestamp<-tin$timestamp.of.fix
# remove duplicate timestamps
tracks.all <- tin[which(!duplicated(tin$timestamp)), ]
# write.table(tin,paste(dir.folders,"/all.locs.csv",sep = ""),sep=",",quote=FALSE,col.names=TRUE,row.names=FALSE)
remove(tin)
} else {
  tin=read.csv(paste(dir.in,"/",file.ids[j], sep=""), header=T, sep=",", strip.white=T)
  tin$timestamp<-tin$timestamp.of.fix
  # remove duplicate timestamps
  tin <- tin[which(!duplicated(tin$timestamp)), ]
  # write.table(tin,paste(dir.folders,"/all.locs.csv",sep = ""), sep = ",", append = TRUE,row.names=FALSE,col.names = FALSE)
  tracks.all<-(rbind(tracks.all,tin))
  remove(tin)  
  }

#### This is defunct and will be rectified by the initial trip breaker
# check and remove any locations >d (in days) before location 1
# # head(tin)
# tDiff=(as.POSIXlt(tin$start.timestamp[2:(length(tin[,1]))],"GMT") - as.POSIXlt(tin$start.timestamp[1:(length(tin[,1])-1)],"GMT"))
# 
# tin<-tin[c(as.numeric(tDiff),1)<tLim,]
# 
####
}
}

# get data
# tracks.all<-read.csv(paste(dir.folders,"/","all.locs.csv", sep=""), header=T, sep=",", strip.white=T)

# now plot it
# library(maps)
# plot.map("world", center=180, col="white",bg="gray",
#          fill=TRUE,ylim=c(-60,90),mar=c(0,0,0,0))
# 
# plot.map("world", center=180, col="white",bg="gray",
#          fill=TRUE,ylim=c(-60,90),mar=c(0,0,0,0))

# make move object
bird.ids <- unique(tracks.all$tag.serial.number)
Lst=list()

data.all<-data.frame( ) 
for (k in 1:length(bird.ids)) {    	  
# k=1
data=subset(tracks.all,tag.serial.number==bird.ids[k] & !is.na(longitude))
data$tag.serial.number[1]
# head(data)
     
data=unique(data[order(as.double(as.POSIXct(data$timestamp.of.fix,format="%Y-%m-%d %H:%M:%S", tz="UTC"))),])

data.all<-(rbind(data.all,data))

# head(data)
Lst[[k]]<-assign(as.character(bird.ids[k]),move(x=data$longitude,y=data$latitude,
          time=as.POSIXct(data$timestamp.of.fix,format="%Y-%m-%d %H:%M:%S", tz="UTC"),
          data=data,proj=CRS("+proj=longlat +ellps=WGS84"),animal=bird.ids[k]))

}

names(data.all)[names(data.all)=="Latitude"] <- "latitude"
names(data.all)[names(data.all)=="Longitude"] <- "longitude"

write.table(data.all,paste(dir.folders,"/all.locs.csv",sep = ""), sep = ",", append = FALSE,row.names=FALSE,col.names = TRUE)











































# name list elements
names(Lst)<-bird.ids

# create moveStack
data<-moveStack(Lst)

data2 <- spTransform(data, CRSobj="+proj=aeqd", center=TRUE)

plot(data2,type="l", col=c(1:length(bird.ids)), lwd=.25)

n.locs(data2)

# for fun

hist(data.all$speed.over.ground[data.all$speed.over.ground<150 & data.all$speed.over.ground>0],1000)




# other stuff for later

dbbmm <- brownian.bridge.dyn(object=tag2602, raster=9, location.error=20, margin=11, window.size=301, ext=.25)

show(data2)

plot(spTransform(data,center=TRUE),type="l", col=c(1:length(file.ids)), lwd=.25)

plot(cont,col="grey", add=TRUE) 


#get coarse resolution world from rworldmap
sPDF <- getMap()  
#mapCountries using the 'continent' attribute  
p2=mapCountryData(sPDF)


# plot continent
# Download the continents shapefile
download.file("http://baruch.cuny.edu/geoportal/data/esri/world/continent.zip",
              "cont.zip")
#Unzip it
unzip("cont.zip")
#Load it
cont <- readShapeSpatial("continent.shp")



