################################################################
#
# IMPORTING COMPILED VHF TAG DATA
#
#
#
################################################################

	# inputs
			# file with VHF_Deploy_ID, vhf locs, utc, Lat, Long, or UTMs
	# outputs
      # file with wgs lat longs

# clear all
rm(list=ls())
						
# install the argosfilter package
library(sp)
library(maptools)
library(raster)
library(rgdal)
library(rgeos)
library(stringr)
library(dplyr)

# set plot option to review plots TRUE or FALSE
plot<-FALSE

dir.in <- "/Users/henry/Documents/Work/Projects/USGS/USGS 08.10.15/Share 8.11.2015/Tracking_Data/VHF/"
dir.out <- "/Users/henry/Documents/Work/Projects/USGS/USGS 08.10.15/Share 8.11.2015/Tracking_Data/VHF/"

#### datafile to read
vhf <- read.table (paste(dir.in,"vhf_data.csv",sep = ""),header=T, sep=",",strip.white=T)

#### read in metadata
meta<-read.table (paste(dir.in,"vhf_metadata.csv",sep = ""),header=T, sep=",",strip.white=T)

vhf$UTM_E<-as.numeric(vhf$UTM_E)
vhf$UTM_N<-as.numeric(vhf$UTM_N)

#### find records without lat lon (utm only)
vhfUTMs<-as.data.frame(vhf[(is.na(vhf$LAT) | is.na(vhf$LON)),])

vhfUTMs.sp <- SpatialPointsDataFrame(coords = cbind(vhfUTMs$UTM_E,vhfUTMs$UTM_N),data<-vhfUTMs)

# define projection, use the WGS84 projection that Argos Data is delivered in
vhfUTMs.sp@proj4string <- CRS("+proj=utm +zone=10 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

# get wgs84 pro
projWant <- "+proj=longlat +ellps=WGS84 +datum=WGS84"

vhfUTMs.sp_WGS<-spTransform(vhfUTMs.sp, CRS(projWant))

vhf$LONG[is.na(vhf$LONG)]<- vhfUTMs.sp_WGS@coords[,1]
vhf$LAT[is.na(vhf$LAT)]<- vhfUTMs.sp_WGS@coords[,2]

vhf.sp <- SpatialPointsDataFrame(coords = cbind(vhf$LONG,vhf$LAT),data<-vhf)

plot(vhf.sp)

# create row index
ridx<-seq(from = 1, to = length(vhf.sp), by = 1)

vhf.sp@data$LAT<-vhf.sp@coords[,2]
vhf.sp@data$LONG<-vhf.sp@coords[,1]

vhf.out<-cbind(ridx, vhf.sp@data)

####
write.table (vhf.out, paste(dir.out,"vhf_corr.csv", sep =""), sep=",", quote=FALSE,col.names=TRUE, row.names=F,na="")