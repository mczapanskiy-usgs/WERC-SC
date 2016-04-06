#############################################
# EBOS point to raster
# Bill Henry USGS
# July 29, 2015
#
# - load shapefiles for IATTC and EEZ
# - load rediscretized points
# - counts GPS EOBS point data in polygons
#
#
#
#
#############################################


#### clear all
rm(list=ls())

# used
library(SDMTools)
library(adehabitatMA)
library(data.table)
library(adehabitatLT)
#library(tidyr)
# library(plyr)
library(data.table)
library(dplyr)

#library(tcltk)
library(proj4)
#library(ggplot2)
library(sp)
library(rgdal)
library(rgeos)
library(raster)
library(maptools)
#library(stringr)
#library(trip)
library(maps)

library(ggplot2)
library(rgeos)
# library(stringr)
library(ggmap)


#### select species
species="BFAL"

####
# dir.out <- 'D:/Share_Data/Tracking_Data/EOBS/All_tracks/'
dir.out <- '/Users/henry/Documents/Work/Projects/USGS/USGS 08.10.15/Share 8.11.2015/Tracking_Data/EOBS/Analyses/'

# get projection wanted
projWant<-"+proj=aea +lat_1=30 +lat_2=70 +lat_0=52 +lon_0=170 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

########################################################################################################################
########################################################################################################################
# read in shapefiles

#############################################
#### get map of the area in question
## directory for world background to read in
# dir.in.poly for loading polygon that can be used to clip data
dir.in.map<-"/Users/henry/Documents/Work/Projects/USGS/USGS 08.10.15/Share 8.11.2015/ARCMAP/Land"

# load a clip ploygon with world land file
clipperName<-as.character("GSHHS_c_L1")
# read in polygon from GSHHS
m <- readOGR(paste(dir.in.map,"/gshhg-shp-2.3.4/GSHHS_shp/c",sep=""),clipperName) # clipper comes in as  GS84

#### transformed m to projWant
m.trans<-spTransform(m, CRS(projWant))
# plot(m.trans)
#### fix topology (fixes problem polygons using a 0 buffer)
m.trans <- gBuffer(m.trans, byid=TRUE, width=0)

plot(m.trans)

#############################################
#### Get IATTC shapeflie
# dir.in.IATTC<-"/Users/henry/Documents/Work/Projects/USGS/USGS 08.10.15/Share 8.11.2015/ARCMAP/Fisheries/IATTC_poly"

IATTC_poly <- readOGR("/Users/henry/Documents/Work/Projects/USGS/USGS 08.10.15/Share 8.11.2015/ARCMAP/Fisheries/IATTC","IATTC_poly") # clipper comes in as  GS84

IATTC_poly.trans<-spTransform(IATTC_poly, CRS(projWant))

plot(m.trans)
plot(IATTC_poly.trans, add=TRUE)

#############################################
#### Get EEZ shapeflie
# dir.in.EEZ<-"/Users/henry/Documents/Work/Projects/USGS/USGS 08.10.15/Share 8.11.2015/ARCMAP/EEZ Maritime Boundaries/"

EEZ_poly <- readOGR("/Users/henry/Documents/Work/Projects/USGS/USGS 08.10.15/Share 8.11.2015/ARCMAP/EEZ/World EEZ/","World_EEZ") # clipper comes in as WGS84

EEZ_poly.trans<-spTransform(EEZ_poly, CRS(projWant))

countries.want<-as.factor(c("United States","Canada","Russia","Japan"))

EEZ_poly.sub.trans<-EEZ_poly.trans[EEZ_poly.trans$Sovereign %in% countries.want,]

plot(m.trans)
plot(IATTC_poly.trans, add=TRUE)
plot(EEZ_poly.sub.trans, add=TRUE)

#############################################
# #### Create EEZ US shapeflie
# EEZ_US_poly <- readOGR("/Users/henry/Documents/Work/Projects/USGS/USGS 08.10.15/Share 8.11.2015/ARCMAP/EEZ/World EEZ/","World_EEZ") # clipper comes in as WGS84
# 
# EEZ_poly.trans<-spTransform(EEZ_poly, CRS(projWant))
# 
# plot(m.trans)
# plot(IATTC_poly.trans, add=TRUE)
# plot(EEZ_poly.trans, add=TRUE)

#############################################
#### Get shapeflie 

PMNM<-readOGR("/Users/henry/Documents/Work/Projects/USGS/USGS 08.10.15/Share 8.11.2015/ARCMAP/Protected Marine Area Boundaries/PMNM/","Papahanaumokuakea_MNM") # clipper comes in as WGS84
# 
PMNM.trans<-spTransform(PMNM, CRS(projWant))

plot(PMNM.trans, col="light green")


#############################################
#### Create WPRFMC shapeflie
N.WPRFMC.grid<-expand.grid(seq(from=132.5,to=207.5, by=5),seq(from=17.5,to=57.5, by=5))
colnames(N.WPRFMC.grid)<-c("x","y")
N.WPRFMC.grid$idx = as.factor(paste(N.WPRFMC.grid$x,N.WPRFMC.grid$y,sep="_"))

#### create spatial points data frame
coordinates(N.WPRFMC.grid) <- ~ x + y
# coerce to SpatialPixelsDataFrame
gridded(N.WPRFMC.grid) <- TRUE
N.WPRFMC.grid@proj4string<- CRS(proj_in)
# coerce to raster - grid@data@attributes = ID levels
N.WPRFMC.raster <- raster(N.WPRFMC.grid)

#### define incoming projection
proj_in<-"+proj=longlat +ellps=WGS84 +datum=WGS84"

#### convert raster to spatial polygons
N.WPRFMC_poly<-rasterToPolygons(N.WPRFMC.raster, fun=NULL, n=4, na.rm=TRUE, digits=12, dissolve=FALSE)

N.WPRFMC_poly@proj4string<- CRS(proj_in)

# Plot grid and tracks
# transform
N.WPRFMC_poly.trans<-spTransform(N.WPRFMC_poly, CRS(projWant))

# Merge polygon
merge_id<-rep(1,length(N.WPRFMC_poly.trans))
N.WPRFMC_poly.trans.merged <- unionSpatialPolygons(N.WPRFMC_poly.trans,merge_id)
poly.id<-as.data.frame(1)
names(poly.id)="poly.id"
N.WPRFMC_poly.trans.merged.SPDF<-SpatialPolygonsDataFrame(N.WPRFMC_poly.trans.merged,data=as.data.frame(poly.id))

N.WPRFMC_poly.merged.SPDF.WGS84<-spTransform(N.WPRFMC_poly.trans.merged.SPDF, CRS(proj_in))

writeOGR(N.WPRFMC_poly.trans.merged.SPDF,'/Users/henry/Documents/Work/Projects/USGS/USGS 08.10.15/Share 8.11.2015/ARCMAP/Fisheries/WPRFMC/','N.WPRFMC.outline.WGS84',overwrite_layer='T', driver="ESRI Shapefile")

plot(N.WPRFMC_poly.trans.merged.SPDF, col="light green")
plot(IATTC_poly.trans, col="light blue",add=TRUE)
plot(EEZ_poly.sub.trans, col="gray", add=TRUE)
plot(PMNM.trans, col="yellow", add=TRUE)
plot(m.trans,  col="black", add=TRUE)
map.axes()

#############################################
#### Get the tracking data

# specify tracking data months and years
tm<-c(2,3,4,5,6); ty=c(2012,2013)

#### enter number of mins to rediscretize (interpolate) the track data to, equal time interval spacing is important to analyses, this will be function of speed and cell size of grid)
mins=20

#### enter radius of data (in km)
r=5

#### directory in
# dir.in <- 'D:/Share_Data/Tracking_Data/EOBS/All_tracks/'
dir.in <- '/Users/henry/Documents/Work/Projects/USGS/USGS 08.10.15/Share 8.11.2015/Tracking_Data/EOBS/Analyses/'

meta<-read.table (paste('/Users/henry/Documents/Work/Projects/USGS/USGS 08.10.15/Share 8.11.2015/GitHub/WERC-SC/trackcode/eobs/','metadata_EOBS.csv',sep = ""),header=T, sep=",", strip.white=T)

#### subset metadata for sex
meta<-meta[meta$Species==species & meta$loc.data==1 & meta$Year %in% ty,]

meta.all<-meta

# i=1
for (i in 1:length(meta$Deploy_ID)) {

meta<-meta.all[meta.all$Deploy_ID==meta.all$Deploy_ID[i],]
# get track data
DT<-read.table (paste(dir.in,species,'_rad_',r,'_locs.behav.annotation.csv',sep = ""),header=T, sep=",", strip.white=T)

DT<-transform(DT,date =as.POSIXct(date, tz = "GMT", format = "%Y-%m-%d %H:%M:%S"))
DT$m<-as.numeric(format(DT$date, format = "%m"))

#### subset trackdata using selected metadata 
DT<-DT[DT$Deploy_ID %in% meta$Deploy_ID,]

#### subset trackdata for months
if (exists("tm")) {
  DT<-DT[DT$m %in% tm,]
}

#### now back subset metadata using Deploy_IDs retained in the tracks to make sure you haven't dropped some Deploy_IDs because tracks were not
Deps.in<-cbind(unique(DT$Deploy_ID), rep(1,length(unique(DT$Deploy_ID))))
colnames(Deps.in)<-c("Deploy_ID","has.data.in.time.window")

meta<-merge(meta,Deps.in,by="Deploy_ID",all=TRUE)

# get some trip summary data
trip_summary<-read.table (paste(dir.in,species,'_rad_',r,"_","trip_summarys.csv",sep = ""),header=T, sep=",", strip.white=T)

# Rediscretize if you care to (recommended for any points in polygon operation)
DT<-as.data.frame(DT)

# convert tracks into a spatial data frame
DT.sp <- SpatialPointsDataFrame(coords = DT[c("longitude360","latitude")], data = DT)
# define projection, use the WGS84 projection that Argos Data is delivered in
DT.sp@proj4string <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
# plot(DT.sp, pch=3,cex=.01)

DT.sp_trans<-spTransform(DT.sp, CRS(projWant))
######
# plot(DT.sp_trans, pch=3,cex=.01) # plot transforms
# plot(DT$longitude360,DT$latitude, pch=3,cex=.01) # plot untransformed

## compare the coords
# cbind(track.sp_trans@coords,tracks_all[c("Longitude","Latitude")])

## get transformed tracks for to calc speed and distance calculations
DT.sp_trans<-as.data.frame(DT.sp_trans@coords)
DT.sp_trans<-rename(DT.sp_trans, longitude_trans=longitude360, latitude_trans=latitude)

DT<-cbind(DT, DT.sp_trans)

DT <- transform(DT, date = as.POSIXct(date, tz = "GMT", format = "%Y-%m-%d %H:%M:%S"))

trackObjs<-as.ltraj(DT[c("longitude_trans","latitude_trans")], DT$date, id=DT$id, burst = DT$id, typeII = TRUE, infolocs =data.frame(DT$tracks.height.above.ellipsoid,DT$tracks.temperature,DT$tracks.trip_no,DT$tracks.tripStComp,DT$tracks.tripEndComp,DT$tracks.trip_comp,DT$tracks.duration.hrs,DT$tracks.Site,DT$Deploy_ID,DT$longitude360,DT$longitude,DT$latitude))

# Rediscretize to x seconds
seconds<-20*mins

Redis<-redisltraj(trackObjs, seconds, burst = burst, samplex0 = FALSE, addbit = FALSE,
                  nnew = 10, type = "time")

Redis.DF<-cbind(data.table(ld(Redis)))
Redis.DF<- as.data.frame(Redis.DF[order(as.numeric(as.character(Redis.DF$burst)),Redis.DF$date),])
Redis.DF$id<-as.character(Redis.DF$burst)
trip_summary$Deploy_ID<-as.character(trip_summary$Deploy_ID)
trip_summary$Site<-as.character(trip_summary$Site)
trip_summary.want<-trip_summary[,c("id","Site","Deploy_ID")]
Redis.DF<- merge(Redis.DF,trip_summary.want,by='id')

Redis.DF.sp <- SpatialPointsDataFrame(coords = Redis.DF[c("x","y")], data = Redis.DF)
# define projection, use the appropriate projection for the data
Redis.DF.sp@proj4string <- CRS(projWant)

plot(N.WPRFMC_poly.trans.merged, col="lightblue4")
plot(IATTC_poly.trans, col="slategray",add=TRUE)
plot(EEZ_poly.trans, col="goldenrod4", add=TRUE)
plot(EEZ_poly.sub.trans, col="goldenrod1", add=TRUE)
plot(m.trans,  col="gray12", add=TRUE)
plot(PMNM.trans, col="darkorange1", add=TRUE)
plot(Redis.DF.sp,pch=3,cex=.005,add=TRUE, col="dark red")
map.axes()

# use over point.in.poly in spatialEco to attribute track points in polygons and Count all points from all birds for all years
library(spatialEco)
# joined<-as.data.table(point.in.poly(Redis.DF.sp,Fish_42.SPDF)@data)
track.pts.in.N.WPRFMC_poly.trans.merged.SPDF<-as.data.frame((as.data.table(point.in.poly(Redis.DF.sp,N.WPRFMC_poly.trans.merged.SPDF)@data)) %>% summarise(count=n()))
track.pts.in.fish.cell<-rename(track.pts.in.fish.cell,sum.track.pts=count)




over(track.filts.sp_proj, clipper_proj.sp)