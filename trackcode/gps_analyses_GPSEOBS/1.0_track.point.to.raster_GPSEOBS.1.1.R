#############################################
# EBOS point to raster
# Bill Henry USGS
# July 29, 2015
#
# - interpolates point data from GPS at even intervals
# -
# 
#
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

#### select sex
sex="NA" # "NA", "M", or "F"

#### enter radius of data (in km)
r=5
#### enter year and month of data to use
# select a combination of months and years of fisheries data to use
#fm<-c(3); fy=c(2012)
fm<-c(2,3,4,5,6); fy=c(2003:2012)
# select a combination of months and years of track data to use
#tm<-c(3); ty=c(2012)
tm<-c(2,3,4,5,6); ty=c(2012,2013)

#### enter number of mins to rediscretize (interpolate) the track data to, equal time interval spacing is important to analyses, this will be function of speed and cell size of grid)
mins=20

#### define incoming projection
proj_in<-"+proj=longlat +ellps=WGS84 +datum=WGS84"

#### directory in
# dir.in <- 'D:/Share_Data/Tracking_Data/EOBS/All_tracks/'
dir.in <- '/Users/henry/Documents/Work/Projects/USGS/USGS 08.10.15/Share 8.11.2015/Tracking_Data/EOBS/Analyses/'

####
# dir.out <- 'D:/Share_Data/Tracking_Data/EOBS/All_tracks/'
dir.out <- '/Users/henry/Documents/Work/Projects/USGS/USGS 08.10.15/Share 8.11.2015/Tracking_Data/EOBS/Analyses/'

#### directory for world background to read in
#### dir.in.poly for loading polygon that can be used to clip data
dir.in.map<-"/Users/henry/Documents/Work/Projects/USGS/USGS 08.10.15/Share 8.11.2015/ARCMAP/Land"

# # dir.in.poly <- 'D:/Share_Data/ARCMAP/World'
# dir.in.poly <- '/Users/henry/Documents/Work/Projects/USGS/USGS 08.10.15/Share 8.11.2015/ARCMAP/World/'

###### directory for desired projection
dir.in.poly <- "/Users/henry/Documents/Work/Projects/USGS/USGS 08.10.15/Share 8.11.2015/Clip Polygons/"

# Get metadata
# meta<-read.table (paste('D:/Share_Data/GitHub/WERC-SC/trackcode/eobs/','metadata_EOBS.csv',sep = ""),header=T, sep=",", strip.white=T)
meta<-read.table (paste('/Users/henry/Documents/Work/Projects/USGS/USGS 08.10.15/Share 8.11.2015/GitHub/WERC-SC/trackcode/eobs/','metadata_EOBS.csv',sep = ""),header=T, sep=",", strip.white=T)

#### subset metadata for sex
if (sex!="NA") {
  meta<-meta[meta$Species==species & meta$loc.data==1 & meta$Year %in% ty  & meta$Sex==sex,]
}

#### subset metadata for years
  if (exists("ty")) {
    meta<-meta[meta$Species==species & meta$loc.data==1 & meta$Year %in% ty,]
  }

meta.all<-meta

# i=2
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

###### transform coordinates to desired projection for speed calculations
# read in list of potential clipper files
clipPolyList<-read.csv (paste(dir.in.poly,"clipPolyList.csv", sep=""), header=T, sep=",", strip.white=T)
#  print(clipPolyList) # show a list of the clipper files

#### select clipperfile
#### ui
rno<-14 # row number of file list

# read in polygon
##  clipper <- readOGR(dir.in.poly,clipper) # clipper comes in as unprojected WGS84

### get projection want
# read in projection best for selected polygon (contained in table clipPolyList)
projWant<-as.character(paste("+",clipPolyList$Proj4.specs[rno],sep=''))

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

# convert tracks into a spatial data frame
Redis.DF.sp <- SpatialPointsDataFrame(coords = Redis.DF[c("x","y")], data = Redis.DF)
# define projection, use the appropriate projection for the data
Redis.DF.sp@proj4string <- CRS(projWant)

########################################################################################################################
########################################################################################################################
########################################################################################################################
########################################################################################################################
#### MAKE A SPATIALPOLYGONSDATAFRAME OF THE DESIRED GRID

#### grid is centered on points ("from" == center of grid cells), 'by' = degrees (size of grid cells)
npac.grid<-expand.grid(seq(from=132.5,to=232.5, by=5),seq(from=17.5,to=57.5, by=5))
colnames(npac.grid)<-c("x","y")
npac.grid$idx = as.factor(paste(npac.grid$x,npac.grid$y,sep="_"))

#### create spatial points data frame
coordinates(npac.grid) <- ~ x + y
# coerce to SpatialPixelsDataFrame
gridded(npac.grid) <- TRUE
npac.grid@proj4string<- CRS(proj_in)
# coerce to raster - grid@data@attributes = ID levels
raster.npac.grid <- raster(npac.grid)
# projection(raster.npac.grid)<-"+init=epsg:4326" 
# to access raster attributes use: grid@data@attributes[[1]]$variable
# head(raster.npac.grid@data@attributes[[1]])

# data<<-asc.from.raster(raster.npac.grid,projs=proj_in)
# projs(asc.npac.grid)<-proj_in

# plot raster on grid
# plot(raster.npac.grid)
# # plot(asc.npac.grid)
# points(tracks.sp, pch=3,cex=.01)
# points(npac.grid, pch=1,cex=.5)
# points(tracks.sp, pch=3,cex=.01)

#### convert raster to spatial polygons
poly.npac.grid<-rasterToPolygons(raster.npac.grid, fun=NULL, n=4, na.rm=TRUE, digits=12, dissolve=FALSE)

# transform grid polygon
poly.npac.grid.trans<-spTransform(poly.npac.grid, CRS(projWant))

# Plot grid and tracks
plot(poly.npac.grid.trans)
plot(Redis.DF.sp, add=TRUE, axes=TRUE, pch=3,cex=.01)


########################################################################################################################
########################################################################################################################
########################################################################################################################
############################################################################################################################ GET AREA of each grid cell, corrected for area of grid cells that are land, could be used for standardization as probability of detecting bird in a grid cell is related to area of grid cell

#### GET MAP
# # another "higher res world map"
# m <- readOGR(paste(dir.in.poly,"/simplified-land-polygons-complete-3857",sep=""),"simplified_land_polygons") # clipper comes in as "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs"
# plot(m)
## dissolve the polygons

#### get a map of the area in question
# lower res world map
# load a clip ploygon with world land file
clipperName<-as.character("GSHHS_c_L1")
# read in polygon from GSHHS
m <- readOGR(paste(dir.in.map,"/gshhg-shp-2.3.4/GSHHS_shp/c",sep=""),clipperName) # clipper comes in as  GS84
# m@proj4string<- CRS(proj_in)
# plot(m)

#### transformed m to projWant
m.trans<-spTransform(m, CRS(projWant))
# plot(m.trans)
#### fix topology (fixes problem polygons using a 0 buffer)
m.trans <- gBuffer(m.trans, byid=TRUE, width=0)



#### get transformed extent of grid containing the data and define projection
cp.trans<-as(extent(poly.npac.grid.trans),"SpatialPolygons")
proj4string(cp.trans)<-CRS(projWant)

#### clip the map
clip.map <- gIntersection(m.trans, cp.trans, byid=TRUE, drop_lower_td=TRUE)
# plot(clip.map)
#### fix topology again (fixes problem polygons using a 0 buffer)
clip.map <- gBuffer(clip.map, byid=TRUE, width=0)

# dissolve polygons - may only need to use for some maps
# clip.IDs<-(sapply(slot(clip.map, "polygons"), slot, "ID"))
# clip.map <- unionSpatialPolygons(clip.map , IDs=clip.IDs)

#clip.map<-SpatialPolygons2PolySet(clip.map)

#### plot it up
# plot(cp.trans)
# plot(clip.map,add=TRUE)
# plot(tracks.sp.trans,pch=3,cex=.01,add=TRUE)
# plot(poly.npac.grid.trans,add=TRUE)
# map.axes()

#### REMOVE THE LAND FROM THE GRID
poly.npac.grid.trans$idx<-as.character(poly.npac.grid.trans$idx)
grid.clip <- gDifference(poly.npac.grid.trans,clip.map, byid=c(TRUE,FALSE),id=poly.npac.grid.trans$idx)
# plot(grid.clip)

#### get areas of grid cells
poly.areas.corrected<-as.data.frame(cbind(sapply(slot(grid.clip, "polygons"), slot, "ID"),sapply(slot(grid.clip, "polygons"), slot, "area")))

options(digits=14)
#poly.areas.corrected[,2]<-as.numeric(as.character(poly.areas.corrected[,2]))
poly.areas.corrected[,2]<-as.numeric(levels(poly.areas.corrected[,2])[poly.areas.corrected[,2]])

poly.areas.corrected$adjust<-poly.areas.corrected[,2]/max(poly.areas.corrected[,2])

#### read in the fisheries polygon (just the fisheries grid)
Fish_42.SPDF <- readOGR("/Users/henry/Documents/Work/Projects/BFAL NFWF Oikonos/Data/WCPFC/Fish_poly_42","Fish_poly_42") # clipper comes in as  GS84
proj4string(Fish_42.SPDF)<-CRS(projWant)

# plot results
plot(grid.clip,col="light blue")
plot(Fish_42.SPDF,col="light green")
plot(clip.map,add=TRUE,col="light grey")
plot(Redis.DF.sp,pch=3,cex=.01,add=TRUE, col="dark red")
map.axes()

########################################################################################################################
########################################################################################################################
########################################################################################################################
########################################################################################################################
#### Attribute track points points with polygons

# use over point.in.poly in spatialEco to attribute track points in polygons and Count all points from all birds for all years
library(spatialEco)
# joined<-as.data.table(point.in.poly(Redis.DF.sp,Fish_42.SPDF)@data)
track.pts.in.fish.cell<-as.data.frame((as.data.table(point.in.poly(Redis.DF.sp,Fish_42.SPDF)@data)) %>% group_by(gridcode) %>% summarise(count=n()))
track.pts.in.fish.cell<-rename(track.pts.in.fish.cell,sum.track.pts=count)

# add the counts back to the SpatialPolygonsDataFrame
Fish_42.track.pts.SPDF<-merge(Fish_42.SPDF, track.pts.in.fish.cell, by='gridcode', all = TRUE)
# recalssify gridcode
Fish_42.track.pts.SPDF@data$gridcode<-as.character(Fish_42.track.pts.SPDF@data$gridcode)

# create a data.frame that contains polygon information for plotting in ggplot
Fish_42.track.pts.SPDF.forplot = fortify(Fish_42.track.pts.SPDF,region="gridcode")

Fish_42.track.pts.SPDF.forplot =inner_join(Fish_42.track.pts.SPDF.forplot, Fish_42.track.pts.SPDF@data, by=c('id'='gridcode'))

ggplot(Fish_42.track.pts.SPDF.forplot) + 
  aes(long,lat,group=id,fill=sum.track.pts) + 
  geom_polygon() +
  geom_path(color="white") +
  coord_equal() +
  scale_fill_gradient(low='light blue', high='red')

########################################################################################################################
########################################################################################################################
########################################################################################################################
########################################################################################################################
############GET THE GRID  ##############################################################################################
# get grid point data (centered on pixels, even spacing)

longline.pts<-read.table ('/Users/henry/Documents/Work/Projects/USGS/USGS 08.10.15/Share 8.11.2015/ARCMAP/Fisheries/Longline/LONGLINE_00.csv',header=T, sep=",", strip.white=T)

#### subset point data to extent of interest (can redefine this from data at later point)
longline.pts<-longline.pts[(longline.pts$LAT_Cent>=15 & (longline.pts$LON_Cent>=130 | longline.pts$LON_Cent<=0)),]

#### create a grid field as longitude 180 to longitude 360
longline.pts$LON_Cent360<-longline.pts$LON_Cent
longline.pts$LON_Cent360[(longline.pts$LON_Cent360 < 0)] <- (longline.pts$LON_Cent360[(longline.pts$LON_Cent360 < 0)]) +360

#### select monts and years of fisheries data to use
# fm<-c(2,3,4,5,6); fy=c(2003:2012)

#### subset grid data by month and year
longline.pts<-longline.pts[(longline.pts$MM %in% fm)  & (longline.pts$YY %in% fy),]
longline.pts$cell_no <- as.character(paste(longline.pts$LAT_Cent,longline.pts$LON_Cent,sep="_"))

# library(dplyr)
# head(summarize(group_by(point.grid,cell_no),sum_HHOOKS=sum(HHOOKS,na.rm=TRUE)))

longline.pts.data_tbl<-tbl_df(longline.pts)
longline.pts.data_tbl_summ<-as.data.frame(longline.pts.data_tbl %>% group_by(cell_no) %>% summarise(SumHHOOKS=sum(HHOOKS,na.rm=TRUE),count=n(),meanHHOOKS=mean(HHOOKS,na.rm=TRUE)))

# merge the track count data with the fisheries HHOOOKS counts data
fish.pt.track.pt.data_tbl_summ<-merge(longline.pts.data_tbl_summ, Fish_42.track.pts.SPDF@data, by='cell_no', all=FALSE)

Fish_42.longline.track.forplot = fortify(Fish_42.track.pts.SPDF,region="gridcode")
Fish_42.longline.track.forplot =inner_join(Fish_42.longline.track.forplot, fish.pt.track.pt.data_tbl_summ, by=c('id'='gridcode'))
      
ggplot(Fish_42.longline.track.forplot) + 
  aes(long,lat,group=group,fill=SumHHOOKS) + 
  geom_polygon() +
  geom_path(color="white") +
  coord_equal() +
  scale_fill_gradient(low='light blue', high='red')

# set na to 0
fish.pt.track.pt.data_tbl_summ[is.na(fish.pt.track.pt.data_tbl_summ)] <- 0

fish.pt.track.pt.data_tbl_summ$perc.SumHHOOKS<-(fish.pt.track.pt.data_tbl_summ$SumHHOOKS/sum(fish.pt.track.pt.data_tbl_summ$SumHHOOKS))*100
fish.pt.track.pt.data_tbl_summ[,paste('DepID',meta$Deploy_ID,'perc.time.track.pts',sep='_')]<-(fish.pt.track.pt.data_tbl_summ$sum.track.pts/sum(fish.pt.track.pt.data_tbl_summ$sum.track.pts))*100
names(fish.pt.track.pt.data_tbl_summ)[names(fish.pt.track.pt.data_tbl_summ)=="sum.track.pts"] <- paste('DepID',meta$Deploy_ID,'sum.track.pts',sep='_')


file.name<-paste(dir.out,'fish.pt.track.pt',"_fm_",min(fm),"-",max(fm),"_fy_",min(fy),"-",max(fy),"_tm_",min(tm),"-",max(tm),"_ty_",min(ty),"-",max(ty),'.csv',sep = "")

####look for previously created .csv file 'tracksinpoly', if TRUE open it and add columns for poly and polyBuffer for which tracks are in and out
if (file.exists(file.name)) {
  out <- read.table(file.name,header=T, sep=",", strip.white=T)
  out2<-cbind(out,fish.pt.track.pt.data_tbl_summ[,c(paste('DepID',meta$Deploy_ID,"sum.track.pts",sep='_'),paste('DepID',meta$Deploy_ID,"perc.time.track.pts",sep='_'))])
  #### output filtered data for each birds
  write.table (out2, file.name, sep=",", quote=FALSE,col.names=TRUE, row.names=FALSE) 
} else {
  #### output filtered data for each birds
  write.table (fish.pt.track.pt.data_tbl_summ, file.name, sep=",", quote=FALSE,col.names=TRUE, row.names=FALSE,na="")
}

}



# spearman's rack
# all
library (pspearman)
out1 <- spearman.test(fish.pt.track.pt.data_tbl_summ$perc.SumHHOOKS,fish.pt.track.pt.data_tbl_summ$perc.time.track.pts, approximation="AS89")

ggplot(fish.pt.track.pt.data_tbl_summ, aes(Id, y = value, color = variable)) + 
  geom_point(aes(y = perc.time.track.pts*100000, col = "track point (*100000)")) +
  geom_point(aes(y = SumHHOOKS, col = "SumHHOOKS"))

################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################# DO THIS by making the fish.pt.track.pt.data_tbl_summ up above to include 
# check if this WORKS!!!

# Read in summary file
file.name<-paste(dir.out,'fish.pt.track.pt',"_fm_",min(fm),"-",max(fm),"_fy_",min(fy),"-",max(fy),"_tm_",min(tm),"-",max(tm),"_ty_",min(ty),"-",max(ty),'_w.sums.csv',sep = "")
x<- read.table(file.name,header=T, sep=",", strip.white=T)

out =inner_join(fish.pt.track.pt.data_tbl_summ, x, by='cell_no')
write.table (out, paste(dir.out,'all.fish.counts.csv',sep=''), sep=",", quote=FALSE,col.names=TRUE, row.names=FALSE,na="")


########################################################################################################################
########################################################################################################################
# export tabulated data as shapefile
# first join it to SPDF
fish.pt.track.pt.data_tbl_summ

join(Fish_42.SPD)


# get # indiv for All, Male, Fem, 2012, 2013

# Males<-2,5,7,8,14,16,17,18,21
male.count<-fish.pt.track.pt.data_tbl_summ[,c(1,11,13,17,19,31,35,37,39,45)]
male.count[male.count>0]=1
male.count<-tbl_df(male.count)
male.count<-as.data.frame(male.count %>% group_by(cell_no) %>% summarise(SumHHOOKS=sum(HHOOKS,na.rm=TRUE),count=n(),meanHHOOKS=mean(HHOOKS,na.rm=TRUE)))

# Females<-6,9,10,11,12,13,15,19,20
fem.count<-fish.pt.track.pt.data_tbl_summ[,c(1,15,21,23,25,27,29,33,41,43)]
fem.count[fem.count>0]=1


rowsum(fem.count,na.rm=FALSE)
















#### make the grid
# create index from point.grid
# point.grid$idx<-as.character(paste(point.grid$LAT_Cent,point.grid$LON_Cent360,sep="_"))
point.grid$idx<-with(point.grid,interaction(LAT_Cent,LON_Cent));

# us data.table package to summarize the data
point.grid.dt <- data.table(point.grid, key='idx')
point.grid.template<-as.data.frame(point.grid.dt[,list(LAT_Cent=unique(LAT_Cent),LON_Cent=unique(LON_Cent),sumHHOOKS=sum(HHOOKS),meanHHOOKS=mean(HHOOKS),medHHOOKS=median(HHOOKS),sdHHOOKS=sd(HHOOKS)),by='idx'])

point.grid.template[is.na(point.grid.template)] <- 0

#### convert point grid to spatial points data frame
point.grid.sp <- SpatialPointsDataFrame(coords = as.data.frame(point.grid.template[c("LON_Cent","LAT_Cent")]), data = as.data.frame(point.grid.template))
#### define projection, use the WGS84 projection that Argos Data is delivered in
point.grid.sp@proj4string <- CRS(proj_in)
plot(point.grid.sp, pch=3,cex=.5)


# 2. count all points from all birds all years

# 1. count all individual by year
# 1. count all individuals by all years



# 2. count all points by track

# 3. count all points by year

# 4. count all points by sex and year

# 5. count all points by all years




# join counts with correction factor
counts<-merge(counts, poly.areas.corrected, by.x = '.id', by.y = 'V1', all = TRUE)
counts[is.na(counts)]<-0

##### YOU ARE HERE!!!!####
#### merge the tables...
counts.annot<-merge(counts,(tracks[,c('idtrip','duration.hrs')]), by = 'idtrip', all.y = TRUE)


collapse tracks by unique(idtrip)

















#### get fishery grid point data
fish.point.df<-read.table ('/Users/henry/Documents/Work/Projects/USGS/USGS 08.10.15/Share 8.11.2015/ARCMAP/Fisheries/Longline/LONGLINE_00.csv',header=T, sep=",", strip.white=T)
# subset point data for extent of interest
fish.point.sub<-fish.point.df[(fish.point.df$LAT_Cent>=15 & (fish.point.df$LON_Cent>=135 | fish.point.df$LON_Cent<=0)),]

# convert grid into a spatial data frame
fish.point.sp.sub <- SpatialPointsDataFrame(coords = fish.point.sub[c("LON_Cent","LAT_Cent")], data = fish.point.sub)
# define projection, use the WGS84 projection that Argos Data is delivered in
fish.point.sp.sub@proj4string <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
gridded(fish.point.sp.sub)<-TRUE
plot(fish.point.sp.sub)

# transform data appropriate projection of the grid
fish.point.sp.sub.trans<-spTransform(fish.point.sp.sub, CRS(projWant))
gridded(fish.point.sp.sub.trans)<-TRUE
plot(fish.point.sp.sub.trans)


# convert grid into a spatial data frame
fish.point.sp <- SpatialPointsDataFrame(coords = fish.point.df[c("LON_Cent","LAT_Cent")], data = fish.point.df)
# define projection, use the WGS84 projection that Argos Data is delivered in
fish.point.sp@proj4string <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
# transform data appropriate projection of the grid
fish.point.sp.trans<-spTransform(fish.point.sp, CRS(paste(projWant))



poly.grid=as(SpatialPixelsDataFrame(fish.point.sp,fish.point.sp.sub.trans@data,tolerance=sqrt(.Machine$double.eps)),"SpatialPolygonsDataFrame")


# Create and project a grid for NPAC
# xlim<-c(17.5,67.5)
# ylim<-c(137.5,247.5)
grid.NPac<-expand.grid(x=seq(137.5,247.5,5),y=c(seq(17.5,67.5,5)))
dat<-seq(1:length(grid.NPac[,1]))
grid.NPac.sp <- SpatialPointsDataFrame(coords = grid.NPac[c("x","y")], data = grid.NPac)
# define projection, use the WGS84 projection that Argos Data is delivered in
grid.NPac.sp@proj4string <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
grid.NPac.sp.trans<-spTransform(grid.NPac.sp, CRS(paste(projWant))
# gridded(grid.NPac.sp.trans)<-TRUE

# Plot grid and tracks
plot(grid.NPac.sp.trans, axes=TRUE, col = 'green', pch=2, cex=.4)
plot(grid.sp.all.trans, add=TRUE, axes=TRUE, col = 'orange', pch=1, cex=.5)
plot(grid.sp.Pac.cent,add=TRUE, axes=TRUE, col = 'blue', pch=1, cex=.5)
plot(Redis.DF.sp, add=TRUE, axes=TRUE, pch=3,cex=.01)

poly.grid=as(SpatialPixelsDataFrame(grid.sp.Pac.cent,grid.sp.Pac.cent@data,tolerance=sqrt(.Machine$double.eps)),"SpatialPolygonsDataFrame")



point.in.polygon(point.x, point.y, pol.x, pol.y, mode.checked=False)

gridded(grid.sp.all.trans)<-TRUE
gridded(grid.sp.all)<-TRUE

# convert grid into a spatial data frame
grid.sp <- SpatialPointsDataFrame(coords = point.grid.sub[c("LON_Cent","LAT_Cent")], data = point.grid.sub)
# define projection, use the WGS84 projection that Argos Data is delivered in
grid.sp@proj4string <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")

poly.grid=as(SpatialPixelsDataFrame(grid.sp,grid.sp@data,tolerance=sqrt(.Machine$double.eps)),"SpatialPolygonsDataFrame")



#notes - correct by area!!!!
# raster mask, remember buffer by 0?












# Do 
# 2. combine redix points and the grid
# extract point from the grid





 
#### get map data
country.sp<-readOGR ('/Users/henry/Documents/Work/Projects/USGS/USGS 08.10.15/Share 8.11.2015/ARCMAP/World/','cntry92') # clipper comes in as unprojected WGS84
# define projection, import shape file in= WGS84 projection
country.sp@proj4string <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")


# plot(country.sp)
map(county.sp,center=180,col="gray",bg="gray",fill-TRUE,ylim=c(5,80,marc(0,0,0,0)))



plot(country.sp)






country.sp.trans@proj4string<-spTransform(country.sp, CRS(projWant))

library(ggmap)
map <- get_map(location = 'North Pacific Ocean', zoom = 2)
ggmap(map)

bbox<-c(-130,30,-105,50)
#locationWant<-c(lon=-178.2956,lat=28.39338)
mapWant<-get_map(location=bbox,source="stamen",maptype="watercolor",crop=FALSE)
ggmap(mapWant)

qmplot(x,y,data=Redis.DF,colour=I('red'),size=I(3),darken=.3)


############ you are here figuring out how to reproject a pacific centered map and then plot in gglot plot

### FIRST work to get SOSH going




data(world2MapEnv) #load in Pacific centric map data, low resolution
map('world2', xlim = c(135, 240), ylim=c(15,80),asp=1)

#Get world map info
world_map <- map_data("world")

world_mapt<-spTransform(world_map, CRS(projWant))

#Creat a base plot
p <- ggplot() + coord_map(xlim = c(135, 240), ylim=c(15,80))

#Add map to base plot
base_world <- p + geom_polygon(data=world_map,
                               aes(x=long,
                                   y=lat,
                                   group=group))
base_world


library(ggmap)
map <- get_map(location = 'Pacific Ocean', zoom = 2)
plot(map)

ggplot(map)+geom_map(map=map1) +
geom_point(grid.sp.Pac.cent, add=TRUE, axes=TRUE, col = '#008B00', pch=1, cex=.5)




  
  
  
  library(rworldmap)
  newmap <- getMap(resolution = "low")
  plot(newmap, xlim = c(40, -80), ylim = c(5,50), asp = 1)
  
  library(ggmap)
  map <- get_map(location = 'North Pacific Ocean', zoom = 2)
  plot(map)
  geom_point(grid.sp.Pac.cent, add=TRUE, axes=TRUE, col = '#008B00', pch=1, cex=.5)
  
  
  tracks.sp <- SpatialPolygonsDataFrame(coords = map1[c("x","y")], data = map1)
  
  grid.sp <- SpatialPolygonsDataFrame(coords = point.grid.sub[c("LON_Cent","LAT_Cent")], data = point.grid.sub)
  
  
  map.axes()
  
  library("ggmap")
  library(maptools)
  library(maps)
  
  library(maps)
  library(mapdata)
  library(mapproj)
  
  map("world2Hires", regions=mapnames$names[c(1:7,14:641)],
      xlim=c(120, 260), 
      ylim=c(-60, 40), 
      boundary=TRUE, 
      interior=TRUE,
      fill=TRUE
  )
  
  country.sp <- readOGR(dir.in.poly,'cntry92') # clipper comes in as unprojected WGS84
  # define projection, import shape file in= WGS84 projection
  country.sp@proj4string <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
  plot(country.sp)
  
  tracks.sp@coords[,1][tracks.sp@coords[,1]<0]=tracks.sp@coords[,1]+360
  
  data(world2MapEnv) #load in Pacific centric map data, low resolution
  map('world2', xlim = c(135, 240), ylim=c(15,80),asp=1)
  
  points(mapproject(y=tracks.sp@coords[,2], x=tracks.sp@coords[,1]), col = '#008B00', pch=1, cex=.5)
  


write.table(annotated.tracks, paste(dir.out,filename,'_trips_annoted.csv',sep = ""),sep=",",quote=FALSE,col.names=TRUE,row.names=FALSE)
