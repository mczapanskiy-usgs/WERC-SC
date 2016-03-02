

#### clear all
rm(list=ls())

# used
library(SDMTools)
library(adehabitatMA)
library(data.table)
library(adehabitatLT)
#library(tidyr)
library(plyr)

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

#### select species
species="BFAL"
#### select sex
#sex="M"
#### enter radius of data (in km)
rad=20
#### enter year data collected

# select a combination of months and years of fisheries data to use
#fm<-c(3); fy=c(2012)
fm<-c(2,3,4,5,6); fy=c(2003:2012)
# select a combination of months and years of track data to use
#tm<-c(3); ty=c(2012)
tm<-c(5,6); ty=c(2012,2013)
#### enter degrees for size of raster grid cells
cell.size.dd<-.25
#### number of mins to interpolate the point data to
no.mins<-20
#### define incoming projection
proj_in<-"+proj=longlat +ellps=WGS84 +datum=WGS84"


#### directory in
# dir.in <- 'D:/Share_Data/Tracking_Data/EOBS/All_tracks/'
dir.in <- '/Users/henry/Documents/Work/Projects/USGS/USGS 08.10.15/Share 8.11.2015/Tracking_Data/EOBS/All_tracks/'

#### dir.in.poly for loading polygon that can be used to clip data
dir.in.poly<-"/Users/henry/Documents/Work/Projects/USGS/USGS 08.10.15/Share 8.11.2015/ARCMAP/Land"

#### directory out
dir.out <- '/Users/henry/Documents/Work/Projects/USGS/USGS 08.10.15/Share 8.11.2015/Tracking_Data/EOBS/'

#### Get metadata
# meta<-read.table (paste('D:/Share_Data/GitHub/WERC-SC/trackcode/eobs/','metadata_EOBS.csv',sep = ""),header=T, sep=",", strip.white=T)
meta<-read.table (paste('/Users/henry/Documents/Work/Projects/USGS/USGS 08.10.15/Share 8.11.2015/GitHub/WERC-SC/trackcode/eobs/','metadata_EOBS.csv',sep = ""),header=T, sep=",", strip.white=T)
# subset metadata based on if sex and ty 
if (exists("sex")) {
  meta<-meta[meta$Species==species & meta$loc.data==1 & meta$Year %in% ty  & meta$Sex==sex,]
} else {
  # subset metadata
  if (exists("ty")) {
    meta<-meta[meta$Species==species & meta$loc.data==1 & meta$Year %in% ty,]
}}

########################
#### GET TRACK DATA
tracks<-read.table (paste(dir.in,species,'_',rad,'_trips_annotated.csv',sep = ""),header=T, sep=",", strip.white=T)

# tracks$Month <- tracks$UTC
# tracks$UTC<-as.POSIXct(strptime(tracks$UTC,"%Y-%m-%d %H:%M:%S"), tz="GMT")

#### subset track data
tracks<-tracks[tracks$species==species & tracks$Deploy_ID %in% meta$Deploy_ID, tracks$Deploy_ID %in% tm]

#### create tracks field as longitude 180 to longitude 360
tracks$longitude360<-tracks$longitude
tracks$longitude360[(tracks$longitude360 < 0)] <- (tracks$longitude360[(tracks$longitude360 < 0)])+360

#### define unit of sample (trips)
tracks$idtrip<-(tracks$Deploy_ID+(tracks$trip_no/100))
# tracks$idtrip<-with(tracks,interaction(Deploy_ID,trip_no));

#### define UTC as.POXISct
tracks$UTC<-as.POSIXct(strptime(tracks$UTC,"%Y-%m-%d %H:%M:%S"), tz="GMT")

#### check for duplicate times, if duplicate subtract second from first of the two timestamps
tracks$check<-rep(0,length(tracks$UTC))
for (i in 2:length(tracks$UTC)) {
  if (tracks$UTC[i]==tracks$UTC[i-1]) {
    tracks$check[i]=1
    tracks$UTC[i-1]=tracks$UTC[i-1]-1 }
  }

#### convert to spatial points data frame with id (c(deploy_id,trip)) and define projection
tracks.sp <- SpatialPointsDataFrame(coords = tracks[c("longitude360","latitude")], data = tracks)
# define projection, use the WGS84 projection that Argos Data is delivered in
tracks.sp@proj4string <- CRS(proj_in)
# plot(tracks.sp, pch=3,cex=.01)

#### good proj for center of pacific
projWant<-"+proj=aea +lat_1=30 +lat_2=70 +lat_0=52 +lon_0=-170 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

tracks.sp.trans<-spTransform(tracks.sp,CRS(projWant))
# plot(tracks.sp.trans, pch=3,cex=.01)

########################
#### CREATE REGULAR TRACKS

#### create track object for AdehabitatLT
trackObjs<-as.ltraj(tracks.sp.trans@coords[,c(2,1)], tracks.sp.trans$UTC, id=tracks.sp.trans$idtrip, burst = as.character(tracks.sp.trans$idtrip), typeII = TRUE)

#### 1. time interp the tracks using adehabitatMA
trackObjs2<-redisltraj(trackObjs, no.mins*60, burst = burst, samplex0 = TRUE, addbit = FALSE,
                       nnew = 5, type="time")

#### create data.table of the redistcretized trackObjs
# library(data.table)
tracks.rd<-cbind(data.table(ld(trackObjs2)))

#### rd = rediscretized tracks
tracks.sp.trans.rd <- SpatialPointsDataFrame(coords = cbind(tracks.rd$y,tracks.rd$x), data = tracks.rd)
tracks.sp.trans.rd@proj4string <- CRS(projWant)
# plot(tracks.sp.trans.rd, pch=3,cex=.01)

########################
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
# projection(poly.npac.grid)<-proj_in 
# plot(poly.npac.grid)

# check projections are the same
# proj4string(tracks.sp)
# proj4string(poly.npac.grid)
# points(npac.grid, pch=1,cex=.5)

################################################
#### GET AREA of each grid cell, corrected for area of grid cells that are land, could be used for standardization as probability of detecting bird in a grid cell is related to area of grid cell

# best projection for area of grid
projWant<-"+proj=aea +lat_1=30 +lat_2=70 +lat_0=52 +lon_0=-170 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

# transform grid polygon
poly.npac.grid.trans<-spTransform(poly.npac.grid, CRS(projWant))
# plot(poly.npac.grid.trans)

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
m <- readOGR(paste(dir.in.poly,"/gshhg-shp-2.3.4/GSHHS_shp/c",sep=""),clipperName) # clipper comes in as  GS84
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

poly.areas.corrected[,2]<-as.numeric(as.character(poly.areas.corrected[,2]))
poly.areas.corrected$adjust<-poly.areas.corrected[,2]/max(poly.areas.corrected[,2])

# plot results
plot(clip.map,col="light grey")
plot(grid.clip,add=TRUE,col="light blue")
plot(tracks.sp.trans.rd,pch=3,cex=.01,add=TRUE, col="dark red")
map.axes()

# Then do points in grid cell and export with layer names - recalling names cannot begin with a number so they become X...

################################################
#### COUNT POINTS IN CELLS
# use over function in raster for counting points in polygons over(SpatialPolygonsDataFrame, SpatialPointsDataFrame, returnList=TRUE)
joined<-(ldply(over(poly.npac.grid,tracks.sp,returnList=TRUE), rbind))
# head(joined)

# count the points in the polygons
counts<-count(joined, c(".id", "idtrip"))
counts<-counts[order(counts$idtrip),]
# head(counts)

# join counts with correction factor
counts<-merge(counts, poly.areas.corrected, by.x = '.id', by.y = 'V1', all = TRUE)
counts[is.na(counts)]<-0

##### YOU ARE HERE!!!!####
#### merge the tables...
counts.annot<-merge(counts,(tracks[,c('idtrip','duration.hrs')]), by = 'idtrip', all.y = TRUE)


collapse tracks by unique(idtrip)

################################################
#### 2. aggregate trips based on relative trip length, normalize cells by proportional track duration (also can use number of locs - as they are evenly spaced)


#### 3. multiply resulting abundances for each track by relative cell size (cell/max cell size)
####### YOU ARE HERE CONFIRM THAT THE IDXs LINE UP!!!!

counts.merge <- reshape(counts, direction="wide", idvar=".id", timevar="idtrip")
counts.merge$.id<-as.numeric(counts.merge$.id)
counts.merge<-counts.merge[order(counts.merge$.id),]
head(counts.merge)
counts.merge$.id<-as.numeric(counts.merge$.id)
adds<-as.data.frame(as.numeric(1:(max(dim(poly.npac.grid)))))
colnames(adds)<-"idx"
counts.mergeb<-merge(counts.merge,adds,by.x = '.id', by.y = 'idx',all.y=TRUE)
head(counts.mergeb)

# relabel columns on counts mergeb
# reorder joined
idtrip<-unique(joined$idtrip[order(joined$idtrip)])
colnames(counts.mergeb)<-c("id",as.character(idtrip))


counts.mergebl<-counts.mergeb[o,]

row.names(poly.npac.grid)<-as.character(poly.npac.grid$idx)
#row.names(counts.mergebl) <- poly.npac.grid$idx

poly.npac.grid@data=data.frame(poly.npac.grid@data,counts.mergeb[match(poly.npac.grid$idx, counts.mergeb$id),])

poly.npac.grid.merged <- spCbind(poly.npac.grid, counts.mergebl)
summary(poly.npac.grid.merged)
# spplot(poly.npac.grid.merged)



# reattach coordinates
cells.w.pts<-as.data.frame(raster.npac.grid@data@attributes)
grid.w.pts<-as.data.frame(npac.grid@data)

grid.w.pts$idxs<-1:length(grid.w.pts[,1])
head(grid.w.pts)

out<-merge(counts, grid.w.pts, by.x = '.id', by.y = 'idxs', all = TRUE)




########################
#### GET THE GRID
# get grid point data (centered on pixels, even spacing)
point.grid<-read.table ('/Users/henry/Documents/Work/Projects/USGS/USGS 08.10.15/Share 8.11.2015/ARCMAP/Fisheries/Longline/LONGLINE_00.csv',header=T, sep=",", strip.white=T)

#### subset point data to extent of interest (can redefine this from data at later point)
point.grid<-point.grid[(point.grid$LAT_Cent>=15 & (point.grid$LON_Cent>=130 | point.grid$LON_Cent<=0)),]

#### create a grid field as longitude 180 to longitude 360
point.grid$LON_Cent360<-point.grid$LON_Cent
point.grid$LON_Cent360[(point.grid$LON_Cent360 < 0)] <- (point.grid$LON_Cent360[(point.grid$LON_Cent360 < 0)]) +360

#### select monts and years of fisheries data to use
# m<-c(3); y=c(2012)
m<-c(3,4,5,6); y=c(2000:2012)

#### subset grid data by month and year
point.grid<-point.grid[(point.grid$MM %in% m)  & (point.grid$YY %in% y),]

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

####?????
# ?fish.coords.sp.trans<-spTransform(fish.coords.sp, CRS(projWant))

#### convert to spatial pixels
grid.spix <- SpatialPixelsDataFrame(point.grid.sp@coords, data=point.grid.sp@data)
grid.spix@proj4string <- CRS(proj_in)
image(grid.spix, 4, rev(heat.colors(100)))

grid.spix@data <- data.frame(grid.spix@data["sumHHOOKS"])
r <- raster(grid.spix)
# projection(r)<-"+init=epsg:4326" 
# CRS("+init=epsg:4326")

plot(r, rev(heat.colors(100)))

tracks.sp1<-tracks.sp
tracks.sp1@data<-tracks.sp1@data["idtrip"]
tracks.sp1@proj4string <- CRS(proj_in)

####
x<-count.points(tracks.sp, grid.spix)

#### try count.points for raster

B




########## NOTES
####for polgons see: http://gis.stackexchange.com/questions/110117/counts-the-number-of-points-in-a-polygon-in-r
# head(over(tracks.sp, grid.spix))


#### convert to raster
# get area (in m)



# count.points
# x<-count.points(tracks.sp.rediscr.trans, fish.grid.spix.trans)


# define a good equal area projection for the Central N Pacific Ocean
projWant<-"+proj=aea +lat_1=30 +lat_2=70 +lat_0=52 +lon_0=-170 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
# transform the track to projection defined for the Central N Pacific Ocean
track.sp_trans<-spTransform(tracks.sp, CRS(projWant))
######
plot(track.sp_trans, pch=3,cex=.01)

################# ################# ################# ################# ################# ################# 
################# recycling pile

# #### worldmap for plotting
# data(world2MapEnv)
# map('world2', xlim = c(125, 240), ylim = c(10, 70))
# map.axes()
