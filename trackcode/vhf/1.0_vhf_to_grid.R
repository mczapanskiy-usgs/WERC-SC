

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

species<-"MAMU"

dir.in <- "/Users/henry/Documents/Work/Projects/USGS/USGS 08.10.15/Share 8.11.2015/Tracking_Data/VHF/"
dir.out <- "/Users/henry/Documents/Work/Projects/USGS/USGS 08.10.15/Share 8.11.2015/Tracking_Data/VHF/"

#### dir.in.poly for loading polygon that can be used to clip data
dir.in.poly<-"/Users/henry/Documents/Work/Projects/USGS/USGS 08.10.15/Share 8.11.2015/ARCMAP/Land"

#### datafile to read
vhf <- read.table (paste(dir.in,"vhf_corr.csv",sep = ""),header=T, sep=",",strip.white=T)

#### read in metadata
meta<-read.table (paste(dir.in,"vhf_metadata.csv",sep = ""),header=T, sep=",",strip.white=T)

# select metadata
meta.want<-meta[meta$SPECIES==species,]

vhf.want<-vhf[vhf$VHF_Deploy_ID %in% meta.want$VHF_Deploy_ID,]

vhf.want.sp <- SpatialPointsDataFrame(coords = cbind(vhf.want$LONG,vhf.want$LAT),data<-vhf.want)

# define projection, use the WGS84 projection that Argos Data is delivered in
vhf.want.sp@proj4string <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")

plot(vhf.want.sp)

projWant <- "+proj=utm +zone=10 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
projWant <- "+init=epsg:32610"

vhf.want.sp.UTM<-spTransform(vhf.want.sp, CRS(projWant))

ext.WGS<-extent(vhf.want.sp)
ext<-extent(vhf.want.sp.UTM)

grid.cell.size<-9000

want.grid<-expand.grid(seq(from=ext@xmin-4*(grid.cell.size),to=ext@xmax+4*(grid.cell.size), by=grid.cell.size),seq(from=ext@ymin-4*(grid.cell.size),to=ext@ymax+4*(grid.cell.size), by=grid.cell.size))
colnames(want.grid)<-c("x","y")
want.grid$idx = as.factor(paste(want.grid$x,want.grid$y,sep="_"))

#### create spatial points data frame
coordinates(want.grid) <- ~ x + y
# coerce to SpatialPixelsDataFrame
gridded(want.grid) <- TRUE
want.grid@proj4string<- CRS(projWant)
# coerce to raster - grid@data@attributes = ID levels
raster.want.grid <- raster(want.grid)
points(vhf.want.sp.UTM, pch=3,cex=.01)

#### convert raster to spatial polygons
poly.want.grid<-rasterToPolygons(raster.want.grid, fun=NULL, n=4, na.rm=TRUE, digits=12, dissolve=FALSE)

#### get areas of grid cells
poly.want.grid.areas<-as.data.frame(cbind(sapply(slot(poly.want.grid, "polygons"), slot, "ID"),sapply(slot(poly.want.grid, "polygons"), slot, "area")))

poly.want.grid.areas[,2]<-as.numeric(as.character(poly.want.grid.areas[,2]))
poly.want.grid.areas$adjust<-poly.want.grid.areas[,2]/max(poly.want.grid.areas[,2])

#### get a map of the area in question
# load a clip ploygon with world land file
clipperName<-as.character("GSHHS_c_L1")
# read in polygon from GSHHS
m <- readOGR(paste(dir.in.poly,"/gshhg-shp-2.3.4/GSHHS_shp/c",sep=""),clipperName) # clipper comes in as  GS84
# m@proj4string<- CRS(proj_in)
# plot(m)

#projWant1<-"+proj=aea +lat_1=30 +lat_2=70 +lat_0=52 +lon_0=-170 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

#### get transformed extent of grid containing the data and define projection
ext.WGS.poly<-as(ext.WGS,"SpatialPolygons")
proj4string(ext.WGS.poly)<- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#### clip the map
clip.map <- gIntersection(m, ext.WGS.poly, byid=TRUE, drop_lower_td=TRUE)

# plot(clip.map)
#### fix topology again (fixes problem polygons using a 0 buffer)
clip.map <- gBuffer(clip.map, byid=TRUE, width=0)

clip.map.trans<-spTransform(clip.map, CRS(projWant))

plot(poly.want.grid)
points(vhf.want.sp.UTM, pch=3,col="blue",cex=.01)
lines(clip.map.trans,col="brown")

################################################
#### COUNT POINTS IN CELLS
# use over function in raster for counting points in polygons over(SpatialPolygonsDataFrame, SpatialPointsDataFrame, returnList=TRUE)
library(plyr)
joined<-(ldply(over(poly.want.grid,vhf.want.sp.UTM,returnList=TRUE), rbind))

counts<-count(joined, c(".id", "VHF_Deploy_ID"))
counts<-counts[order(counts$VHF_Deploy_ID),]

# join counts with correction factor
counts<-merge(counts, poly.want.grid.areas, by.x = '.id', by.y = 'V1', all = TRUE)
counts[is.na(counts)]<-0

#### merge the tables...
counts.annot<-merge(counts,(vhf.want[,c('VHF_Deploy_ID','VHF_Deploy_ID')]), by = 'VHF_Deploy_ID', all.y = TRUE)

collapse vhf.want by unique(VHF_Deploy_ID)