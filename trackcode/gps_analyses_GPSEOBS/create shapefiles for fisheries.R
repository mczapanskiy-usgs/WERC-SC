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


#### define incoming projection
proj_in<-"+proj=longlat +ellps=WGS84 +datum=WGS84"

###### directory for desired projection
dir.in.poly <- "/Users/henry/Documents/Work/Projects/USGS/USGS 08.10.15/Share 8.11.2015/Clip Polygons/"

# ###### directory for data
# dir.in.points <- "/Users/henry/Documents/Work/Projects/USGS/USGS 08.10.15/Share 8.11.2015/ARCMAP/Fisheries/Longline/"
# 
# WCPFC.pts<-read.csv (paste(dir.in.points,"LONGLINE_00.csv", sep=""), header=T, sep=",", strip.white=T)
# 
# WCPFC.pts$cell_no<-paste(WCPFC.pts$LAT_Cent,WCPFC.pts$LON_Cent,sep="_")
# 
# unique(WCPFC.pts$cell_no)

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
projWant<-"+proj=aea +lat_1=30 +lat_2=70 +lat_0=52 +lon_0=170 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

#### grid is centered on points ("from" == center of grid cells), 'by' = degrees (size of grid cells)
npac.grid<-expand.grid(seq(from=132.5,to=207.5, by=5),seq(from=17.5,to=57.5, by=5))
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
# Plot grid and tracks

plot(poly.npac.grid)

# transform grid polygon
poly.npac.grid.trans<-spTransform(poly.npac.grid, CRS(projWant))

# Plot grid and tracks
plot(poly.npac.grid.trans)



plot(Redis.DF.sp, add=TRUE, axes=TRUE, pch=3,cex=.01)
