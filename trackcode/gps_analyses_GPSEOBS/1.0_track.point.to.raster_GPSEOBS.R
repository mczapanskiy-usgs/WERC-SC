#############################################
# EBOS point to raster
# Bill Henry USGS
# July 29, 2015
#
# interpolates point data from GPS at even intervals
# converts data to raster
#
#
#
#
#
#
#############################################



#### clear all
rm(list=ls())


library(SDMTools)
library(adehabitat)

library(proj4)
library(ggplot2)
library(sp)
library(maptools)
library(raster)
library(rgdal)
library(rgeos)
library(stringr)
        

#### select species
species="BFAL"

#### enter radius of data (in km)
r=5

#### directory in
dir.in <- 'D:/Share_Data/Tracking_Data/EOBS/All_tracks/'

####
dir.out <- 'D:/Share_Data/Tracking_Data/EOBS/All_tracks/'

#### directory for world background to read in
dir.in.poly <- 'D:/Share_Data/ARCMAP/World'

# Get metadata
meta<-read.table (paste('D:/Share_Data/GitHub/WERC-SC/trackcode/eobs/','metadata_EOBS.csv',sep = ""),header=T, sep=",", strip.white=T)
# subset metadata
meta<-meta[meta$Species==species & meta$loc.data==1,]

# get track data
tracks<-read.table (paste(dir.in,species,'_',r,'_trips_annotated.csv',sep = ""),header=T, sep=",", strip.white=T)
# subset track data
tracks<-tracks[tracks$species==species,]
# convert tracks into a spatial data frame
tracks.sp <- SpatialPointsDataFrame(coords = tracks[c("longitude","latitude")], data = tracks)
# define projection, use the WGS84 projection that Argos Data is delivered in
tracks.sp@proj4string <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")

# get grid point data
point.grid<-read.table ('D:/Share_Data/ARCMAP/Fisheries/Longline/LONGLINE_00.csv',header=T, sep=",", strip.white=T)
# subset point data
point.grid.sub<-point.grid[(point.grid$LAT_Cent>=15 & (point.grid$LON_Cent>=145 | point.grid$LON_Cent<=0)),]
# convert grid into a spatial data frame
grid.sp <- SpatialPointsDataFrame(coords = point.grid.sub[c("LON_Cent","LAT_Cent")], data = point.grid.sub)
# define projection, use the WGS84 projection that Argos Data is delivered in
grid.sp@proj4string <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")

projWant <- paste("+","proj=tmerc +lat_0=21.833 +lon_0=-185 +k=0.99999 +x_0=500000 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs",sep="")

# transform data ppropriate projection for data
# grid.sp.Pac.cent<-recenter(grid.sp)
tracks.sp.Pac.cent<-spTransform(tracks.sp, CRS(projWant))
# grid.sp.Pac.cent<-recenter(grid.sp)
grid.sp.Pac.cent<-spTransform(grid.sp, CRS(projWant))

# Plot tracks
plot(tracks.sp.Pac.cent, pch=3,cex=.01)
plot(grid.sp.Pac.cent, add=TRUE, axes=TRUE, col = '#008B00', pch=1, cex=.5)
  


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
  plot(newmap, xlim = c(140, -160), ylim = c(15, 75), asp = 1)
  
  library(ggmap)
  map <- get_map(location = 'Pacific Ocean', zoom = 2)
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
