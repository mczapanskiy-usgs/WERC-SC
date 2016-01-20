#################################
## Bill Henry  USGS WERC
## April 20, 2015
##
## For PTT Tracking data:
## 1. flags points a lying within a given polygon
## 
## Inputs
## dir.in: input directory
## dir.out: ouput directory
## indiv.shp.out (optional for shape file output)
## year.shp.out (optional for shape file output)
## species: AOU
## meta: metadata table
## clipPolyList: list of polygon and associated projection to use for clipping, polygons in WGS84 and transformed to appropriate projection in R
## .csv of "All_tracks_and_data_", essentially time/lat/long and filtering status (keeps = binary field)
##
## Outputs
## shapefiles of: individuals track
## shapefiles of: all tracks
## tracksinpoly.csv: annotated list of filtered track data with column for each polygon that holds
## vector with 1=in polygon, 0 = not in polygon
##
## * note cannot overwrite exiwsitng shapefiel with writeOGR, so if updating shapefiles - delete previous versions first
##
#################################

rm(list=ls())

library(sp)
library(maptools)
library(raster)
library(rgdal)
library(rgeos)
library(stringr)

# set directories

#### dir.in
dir.in <- "D:/Share_Data/Tracking_Data/PTT/"

#### dir.in.poly in polygon .shp file for clipping
dir.in.poly <- "D:/Share_Data/Clip Polygons"

#### dir.out for outputs
dir.out <- "D:/Share_Data/Tracking_Data/PTT/"

#### dir.in of metadata
dir.in.meta <- "D:/Share_Data/GitHub/WERC-SC/trackcode/ptt/"

#### set species AUO Code
species<-"NESH"

#### read in metadata
meta<-read.table (paste(dir.in.meta,"PTT_metadata_all.csv",sep = ""),header=T, sep=",", strip.white=T,na.strings = "")

#### select metadata want
meta<-meta[meta$species==species & meta$loc_data==1,]

#### set if you want to see plots on or off
plot <- "off"

#### set if you want individuals shape files exported on or off, note if shape in question already exists it cannot be overwritten and will generate an error
indiv.shp.out <- "off"

#### set if you want shape files for all tags for each year exported on or off, note if shape in question already exists it cannot be overwritten and will generate an error
year.shp.out <- "off"

#### Read in shapefile(s) for indexing data, files = (1) shapefile (2) shapefile with buffer
# lme for selecting data in a given lme
# lme buffer for selecting data that will be processed (rasterized), thus resulting file extent will
# excede boundaries of lme
# to get complete info on projection and fields in shapefile use: ogrInfo(dsn=dsn,layer='lmes.Buf100km')

# read in list of potential clipper files
clipPolyList<-read.csv (paste(dir.in.poly,"/clipPolyList.csv", sep=""), header=T, sep=",", strip.white=T)
print(clipPolyList) # show a list of the clipper files

#### select clipperfile
rno<-14 # row number of file list
clipper<-as.character(clipPolyList$clipFileName[rno])
clipperName<-as.character(clipPolyList$name[rno])

# read in polygon
clipper <- readOGR(dir.in.poly,clipper) # clipper comes in as unprojected WGS84
# read in projection best for selected polygon (contained in table clipPolyList)
projWant<-paste("+",as.character(clipPolyList$Proj4.specs[rno]),sep="")

# if polygon multipart, select polygon
if (clipPolyList$mult_polygons[rno]==1) {
  clipper<-subset(clipper,(clipper$LME_NAME)==as.character(clipPolyList$poly_want[rno]))
}

##
# print(proj4string(clipper))
# plot(clipper)

# get more summary info once shapefile is read summary(clipper)

# transform spatial polygons to projection appropriate for region in question (California Current in this case)
clipper_proj<-spTransform(clipper, CRS(projWant))

# plot(clipper_proj)

#### ui enter buffer distance
buffDist<-clipPolyList$buffer_to_km[rno]*1000 # convert km to m

# create buffer
clipperBuff_proj<-gBuffer(clipper_proj, byid=F, id=NULL, width=buffDist, quadsegs=5, capStyle="ROUND", joinStyle="ROUND", mitreLimit=1.0)

# print(proj4string(clipper))
# plot(clipperBuff_proj)
# plot(clipperBuff_proj, add=T, axes=T, border="gray")

## plot shape polygon for clipping
 if (plot=="on") {
  p1<- plot(clipper_proj, axes=T,  border="gray")
  p1<-p1 + plot(clipperBuff_proj, add=T)
  }

#### get deploy ids
ptt_deploy_ids<-meta$ptt_deploy_id

#### Get years from All_tracks_and_data_SPECIES file in the 2_SDA_Freitas_out directory
all_tracks <- read.table (paste(dir.in,species,"/2_SDA_Freitas_out/All_tracks_and_data_",species,".csv",sep = ""),header=T, sep=",", strip.white=T)

# i<-31
# for (i in 1:3) {
for (i in 1:length(ptt_deploy_ids)) {
  
print(c("track number",i,"of",length(ptt_deploy_ids)))

ptt_deploy_id <- ptt_deploy_ids[i]
year.id <- meta$year[i]
print(c("ptt_deploy_id",ptt_deploy_id))

track <- all_tracks[all_tracks$ptt_deploy_id==ptt_deploy_id,]
# print ('Total rows')
track.filts <- track[track$keeps==1,]
TrackLength<-length(track.filts[,1])
print(c("TrackLength",TrackLength))

# convert tracks into a spatial data frame
track.filts.sp <- SpatialPointsDataFrame(coords = track.filts[c("lon1","lat1")], data = track.filts)
# define projection, use the WGS84 projection that Argos Data is delivered in
track.filts.sp@proj4string <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
##
# summary(track.sp)
# head(track.sp)

##
# print(proj4string(track.sp))

# transform the track to projection defined for the polygon
track.filts.sp_proj<-spTransform(track.filts.sp, CRS(projWant))
##
# print(proj4string(track.sp_aea))

# if (plot=="on") {
#   # plot CCS LME and Track
#   plot(track.sp_aea, col = 'red', pch=1, cex=.5)
# }

if (plot=="on") {
  # plot CCS LME and Track
  plot(clipper_proj, axes=TRUE)
  plot(clipperBuff_proj, add=TRUE, axes=TRUE)
  plot(track.filts.sp_proj, add=TRUE, col = '#008B00', pch=1, cex=.5)
  }

#################################

# create a spatialpolygon from the spatialpolygonsdataframe
clipper_proj.sp <- as(clipper_proj, "SpatialPolygons")

#### Create index for row with data bounded by desired ploygon
track_in.polyBuffer <- over(track.filts.sp_proj, clipperBuff_proj)
track_in.poly <- over(track.filts.sp_proj, clipper_proj.sp)
##
# head(track_in.polyBuffer)

# replace NAs generate for points not in clipper and clipperBuff with 0
track_in.poly[is.na(track_in.poly)] <- 0
track_in.polyBuffer[is.na(track_in.polyBuffer)] <- 0

#### Add the spatially joined in.out points to the unprojected spatial data frame.
track.filts.sp$in_poly <- track_in.poly
track.filts.sp$in_polyBuffer <- track_in.polyBuffer

# replace col names require(stringr), note will replace all string in header(s) with clipperName!!
names(track.filts.sp@data) <- str_replace(string=names(track.filts.sp@data), pattern="in_poly", replacement=as.character(clipperName))

##
# head(track.sp)
# print(proj4string(track.sp))
track.filts.sp
#### do this again just for the plot - which uses aea
#### Add the spatially joined in.out points to the projected spatial data frame
track.filts.sp_proj$in_poly <- track_in.poly
track.filts.sp_proj$in_polyBuffer <- track_in.polyBuffer
# track.filts.sp_proj@data

#### optional plot track in 
if (plot=="on") {
  plot(clipper_proj, axes=TRUE)
  plot(clipperBuff_proj, add=T, axes=TRUE) 
  plot(subset(track.filts.sp_proj,track_in.polyBuffer==1), add=T, col = '#0000FF', pch=16, cex=.5)
  plot(subset(track.filts.sp_proj,track_in.polyBuffer==0), add=T, col = '#FFB90F', pch=16, cex=.5)
  plot(subset(track.filts.sp_proj,track_in.poly==1), add=T, col = '#0000FF', pch=16, cex=.5)
  plot(subset(track.filts.sp_proj,track_in.poly==0), add=T, col = '#FF3030', pch=16, cex=.5)
}

head(track.filts.sp_proj)

# replace col names require(stringr)
names(track.filts.sp_proj@data) <- str_replace(string=names(track.filts.sp_proj@data), pattern="in_poly", replacement=as.character(clipperName))

##
# head(track.sp_aea)
# print(proj4string(track.sp_aea))

#### optional create shapefile of Freitas filtered data for each individual track
if (indiv.shp.out=="on") {
  writeOGR(track.filt.sp,paste(dir.out,species,"/3_Clipped/Shape_out/", sep =""),paste(bird.id,'_Freitas_in_,',clipperName,'.csv', sep = ""),overwrite_layer='T', driver="ESRI Shapefile")
}

#### concatenate each track into spatial dataframe (shapefile)
if (i==1) {
  tracks.filts.sp<-track.filts.sp
  }else{
    tracks.filts.sp<-rbind(tracks.filts.sp, track.filts.sp)
  }

# ls()
rm(track, track.filts, track.filts.sp, track.filts.sp_proj, track.filts.sp_proj, TrackLength)
}

# create output directory, will not replace if already exists
dir.create(file.path(paste(dir.out,species,"/",sep=""),"3_Clipped"),showWarnings=TRUE)

#### export .shp with tracking data for all bird.ids for each year
if (year.shp.out=="on") {
  writeOGR(tracks.filts.sp,paste(dir.out,species,"/3_Clipped/Shape_out/", sep = ""),paste(species,'all_pts_Freitas_in',clipperName, sep = ""),overwrite_layer='T', driver="ESRI Shapefile")
}

# create row index
ridx<-seq(from = 1, to = length(tracks.filts.sp), by = 1)

tracks.out<-cbind(ridx, tracks.filts.sp@data)

####look for previously created .csv file 'tracksinpoly', if TRUE open it and add columns for poly and polyBuffer for which tracks are in and out
if (file.exists(paste(dir.out,species,"/3_Clipped/tracksinpoly.csv", sep =""))) {
  tracks.out2 <- read.table(paste(dir.out,species,"/3_Clipped/tracksinpoly.csv", sep =""),header=T, sep=",", strip.white=T)
  tracks.out2[clipperName]<-tracks.out[clipperName]
  tracks.out2[paste(clipperName,"Buffer",sep="")]<-tracks.out[paste(clipperName,"Buffer",sep="")]
    #### output filtered data for each birds
  write.table (tracks.out2, paste(dir.out,species,"/3_Clipped/tracksinpoly.csv", sep =""), sep=",", quote=FALSE,col.names=TRUE, row.names=FALSE,na="") 
  } else {
  #### output filtered data for each birds
  write.table (tracks.out, paste(dir.out,species,"/3_Clipped/tracksinpoly.csv", sep =""), sep=",", quote=FALSE,col.names=TRUE, row.names=FALSE,na="")
  }

# END
# ===========================================================================================================

