# FINISH ANNOTATing AND CLEAN THIS CODE


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





############
##  Breaks tracks that enter and exit a box into multiple segments based on time spent out of box in question
##  Bill Henry June 21, 2013
##
##  core function from package trip
## 
############

############
# user inputs:
#
# directories
# track with logic vector (0 & 1, in and out of given box)
# clipPolyList - file of specifics on each box
# the clipPolyList number of the polygon you wish to break trip on
# minGap
# 
############

rm(list=ls())

library(trip)

# in .csv files
#### dir.in
dir.in <- "D:/Share_Data/Tracking_Data/PTT/"

#### in polygon .shp file for setting raster grid extent
dir.in.poly <- "D:/Share_Data/Clip Polygons"

#### dir.out
dir.out <- "D:/Share_Data/Tracking_Data/PTT/"

#### dir.in of metadata
dir.in.meta <- "D:/Share_Data/GitHub/WERC-SC/trackcode/ptt/"

#### set species
species="SOSH"

#### set hrs for minimum gap in second (converted to sec with time gap used create new segment each time animal leaves and returns in to box)
hrs<-60
minGap<-3600 * hrs

#### read in list of potential clipper files
clipPolyList<-read.csv (paste(dir.in.poly,"/clipPolyList.csv", sep=""), header=T, sep=",", strip.white=T)
print(clipPolyList) # show a list of the clipper files

# select clipperfile
### uid
rno<-21 # row number of file list
clip.name<-(paste(as.character(clipPolyList$name[rno]),"Buffer",sep=""))
print(clip.name)

#### read in metadata
meta<-read.table (paste(dir.in.meta,"PTT_metadata_all.csv",sep = ""),header=T, sep=",", strip.white=T,na.strings = "")

#### select metadata want
meta<-meta[meta$species==species & meta$loc_data==1,]

ptt_deploy_ids<-meta$ptt_deploy_id

tracks <- read.table (paste(dir.in,species,"/3_Clipped/tracksinpoly.csv",sep = ""),header=T, sep=",", strip.white=T)

tracks$utc<-as.POSIXct(format(strptime(as.character(tracks$utc), "%Y-%m-%d %H:%M:%S"), "%Y-%m-%d %H:%M:%S"), tz = "GMT")

tracks.want<-subset(tracks,(tracks[,clip.name])==1)

tracks.want[,paste(clip.name,"_id2",sep="")]<-sepIdGaps(tracks.want$ptt_deploy_id, tracks.want$utc, minGap=minGap)
# head(tracks.want)
# tail(tracks.want)

tracks[,paste(clip.name,"_id2",sep="")]<-tracks[,clip.name]

#### create decimal number for deploy ids with more than one entry/exit of polygon
tracks.want[,paste(clip.name,"_id2",sep="")]<-as.numeric(sub("_", ".", tracks.want[,paste(clip.name,"_id2",sep="")]))

fixes <- match(tracks$ridx,tracks.want$ridx,nomatch=0)
tracks[,paste(clip.name,"_id2",sep="")][fixes!=0] <- as.numeric(tracks.want[,paste(clip.name,"_id2",sep="")][fixes])
# head(tracks)

####look for previously created .csv file 'tracksinpoly', if TRUE open it and add columns for poly and polyBuffer for which tracks are in and out
if (file.exists(paste(dir.out,species,"/3_Clipped/tracksinpoly.csv", sep =""))) {
  tracks.out2 <- read.table(paste(dir.out,species,"/3_Clipped/tracksinpoly.csv", sep =""),header=T, sep=",", strip.white=T)
  tracks.out2[,paste(clip.name,"_id2",sep="")]<-tracks[,paste(clip.name,"_id2",sep="")]
  #### output filtered data for each birds
  write.table (tracks.out2, paste(dir.out,species,"/3_Clipped/tracksinpoly.csv", sep =""), sep=",", quote=FALSE,col.names=TRUE, row.names=FALSE,na="") 
} else {
  #### output filtered data for each birds
  write.table (tracks, paste(dir.out,species,"/3_Freitas_Shapes/",year.id[yr],"tracksinpoly.csv", sep =""), sep=",", quote=FALSE,col.names=TRUE, row.names=FALSE,na="")
}

