#################
##
## Calculate Brownian Bridge Kernel Space
##
## core function package adehabitat
##
## Bill Henry June 21, 2013
##
## updates
## July 16, 2013
##    add functionality to clip data with polygon that disctates kerneled data, bb is run only on those segments of track that
##      fall within polygon
##
##  Output Destination (next processing step to follow after running this code)
##    sum of segments based on weighted sum using number days tracked within polygon
##
##  fixes/work needed
##    July 16, 2003 option to run all data
##
#################

#################
#
# user inputs
#
# directories
# track with logic vector (0 & 1, in and out of given box and optional track.id based on tracks that go in and out of clipping box)
# clipPolyList - file of specifics on each box - used to get extents for raster
# params params$vmax=max velocity of bird taked (from Spear and Ainley) FilterParametersUsed_ table to get vmax for the species
# the clipPolyList number of the polygon you wish to break trip on
# countour = the maximum contour to return, use 99.999 for 100 ud contours
# cellsize<-  related to error tags, in m
# species<- AOU code for the species you are running (eg SOSH)
# rno = row number to select data from the clipPOlyList,
# minNo<- minimum number of point to run for the bb - important with small clip areas where tracks are cut up when animal enters and leaves a box
# id.2 = taken from clipPolyList, 0=bird.id (run entire ptt) or 1=clip.name_id2 (run segments of track that are in box then sum them based on number of days tracked)
#
#################
#
# outputs
# .png and asci file of uds
# .csv file with summary of tracking data used to make .asci and .png including # hrs tracked
#
#
#
#################

rm(list=ls())

library(sp)
library(maptools)
library(raster)
library(rgdal)
library(proj4)
library(ggplot2)
library(rgeos)
library(SDMTools)

library(adehabitat)

## enter years here
## 
#   year <- c(2008)			
    
##
# id.out = c("99999")    # = c("68019a","68022a3")     #to exclude birds or segments "99999" excludes none

# set directories

####  dir.in for .csv files
dir.in <- "D:/Share_Data/Tracking_Data/PTT/"

#### dir.out for .csv files
dir.out <- "D:/Share_Data/Tracking_Data/PTT/"

####  in polygon .shp file for setting raster grid extent
dir.in.poly <- "D:/Share_Data/Clip Polygons"

#### dir.in of metadata
dir.in.meta <- "D:/Share_Data/GitHub/WERC-SC/trackcode/ptt/"

####  in polygon .shp file for setting raster grid extent
dir.in.params <- "D:/Share_Data/Tracking_Data/PTT/"

# out direcvtory for .png and .asc
### dir.in.poly <- "/Users/henry/Documents/Work/Projects/CA_Seabird_ALTAS/Josh Method/SOSH/LMES/"
dir.out.rast <- "D:/Share_Data/Tracking_Data/PTT/"

# set species
species="COMU"

#### Get years from file names in the 3_Freitas_Shapes directory
#year.site.id<-as.numeric(na.omit(list.files(paste("D:/RWH/CA Atlas/",species,"/3_Freitas_Shapes/",sep = ""), include.dirs = TRUE)))
#year.site.id<-year.site.id[!is.na(year.site.id)] 

# or select independently
## year.site.id<-c(2007, 2008, 2009)

#### read in metadata
meta<-read.table (paste(dir.in.meta,"PTT_metadata_all.csv",sep = ""),header=T, sep=",", strip.white=T,na.strings = "")

#### select metadata want
meta<-meta[meta$species==species & meta$loc_data==1,]
year.id<-unique(meta$year)
site.id<-unique(meta$year)
year.site.id<-unique(paste(meta$year,"_",meta$site_abbrev,sep=""))
deploy_id<-meta$ptt_deploy_id
  
#### set raster and contour parameters
cellsize <- 3000 # in m, this should be related to location error, this can be the mean error for all
# birds where error for each bird = average error of all location classes
# weighted by number of locations in each location class (Ex. 2008 SOSH was 2.98km)
contour <- 99.999  # 1 through 100

## id<-1 # for length deploy_id
# for (id in 1:length(deploy_id)) {			

# get speed value used in Frietas SDA filter
params <- read.table(paste(dir.in.params,species,"/2_SDA_Freitas_out/FilterParametersUsed_",species,"_ref.csv",sep=""),header=T,sep=",",strip.white=T)
speed=params$vmax
rm(params)  

## read in all tracking data
ptt <- read.table (paste(dir.in,species,"/3_Clipped/tracksinpoly.csv",sep = ""),header=T, sep=",", strip.white=T)
# head(ptt)

## set user input minimum number of locations to allow to render the BB model
minNo<-2

# read in list of potential clipper files
clipPolyList<-read.csv (paste(dir.in.poly,"/clipPolyList.csv", sep=""), header=T, sep=",", strip.white=T)
print(clipPolyList) # show a list of the clipper files

#### select clipperfile
#### ui set rno for clipPolyList
rno<-19 # row number of file list
clipper<-as.character(clipPolyList$clipFileName[rno])
clipperName<-as.character(clipPolyList$name[rno])

#### remove tracks with fewer than 10 observations
pttIDFreq<-as.data.frame(table(ptt[,paste(clipperName,'Buffer_id2',sep='')]))
# pttIDFreq
pttIDFreq<-pttIDFreq[(pttIDFreq$Var1)!=0 & pttIDFreq$Freq>minNo,]
print(pttIDFreq)

#### tally number of relocations
pttIDFreq.out<-as.data.frame(table(floor(ptt[,paste(clipperName,'Buffer_id2',sep='')])))
pttIDFreq.out<-pttIDFreq.out[(pttIDFreq.out$Var1)!=0 & pttIDFreq.out$Freq>minNo,]
print(pttIDFreq.out)

# subset orginal ptt to those Buffer_id1 w/ > minNo obs
ptt <- ptt[ptt[,paste(clipperName,'Buffer_id2',sep='')] %in% pttIDFreq$Var1,]

# determine if bounding polygon breaks tracks into secondary segments (due to birds that leave polygon for time >t as defined in previous code break by time after point in Polygon)
id_2<-clipPolyList$id_2[rno]

# read in polygon, set to projection, shapefiles should all come in as WGS84
clipper <- readOGR(dir.in.poly,clipper) # clipper comes in as unprojected WGS84

################# check this next string as you may not need to run!!!!!!
# clipper@proj4string <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
# plot(clipper, axes=T)

# if polygon multipart, select polygon
if (clipPolyList$mult_polygons[rno]==1) {
  clipper<-subset(clipper,(clipper@data[,grep("*NAME",colnames(clipper@data))])==as.character(clipPolyList$poly_want[rno]))
} else {
  clipper@data$NAME<-(as.character(clipPolyList$poly_want[rno]))
}
# plot(clipper, axes=T)

# read in projection best for selected polygon (contained in table clipPolyList)
projWant<-as.character(clipPolyList$Proj4.specs[rno])
# for USGS Albers Equal Area projWant<-"+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23.0 +lon_0=-96.0 +x_0=0.0 +y_0=0.0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs "
# for CCS LME projWant<-"+proj=aea +lat_1=30 +lat_2=50 +lat_0=40 +lon_0=-125 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
# we're using ccs_lme projection

# transform spatial polygons to projection appropriate for region in question (California Current in this case)
clipper_proj<-spTransform(clipper, CRS(paste("+",projWant,sep="")))
# plot(clipper_proj, axes=T)

# get buffer distance
buffDist<-clipPolyList$buffer_to_km[rno]*1000 # note convert buffer dist from km to m

# create buffer
clipperBuff_proj<-gBuffer(clipper_proj, byid=F, id=NULL, width=buffDist, quadsegs=5, capStyle="ROUND", joinStyle="ROUND", mitreLimit=1.0)
# print(proj4string(clipper))
# plot(clipper_proj,axes=T)
# plot(clipperBuff_proj, add=T, border="gray")

# get extent of shapefile in question
ext<-extent(clipperBuff_proj[1,1])
# plot(ext, add=T)

# subset tracking data contained in buffer
ptt<-subset(ptt[ptt[paste(clipperName,"Buffer",sep="")]==1,])
# head(ptt)

#### convert ptt data to spatial, points are brought in WGS84
tracks.sp <- SpatialPointsDataFrame(coords = ptt[c("lon1","lat1")], data = data.frame(utc = ptt$utc))
# define projection, use the WGS84 projection that Argos Data is delivered in
tracks.sp@proj4string <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
##
# head(tracks.sp)

# transform the track to CCS LME preferred projection
tracks.sp_proj<-spTransform(tracks.sp, CRS(paste("+",projWant,sep="")))
#   plot(tracks.sp_proj, add=TRUE, col = '#008B00', pch=1, cex=.5)

##
# # plot(tracks.sp_aea, axes=TRUE)
# # head(tracks.sp_aea)
# min(tracks.sp_aea@coords[,1])
# max(tracks.sp_aea@coords[,1])
# min(tracks.sp_aea@coords[,2])
# max(tracks.sp_aea@coords[,2])
 
# plot(CCS.lme.Buf100km_aea, axes=TRUE)
# plot(tracks.sp_aea, add=TRUE)

#                                  "\\Projection_corrected\\HAPE_LANAI_", year[yr.site],"_SDA_Mollyweide.csv", sep = ""), 
#                                      header=T, sep=",", na.string="", strip.white=T)

# select relevant data for kernel density analysis
date_time <- as.POSIXct(strptime (tracks.sp_proj@data$utc, "%Y-%m-%d %H:%M:%S"), "GMT")
loc       <- tracks.sp_proj@coords    # where _projection is aea
# head(tracks.sp_aea@coords)

if (id_2==0) {
  id<- ptt$ptt_deploy_id
  # head(ptt)
} else {
  id<- ptt[,paste(clipperName,'Buffer_id2',sep='')]
}

# check.n<-as.data.frame(matrix(0,ncol=2,nrow=length(unique(id))))
# for (n in 1:length(unique(id))) {
#   check.n[n,1]=length(ptt[id %in% unique(id)[n],"utc"])
#   check.n[n,2]=length(unique(ptt[id %in% unique(id)[n],"utc"]))
# }

# CREATE TRACKS USING YOUR TIME AND LOCATION DATA FOR KERNELBB ANALYSIS
track <- as.ltraj(loc, date_time, id, burst = id, typeII = TRUE)

track.all<-summary(track)
track.all$id<-as.numeric(as.vector.factor(track.all$id))
track.all<-track.all[order(track.all$id),]
days<-(as.numeric(track.all$date.end-track.all$date.begin))/24
#pttIDFreq<-as.data.frame(pttIDFreq)
tracksums.out<-cbind(track.all, days)
tracksums.out$deploy_id<-floor(tracksums.out$id)
tracksums.out<- merge(tracksums.out, meta, by.x = "deploy_id", by.y = "ptt_deploy_id" )  

#d<-as.data.frame(matrix(pttIDFreq))
#clipper<-subset(clipper,(clipper@data[,grep("*NAME",colnames(clipper@data))])==as.character(clipPolyList$poly_want[rno]))

# to create 9-km grid for kernel analysis using the maximum extent of your location data
#rast <- ascgen(loc, cellsize = 9000)
    
# to create 9-km grid using your own grid extent

#### come up with fixed extents for our areas, could make an option .txt file to be stored in file directory and loaded at beginning of this code
     
#grid.lon <- c(INSERT MINLON, MINLON, MAXLON, MAXLON)
#grid.lat <- c(INSERT MAXLAT, MINLAT, MINLAT, MAXLAT)
    grid.lon <- c(ext@xmin, ext@xmin, ext@xmax, ext@xmax)
    grid.lat <- c(ext@ymax, ext@ymin, ext@ymin, ext@ymax)
    grid.loc <- cbind(grid.lon, grid.lat)

#### set parameters done above

#   cellsize <- 3000 # this should be related to location error, this can be the mean error for all
#      # birds where error for each bird = average error of all location classes
#      # weighted by number of locations in each location class (Ex. 2008 SOSH was 2.98km)
#   contour <-100  # 1 through 100
       
    # create grid in km for kernel analysis, this will be speific for each area

    rast <- ascgen(grid.loc, cellsize)
# plot(rast)  
# plot(clipper_proj, add=T)
# plot(clipperBuff_proj, add=T, border="gray")
# plot(ext, add=T)

   # THIS SECTION WAS INTENDED TO CALCULATE SIG1 WITH FINAL STEP CALCULATING UDBB 
   # USING THE MEAN SIG1 ACROSS FOR ALL INDIVIDUALS
    #blah <- liker(track, sig2 = 27000, rangesig1 = c(0, 30), byburst = T, plot=F)
    #id.sig1 <- numeric(length=32) 
    #for (i in 1:32) id.sig1[i] <- blah[[i]]$sig1
    #sig1 <- mean (id.sig1)
    #bb <- kernelbb(track, 16.4, 27000, grid = rast, byburst=T)
    
   # CALCULATE UDBB WITH YOUR SPECIFIED SPEED AND SMOOTHING TERMS 
  bb <- kernelbb(track, speed, cellsize, grid = rast, byburst=T)  ## speed in m/s use same for each species and MATCH what you'be done for SDA, this possibly done b/c BB has prefilter based on speed, we've already done speed distance and angle
#  bb <- kernelbb(track[1,],speed, cellsize, grid = rast, byburst=F)  ## speed in m/s use same for each species and MATCH what you'be done for SDA, this possibly done b/c BB has prefilter based on speed, we've already done speed distance and angle
  bbvol = getvolumeUD (bb)
    ## Create a list of raster maps containing ud if the pixel is inside the100% home range and 0 otherwise
  ## NOTE: ud estimates have been scaled by multiplying each value by 9000^2 to make prob vol that sums to 1.00 accross pixel space
  
    # create UD clipping function to extract the UD within the 100% contour (which can be varied)

# create output directory, will not replace if already exists
dir.create(file.path(paste(dir.out,species,"/",sep=""),"4_BB_out"),showWarnings=TRUE)
#dir.create(file.path(paste(dir.out,species,"/4_BB_out/",sep="")),showWarnings=TRUE)

# export track summary data
write.table(as.data.frame(tracksums.out), paste(dir.out,species,"/4_BB_out/","tracksums_",species,"_BB_out_",clipperName,".csv", sep = ""), sep=",", quote=FALSE,col.names=TRUE, row.names=FALSE,na="")
            
    ## set mapcont to mapcontour
    mapcont <- function (x,y) {x[y<contour]<- x[y<=contour] * cellsize^2; x[y>contour]<-0; return(x)}   # set cellsize        
    # extract data for each bird
     tag <- names (bb)
  ## create a list of maps of Utilization Distribution
 udmap <-lapply(bb, function(x) x$UD)
 vmap <-lapply(bbvol, function(x) x$UD)                      
       # export ASCII files for ArcMap
      for (i in 1:length(tag)) {
      temp=mapcont(udmap[[i]],vmap[[i]])
      
      # directories business
      raster.dir.out<-paste(dir.out,species,"/4_BB_out/", sep = "")
      file.out<-paste(species,"_BB_out_",clipperName,"_",contour,"_",tag[i],".asc",sep="")

      export.asc(temp, paste(raster.dir.out,file.out,sep=""))            
      
      ####
      # map <- import.asc( paste(raster.dir.out,file.out,sep=""))
      # map.rst<-raster.from.asc(map, projs = paste("+",projWant,sep=""))
#       
#       proj4string(map.rst) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#       
#       hab <- asc2spixdf(map.rst) 
#       
#       
#        projection(map) <- paste("+",projWant,sep="")
#        export.asc(map, file.out)
#        
#        write.asc(map, file.out)
#               
#        
#        write.asc(temp, file.out, gz = FALSE)
#        
#        crs(map) <- paste("+",projWant,sep="")
#        write.asc(map, file.out)
#        
#        temp1<-asc2spixdf(read.asc(file.out, gz = FALSE))
#        proj4string(temp1)<-paste("+",projWant,sep="")
#        write.asc(temp1, file.out, gz = FALSE)
#        
#        > r1 <- raster(ncol=104, nrow=179)
#        > r2 <- r1
#        > r1[] <- runif(ncell(r1))
#        > r2[] <- runif(ncell(r1))
#        > s <- stack(r1, r2)
#        > sgdf <- as(s, 'SpatialGridDataFrame')
#        > newr2 <- raster(sgdf, 2)
#        
       
       
       # export.asc(temp, paste(dir.out,species,"/4_BB_out/",species,"_",year.site.id[yr.site],"_BB_out_",clipperName,"_",contour,"_",tag[i], sep = "")) ####set out file to contour concatenate this filename
       # writeRaster(temp, as.character(paste("D:/RWH/CA Atlas/",species,"/4_BB/",year.site.id[yr.site],"/",species,"_BB_out_",clipperName,"_",contour,"_",tag[i],".grd", sep = "")),format="ascii") ####set out file to contour concatenate this filename
       #export.asc(temp, paste("D:/RWH/CA Atlas/LMESBB_out_",contour,"_", tag[i], sep = "")) ####set out file to contour concatenate this filename

######## KEEP THIS SECTION
print (tag[i])
print (sum(temp))
#### if you want to save an image of each ud use the following code  ### The way this is coded it writes the full BBUD (not the 25, or 50...specified)
#   {png(file= paste(dir.out,species,"/4_BB_out/",species,"_BB_out_",clipperName,"_",contour,"_",tag[i],".png",sep = "")) ### double check outfile name
#      #image(bb[[i]]$UD, col=c("light grey", topo.colors(40)))
#      image(temp, col=c("light grey", topo.colors(40)))
#      dev.off()
#    }
}

# rm(track.all,hours,pttIDFreq,tracksums.out,tag,temp,mapcont,bb,bbvol)


# ####
#      i=1
# #     for (i in 1:length(tag)) {
#        temp=mapcont(udmap[[i]],vmap[[i]])
#        export.asc(temp, paste("D:/RWH/CA Atlas/",species,"/4_BB/",species,"_BB_out_",contour,"_",tag[i], sep = "")) ####set out file to contour concatenate this filename
#        #export.asc(temp, paste("D:/RWH/CA Atlas/LMESBB_out_",contour,"_", tag[i], sep = "")) ####set out file to contour concatenate this filename
#        print (tag[i])
#        print (sum(temp))
#        # if you want to save an image of each ud use the following code  ### The way this is coded it writes the full BBUD (not the 25, or 50...specified)    
# {
#          png(file= paste("D:/RWH/CA Atlas/",species,"/4_BB/",species,"_BB_out_",contour,"_",tag[i],".png",sep = "")) ### double check outfile name
#          #image(bb[[i]]$UD, col=c("light grey", topo.colors(40)))
#          image(temp, col=c("light grey", topo.colors(40)))
#          image(temp, col=c("light grey", topo.colors(40)))
#          
#          dev.off()  
#          
#          }
#          
#       
#          ###### I would like to add code to SUM the individual BBUDs  & export of summed .asc---- as was done in the original Calculating UD estimates.r script
#       # sum UD estimates for each tag in that year. I assume I can tweak the individuals in summed UD and downweighted index hallet.r scripts to 
#       # work with these BBUDs too.
# 
#    }}
#   
# bbox(CCS.lme.Buf100km_aea)
# bbox(temp)
# projection(temp)<-projWant
# plot(temp, col=c("light grey", topo.colors(40)))
# plot(CCS.lme.Buf100km_aea, col=NA, add=TRUE)