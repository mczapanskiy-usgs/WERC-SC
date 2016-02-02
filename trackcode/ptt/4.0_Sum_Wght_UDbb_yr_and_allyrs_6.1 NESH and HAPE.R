
############################################################################################################################################################
#####
#####   This code sums up kernel distribution by year or other defined aggregating variable
#####   
#####   7/16/13 Brownian Bridge run seperately on track segments found within clipper
#####   7/16/13 individual Brownian Bridge results are summed together for each year and are weighted on the number of tracked idividuals
#####   7/22/13 added functionality to sum all years of tracking and weight sum by number of tracked individuals per year
#####
############################################################################################################################################################

#################
#
#
# directories
# species<- AOU code for the species you are running (eg SOSH)
# clipPolyList - file of specifics on each box - used to get extents for raster
# the clipPolyList number of the polygon you wish to break trip on
# rno = row number to select data from the clipPOlyList
# countour = used for file labelling only in this script, represents the maximum contour to return, use 99.999 for 100 ud contours
#
#################
#
# outputs
# .asc file for each year weighted by number of tracking days and number individuals tracked
# .asc file for all years summed by number of tracking days and number individuals tracked
# .csv file with summary of # tracked individuals, # segments, ptt id for each year
#
#################


#### clear all
rm(list=ls())

library(adehabitat)
library(SDMTools)
library(rgdal)
library(sp)
library(raster)
library(SDMTools)

#### select species
species="HAPE"

#### select resolution and contour (normal export at "99.999" to include all bb data)
resolution="3km" # cell size in km
contour <- 99.999

####
id.out = c("99999")    # = c("68019a","68022a3")     #to exclude birds or segments "99999" excludes none

####
dir.in <- "D:/Share_Data/Tracking_Data/PTT/"
dir.out <- "D:/Share_Data/Tracking_Data/PTT/"
dir.in.poly <- "D:/Share_Data/Clip Polygons" # Directory of list of clipping polygons
dir.in.asc <-  (paste(dir.in,species,"/4_BB_out/", sep="")) # dir.in directory containing BB.asc files
dir.in.meta <- "D:/Share_Data/GitHub/WERC-SC/trackcode/ptt/"

#### load clipperPolyLIst and select clipper file of interest
clipPolyList<-read.csv (paste(dir.in.poly,"/clipPolyList.csv", sep=""), header=T, sep=",", strip.white=T)
print(clipPolyList) # show a list of the clipper files
rno<-14 # select clipperfile, row number of clipperPolyLIst list (selects data bounded by clipper)
clipper<-as.character(clipPolyList$clipFileName[rno])
clipperName<-as.character(clipPolyList$name[rno])

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
  clipper@data$NAME<-(as.character(clipPolyList$name[rno]))
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

#### read in metadata,select metadata based on species
meta<-read.table (paste(dir.in.meta,"PTT_metadata_all.csv",sep = ""),header=T, sep=",", strip.white=T,na.strings = "")

meta<-meta[meta$species==species & meta$loc_data==1,]

#### desingated a grouping variable, this can be any variable in your metadata (e.i. year, site~year, site~sex~year)
## define grouping unit
# grping.var<-"year"
grping.var<-"species"
#grping.var<-"year_site"
## select metadata vased on grping.var
#grp.meta<-meta$species
grp.meta<-meta$species
#grp.meta<-paste(meta$year, meta$site_abbrev, sep="_")

#### get reference file for ud files and select grouping column, note on 4.28.15 reference file contains all metadata field
# read in track summary table of rasters to get a) list of files to read and b) number days birds were tracked 
tracksums <- read.table (paste(dir.in.asc,"tracksums_",species,"_BB_out_",clipperName,".csv", sep = ""),sep=",",header=T)

#### set grouping variable again, can lump all into 1 group (i.e. by species)
#tracksums$grp <-tracksums$year
#  tracksums$grp <-paste(tracksums$year, tracksums$site_abbrev, sep="_")
tracksums$grp <-tracksums$species

  
# get unique groups (use tracksums b/c these points are contained in polygon - not a tracks will necessarily be represented in a given polygon)
grp.ids<-unique(tracksums$grp)

#### initialize lists to house data by desired grouping variable (group.uniq)
# list to house uds normalized by track duration
ud.grp.ids <- vector ("list", length(grp.ids))
# list to house raster of individuals/cell
noindiv.grp.ids <- vector ("list", length(grp.ids))
# list to house summary data for each year
summary.grp.ids <- vector ("list", length(grp.ids))

#### loop through groups
# grp.id <-1
for (grp.id in 1:length(grp.ids)) {
  
  tracksums.want<-tracksums[which(tracksums$grp==grp.ids[grp.id]),]
  
  # create summary table of # of segments from each track
  track.freq<-as.data.frame(table(tracksums.want$deploy_id))
  
  # initialize lists to house data for segment based on deploy_id
  ud.track <- vector ("list", length(track.freq$Var1))
  track.days <- vector ("list", length(track.freq$Var1))
  
  track.grp<-cbind(rep(grp.ids[grp.id],length(track.freq[,1])))
  summary.grp.ids[[grp.id]]<-cbind(track.grp,track.freq)

  # sum up segments for each track
  # run through track.freq table summing segments >1
  # j=1
  for (j in 1:length(track.freq$Var1)) {
    if (track.freq$Freq[j]==1) {
      # operation for only one segment in polygon (track.freq$Freq[j]==1) == TRUE
       
      # open .asc
      ud.track[[j]] <- import.asc(paste(dir.in.asc,species,"_BB_out_",clipperName,"_",contour,"_",tracksums.want$id[tracksums.want$deploy_id==track.freq$Var1[j]],".asc", sep = ""), type = "numeric")
      # get number of track days (in decimal days)
      track.days[[j]]<-tracksums.want$days[tracksums.want$deploy_id==track.freq$Var1[j]]
      
      } else {
        # operation for multiple segments in polygon (track.freq$Freq[j]>1) == TRUE
        # get multiple segments
        tracksums.want$id[tracksums.want$deploy_id==track.freq$Var1[j]]
        
        segs<-tracksums.want$id[floor(tracksums.want$id)==track.freq$Var1[j]]
        days.segs<-tracksums.want$days[tracksums.want$deploy_id==track.freq$Var1[j]]
        # list to house asc for each segment
        ud.segs.new <- vector ("list", length(segs))
        # k=1
        for (k in 1:length(segs)) {
          # open .asc
          ud.seg <- import.asc(paste(dir.in.asc,species,"_BB_out_",clipperName,"_",contour,"_",segs[k],".asc", sep = ""), type = "numeric")
          # weigh each segment it's proportion of total hours tracked within the clipperName (Freiberg 20XX paper)
          ud.segs.new[[k]] <- ud.seg*(days.segs[k]/sum(days.segs))
        }
      # normalize divide by max cell value
        ud.track[[j]]<-Reduce("+",ud.segs.new)
      # get number of track days
        track.days[[j]]<-sum(days.segs)
      }
    }
  
## sum by grouping variable weight by:
  # a) number of days tracked
  # multiply new ud by (# of decimal days of each track/sum decimal days all tracks for that year)
  
  print(paste("grouping variable = ", grping.var,sep=""))
  
  # initialize rasters
  ud.grp.id<-ud.track[[1]]*0  
  noindiv.grp.id<-ud.track[[1]]*0
  # run through all rasters to sum by grouping variable
  # l=2
  for (l in 1:length(ud.track)) {
    # calculate ud weighted by track.days, weigh each track it's proportion of total hours tracked within the clipperName (Freiberg 20XX paper)
    ud.grp.id<-ud.grp.id + (ud.track[[l]])*(track.days[[l]]/sum(unlist(track.days)))
    # calculate the number of individuals per cell (for weighting upon summing of all years)
    indiv<-ud.track[[1]]*0
    indiv[ud.track[[l]]>0] <-1
    noindiv.grp.id<-noindiv.grp.id+indiv  
  }

# plot(ud.grp.id)
# plot(noindiv.grp.id)
# downweight summed raster by number of individuals in each cell
  sum.all.tracks.1n<-ud.grp.id/(1/noindiv.grp.id)


# ## to visualize with mask convert to raster
# ## convert asc to  'rasters' 
#   sum.grp.tracks.rast <- raster.from.asc(sum.all.tracks.1n, projs = CRS(paste("+",projWant,sep="")))
#   plot(sum.grp.tracks.rast)
#   
#   noindiv.grp.id.rast <- raster.from.asc(noindiv.grp.id, projs = CRS(paste("+",projWant,sep="")))
#   plot(sum.grp.tracks.rast)
#   
# # mask the sum.all.tracks with the clipping polygon
#   sum.grp.tracks.rast.masked<-mask(sum.grp.tracks.rast, clipper_proj)
#   
# # mask the number of individuals with the clipping polygon
#   noindiv.grp.id.rast.masked<-mask(noindiv.grp.id.rast, clipper_proj)
#   
# #   plot(sum.grp.tracks.rast.masked, axes=T)
# #   plot(clipper_proj, add=T)
# #   plot(noindiv.grp.id.rast.masked)

  

  
      # export ASCII files
      #           export.asc(udyear.dur.norm[[yr]], paste("D:/RWH/CA Atlas/",species,"/4_BB/",year.id[yr],"/",species,"_",clipperName,"_",contour,"_",year.id[yr],"_Summed_UDBB", sep=""))
      #           export.asc(year.indiv[[yr]], paste("D:/RWH/CA Atlas/",species,"/4_BB/",year.id[yr],"/",species,"_",clipperName,"_",contour,"_",year.id[yr],"_No_individuals", sep=""))
      #           export.asc(udyear.dur.norm.ind.wght[[yr]], paste("D:/RWH/CA Atlas/",species,"/4_BB/",year.id[yr],"/",species,"_",clipperName,"_",contour,"_",year.id[yr],"_Downweighted_UDBB_index", sep=""))
      #   write.asc(udyear.dur.norm[[yr]], paste("D:/RWH/CA Atlas/",species,"/4_BB/",year.id[yr],"/",species,"_",clipperName,"_",contour,"_",year.id[yr],"_Summed_UDBB", sep=""),gz=FALSE)
      #   write.asc(year.indiv[[yr]], paste("D:/RWH/CA Atlas/",species,"/4_BB/",year.id[yr],"/",species,"_",clipperName,"_",contour,"_",year.id[yr],"_No_individuals", sep=""),gz=FALSE)
      #   write.asc(udyear.dur.norm.ind.wght[[yr]], paste("D:/RWH/CA Atlas/",species,"/4_BB/",year.id[yr],"/",species,"_",clipperName,"_",contour,"_",year.id[yr],"_Downweighted_UDBB_index", sep=""),gz=FALSE)

# export as .acs (ASCII = although Arc does not recognize header info), used to import into arc  
    #   #drop
  # create output directory, will not replace if already exists
  dir.create(file.path(paste(dir.out,species,"/",sep=""),"5_Compiled"),showWarnings=TRUE)
  dir.create(file.path(paste(dir.out,species,"/5_Compiled/",sep=""),clipperName),showWarnings=TRUE)
  dir.create(file.path(paste(dir.out,species,"/5_Compiled/",clipperName,"/",sep=""),resolution),showWarnings=TRUE)

  # write.asc(udyear.dur.norm[[yr]], paste("D:/RWH/CA Atlas/",species,"/5_Compiled/",clipperName,"/",year.id[yr],"_sum", sep=""),gz=FALSE)
    write.asc(noindiv.grp.id, paste(dir.out,species,"/5_Compiled/",clipperName,"/",resolution,"/",species,"_",grp.ids[grp.id],"_",resolution,"_ni", sep=""),gz=FALSE)
    write.asc(sum.all.tracks.1n, paste(dir.out,species,"/5_Compiled/",clipperName,"/",resolution,"/",species,"_",grp.ids[grp.id],"_",resolution,"_bb", sep=""),gz=FALSE)

#    writeRaster(sum.grp.tracks.rast, paste(dir.out,species,"/5_Compiled/",clipperName,"/",resolution,"/",species,"_",grp.ids[grp.id],"_",resolution,"_bb", sep=""), format="ascii", overwrite=TRUE)

    
# # export as .csv - for Jarod
#     write.csv(udyear.dur.norm[[yr]], paste("D:/RWH/CA Atlas/",species,"/4_BB/Iterate/",clipperName,"/",year.id[yr],"_sum.csv", sep=""))
#     write.csv(year.indiv[[yr]], paste("D:/RWH/CA Atlas/",species,"/4_BB/Iterate/",clipperName,"/",year.id[yr],"_ni.csv", sep=""))
#     write.csv(udyear.dur.norm.ind.wght[[yr]], paste("D:/RWH/CA Atlas/",species,"/4_BB/Iterate/",clipperName,"/",year.id[yr],"_dw.csv", sep=""))
  
  
  
  
  } # END grping.var loop




# 
# 
# # make summary table for all years
# summary.grp.ids.mat<-do.call(rbind, summary.grp.ids)
# grp.id.wants<-as.data.frame(table(summary.grp.ids.mat[,1]))
#       
# # initiate loop to sum all years to create raster for 
# # 1. all years weighted by (number of individuals tracked for that year/total number individuals tracks for all years)
# # 2. number of birds per cell for all years
# 
# # grp.id <-5
# for (grp.id in 1:length(grp.ids)) {
#   
#   
# # allyrs.dur.notracks.wght=ud.years[[yr]] * (# individuals tracked for year i/sum # individuals tracked for all years)
#   
# if (exists("allgrps.dur.notracks.wght")) {
#   allgrps.dur.notracks.wght<-allgrps.dur.notracks.wght + (ud.grp.ids[[grp.id]]*(grp.id.wants[grp.id,2]/sum(grp.id.wants[,2])))
#   allgrps.indiv<-allgrps.indiv + noindiv.grp.ids[[grp.id]]  
# } else {
#   allgrps.dur.notracks.wght<-(ud.grp.ids[[grp.id]]*(grp.id.wants[grp.id,2]/sum(grp.id.wants[,2])))
#   allgrps.indiv<-noindiv.grp.ids[[grp.id]] 
# } 
# }
# 
#     # drop    
#     # #normalize to one
#     # allyrs.ud.dur.norm.ind.wght.tracks.wght<-allyrs.ud.dur.norm.ind.wght.tracks.wght/max(allyrs.ud.dur.norm.ind.wght.tracks.wght)
# # examine distribution of contours
# hist(log(allgrps.dur.notracks.wght),100)
# hist((allgrps.dur.notracks.wght),1000)
# hist(log(allgrps.indiv),100)
# hist((allgrps.indiv),1000)
# 
# # export summary table
# if (length(as.numeric(grp.ids))>1) {
#   write.table(as.data.frame(summary.grp.ids.mat), paste(dir.out,species,"/4_BB_out/",species,"_",clipperName,"_",resolution,"_",min(grp.ids),"_",max(grp.ids),"_tracksummary.csv", sep=""), sep=",", quote=FALSE,col.names=TRUE, row.names=FALSE,na="")
# } else {
#   write.table(as.data.frame(summary.grp.ids.mat), paste(dir.out,species,"/4_BB_out/",species,"_",clipperName,"_",resolution,"_",(grp.ids),"_tracksummary.csv", sep=""), sep=",", quote=FALSE,col.names=TRUE, row.names=FALSE,na="")
# }
# 
# # export files for all tracks weighted by no ind tracked/year
# 
# # export.asc(ud.sum, paste("D:/RWH/CA Atlas/",species,"/4_BB/",species,"_",clipperName,"_",min(year.id),"_",max(year.id),"_all_Summed_UDBB", sep=""))
# #     export.asc(allyrs.indiv, paste("D:/RWH/CA Atlas/",species,"/4_BB/",species,"_",clipperName,"_",min(year.id),"_",max(year.id),"_all_No_individuals", sep=""))
# #     export.asc(allyrs.ud.dur.norm.ind.wght.tracks.wght, paste("D:/RWH/CA Atlas/",species,"/4_BB/",species,"_",clipperName,"_",min(year.id),"_",max(year.id),"_all_Downweighted_UDBB_index", sep=""))
# 
# # export as .acs (ASCII = although Arc does not recognize header info), used to import into arc
# write.asc(allgrps.indiv, paste(dir.out,species,"/5_Compiled/",clipperName,"/",resolution,"/",species,"_",resolution,"_all_ni", sep=""),gz=FALSE)
# write.asc(allgrps.dur.notracks.wght, paste(dir.out,species,"/5_Compiled/",clipperName,"/",resolution,"/",species,"_",resolution,"_all_bb", sep=""),gz=FALSE)
# 
# # # export as .csv - for Jarod
# # write.table(allyrs.indiv, paste("D:/RWH/CA Atlas/",species,"/4_BB/Iterate/",clipperName,"/",species,"_alni.csv", sep=""))
# # write.table(allyrs.ud.dur.norm.ind.wght.tracks.wght, paste("D:/RWH/CA Atlas/",species,"/4_BB/Iterate/",clipperName,"/",species,"_aldw.csv", sep=""))
# 
# # END