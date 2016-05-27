
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

#### select species
species="SOSH"

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
rno<-5 # select clipperfile, row number of clipperPolyLIst list (selects data bounded by clipper)
clipperName<-as.character(clipPolyList$name[rno])

#### read in metadata,select metadata based on species
meta<-read.table (paste(dir.in.meta,"PTT_metadata_all.csv",sep = ""),header=T, sep=",", strip.white=T,na.strings = "")

meta<-meta[meta$species==species & meta$loc_data==1,]

#### desingated a grouping variable, this can be any variable in your metadata (e.i. year, site~year, site~sex~year)
## define grouping unit
grping.var<-"year"
#grping.var<-"year_site"
## select metadata vased on grping.var
grp.meta<-meta$year
#grp.meta<-paste(meta$year, meta$site_abbrev, sep="_")

#### get reference file for ud files and select grouping column, note on 4.28.15 reference file contains all metadata field
# read in track summary table of rasters to get a) list of files to read and b) number days birds were tracked 
tracksums <- read.table (paste(dir.in.asc,"tracksums_",species,"_BB_out_",clipperName,".csv", sep = ""),sep=",",header=T)

#### set grouping variable again
tracksums$grp <-tracksums$year
#  tracksums$grp <-paste(tracksums$year, tracksums$site_abbrev, sep="_")
  
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
print(paste("grouping variable = ", grping.var,sep=""))
  
# multiply new ud by (# of decimal days of each track/sum decimal days all tracks for that year)
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

  ud.track.nowght<-ud.track
  for (l in 1:length(ud.track.nowght)) {
    # calculate ud weighted by track.days, weigh each track it's proportion of total hours tracked within the clipperName (Freiberg 20XX paper)
    ud.track.nowght[[l]]<-ud.track[[l]]*noindiv.grp.id
  }
  
  ud.track.nowght.all<-Reduce("+",ud.track.nowght)  
  
  plot(ud.track.nowght.all)
  
  ud.grp.ids[[grp.id]]<-ud.grp.id
#   max(udall)
#   max(indiv)
#   max(indiv.sum)

    #   # drop this section
    #    downweight by number of individuals tracked in each cell
    #    normalize udall: divide by max cell value, and store in list  
    #    udyear.dur.norm[[yr]]<-(udall/max(udall))
  
  # save sum individuals per cell
  noindiv.grp.ids[[grp.id]]<-noindiv.grp.id
  
    #   # drop
    #    create matrix with downweighting variable  (1/n)
    #    year.indiv.wght <- indiv.sum
    #    year.indiv.wght[year.indiv.wght>0] <- 1/year.indiv.wght[year.indiv.wght>0]
  
    #  # drop
    #   # create new index by dividing the summed ud estimates by the number of individuals
    #   udyear.dur.norm.ind.wght[[yr]]<-udyear.dur.norm[[yr]]
    #   udyear.dur.norm.ind.wght[[yr]] <-  (udyear.dur.norm.ind.wght[[yr]]/year.indiv.wght)/max(udyear.dur.norm.ind.wght[[yr]])
    #   udyear.dur.norm.ind.wght[[yr]][is.na(udyear.dur.norm.ind.wght[[yr]]) == T] <- 0
    #   # normalize udyear.dur.norm.ind.wght[[yr]] to 1 by dividing by number of tracked individuals
    #   udyear.dur.norm.ind.wght[[yr]]<-udyear.dur.norm.ind.wght[[yr]]/max(udyear.dur.norm.ind.wght[[yr]])
  
# now have lists with rasters for year.indiv.wght, year.indiv, udyear.dur.norm.ind.wght

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
    write.asc(noindiv.grp.ids[[grp.id]], paste(dir.out,species,"/5_Compiled/",clipperName,"/",resolution,"/",species,"_",grp.ids[grp.id],"_",resolution,"_ni", sep=""),gz=FALSE)
    write.asc(ud.grp.ids[[grp.id]], paste(dir.out,species,"/5_Compiled/",clipperName,"/",resolution,"/",species,"_",grp.ids[grp.id],"_",resolution,"_bb", sep=""),gz=FALSE)

# # export as .csv - for Jarod
#     write.csv(udyear.dur.norm[[yr]], paste("D:/RWH/CA Atlas/",species,"/4_BB/Iterate/",clipperName,"/",year.id[yr],"_sum.csv", sep=""))
#     write.csv(year.indiv[[yr]], paste("D:/RWH/CA Atlas/",species,"/4_BB/Iterate/",clipperName,"/",year.id[yr],"_ni.csv", sep=""))
#     write.csv(udyear.dur.norm.ind.wght[[yr]], paste("D:/RWH/CA Atlas/",species,"/4_BB/Iterate/",clipperName,"/",year.id[yr],"_dw.csv", sep=""))
  
  
  
  
  } # END grping.var loop

# make summary table for all years
summary.grp.ids.mat<-do.call(rbind, summary.grp.ids)
grp.id.wants<-as.data.frame(table(summary.grp.ids.mat[,1]))
      
# initiate loop to sum all years to create raster for 
# 1. all years weighted by (number of individuals tracked for that year/total number individuals tracks for all years)
# 2. number of birds per cell for all years

# grp.id <-5
for (grp.id in 1:length(grp.ids)) {
  
  
# allyrs.dur.notracks.wght=ud.years[[yr]] * (# individuals tracked for year i/sum # individuals tracked for all years)
  
if (exists("allgrps.dur.notracks.wght")) {
  allgrps.dur.notracks.wght<-allgrps.dur.notracks.wght + (ud.grp.ids[[grp.id]]*(grp.id.wants[grp.id,2]/sum(grp.id.wants[,2])))
  allgrps.indiv<-allgrps.indiv + noindiv.grp.ids[[grp.id]]  
} else {
  allgrps.dur.notracks.wght<-(ud.grp.ids[[grp.id]]*(grp.id.wants[grp.id,2]/sum(grp.id.wants[,2])))
  allgrps.indiv<-noindiv.grp.ids[[grp.id]] 
} 
}

    # drop    
    # #normalize to one
    # allyrs.ud.dur.norm.ind.wght.tracks.wght<-allyrs.ud.dur.norm.ind.wght.tracks.wght/max(allyrs.ud.dur.norm.ind.wght.tracks.wght)
# examine distribution of contours
hist(log(allgrps.dur.notracks.wght),100)
hist((allgrps.dur.notracks.wght),1000)
hist(log(allgrps.indiv),100)
hist((allgrps.indiv),1000)

# export summary table
    write.table(as.data.frame(summary.grp.ids.mat), paste(dir.out,species,"/4_BB_out/",species,"_",clipperName,"_",resolution,"_",min(grp.ids),"_",max(grp.ids),"_tracksummary.csv", sep=""), sep=",", quote=FALSE,col.names=TRUE, row.names=FALSE,na="")

# export files for all tracks weighted by no ind tracked/year

# export.asc(ud.sum, paste("D:/RWH/CA Atlas/",species,"/4_BB/",species,"_",clipperName,"_",min(year.id),"_",max(year.id),"_all_Summed_UDBB", sep=""))
#     export.asc(allyrs.indiv, paste("D:/RWH/CA Atlas/",species,"/4_BB/",species,"_",clipperName,"_",min(year.id),"_",max(year.id),"_all_No_individuals", sep=""))
#     export.asc(allyrs.ud.dur.norm.ind.wght.tracks.wght, paste("D:/RWH/CA Atlas/",species,"/4_BB/",species,"_",clipperName,"_",min(year.id),"_",max(year.id),"_all_Downweighted_UDBB_index", sep=""))

# export as .acs (ASCII = although Arc does not recognize header info), used to import into arc
write.asc(allgrps.indiv, paste(dir.out,species,"/5_Compiled/",clipperName,"/",resolution,"/",species,"_",resolution,"_all_ni", sep=""),gz=FALSE)
write.asc(allgrps.dur.notracks.wght, paste(dir.out,species,"/5_Compiled/",clipperName,"/",resolution,"/",species,"_",resolution,"_all_bb", sep=""),gz=FALSE)

# # export as .csv - for Jarod
# write.table(allyrs.indiv, paste("D:/RWH/CA Atlas/",species,"/4_BB/Iterate/",clipperName,"/",species,"_alni.csv", sep=""))
# write.table(allyrs.ud.dur.norm.ind.wght.tracks.wght, paste("D:/RWH/CA Atlas/",species,"/4_BB/Iterate/",clipperName,"/",species,"_aldw.csv", sep=""))

# END