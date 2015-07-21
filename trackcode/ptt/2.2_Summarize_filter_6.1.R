#################################
## Bill Henry  USGS WERC
## Jul 13, 2015
##
## Output
## For PTT Tracking data Summarizes:
## 1. points filtered by Freitas Filter
## 2. calculated mean error for each bird and mean and sd for all birds
## 
## Inputs
## dir.in: input directory
## dir.out: ouput directory
## dir.error: error structure by lc (3,2,1,0,-1,-2,-9)
## species: AOU
## meta: metadata table
## 
## Outputs
## SummaryFreitas_errorRetained: mean error for retained (unfiltered) points for each tag
## SummaryFreitas_filtering: summary of points filtered by Frietas Filter
##
#################################

rm(list=ls())

library(trip)
library(reshape2)
library(plyr)

# in .csv files
#### dir.in
dir.in <- "D:/Share_Data/Tracking_Data/PTT/"

#### dir.out
dir.out <- "D:/Share_Data/Tracking_Data/PTT/"

# directory for files with ptt error by location class
dir.error<-"D:/Share_Data/Tracking_Data/Support_Files/"

#### were the data filtered externally to this USGS routine package? (if true assumes all records have been previously filtered and will calculate mean error based on the full incoming dataset))
data_filt_ext="FALSE"

#### set species
species="SOSH"

#### read in metadata
meta<-read.table (paste(dir.in,"PTT_metadata_1.0_5.08.2015_working.csv",sep = ""),header=T, sep=",", strip.white=T)

#### select metadata want
meta<-meta[meta$species==species & meta$loc_data==1,]

ptt_deploy_ids<-meta$ptt_deploy_id

tracks <- read.table (paste(dir.in,species,"/2_SDA_Freitas_out/All_tracks_and_data_",species,".csv",sep = ""),header=T, sep=",", strip.white=T)
tracks$meas <- rep(1, length(tracks[,1]))

tracks$ptt_deploy_id<-as.factor(tracks$ptt_deploy_id)
tracks$lc<-as.factor(tracks$lc)
tracks$keeps<-as.character(as.logical(tracks$keeps))
tracks$keeps[tracks$keeps=="TRUE"]<-"retained"
tracks$keeps[tracks$keeps=="FALSE"]<-"filtered"

# organize data by ptt
# melt the data
tracks.m<-melt(tracks, c("ptt_deploy_id", "lc","filtered","keeps"),"meas")
# cast the data (make the pivot)
filter.results<-dcast(tracks.m, ptt_deploy_id + lc ~ keeps + value , sum)
#filter.results<-filter.results[,c(1,length(filter.results[1,]),2:(length(filter.results[1,])-1))]

write.table(filter.results,paste(dir.out,species,"/2_SDA_Freitas_out/SummaryFreitas_filtering_",species,"_ref.csv",sep=""),sep=",",quote=FALSE,col.names=TRUE,row.names=FALSE)

lcerrors <- read.csv (paste(dir.error,"lcerrors.csv",sep=""), header=T, sep=",", strip.white=T)

indiv.error<-merge(filter.results, lcerrors, by = 'lc', all = FALSE)

indiv.error$error.prod<-indiv.error$X68errCosta*indiv.error$retained_1

indiv.error.m<-melt(indiv.error, id=c("ptt_deploy_id", "lc", "retained_1", "error.prod"), measure = c("error.prod", "retained_1"))
indiv.error.results<-dcast(indiv.error.m, ptt_deploy_id ~ variable, sum)

indiv.error.results$mean.error.track<-indiv.error.results$error.prod/indiv.error.results$retained_1
## get mean error for the dataset
indiv.error.results<-rbind(indiv.error.results,c(0,0,0,mean(indiv.error.results$mean.error.track)),c(0,0,0,sd(indiv.error.results$mean.error.track)),c(0,0,0,length(indiv.error.results[,1])))

write.table(indiv.error.results,paste(dir.out,species,"/2_SDA_Freitas_out/SummaryFreitas_errorRetained_",species,"_ref.csv",sep=""),sep=",",quote=FALSE,col.names=TRUE,row.names=FALSE)

