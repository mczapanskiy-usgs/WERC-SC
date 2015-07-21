################################################################
#
# FILTERING SATELLITE TAG DATA USING THE ARGOSFILTER PACKAGE
#
# (Using location data edited by the swap filter in the STAT package)
#
################################################################

# for SDA filtering of Argos data where:
	# doppler mirrors have been filtered (switched) via Seaturle.org's Algorithm
	# inputs
			# folder containing .csv's of bird movements with time(UTC, 6/19/2008 8:20),lat (dd),lon (dd),lc,nb_mess
					# where file name is unique id
			# table of filter parameters
			# table of location errors, (default is lcerrors from Costa et al 2010)
	# outputs
			# .csv of time lat long
			# FilterParametersUsed_ - species,vmax,ang1,ang2,distlim1,distlim2
			# SummaryFriedasErrors_ - summary of individual errors, animal.id,meanerr
			# SummaryFriedasFiltered_ -animal.ids1,lc.1,end_location,not,removed

# clear all
rm(list=ls())
						
# install the argosfilter package
library(argosfilter)
library(plyr)

# set species AUO Code
species<-"PFSH"

# set plot option to review plots TRUE or FALSE
plot<-FALSE

dir.in <- "D:/Share_Data/Tracking_Data/PTT/"
dir.out <- "D:/Share_Data/Tracking_Data/PTT/"

#### datafile to read
# for ZACA data
# ptt <- read.table (paste(dir.in,species,"/2_SDA_Freitas_out/","All_tracks_and_data_",species,".csv",sep = ""),header=T, sep=",",strip.white=T)
# for early PFSH data
ptt <- read.table (paste(dir.in,species,"/0.1_Raw_Data_Other_Source/","PFSH_2002-05_argos-data_noSwaps.csv",sep = ""),header=T, sep=",",strip.white=T)

#### read in metadata
meta<-read.table (paste(dir.in,"PTT_metadata_1.0_5.08.2015_working.csv",sep = ""),header=T, sep=",", strip.white=T,na.strings = "")

#### select metadata want
meta<-meta[meta$species==species & meta$UnfilteredDataInCompiledFile==1 & meta$loc_data==1,]

# loop through birds want
# i=1
for (i in 1:length(meta[,1])) {			  

animal.id <- meta$animal_id[i]
file_name <- meta$file_name[i]
print(animal.id)

#### read in track
# track <- read.table (paste(dir.in,species,"/1_SeaTurtle_out/",file_name,".csv",sep = ""),header=T, sep=",",strip.white=T)
track <- ptt[ptt$ptt==file_name,]

# set uid for track
track$uid<-1:length(track[,1])

####
write.table (track, paste(dir.out,species,"/1_SeaTurtle_out//",file_name,".csv", sep =""), sep=",", quote=FALSE,col.names=TRUE, row.names=F,na="")

rm(track)
}