################################################################
##
## Bill Henry June 21, 2013
##
## created
## April  26, 2015
##
## flags duplicate locations from filtered data not run through USGS coding
##
##
################################################################

	# inputs
			# folder containing a .csv's of animal IDs with time(UTC, 6/19/2008 8:20),lat (dd),lon (dd),lc,nb_mess, and any other data
					# where ptt_deploy_id is  unique id (note some ptt numbers can be recycled)
	# outputs
			# .csv of deploy_id, time, lat, lot, lc and other unused fields

# clear all
rm(list=ls())
						
# install the argosfilter package
library(argosfilter)
library(plyr)

# set species AUO Code
species<-"ZACA"

dir.in <- "D:/Share_Data/Tracking_Data/PTT/"
dir.out <- "D:/Share_Data/Tracking_Data/PTT/"

## assumes lc quality are coded on the scale of 3,2,1,0,-1,-2,-9

#### 
ptt <- read.table (paste(dir.in,species,"/2_SDA_Freitas_out/","All_tracks_and_data_",species,".csv",sep = ""),header=T, sep=",",strip.white=T)

#### delete duplicate points retaining based on 1. >lc & 2. >nb_mes, or 3. first duplicate, trash others
  ptt$dep_utc<-factor(paste(ptt$ptt_deploy_id,"_",ptt$utc,sep=""))
  dups<-  ptt[ptt$dep_utc %in% ptt$dep_utc[duplicated(ptt$dep_utc)],]
  dups$dep_utc<-factor(dups$dep_utc)

    if (length(dups[,1])!=0) {
		# identify duplicate records
		dup.freq <- as.data.frame.table (tapply(dups$dep_utc, dups$dep_utc, length))    
    # k<-1
    for (k in 1:length(dup.freq[,1])) {
      dups.test<-dups[dups$dep_utc %in% dup.freq$Var1[k],]
      if (length(unique(dups.test$lc))!=1) {
        # first check and retain location higher lc
        ptt$filtered[which(ptt$uid==dups.test$uid[dups.test$lc==min(dups.test$lc)])]="dup" 
        # second if same lc, check and retain location higher num.mes 
      } else if (length(unique(dups.test$nb_mes))!=1) {
        ptt$filtered[which(ptt$uid==dups.test$uid[dups.test$nb_mes==min(dups.test$nb_mes)])]="dup" 
        # third if same nb.mess, retain the first duplicate
      } else {
        ptt$filtered[which(ptt$uid==dups.test$uid[2:(length(dups.test$uid))])]="dup"
      }
    }
    } else { # if there are no duplicates fill blanks in for NA values in filtered column
      ptt$filtered[is.na(ptt$filtered)] <- "not"
    }
    ptt$filtered[is.na(ptt$filtered)] <- "not"

    dupremoved=length(ptt$filtered[ptt$filtered=="dup"])

#### create vector for points kept
ptt$keeps[which(ptt$filtered=='dup')] = 0


#### output all tracks concatenated
write.table (ptt, paste(dir.out,species,"/2_SDA_Freitas_out/","All_tracks_and_data_",species,".csv", sep =""), sep=",", quote=FALSE,col.names=TRUE, row.names=F,na="")

