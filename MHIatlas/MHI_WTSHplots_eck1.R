## This script analyzes WTSH burrow plot data from KPNWR 2019
## created: September 3, 2019 by E. Kelsey
## last edited: 

setwd("~/WERC-SC/MHIatlas")

### load packages
library(data.table)
library(plyr)
library(dplyr)
library(tidyr)

### load data
read.csv('~/WERC-SC/MHIatlas/WTSH_KPNWR_plots.csv',
         stringsAsFactors = FALSE) -> plots
read.csv('~/WERC-SC/MHIatlas/WTSH_KPNWR_polygons.csv',
         stringsAsFactors = FALSE) -> polygons
read.csv('~/WERC-SC/MHIatlas/WTSH_KPNWR_summary.csv',
         stringsAsFactors = FALSE) -> summary

### add occupancy totals and proportions to summary of each polygon
occ_Summary <- plots %>% 
  group_by(Section) %>% 
  summarise(total_occ_checked = sum(Occ_Total_nUnk),
            total_occ = sum(Occ_bird + Occ_egg)) %>% 
  right_join(summary, by = "Section") %>% 
  mutate(occ_prop = total_occ/total_occ_checked,
         burrowOcc_Planar = occ_prop*totalBurrows_Planar,
         burrowOcc_Planar_CI = occ_prop*CI95_Burrows_Planar,
         burrowOcc_SA = occ_prop*totalBurrows_SA,
         burrowOcc_SA_CI = occ_prop*CI95_Burrows_SA) 

write.csv(occ_Summary, file = '~/WERC-SC/MHIatlas/Summary_occ.csv',
          row.names = FALSE)
