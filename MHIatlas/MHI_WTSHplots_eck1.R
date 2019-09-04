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


###
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



  # mutate(Occ_Total = as.numeric(Occ_Total),
  #        Occ_Total_nUnk = as.numeric(Occ_Total_nUnk),
  #        Occ_bird = as.numeric(Occ_bird),
  #        Occ_egg = as.numeric(Occ_egg),
  #        Unocc_sign = as.numeric(Unocc_sign),
  #        Unocc = as.numeric(Unocc),
  #        Unk_sign = as.numeric(Unk_sign),
  #        Unk_NoSign = as.numeric(Unk_NoSign),
  #        Occ_all = Occ_bird + Occ_egg,
  #        Occ_pct = Occ_all/Occ_Total_nUnk,
  #        Occ_dens = Occ_pct/PlotAreaFinal) 
  # select(Occ_Total_nUnk, Occ_bird, Occ_egg, Occ_all, Occ_pct)

summary(occ$Occ_dens)
