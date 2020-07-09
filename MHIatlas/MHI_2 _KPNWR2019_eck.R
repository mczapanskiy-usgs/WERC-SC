## This script analyzes WTSH burrow plot data from KPNWR 2019
## created: September 3, 2019 by E. Kelsey
## last edited: 

setwd("~/WERC-SC/MHIatlas")

### load packages
# library(data.table)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)

### load data
## WTSH
read.csv('~/WERC-SC/MHIatlas/WTSH_KPNWR_plots.csv',
         stringsAsFactors = FALSE) -> plots
read.csv('~/WERC-SC/MHIatlas/WTSH_KPNWR_polygons.csv',
         stringsAsFactors = FALSE) -> polygons
read.csv('~/WERC-SC/MHIatlas/WTSH_KPNWR_summary.csv',
         stringsAsFactors = FALSE) -> summary
## RTTR
read.csv('~/WERC-SC/MHIatlas/TropicbirdsKPNWR2019.csv',
         stringsAsFactors = FALSE) %>% 
  filter(Species == "RTTR",
         NestLoc != "Mokueae") %>% 
  mutate_at(c("Structure", "Landform", "VegDom1"),
            .funs = ~as.factor(.)) -> RTTR


### add WTSH occupancy totals and proportions to summary of each polygon
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

write.csv(occ_Summary, file = '~/WERC-SC/MHIatlas/WTSH_KPNWR_summary_occ.csv',
          row.names = FALSE)


#### graph RTTR habitat
loc <- RTTR %>% 
  group_by(NestLoc) %>% 
  tally()

RTTRstruc <- RTTR %>% 
  filter(Structure != "")

structure <- ggplot(RTTRstruc, aes(NestLoc, fill = Structure)) +
  geom_bar(position = "fill") +
  ylab("Prop of nests in diff habitat structures") + xlab("Nest Location at KPNWR") + labs(fill = "Habitat Structure") +
  scale_fill_brewer(palette="BrBG") +
  theme_bw()
structure %+% subset(RTTRstruc, NestLoc %in% c("Crater Hill", "Kilauea Point", "Mokolea"))
ggsave(width = 8.5, height = 5, dpi=300, filename = "~/WERC-SC/MHIatlas/RTTR_nestStruct.pdf")


RTTRland <- RTTR %>% 
  filter(Landform != "")

landform <- ggplot(RTTRland, aes(NestLoc, fill = Landform)) +
  geom_bar(position = "fill") +
  ylab("Prop of nests on diff landforms") + xlab("Nest Location at KPNWR") + labs(fill = "Landform Type") +
  scale_fill_brewer(palette="BrBG") +
  theme_bw()
landform %+% subset(RTTRland, NestLoc %in% c("Crater Hill", "Kilauea Point", "Mokolea"))
ggsave(width = 8.5, height = 5, dpi=300, filename = "~/WERC-SC/MHIatlas/RTTR_nestLand.pdf")

RTTRveg <- RTTR %>% 
  filter(VegDom1 != "")

vegdom <- ggplot(RTTRveg, aes(NestLoc, fill = VegDom1)) +
  geom_bar(position = "fill") +
  ylab("Prop of nests on dominant vegetation types") + xlab("Nest Location at KPNWR") + labs(fill = "Dominant Vegetation") +
  scale_fill_brewer(palette="BrBG") +
  theme_bw()
vegdom %+% subset(RTTRveg, NestLoc %in% c("Crater Hill", "Kilauea Point", "Mokolea"))
ggsave(width = 8.5, height = 5, dpi=300, filename = "~/WERC-SC/MHIatlas/RTTR_nestVeg.pdf")



