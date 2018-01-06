## this script creates spatial vulnerability scores for mapping
## input: survey densities (spp and spp group names standardized manually) & spp and spp group vulnerability scores
## calculates: percent rank of survey density and vulnerability scores for all spp/spp groups
## output: %rank_density_PCV and %rank_density_PDV for each spp, spp groups, and all spp together

setwd("~/WERC-SC/Vuln_Index/spatial")

library(data.table)
library(dplyr)
library(tidyr)


### read in files
read.csv('~/WERC-SC/Vuln_Index/spatial/SoCAvulnScores_20171214.csv',
         stringsAsFactors = FALSE) -> vulnScores
read.csv('~/WERC-SC/Vuln_Index/spatial/SoCAvulnPcts_20180104.csv',
         stringsAsFactors = FALSE) %>% 
  mutate(PCV100 = PCV*100, PDV100 = PDV*100, 
         PV100 = PV*100, DV100 = DV*100, CV100 = CV*100) -> vulnPcts
read.csv('~/WERC-SC/Vuln_Index/spatial/SoCA5minDensities.csv',
         stringsAsFactors = FALSE) -> densities

densities <- select(densities, -ALL_BIRDS)

### create species name string
# ## scores
# spp <- as.vector(vulnScores$SurveySpp)
# PCVspp <- paste(spp, "PCV", "density", sep = "_")
# PDVspp <- paste(spp, "PDV", "density", sep = "_") 
## percents
spp <- as.vector(vulnPcts$SurveySpp)
PCVspp <- paste(spp, "PCV", "density", sep = "_")
PDVspp <- paste(spp, "PDV", "density", sep = "_") 
PVspp <- paste(spp, "PV", "density", sep = "_")
CVspp <- paste(spp, "CV", "density", sep = "_")
DVspp <- paste(spp, "DV", "density", sep = "_") 

# ### vuln score percent rank
# vulnRanks <- mutate(vulnScores,
#                     PCVrank = percent_rank(PCV), 
#                     PDVrank = percent_rank(PDV))

### density percent rank and merge with vulnRanks
# ## scores -> percent rank
# densities_long <- gather(densities, species, dens, ASSP:XAMU) %>% # change to long format (tidyr)
#   mutate(densityRank = percent_rank(dens)) %>% # add percent rank of all survey densities
#   merge(vulnRanks, by.x = "species", by.y = "SurveySpp") %>%  # merge with vulnRanks table
#   mutate(rankDensPCV = densityRank*PCVrank,
#          rankDensPDV = densityRank*PDVrank)
## percents (lowest possible to highest possible)
# # with % rank of density
# densRank_pct_long <- gather(densities, species, dens, ASSP:XAMU) %>% # change to long format (tidyr)
#   mutate(densityRank = percent_rank(dens)) %>% # add percent rank of all survey densities
#   merge(vulnPcts, by.x = "species", by.y = "SurveySpp") %>%  # merge with vulnRanks table
#   mutate(pctDensPCV = densityRank*PCV100,
#          pctDensPDV = densityRank*PDV100)
# with density (not % ranked)
density_pct_long <- gather(densities, species, dens, ASSP:XAMU) %>% # change to long format (tidyr)
  merge(vulnPcts, by.x = "species", by.y = "SurveySpp") %>%  # merge with vulnRanks table
  mutate(pctDensPCV = dens*PCV100,
         pctDensPDV = dens*PDV100, 
         pctDensPV = dens*PV100,
         pctDensCV = dens*CV100,
         pctDensDV = dens*DV100)

### create tables to import into ARC
# ## PERCENT RANK
# # PCV
# ranksDensityPCV <- select(densities_long, species, TGRIDALB_I, rankDensPCV) %>% 
#   spread(species, rankDensPCV) %>% 
#   replace(is.na(.), 0) %>%
#   mutate(ALL_PCV_density = rowSums(.[2:66])) %>% 
#   setnames(old = c(spp), new = c(PCVspp))
# # PDV
# ranksDensityPDV <- select(densities_long, species, TGRIDALB_I, rankDensPDV) %>% 
#   spread(species, rankDensPDV) %>% 
#   replace(is.na(.), 0) %>%
#   mutate(ALL_PDV_density = rowSums(.[2:66])) %>% 
#   setnames(old = c(spp), new = c(PDVspp))

### PERCENTAGES BASED ON SCORE RANGES
# ## with % rank of density
# # PCV
# pctDensRankPCV <- select(densRank_pct_long, species, TGRIDALB_I, pctDensPCV) %>% 
#   spread(species, pctDensPCV) %>% 
#   replace(is.na(.), 0) %>%
#   mutate(ALL_PCV_density = rowSums(.[2:66])) %>% 
#   setnames(old = c(spp), new = c(PCVspp))
# # PDV
# pctDensRankPDV <- select(densRank_pct_long, species, TGRIDALB_I, pctDensPDV) %>% 
#   spread(species, pctDensPDV) %>% 
#   replace(is.na(.), 0) %>%
#   mutate(ALL_PDV_density = rowSums(.[2:66])) %>% 
#   setnames(old = c(spp), new = c(PDVspp))

## with density (not % ranked)
# PV
pct_DensPV <- select(density_pct_long, species, TGRIDALB_I, pctDensPV) %>% 
  spread(species, pctDensPV) %>% 
  replace(is.na(.), 0) %>%
  mutate(ALL_PV_density = rowSums(.[2:66])) %>% 
  setnames(old = c(spp), new = c(PVspp))
# CV
pct_DensCV <- select(density_pct_long, species, TGRIDALB_I, pctDensCV) %>% 
  spread(species, pctDensCV) %>% 
  replace(is.na(.), 0) %>%
  mutate(ALL_CV_density = rowSums(.[2:66])) %>% 
  setnames(old = c(spp), new = c(CVspp))
# DV
pct_DensDV <- select(density_pct_long, species, TGRIDALB_I, pctDensDV) %>% 
  spread(species, pctDensDV) %>% 
  replace(is.na(.), 0) %>%
  mutate(ALL_DV_density = rowSums(.[2:66])) %>% 
  setnames(old = c(spp), new = c(DVspp))
# PCV
pctDensPCV <- select(density_pct_long, species, TGRIDALB_I, pctDensPCV) %>% 
  spread(species, pctDensPCV) %>% 
  replace(is.na(.), 0) %>%
  mutate(ALL_PCV_density = rowSums(.[2:66])) %>% 
  setnames(old = c(spp), new = c(PCVspp))
# PDV
pctDensPDV <- select(density_pct_long, species, TGRIDALB_I, pctDensPDV) %>% 
  spread(species, pctDensPDV) %>% 
  replace(is.na(.), 0) %>%
  mutate(ALL_PDV_density = rowSums(.[2:66])) %>% 
  setnames(old = c(spp), new = c(PDVspp))

# ranksDensity <- inner_join(ranksDensityPCV, ranksDensityPDV, by = "TGRIDALB_I")
# pctDensRank <- inner_join(pctDensityPCV, pctDensityPDV, by = "TGRIDALB_I")
pct_PCVPDV_Density <- inner_join(pctDensPCV, pctDensPDV, by = "TGRIDALB_I")
pct_Density <- inner_join(pct_DensPV, pct_DensCV, by = "TGRIDALB_I") %>%
  inner_join(., pct_DensDV, by = "TGRIDALB_I")
  

pct_DensityAll <- pct_Density %>% 
  select(TGRIDALB_I, ALL_PV_density, ALL_CV_density, ALL_DV_density)

### save ranksDensityPCV and ranksDensityPDV data file to GitHub file
# ## PERCENT RANKS
# # PCV
# write.csv(ranksDensityPCV, file = '~/WERC-SC/Vuln_Index/spatial/ranksDensityPCV_soCal_20170524.csv',
#           row.names = FALSE) 
# # PDV 
# write.csv(ranksDensityPDV, file = '~/WERC-SC/Vuln_Index/spatial/ranksDensityPDV_soCal_20170524.csv',
#           row.names = FALSE) 
# # BOTH
# write.csv(ranksDensity, file = '~/WERC-SC/Vuln_Index/spatial/ranksDensity_soCal_20170602.csv',
#           row.names = FALSE) 
# 
# ## PERCENTAGES BASED ON SCORE RANGES BY DENSITY PERCENT RANK
# # PCV
# write.csv(pctDensRankPCV, file = '~/WERC-SC/Vuln_Index/spatial/pctDensRankPCV_soCal_20171215.csv',
#           row.names = FALSE) 
# # PDV 
# write.csv(pctDensRankPDV, file = '~/WERC-SC/Vuln_Index/spatial/pctDensRankPDV_soCal_20171215.csv',
#           row.names = FALSE) 
# # BOTH
# write.csv(pctDensRank, file = '~/WERC-SC/Vuln_Index/spatial/pctDensRank_soCal_20171215.csv',
#           row.names = FALSE) 

## PERCENTAGES BASED ON SCORE RANGES BY DENSITY (NOT % RANKED)
# PCV
write.csv(pctDensPCV, file = '~/WERC-SC/Vuln_Index/spatial/pctDensPCV_soCal_20171215.csv',
          row.names = FALSE) 
# PDV 
write.csv(pctDensPDV, file = '~/WERC-SC/Vuln_Index/spatial/pctDensPDV_soCal_20171215.csv',
          row.names = FALSE) 
# BOTH
write.csv(pctDensity, file = '~/WERC-SC/Vuln_Index/spatial/pctDens_soCal_20171215.csv',
          row.names = FALSE) 

# PV, CV, DV 
write.csv(pct_DensityAll, file = '~/WERC-SC/Vuln_Index/spatial/pctDens_3vuln_soCal_20180105.csv',
          row.names = FALSE) 
