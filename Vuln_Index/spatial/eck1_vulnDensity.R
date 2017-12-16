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
read.csv('~/WERC-SC/Vuln_Index/spatial/SoCAvulnPcts_20171215.csv',
         stringsAsFactors = FALSE) %>% 
  mutate(PCV = PCV*100,
         PDV = PDV*100) -> vulnPcts
read.csv('~/WERC-SC/Vuln_Index/spatial/SoCA5minDensities.csv',
         stringsAsFactors = FALSE) -> densities
densities <- select(densities, -ALL_BIRDS)

### create species name string
## scores
spp <- as.vector(vulnScores$SurveySpp)
PCVspp <- paste(spp, "PCV", "density", sep = "_")
PDVspp <- paste(spp, "PDV", "density", sep = "_") 
## percents
spp <- as.vector(vulnPcts$SurveySpp)
PCVspp <- paste(spp, "PCV", "density", sep = "_")
PDVspp <- paste(spp, "PDV", "density", sep = "_") 

### vuln score percent rank
vulnRanks <- mutate(vulnScores,
                    PCVrank = percent_rank(PCV), 
                    PDVrank = percent_rank(PDV))

### density percent rank and merge with vulnRanks
## scores -> percent rank
densities_long <- gather(densities, species, dens, ASSP:XAMU) %>% # change to long format (tidyr)
  mutate(densityRank = percent_rank(dens)) %>% # add percent rank of all survey densities
  merge(vulnRanks, by.x = "species", by.y = "SurveySpp") %>%  # merge with vulnRanks table
  mutate(rankDensPCV = densityRank*PCVrank,
         rankDensPDV = densityRank*PDVrank)
## percents
density_pct_long <- gather(densities, species, dens, ASSP:XAMU) %>% # change to long format (tidyr)
  mutate(densityRank = percent_rank(dens)) %>% # add percent rank of all survey densities
  merge(vulnPcts, by.x = "species", by.y = "SurveySpp") %>%  # merge with vulnRanks table
  mutate(pctDensPCV = densityRank*PCV,
         pctDensPDV = densityRank*PDV)

### create tables to import into ARC
## PERCENT RANK
# PCV
ranksDensityPCV <- select(densities_long, species, TGRIDALB_I, rankDensPCV) %>% 
  spread(species, rankDensPCV) %>% 
  replace(is.na(.), 0) %>%
  mutate(ALL_PCV_density = rowSums(.[2:66])) %>% 
  setnames(old = c(spp), new = c(PCVspp))
# PDV
ranksDensityPDV <- select(densities_long, species, TGRIDALB_I, rankDensPDV) %>% 
  spread(species, rankDensPDV) %>% 
  replace(is.na(.), 0) %>%
  mutate(ALL_PDV_density = rowSums(.[2:66])) %>% 
  setnames(old = c(spp), new = c(PDVspp))

## PERCENTAGES BASED ON SCORE RANGES
# PCV
pctDensityPCV <- select(density_pct_long, species, TGRIDALB_I, pctDensPCV) %>% 
  spread(species, pctDensPCV) %>% 
  replace(is.na(.), 0) %>%
  mutate(ALL_PCV_density = rowSums(.[2:66])) %>% 
  setnames(old = c(spp), new = c(PCVspp))
# PDV
pctDensityPDV <- select(density_pct_long, species, TGRIDALB_I, pctDensPDV) %>% 
  spread(species, pctDensPDV) %>% 
  replace(is.na(.), 0) %>%
  mutate(ALL_PDV_density = rowSums(.[2:66])) %>% 
  setnames(old = c(spp), new = c(PDVspp))

ranksDensity <- inner_join(ranksDensityPCV, ranksDensityPDV, by = "TGRIDALB_I")
pctDensity <- inner_join(pctDensityPCV, pctDensityPDV, by = "TGRIDALB_I")

### save ranksDensityPCV and ranksDensityPDV data file to GitHub file
## PERCENT RANKS
# PCV
write.csv(ranksDensityPCV, file = '~/WERC-SC/Vuln_Index/spatial/ranksDensityPCV_soCal_20170524.csv',
          row.names = FALSE) 
# PDV 
write.csv(ranksDensityPDV, file = '~/WERC-SC/Vuln_Index/spatial/ranksDensityPDV_soCal_20170524.csv',
          row.names = FALSE) 
# BOTH
write.csv(ranksDensity, file = '~/WERC-SC/Vuln_Index/spatial/ranksDensity_soCal_20170602.csv',
          row.names = FALSE) 

## PERCENTAGES BASED ON SCORE RANGES
# PCV
write.csv(pctDensityPCV, file = '~/WERC-SC/Vuln_Index/spatial/pctDensityPCV_soCal_20171215.csv',
          row.names = FALSE) 
# PDV 
write.csv(pctDensityPDV, file = '~/WERC-SC/Vuln_Index/spatial/pctDensityPDV_soCal_20171215.csv',
          row.names = FALSE) 
# BOTH
write.csv(pctDensity, file = '~/WERC-SC/Vuln_Index/spatial/pctDensity_soCal_20171215.csv',
          row.names = FALSE) 