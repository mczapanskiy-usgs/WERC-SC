## this script takes creates spatial vulnerability scores for mapping
## input: survey densities (spp and spp group names standardized manually) & spp and spp group vulnerability scores
## calculates: percent rank of survey density and vulnerability scores for all spp/spp groups
## output: %rank_density_PCV and %rank_density_PDV for each spp and all spp together

setwd("~/WERC-SC/Vuln_Index/spatial")

library("data.table", lib.loc="~/R/win-library/3.2")
library("dplyr", lib.loc="~/R/win-library/3.2")
library(tidyr)


# read in files
read.csv('~/WERC-SC/Vuln_Index/spatial/SoCAvulnScores.csv',
         stringsAsFactors = FALSE) -> vulnScores
read.csv('~/WERC-SC/Vuln_Index/spatial/SoCA5minDensities.csv',
         stringsAsFactors = FALSE) -> densities
densities <- select(densities, -ALL_BIRDS)

# vuln score percent rank
vulnRanks <- mutate(vulnScores,
                    PCVrank = percent_rank(PCV), 
                    PDVrank = percent_rank(PDV))

# density percent rank and merge with vulnRanks
densities_long <- gather(densities, species, dens, ASSP:XAMU) %>% # change to long format (tidyr)
  mutate(densityRank = percent_rank(dens)) %>% # add percent rank of all survey densities
  merge(vulnRanks, by.x = "species", by.y = "SurveySpp") %>%  # merge with vulnRanks table
  mutate(rankPCV = densityRank*PCVrank,
         rankPDV = densityRank*PDVrank)

densityVulnRanks <- spread(densities_long, species, TGRIDALB_I)
