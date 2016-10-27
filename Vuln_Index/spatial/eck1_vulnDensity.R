## this script takes creates spatial vulnerability scores for mapping
## input: survey densities (spp and spp group names standardized manually) & spp and spp group vulnerability scores
## calculates: percent rank of survey density and vulnerability scores for all spp/spp groups
## output: %rank_density_PCV and %rank_density_PDV for each spp and all spp together

setwd("~/WERC-SC/Vuln_Index/spatial")

library("data.table", lib.loc="~/R/win-library/3.2")
library("dplyr", lib.loc="~/R/win-library/3.2")

# read in files
read.csv('~/WERC-SC/Vuln_Index/spatial/SoCAvulnScores.csv',
         stringsAsFactors = FALSE) -> vulnScores
read.csv('~/WERC-SC/Vuln_Index/spatial/SoCA5minDensities.csv',
         stringsAsFactors = FALSE) -> densities

# vuln score percent rank
vulnRanks <- mutate(vulnScores,
                    PCVrank = percent_rank(PCV), 
                    PDVrank = percent_rank(PDV))

# density percent rank
densityRanks <- mutate_each(densities,
                         funs(percent_rank))        #, matches("^[A-Z]\\."), ignore.case=FALSE

# rank <- names(vulnRanks$SurveySpp)[1:70]
# rank <- setNames(rank, paste0(rank, "_rank"))

# write for loop to apply percent rank to all species densities
for (spp in vulnRanks$SurveySpp) {
  mutate(densityRanks, spp = funs(.*spp))
}