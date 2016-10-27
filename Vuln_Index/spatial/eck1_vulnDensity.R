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
vulnScores <- vulnScores[ , complete.cases(vulnScores)] # remove blank observations at bottom of matrix
read.csv('~/WERC-SC/Vuln_Index/spatial/SoCA5minDensities.csv',
         stringsAsFactors = FALSE) -> densities
densities <- densities[ , complete.cases(densities)] # remove blank observations at bottom of matrix

# vuln score percent rank
vulnRanks <- mutate(vulnScores,
                    PCVrank = percent_rank(PCV), 
                    PDVrank = percent_rank(PDV))


