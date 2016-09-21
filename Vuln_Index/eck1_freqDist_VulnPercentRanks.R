### GRAPH THE FREQUENCY DISTRIBUTIONS AND QUARTILE BREAKS OF VULNERABILITY %RANK DENSITIES
setwd("~/WERC-SC/Vuln_Index")

library(ggplot2)

read.csv('~/VulnerabilityIndex/spatial/vulnScores_spatial_Final%rank.csv',
         stringsAsFactors = FALSE) -> rank

hist <- ggplot(rank, aes(ALL_PDV_.rank)) +
  geom_bar(width = 0.1) +
  labs(x = 'PDV density rank', y = 'Frequency')
  
