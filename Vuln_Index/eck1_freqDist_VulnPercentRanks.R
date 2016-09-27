### GRAPH THE FREQUENCY DISTRIBUTIONS AND QUARTILE BREAKS OF VULNERABILITY %RANK DENSITIES
setwd("~/WERC-SC/Vuln_Index")

library(ggplot2)
library(dplyr)

read.csv('~/VulnerabilityIndex/spatial/vulnScores_spatial_Final%rank.csv',
         stringsAsFactors = FALSE) -> rank

histPCV <- ggplot(rank, aes(ALL_PCV_.rank)) +
  geom_bar(binwidth = 0.025) +
  labs(x = 'PCV density rank', y = 'Frequency') +
  geom_vline(xintercept = c(1.114, 1.671, 2.588, 9.111), color = "red") +
  coord_cartesian(ylim = c(0,8))

histPDV <- ggplot(rank, aes(ALL_PDV_.rank)) +
  geom_bar(binwidth = 0.025) +
  labs(x = 'PDV density rank', y = 'Frequency') +
  geom_vline(xintercept = c(1.139, 1.7433, 2.709, 8.6301), color = "red") +
  coord_cartesian(ylim = c(0,8))
  
# q_PCV <- quantile(rank$ALL_PCV_.rank, probs = c(0.2, 0.4, 0.6, 0.8))
# q_PDV <-  quantile(rank$ALL_PDV_.rank, probs = c(0.25, 0.5, 0.75))

#   hist(rank$ALL_PCV_.rank, 
#        breaks = 484, 
#        xlab = "PCV density rank",
#        xlim = c(0,10))