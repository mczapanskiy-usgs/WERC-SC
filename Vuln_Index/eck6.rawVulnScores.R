## this script creates scatter plots, similar to eck3
## summary scores for collision and displacement vuln scores before being multiplied by pop vuln score
## box and whisker plots

prelimScores <- read.csv("prelimVulnScores.csv") ## matric of raw CV and DV
prelimScores <- prelimScores[complete.cases(prelimScores), ] # remove blank observations at bottom of matrix

library(ggplot2)

# Collision Vulnerability
prelimScores$CommonName <- factor(prelimScores$CommonName, levels = prelimScores$CommonName[order(prelimScores$Order)]) # will keep original order of species
CV <- ggplot(prelimScores, aes(x=CommonName, y=ColBest, color=Groups))
CV <- CV + geom_errorbar(aes(ymin=ColBest-ColLower, ymax=ColBest+ColUpper, width=.5)) + geom_point(width=1, color="black") # establish graph and uncertainty lines
CV <- CV + facet_grid(~Groups, scales="free_x", space="free", labeller=labeller(CommonName=label_both)) # facets = species groupings
CV <- CV + theme_bw() + theme(axis.text.x=element_text(angle=90), axis.text.y=element_text(angle=90), axis.text=element_text(size=10), axis.title=element_text(size=16,face="bold")) # axis labels orientation and size
CV + theme(legend.position="none") + ylab("Collision Vulnerability") + xlab("Species Name") ## + scale_y_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10), limits=(c(0,10))) # axis labels size and tick marks 

# Displacement Vulnerability
prelimScores$CommonName <- factor(prelimScores$CommonName, levels = prelimScores$CommonName[order(prelimScores$Order)]) # will keep original order of species
DV <- ggplot(prelimScores, aes(x=CommonName, y=DispBest, color=Groups))
DV <- DV + geom_errorbar(aes(ymin=DispBest-DispLower, ymax=DispBest+DispUpper, width=.5)) + geom_point(width=1, color="black") # establish graph and uncertainty lines
DV <- DV + facet_grid(~Groups, scales="free_x", space="free", labeller=labeller(CommonName=label_both)) # facets = species groupings
DV <- DV + theme_bw() + theme(axis.text.x=element_text(angle=90), axis.text.y=element_text(angle=90), axis.text=element_text(size=10), axis.title=element_text(size=16,face="bold")) # axis labels orientation and size
DV + theme(legend.position="none") + ylab("Displacement Vulnerability") + xlab("Species Name") ## + scale_y_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10), limits=(c(0,10))) # axis labels size and tick marks 
