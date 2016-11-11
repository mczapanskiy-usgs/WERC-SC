## this script is used to graph the summary scores for population, collision and displacement sensitivity
## box and whisker plots

scores <- read.csv("VulnIndexFinalSensitivityScores.csv") ## matrix of final PS, CS, and DS

library(ggplot2)

# Population Vulnerability
scores$CommonName <- factor(scores$CommonName, levels = scores$CommonName[order(scores$Order)]) # will keep original order of species
pop <- ggplot(scores, aes(x=CommonName, y=PopBest, color=Groups))
pop <- pop + geom_errorbar(aes(ymin=PopBest-PopLower, ymax=PopBest+PopUpper, width=.5)) + geom_point(width=1, color="black") # establish graph and uncertainty lines
pop <- pop + facet_grid(~Groups, scales="free_x", space="free", labeller=labeller(CommonName=label_both)) # facets = species groupings
pop <- pop + theme_bw() + theme(axis.text.x=element_text(angle=90), axis.text.y=element_text(angle=90), axis.text=element_text(size=10), axis.title=element_text(size=16,face="bold")) # axis labels orientation and size
pop <- pop + theme(legend.position="none") + ylab("Population Vulnerability") + xlab("Species Name") + scale_y_continuous(breaks=c(1,2,3,4,5), limits=(c(0,5.5))) # axis labels size and tick marks 

# Collision Vulnerability
scores$CommonName <- factor(scores$CommonName, levels = scores$CommonName[order(scores$Order)]) # will keep original order of species
CV <- ggplot(scores, aes(x=CommonName, y=ColBest, color=Groups))
CV <- CV + geom_errorbar(aes(ymin=ColBest-ColLower, ymax=ColBest+ColUpper, width=.5)) + geom_point(width=1, color="black") # establish graph and uncertainty lines
CV <- CV + facet_grid(~Groups, scales="free_x", space="free", labeller=labeller(CommonName=label_both)) # facets = species groupings
CV <- CV + theme_bw() + theme(axis.text.x=element_text(angle=90), axis.text.y=element_text(angle=90), axis.text=element_text(size=10), axis.title=element_text(size=16,face="bold")) # axis labels orientation and size
CV <- CV + theme(legend.position="none") + ylab("Population Collision Vulnerability") + xlab("Species Name") + scale_y_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10), limits=(c(0,10))) # axis labels size and tick marks 

# Displacement Vulnerability
scores$CommonName <- factor(scores$CommonName, levels = scores$CommonName[order(scores$Order)]) # will keep original order of species
DV <- ggplot(scores, aes(x=CommonName, y=DispBest, color=Groups))
DV <- DV + geom_errorbar(aes(ymin=DispBest-DispLower, ymax=DispBest+DispUpper, width=.5)) + geom_point(width=1, color="black") # establish graph and uncertainty lines
DV <- DV + facet_grid(~Groups, scales="free_x", space="free", labeller=labeller(CommonName=label_both)) # facets = species groupings
DV <- DV + theme_bw() + theme(axis.text.x=element_text(angle=90), axis.text.y=element_text(angle=90), axis.text=element_text(size=10), axis.title=element_text(size=16,face="bold")) # axis labels orientation and size
DV <- DV + theme(legend.position="none") + ylab("Population Displacement Vulnerability") + xlab("Species Name") + scale_y_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10), limits=(c(0,10))) # axis labels size and tick marks 

