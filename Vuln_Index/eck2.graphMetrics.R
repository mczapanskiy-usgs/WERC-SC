## This script creates box and whisker plots of all the metrics and their uncertainty values

## load data
classes = c("character", "character", "character", "character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")
metrics <- read.csv("VulnIndexMetricScores.csv", colClasses = classes) ## matrix of all metrics, and uncertainty ranges

## graph individual metric by species in ggplot2
library(ggplot2)
metrics$CommonName <- factor(metrics$CommonName, levels = metrics$CommonName[order(metrics$Order)]) # will keep original order of species
MA <- ggplot(metrics, aes(x=CommonName, y=MA, color=Groups))
MA <- MA + geom_errorbar(aes(ymin=MA-MAuncert, ymax=MA+MAuncert, width=.5)) + geom_point(width=1, color="black") # establish graph and uncertainty lines
MA <- MA + facet_grid(~Groups, scales="free_x", space="free", labeller=labeller(CommonName=label_both)) # facets = species groupings
MA <- MA + theme_bw() + theme(axis.text.x=element_text(angle=90), axis.text.y=element_text(angle=90), axis.text=element_text(size=10), axis.title=element_text(size=18,face="bold"))
MA <- MA + theme(legend.position="none") + ylab("Macro-Avoidance") + xlab("Species Name") + scale_y_continuous(breaks=c(1,2,3,4,5,6,7,8)) # axis labels size and tick marks # labels

metrics$CommonName <- factor(metrics$CommonName, levels = metrics$CommonName[order(metrics$Order)]) # will keep original order of species
NFA <- ggplot(metrics, aes(x=CommonName, y=NFR, color=Groups))
NFA <- NFA + geom_errorbar(aes(ymin=NFR-NFRuncert, ymax=NFR+NFRuncert, width=.5)) + geom_point(width=1, color="black") # establish graph and uncertainty lines
NFA <- NFA + facet_grid(~Groups, scales="free_x", space="free", labeller=labeller(CommonName=label_both)) # facets = species groupings
NFA <- NFA + theme_bw() + theme(axis.text.x=element_text(angle=90), axis.text.y=element_text(angle=90), axis.text=element_text(size=10), axis.title=element_text(size=18,face="bold"))
NFA <- NFA + theme(legend.position="none") + ylab("Nocturnal Flight Activity") + xlab("Species Name") + scale_y_continuous(breaks=c(1,2,3,4,5,6,7,8)) # axis labels size and tick marks # labels

metrics$CommonName <- factor(metrics$CommonName, levels = metrics$CommonName[order(metrics$Order)]) # will keep original order of species
DFA <- ggplot(metrics, aes(x=CommonName, y=DFR, color=Groups))
DFA <- DFA + geom_errorbar(aes(ymin=DFR-DFRuncert, ymax=DFR+DFRuncert, width=.5)) + geom_point(width=1, color="black") # establish graph and uncertainty lines
DFA <- DFA + facet_grid(~Groups, scales="free_x", space="free", labeller=labeller(CommonName=label_both)) # facets = species groupings
DFA <- DFA + theme_bw() + theme(axis.text.x=element_text(angle=90), axis.text.y=element_text(angle=90), axis.text=element_text(size=10), axis.title=element_text(size=18,face="bold"))
DFA <- DFA + theme(legend.position="none") + ylab("Diurnal Flight Activity") + xlab("Species Name") + scale_y_continuous(breaks=c(1,2,3,4,5,6,7,8)) # axis labels size and tick marks # labels

metrics$CommonName <- factor(metrics$CommonName, levels = metrics$CommonName[order(metrics$Order)]) # will keep original order of species
AS <- ggplot(metrics, aes(x=CommonName, y=SR, color=Groups))
AS <- AS + geom_errorbar(aes(ymin=SR-SRuncert, ymax=SR+SRuncert, width=.5)) + geom_point(width=1, color="black") # establish graph and uncertainty lines
AS <- AS + facet_grid(~Groups, scales="free_x", space="free", labeller=labeller(CommonName=label_both)) # facets = species groupings
AS <- AS + theme_bw() + theme(axis.text.x=element_text(angle=90), axis.text.y=element_text(angle=90), axis.text=element_text(size=10), axis.title=element_text(size=18,face="bold"))
AS <- AS + theme(legend.position="none") + ylab("Adult Survival") + xlab("Species Name") + scale_y_continuous(breaks=c(1,2,3,4,5,6,7,8)) # axis labels size and tick marks # labels


# save graphs: width = 1500
## last_plot() + coord_flip() # plot graph with spp names on y axis


# ## plot graph with spp names on y axis, in the basic plotting system
# par(las=1)
# plot(metrics$Alpha.Code, metrics$SR, horizontal=TRUE)
# arrows(metrics$Alpha.Code, metrics$SR-metrics$SRLower, metrics$SR+metrics$SRUpper, length=0.5, angle=90, code=3)