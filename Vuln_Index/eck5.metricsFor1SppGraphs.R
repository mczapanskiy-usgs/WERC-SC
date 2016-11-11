## this scrip makes graphs for PSG 2015 poster- focusing on one species as an example of how the metrics work
## load data and packages
classes = c("character", "character", "character", "character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")
metrics <- read.csv("VulnIndexMetricScores.csv", colClasses = classes) ## matrix of all metrics, and uncertainty ranges

library(ggplot2)

## subset data from desired species: PFSH, BRPE, PAAU
metsub <- metrics[(metrics$Alpha.Code == "BRPE")|(metrics$Alpha.Code == "PFSH")|(metrics$Alpha.Code == "PAAU"),]
# PFSH <- metircs[(metrics$Alpha.Code == "PFSH"),]

# colors <- c("##FFFF66","##00033", "##FFFF66")

## RSZ
metsub$CommonName <- factor(metsub$CommonName, levels = metsub$CommonName[order(metsub$CCSpop)]) # set order of species
rsz <- ggplot(metsub, aes(x=CommonName, y=RSZ))
rsz <- rsz + geom_errorbar(aes(ymin=RSZ-RSZuncert, ymax=RSZ+RSZuncert, width=0.5)) + geom_point(size=4.5) # establish graph and uncertainty lines
rsz <- rsz + theme_bw() + theme(axis.text=element_text(size=18, face="bold"), axis.title=element_text(size=18,face="bold"))
rsz <- rsz + theme(legend.position="none") + ylab("Higher % Time       RSZ       Lower % Time") + xlab(" ") + ylim(0,8.5) # axis labels size and tick marks
last_plot() + coord_flip() # plot graph with spp names on y axis
## export dimensions: 800x433

## AO
metsub$CommonName <- factor(metsub$CommonName, levels = metsub$CommonName[order(metsub$CCSpop)]) # set order of species
AO <- ggplot(metsub, aes(x=CommonName, y=AO))
AO <- AO  + geom_point(size=4.5) # establish graph and uncertainty line
AO <- AO + theme_bw() + theme(axis.text=element_text(size=14), axis.title=element_text(size=18,face="bold"))
AO <- AO + theme(legend.position="none") + ylab("Months in CCS") + xlab("Species Name") + ylim(0,12.5)
## export dimensions: 500x300

## global population size
metsub$CommonName <- factor(metsub$CommonName, levels = metsub$CommonName[order(metsub$CCSpop)]) # set order of species
pop <- ggplot(metsub, aes(x=CommonName, y=PopSize))
pop <- pop + geom_errorbar(aes(ymin=PopSize-PopSizeUncert, ymax=PopSize+PopSizeUncert, width=0.5)) + geom_point(size=4.5) # establish graph and uncertainty lines
pop <- pop + theme_bw() + theme(axis.text=element_text(size=14), axis.title=element_text(size=18,face="bold"))
pop <- pop + theme(legend.position="none") + ylab("Global Population Size") + xlab("Species Name") + ylim(0,8.5) # axis labels size and tick marks
## export dimensions: 550X388

## % pop in CCS
metsub$CommonName <- factor(metsub$CommonName, levels = metsub$CommonName[order(metsub$CCSpop)]) # set order of species
CCS <- ggplot(metsub, aes(x=CommonName, y=CCSpop))
CCS <- CCS + geom_errorbar(aes(ymin=CCSpop-CCSpopUncert, ymax=CCSpop+CCSpopUncert, width=0.5)) + geom_point(size=4.5) # establish graph and uncertainty lines
CCS <- CCS + theme_bw() + theme(axis.text=element_text(size=14), axis.title=element_text(size=18,face="bold"))
CCS <- CCS + theme(legend.position="none") + ylab("% of Populatin in CCS") + xlab("Species") + ylim(0,8.5) # axis labels size and tick marks
## export dimensions: 550X388

## Adult Survival
metsub$CommonName <- factor(metsub$CommonName, levels = metsub$CommonName[order(metsub$CCSpop)]) # set order of species
sr <- ggplot(metsub, aes(x=CommonName, y=SR))
sr <- sr + geom_errorbar(aes(ymin=SR-SRuncert, ymax=SR+SRuncert, width=0.5)) + geom_point(size=4.5) # establish graph and uncertainty lines
sr <- sr + theme_bw() + theme(axis.text=element_text(size=18, face="bold"), axis.title=element_text(size=18,face="bold"))
sr <- sr + theme(legend.position="none") + ylab("Adult Survival") + xlab(" ") + ylim(0,8.5) # axis labels size and tick marks
last_plot() + coord_flip() # plot graph with spp names on y axis
## export dimensions: 550X388

## Macro-Avoidance: DV
metsub$CommonName <- factor(metsub$CommonName, levels = metsub$CommonName[order(metsub$CCSpop)]) # set order of species
ma <- ggplot(metsub, aes(x=CommonName, y=MA))
ma <- ma + geom_errorbar(aes(ymin=MA-MAuncert, ymax=MA+MAuncert, width=0.5)) + geom_point(size=4.5) # establish graph and uncertainty lines
ma <- ma + theme_bw() + theme(axis.text=element_text(size=18, face="bold"), axis.title=element_text(size=18,face="bold"))
ma <- ma + theme(legend.position="none") + ylab("Macro-avoidance") + xlab(" ") + ylim(0,8.5) # axis labels size and tick marks
last_plot() + coord_flip() # plot graph with spp names on y axis
## export dimensions: 550X388

## Macro-Avoidance: CV
metsub$CommonName <- factor(metsub$CommonName, levels = metsub$CommonName[order(metsub$CCSpop)]) # set order of species
ma2 <- ggplot(metsub, aes(x=CommonName, y=MA2))
ma2 <- ma2 + geom_errorbar(aes(ymin=MA2-MA2uncert, ymax=MA2+MA2uncert, width=0.5)) + geom_point(size=4.5) # establish graph and uncertainty lines
ma2 <- ma2 + theme_bw() + theme(axis.text=element_text(size=18, face="bold"), axis.title=element_text(size=18,face="bold"))
ma2 <- ma2 + theme(legend.position="none") + ylab("Macro-avoidance") + xlab(" ") + ylim(0,8.5) # axis labels size and tick marks
last_plot() + coord_flip() # plot graph with spp names on y axis
## export dimensions: 550X388

## Habitat Flexibility
metsub$CommonName <- factor(metsub$CommonName, levels = metsub$CommonName[order(metsub$CCSpop)]) # set order of species
hf <- ggplot(metsub, aes(x=CommonName, y=HF))
hf <- hf + geom_errorbar(aes(ymin=HF-HFuncert, ymax=HF+HFuncert, width=0.5)) + geom_point(size=4.5) # establish graph and uncertainty lines
hf <- hf + theme_bw() + theme(axis.text=element_text(size=18, face="bold"), axis.title=element_text(size=18,face="bold"))
hf <- hf + theme(legend.position="none") + ylab("Habitat Flexibility") + xlab(" ") + ylim(0,8.5) # axis labels size and tick marks
last_plot() + coord_flip() # plot graph with spp names on y axis
## export dimensions: 550X388

## DFA
metsub$CommonName <- factor(metsub$CommonName, levels = metsub$CommonName[order(metsub$CCSpop)]) # set order of species
DFA <- ggplot(metsub, aes(x=CommonName, y=DFR))
DFA <- DFA + geom_errorbar(aes(ymin=DFR-DFRuncert, ymax=DFR+DFRuncert, width=0.5)) + geom_point(size=4.5) # establish graph and uncertainty lines
DFA <- DFA + theme_bw() + theme(axis.text=element_text(size=18, face="bold"), axis.title=element_text(size=18,face="bold"))
DFA <- DFA + theme(legend.position="none") + ylab("Diurnal Flight Activity") + xlab(" ") + ylim(0,8.5) # axis labels size and tick marks
last_plot() + coord_flip() # plot graph with spp names on y axis
## NFA
metsub$CommonName <- factor(metsub$CommonName, levels = metsub$CommonName[order(metsub$CCSpop)]) # set order of species
NFA <- ggplot(metsub, aes(x=CommonName, y=NFR))
NFA <- NFA + geom_errorbar(aes(ymin=NFR-NFRuncert, ymax=NFR+NFRuncert, width=0.5)) + geom_point(size=4.5) # establish graph and uncertainty lines
NFA <- NFA + theme_bw() + theme(axis.text=element_text(size=18, face="bold"), axis.title=element_text(size=18,face="bold"))
NFA <- NFA + theme(legend.position="none") + ylab("Nocturnal Flight Activity") + xlab(" ") + ylim(0,8.5) # axis labels size and tick marks
last_plot() + coord_flip() # plot graph with spp names on y axis

## subset data for just one species
BRPE <- subset(metrics, Alpha.Code == "BRPE", select = -c(Groups, Taxonomy, CommonName, Alpha.Code, Order))
metric <- subset(BRPE, select = c(PopSize,CCSpop, SR, NFR, DFR, RSZ, MA, HF))
uncert <- subset(BRPE, select = c(PopSizeUncert,CCSpopUncert, SRuncert, NFRuncert, DFRuncert, RSZuncert, MAuncert, HFuncert))
label <- c("Global Population", "CCS Population", "Survival Rank", "Nocturnal Flight Rank", "Diurnal Flight Rank", "Time in RSZ", "Macro-Avoidance", "Habitat Flexibility")
## graph all metrics for speices- scatter plot with uncertainty ranges
plot <- plot(y, aes(x=V2))
