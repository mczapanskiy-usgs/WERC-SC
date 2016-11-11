## this script is used to graph the final senstivity scores from the vulnerability matrix
## for final presentation

## load data and toolboxes
scores <- read.csv("VulnIndexFinalSensitivityScores.csv") ## matrix of final PS, CS, and DS

library(ggplot2)

## maintain order
scores$AlphaCode <- factor(scores$AlphaCode, levels = scores$AlphaCode[order(scores$Order)]) # will keep original order of species
## pie chart of collision sensitivities
CS <- ggplot(scores, aes(x=AlphaCode, fill = ColBest, color="red"))
CS <- CS + geom_bar(width=1)+coord_polar()

## maintain order
scores$AlphaCode <- factor(scores$AlphaCode, levels = scores$AlphaCode[order(scores$Order)]) # will keep original order of species
## graph CS vs DS with species names as points
x <- ggplot(scores, aes(ColBest, DispBest, label=as.character(AlphaCode))) + geom_text(aes(color=Groups), size=4, face="bold") + theme_bw()
x <- x <- x + ylab("Displacement Sensitivity") + xlab("Collision Sensitivity") + ylim(0,10) + xlim(0,10)
x <- x + theme(legend.text=element_text(size=14), axis.title.y=element_text(size=rel(1.5)), axis.title.x=element_text(size=rel(1.5)))