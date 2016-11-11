## this script is used to graph the final vulnerability scores from the three indices
## for final presentation

## load data and toolboxes
# rank <- read.csv("VulnIndexFinalSensitivityScores.csv") ## matrix of final PV, CPV, and DPV 1-10 rankings
PVscores <- read.csv("PCV&PDVscores.csv") ## matrix of cumulative PV, CPV, and DPV before 1-10 ranking
PVscores <- PVscores[complete.cases(PVscores), ] # remove blank observations at bottom of matrix
scores <- read.csv("VulnScores.csv") ## matrix of final PV, CV, and DV
scores <- scores[complete.cases(scores), ] # remove blank observations at bottom of matrix

library(ggplot2)

## graph CPV vs DPV with species names as points

PVscores$AlphaCode <- factor(PVscores$AlphaCode, levels = PVscores$AlphaCode[order(PVscores$Order)]) # will keep original order of species
x1 <- ggplot(PVscores, 
             aes(ColBest, 
                 DispBest, 
                 label=as.character(AlphaCode))) + 
  geom_text(aes(color=Taxonomy), 
            size=5, 
            face="bold") + 
  scale_x_log10(limits = c(5, 1600)) +
  scale_y_log10(limits = c(9, 1400), breaks = c(10, 100, 1000)) +
  theme_bw(base_size = 14) + 
  ylab("Population Displacement Vulnerability") + 
  xlab("Population Collision Vulnerability") +
  theme(legend.position = 'bottom')  +   
  guides(color = guide_legend(nrow = 1,title = NULL))


#   theme(legend.text = element_text(size=14), 
#         axis.title.y = element_text(size=rel(1.5)), 
#         axis.title.x=element_text(size=rel(1.5)))

# x4 <- x3 + theme(legend.position=c("bottom")) # put the legend in the graph, in the top right corner

# ## for raw CV and DV scores
# scores$AlphaCode <- factor(scores$AlphaCode, levels = scores$AlphaCode[order(scores$Order)]) # will keep original order of species
# p1 <- ggplot(scores, aes(ColBest, DispBest, label=as.character(AlphaCode))) + geom_text(aes(color=Groups, size=PopBest), face="bold") + theme_bw()
# p2 <- p1 + ylab("Displacement Vulnerability") + xlab("Collision Vulnerability") #  + ylim(0,10) + xlim(0,10)
# p3 <- p2 + theme(legend.text=element_text(size=14), axis.title.y=element_text(size=rel(1.5)), axis.title.x=element_text(size=rel(1.5)))
# p4 <- p3 + theme(legend.justification=c(1,1), legend.position=c(1,1)) # put the legend in the graph, in the top right corner
# p4
# graph size = 1000x800

## for PCV and PDV scores

# y <- ggplot(PVscores, aes(ColBest, DispBest, label=as.character(AlphaCode))) + geom_text(aes(color=Groups, size=PopBest), face="bold") + theme_bw()
# y <- y + ylab("Displacement Vulnerability") + xlab("Collision Vulnerability") #  + ylim(0,10) + xlim(0,10)
# y + theme(legend.text=element_text(size=14), axis.title.y=element_text(size=rel(1.5)), axis.title.x=element_text(size=rel(1.5)))

# y <- ggplot(PVscores, aes(ColBest, DispBest, label=as.character(AlphaCode))) + geom_text(aes(color=Groups), size=4, face="bold") + theme_bw()
# y <- y + ylab("Displacement Vulnerability") + xlab("Collision Vulnerability") #  + ylim(0,10) + xlim(0,10)
# y + theme(legend.text=element_text(size=14), axis.title.y=element_text(size=rel(1.5)), axis.title.x=element_text(size=rel(1.5)))

## pie chart of collision sensitivities
## maintain order
# scores$AlphaCode <- factor(scores$AlphaCode, levels = scores$AlphaCode[order(scores$Order)]) # will keep original order of species

# CS <- ggplot(scores, aes(x=AlphaCode, fill = ColBest, color="red"))
# CS <- CS + geom_bar(width=1)+coord_polar()
