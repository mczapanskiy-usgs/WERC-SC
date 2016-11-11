## this script is used to graph the final vulnerability scores from the three indices
## for final presentation

setwd("~/WERC-SC/Vuln_Index")

## load data and toolboxes

PVscores <- read.csv("PCV&PDVscores.csv") ## matrix of cumulative PV, CPV, and DPV before 1-10 ranking
PVscores <- PVscores[complete.cases(PVscores), ] # remove blank observations at bottom of matrix
scores <- read.csv("PV.CV.DVscores.csv") ## matrix of final PV, CV, and DV
scores <- scores[complete.cases(scores), ] # remove blank observations at bottom of matrix

library(ggplot2)
library(jpeg)
library(plyr)

## establish color palettes
cbbPalette <- c("#FF0033", "#000000", "#56B4E9", "#E69F00", "#009E73", "#666666", "#0072B2", "#D55E00", "#FF33CC", "#6600CC") # http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
#  cbbPalette <- c("#000000", "#0072B2") ### select colors if only graphing a couple species groups

## graph PCV vs PDV with species names as points
PVscores$AlphaCode <- factor(PVscores$AlphaCode, levels = PVscores$AlphaCode[order(PVscores$Order)]) # will keep original order of species
x1 <- ggplot(PVscores, 
             aes(PDVbest, 
                 PCVbest, 
                 label=as.character(AlphaCode))) + 
  geom_text(aes(color=Taxomony), 
            size=3, 
            face="bold") +
            # , subset = .(Taxomony %in% c("Pelicans", "Cormorants"))) +  ## select only a couple spp groups- *change the color options accordingly*
  scale_x_continuous(name="Population Displacement Vulnerability", limits=c(5,200)) +
  scale_y_continuous(name="Population Collision Vulnerability", limits=c(5,300)) +
  theme_bw(base_size = 14) + 
  scale_colour_manual(values=cbbPalette) + # scale_colour_brewer(palette = "Set1") 
x1
ggsave("PCVvsPDV.pdf")
  
## graph CV vs DV colored by species group with PV scaling points
scores$AlphaCode <- factor(scores$AlphaCode, levels = scores$AlphaCode[order(scores$Order)]) # will keep original order of species
x2 <- ggplot(scores, 
             aes(DispBest, 
                 ColBest, 
                 size=PopBest)) + 
  geom_point(aes(colour = factor(Taxomony)), 
            size=3, 
            face="bold") +
  # , subset = .(Taxomony %in% c("Pelicans", "Cormorants"))) +  ## select only a couple spp groups- *change the color options accordingly*
  scale_x_continuous(name="Population Displacement Vulnerability", limits=c(5,200)) +
  scale_y_continuous(name="Population Collision Vulnerability", limits=c(5,300)) +
  theme_bw(base_size = 14) + 
  scale_colour_manual(values=cbbPalette) + # scale_colour_brewer(palette = "Set1") 
x2
ggsave("PCVvsPDV.pdf")

#   guides(color = guide_legend(nrow = 2,title = NULL, label.position = "bottom")) +
#   theme(legend.position = 'bottom')

# plot using points instead of alpha codes
# x1 <- ggplot(PVscores, 
#              aes(PDVbest, 
#                  PCVbest)) + 
#   geom_point(aes(color=Taxomony), 
#             size=3, 
#             face="bold") +  
#   scale_x_continuous(name="Population Displacement Vulnerability", limits=c(5,200)) +
#   scale_y_continuous(name="Population Collision Vulnerability", limits=c(5,300)) +
#   theme_bw(base_size = 14) + 
#   scale_colour_manual(values=cbbPalette) + # scale_colour_brewer(palette = "Set1") +
#   guides(color = guide_legend(nrow = 1,title = NULL)) +
#   theme(legend.position = 'bottom')

#   scale_x_log10(limits = c(5, 1600)) +
#   scale_y_log10(limits = c(9, 1400), breaks = c(10, 100, 1000)) +
#   ylab("Population Displacement Vulnerability") + 
#   xlab("Population Collision Vulnerability") +

#   theme(legend.text = element_text(size=14), 
#         axis.title.y = element_text(size=rel(1.5)), 
#         axis.title.x=element_text(size=rel(1.5)))

# x4 <- x3 + theme(legend.position=c("bottom")) # put the legend in the graph, in the top right corn=er


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
