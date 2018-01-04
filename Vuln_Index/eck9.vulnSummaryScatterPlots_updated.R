## this script is used to graph the final vulnerability scores from the three indices
## for final presentation

setwd("~/WERC-SC/Vuln_Index")

library(ggplot2)
library(jpeg)
library(plyr)
library(dplyr)
library(extrafont)
library(RColorBrewer)

#### load data
scores <- read.csv("vulnScores_20171214.csv") %>% ## matrix of final PV, CV, and DV
  mutate(AlphaCode = factor(AlphaCode, levels = AlphaCode[order(TaxNumCl)])) # will keep original order of species

#### establish color palettes
cbbPalette6 <- c("#000000", "#009E73", "#e79f00", "#F0E442", "#D55E00", "#0072B2")
cbbPalette11 <- c("#666666", "#66FF00", "#FF0033", "#56B4E9", "#0072B2", "#6600CC", "#E69F00", "#009E73", "#D55E00", "#FF33CC", "#000000") # http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/

#### graph PCV vs PDV with species names as points
# scores$AlphaCode <- factor(scores$AlphaCode, levels = scores$AlphaCode[order(scores$TaxNumCl)]) # will keep original order of species
x1 <- ggplot(scores, aes(PDVbest, PCVbest, 
                 label=as.character(AlphaCode))) + # SHOULD MAKE LETTERS BOLD
  geom_text(aes(color=Taxonomy), size=3, face="bold") +
            # , subset = .(Taxomony %in% c("Pelicans", "Cormorants"))) +  ## select only a couple spp groups- *change the color options accordingly*
  scale_x_continuous(name="Population Displacement Vulnerability", limits=c(5,200)) +
  scale_y_continuous(name="Population Collision Vulnerability", limits=c(5,300)) +
  theme_bw(base_size = 14) + 
  scale_colour_manual(values=cbbPalette6) # scale_colour_brewer(palette = "Set1") # 
x1
ggsave("PCVvsPDV_20170523.pdf", width = 12, height = 8)

#### graph PVvCV and PVvDV
CV <- ggplot(scores,aes(CVbest, PVbest, label=as.character(AlphaCode))) +
  geom_text(aes(color=Taxonomy),
            size=5, face="bold") +
  scale_x_continuous(name="Collision Vulnerability", limits=c(0,15)) + # min = 3, max = 15
  scale_y_continuous(name="Population Vulnerability", limits=c(0,30)) + # min = 4, max = 30
  theme_bw(base_size = 14, panel.background = element_blank()) +
  scale_colour_manual(values=cbbPalette) # + scale_colour_brewer(palette = "Set1")
CV
ggsave(width = 12, height = 8, filename = "~/WERC-SC/Vuln_Index/outputs/CVvsPV.pdf")

DV <- ggplot(scores,aes(DVbest, PVbest, label=as.character(AlphaCode))) +
  geom_text(aes(color=Taxonomy),
            size=5, face="bold") +
  scale_x_continuous(name="Displacement Vulnerability", limits=c(0,10)) + # min = 3, max = 15
  scale_y_continuous(name="Population Vulnerability", limits=c(0,30)) + # min = 4, max = 30
  theme_bw(base_size = 14) +
  scale_colour_manual(values=cbbPalette) # + scale_colour_brewer(palette = "Set1")
DV
ggsave(width = 12, height = 8, filename = "~/WERC-SC/Vuln_Index/outputs/DVvsPV.pdf")

## percentages
CVpercent<- ggplot(scores,aes(CVpct, PVpct, label=as.character(AlphaCode))) +
  geom_text(aes(color=Taxonomy),
            size=5, face="bold") +
  scale_x_continuous(name="Collision Vulnerability", limits=c(0,1)) + # min = 3, max = 15
  scale_y_continuous(name="Population Vulnerability", limits=c(0,1)) + # min = 4, max = 30
  theme_bw(base_size = 14) +
  theme(axis.line = element_line(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), panel.background = element_blank()) +
  scale_colour_manual(values=cbbPalette6) # + scale_colour_brewer(palette = "Set1")
CVpercent
ggsave(width = 12, height = 8, filename = "~/WERC-SC/Vuln_Index/outputs/CVvsPV_pct.pdf")

DVpercent <- ggplot(scores,aes(DVpct, PVpct, label=as.character(AlphaCode))) +
  geom_text(aes(color=Taxonomy),
            size=5, face="bold") +
  scale_x_continuous(name="Displacement Vulnerability", limits=c(0,1)) + # min = 3, max = 15
  scale_y_continuous(name="Population Vulnerability", limits=c(0,1)) + # min = 4, max = 30
  theme_bw(base_size = 14) +
  theme(axis.line = element_line(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), panel.background = element_blank()) +
  scale_colour_manual(values=cbbPalette6) # + scale_colour_brewer(palette = "Set1")
DVpercent
ggsave(width = 12, height = 8, filename = "~/WERC-SC/Vuln_Index/outputs/DVvsPV_pct.pdf")


## graph CV vs DV colored by species group with convex hull polygons around species groups
# make convex hull
scores_hull <- scores
find_hull <- function(scores) scores[chull(scores$ColBest, scores$DispBest), ]
hulls <- ddply(scores_hull, "Taxonomy", find_hull)
# graph
x2 <- ggplot(scores, 
             aes(ColBest,
                 DispBest, 
                 label = as.character(AlphaCode),
                 color = Taxonomy,
                 fill = Taxonomy)) + 
  geom_point() + 
  scale_x_continuous(name = "Collision Vulnerability") + 
  scale_y_continuous(name = "Displacement Vulnerability") + 
  theme_bw(base_size = 14) + 
  geom_polygon(data=hulls, alpha=.2)
x2 
ggsave("CVvDV_chull.pdf")

## graph CV vs DV by alpha code with PV scaling points
x3 <- ggplot(scores, 
             aes(ColBest,
                 DispBest,
                 size = PopBest,
                 label = as.character(AlphaCode),
                 color = Taxonomy)) + 
  geom_text() + # (aes(colour = factor(Taxomony)), size=3,face="bold") +
  # , subset = .(Taxomony %in% c("Pelicans", "Cormorants"))) +  ## select only a couple spp groups- *change the color options accordingly*
  scale_x_continuous(name = "Collision Vulnerability") + # , limits=c(5,200)
  scale_y_continuous(name = "Displacement Vulnerability") + # , limits=c(5,300)
  theme_bw(base_size = 14) + 
  scale_colour_manual(values=cbbPalette) # + scale_colour_brewer(palette = "Set1") 
x3
ggsave("CVvDV_spp_PVscale.pdf")

#   guides(color = guide_legend(nrow = 2,title = NULL, label.position = "bottom")) +
#   theme(legend.position = 'bottom')

