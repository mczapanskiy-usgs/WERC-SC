## this script is used to graph the final vulnerability scores from the three indices
## for final presentation

setwd("~/WERC-SC/Vuln_Index")

library(ggplot2)
library(jpeg)
library(plyr)
library(dplyr)

#### load data
### PCV, PDV scores
PVscores <- read.csv("PCV&PDVscores_20171207.csv") ## matrix of cumulative PV, CPV, and DPV before 1-10 ranking
PVscores <-   PVscores[complete.cases(PVscores), ] %>% # remove blank observations at bottom of matrix
  mutate(AlphaCode = factor(AlphaCode, levels = AlphaCode[order(TaxNumCl)])) 
# PVscores_old <- read.csv("PCV&PDVscores_old.csv")
#   PVscores_old <-   PVscores_old[complete.cases(PVscores_old), ]
### PV, CV, DV scores
scores <- read.csv("PV.CV.DVscores_20171207.csv") %>% ## matrix of final PV, CV, and DV
  mutate(AlphaCode = factor(AlphaCode, levels = AlphaCode[order(TaxNumCl)])) # will keep original order of species


#### establish color palettes
cbbPalette <- c("#666666", "#66FF00", "#FF0033", "#56B4E9", "#0072B2", "#6600CC", "#E69F00", "#009E73", "#D55E00", "#FF33CC", "#000000") # http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
# cbbPalette <- c("#FF33CC", "#6600CC", "#FF0033", "#D55E00", "#E69F00", "#66FF00", "#009E73", "#0072B2", "#56B4E9", "#666666", "#000000") # http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/

#### graph PCV vs PDV with species names as points
PVscores$AlphaCode <- factor(PVscores$AlphaCode, levels = PVscores$AlphaCode[order(PVscores$TaxNumCl)]) # will keep original order of species
x1 <- ggplot(PVscores, aes(PDVbest, PCVbest, 
                 label=as.character(AlphaCode))) + # SHOULD MAKE LETTERS BOLD
  geom_text(aes(color=Taxonomy), size=3, face="bold") +
            # , subset = .(Taxomony %in% c("Pelicans", "Cormorants"))) +  ## select only a couple spp groups- *change the color options accordingly*
  scale_x_continuous(name="Population Displacement Vulnerability", limits=c(5,200)) +
  scale_y_continuous(name="Population Collision Vulnerability", limits=c(5,300)) +
  theme_bw(base_size = 14) + 
  scale_colour_manual(values=cbbPalette) # + scale_colour_brewer(palette = "Set1") 
x1
ggsave("PCVvsPDV_20170523.pdf", width = 12, height = 8)



#### graph PVvCV and PVvDV
# scores$AlphaCode <- factor(scores$AlphaCode, levels = scores$AlphaCode[order(scores$TaxNumCl)]) # will keep original order of species
CV <- ggplot(scores,aes(PVbest, CVbest, label=as.character(AlphaCode))) +
  geom_text(aes(color=Taxonomy),
            size=3,
            face="bold") +
  # , subset = .(Taxomony %in% c("Pelicans", "Cormorants"))) +  ## select only a couple spp groups- *change the color options accordingly*
  scale_x_continuous(name="Population Displacement Vulnerability", limits=c(5,200)) +
  scale_y_continuous(name="Population Collision Vulnerability", limits=c(5,300)) +
  theme_bw(base_size = 14) +
  scale_colour_manual(values=cbbPalette) # + scale_colour_brewer(palette = "Set1")
CV
ggsave("PCVvsPDV_old.pdf", width = 12, height = 8)




## graph CV vs DV colored by species group with convex hull polygons around species groups
# keep original order of species
scores$AlphaCode <- factor(scores$AlphaCode, levels = scores$AlphaCode[order(scores$Order)]) 
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

