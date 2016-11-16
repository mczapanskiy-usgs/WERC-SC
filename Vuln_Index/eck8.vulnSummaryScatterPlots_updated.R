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
  scale_colour_manual(values=cbbPalette) # + scale_colour_brewer(palette = "Set1") 
x1
ggsave("PCVvsPDV.pdf")

## graph CV vs DV colored by species group with PV scaling points
scores$AlphaCode <- factor(scores$AlphaCode, levels = scores$AlphaCode[order(scores$Order)]) # will keep original order of species
x2 <- ggplot(scores, 
             aes(ColBest,
                 DispBest, 
                 size = PopBest, 
                 color = Taxonomy)) + 
  geom_point() + # (aes(colour = factor(Taxomony)), size=3,face="bold") +
  # , subset = .(Taxomony %in% c("Pelicans", "Cormorants"))) +  ## select only a couple spp groups- *change the color options accordingly*
  scale_x_continuous(name = "Collision Vulnerability") + # , limits=c(5,200)
  scale_y_continuous(name = "Displacement Vulnerability") + # , limits=c(5,300)
  theme_bw(base_size = 14) + 
  scale_colour_manual(values=cbbPalette) # + scale_colour_brewer(palette = "Set1") 
x2
ggsave("CVvDV_point_PVscale.pdf")

## graph CV vs DV by alpha code with PV scaling points
scores$AlphaCode <- factor(scores$AlphaCode, levels = scores$AlphaCode[order(scores$Order)]) # will keep original order of species
# scores_hull = scores %>% 
#   group_by(Taxonomy) %>% 
#   mutate(hull = 1:n(), hull = factor(hull, chull(ColBest, DispBest))) %>% 
#   arrange(hull)

scores_hull <- scores
find_hull <- function(scores_hull) df[chull(scores_hull$ColBest, scores_hull$DispBest), ]
hulls <- ddply(scores_hull, "issue", find_hull)

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


loc = na.omit(loc) %>%
  group_by(country) %>%
  mutate(hull = 1:n(), hull = factor(hull, chull(lat, lon))) %>%
  arrange(hull)

ggplot(loc, aes(lon, lat, color = country, fill = country)) +
  geom_polygon(data = filter(loc, !is.na(hull)), alpha = 0.5) +
  geom_point() +
  guides(color = FALSE, fill = FALSE) +
  theme_bw() +
  theme(axis.text = element_blank())
#   guides(color = guide_legend(nrow = 2,title = NULL, label.position = "bottom")) +
#   theme(legend.position = 'bottom')

