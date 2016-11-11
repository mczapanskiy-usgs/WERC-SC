## PCV vs PDV for each species, seperate graph for each species group
## these graphs are made for the species account sections of the vulnerability index report

## load data and toolboxes
PVscores <- read.csv("PCV&PDVscores.csv") ## matrix of cumulative PV, CPV, and DPV scores
PVscores <- PVscores[complete.cases(PVscores), ] # remove blank observations at bottom of matrix

library(ggplot2)

PVscores$AlphaCode <- factor(PVscores$AlphaCode, levels = PVscores$AlphaCode[order(PVscores$Order)]) # will keep original order of species

for (i in unique(PVscores$Groups)) {
        mypath <- file.path("C:","Users","ekelsey","Desktop","graphs",paste(i, "_PCVvPDV",".pdf", sep = "")) ## where to save graph and what to name it
        pdf(file=mypath) #save graph 
        
        p <- subset(PVscores, Groups == i)
        y <- ggplot(p, aes(ColBest, DispBest, label=as.character(AlphaCode))) + geom_text() + theme_bw() + 
          ylab("Population Displacement Vulnerability") + 
          xlab("Population Collision Vulnerability")  + 
          ylim(0,1500) + xlim(0,1600) + ggtitle(i) + 
          theme(legend.position="none", plot.title=element_text(size=rel(1.5), face="bold"), 
                axis.title.y=element_text(size=rel(1.5)), axis.title.x=element_text(size=rel(1.5)))
        plot(y)
        dev.off() ## stops saving to .png files
}




