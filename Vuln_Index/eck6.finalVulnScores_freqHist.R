## this script creates a frequency distribution of the final Collision and Displacement Vulnerability Scores
## importing final percent rank scores (1-10)

## load data and toolboxes
scores <- read.csv("VulnIndexFinalSensitivityScores.csv") ## matrix of final PS, CS, and DS

library(ggplot2)

## historgrams for 1-10 scores
colHist <- ggplot(scores, aes(x=ColBest, fill=Groups))
colHist <- colHist + geom_histogram(binwidth=0.1)
colHist <- colHist + theme_bw() + theme(axis.text=element_text(size=18, face="bold"), axis.title=element_text(size=18,face="bold"))
colHist + ylab("Frequency") + xlab("Collision Vulnerability Score")
        
dispHist <- ggplot(scores, aes(x=DispBest, fill=Groups))
dispHist <- dispHist + geom_histogram(binwidth=0.1)
dispHist <- dispHist + theme_bw() + theme(axis.text=element_text(size=18, face="bold"), axis.title=element_text(size=18,face="bold"))
dispHist + ylab("Frequency") + xlab("Displacement Vulnerability Score")

## historgrams for raw vulnerability values
colHistRaw <- ggplot(rawScores, aes(x=ColBest, fill=Groups))
colHistRaw <- colHistRaw + geom_histogram(binwidth=10)
colHistRaw <- colHistRaw + theme_bw() + theme(axis.text=element_text(size=18, face="bold"), axis.title=element_text(size=18,face="bold"))
colHistRaw + ylab("Frequency") + xlab("Collision Vulnerability")

dispHistRaw <- ggplot(rawScores, aes(x=DispBest, fill=Groups))
dispHistRaw <- dispHistRaw + geom_histogram(binwidth=10)
dispHistRaw <- dispHistRaw + theme_bw() + theme(axis.text=element_text(size=18, face="bold"), axis.title=element_text(size=18,face="bold"))
dispHistRaw + ylab("Frequency") + xlab("Displacement Vulnerability")

## for raw vuln scores, using upper and lower scores too
colHistAll <- ggplot(rawScores, aes(x=ColLower, fill=Groups)) + geom_bar(binwidth=5)
colHistAll <- colHistAll + ggplot(rawScores, aes(x=ColBest, fill=Groups)) + geom_bar(binwidth=5)
colHistAll <- colHistAll + ggplot(rawScores, aes(x=ColUpper, fill=Groups)) + geom_bar(binwidth=5)
colHistAll <- colHistAll + theme_bw() + theme(axis.text=element_text(size=18, face="bold"), axis.title=element_text(size=18,face="bold"))
colHistAll + ylab("Frequency") + xlab("Collision Vulnerability") + xlim(0,2300)

dispHistAll <- ggplot(rawScores, aes(x=DispLower, fill=Groups)) + geom_bar(binwidth=5)
dispHistAll <- dispHistAll + ggplot(rawScores, aes(x=DispBest, fill=Groups)) + geom_bar(binwidth=5)
dispHistAll <- dispHistAll + ggplot(rawScores, aes(x=DispUpper, fill=Groups)) + geom_bar(binwidth=5)
dispHistAll <- dispHistAll + theme_bw() + theme(axis.text=element_text(size=18, face="bold"), axis.title=element_text(size=18,face="bold"))
dispHistAll + ylab("Frequency") + xlab("Displacement Vulnerability") + xlim(0,3400)

        