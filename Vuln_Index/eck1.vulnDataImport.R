## this script loads the metric and final senstivity score data for analysis
## and performs cluster and correlation analyses on the metrics to determine which ones the most like eachother

classes = c("character", "character", "character", "character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")
metrics <- read.csv("VulnIndexMetricScores.csv", colClasses = classes) ## matrix of all metrics, and uncertainty ranges
sClasses = c("character", "character", "character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")
scores <- read.csv("VulnIndexFinalSensitivityScores.csv", colClasses = sClasses) ## matrix of final PS, CS, and DS

## hierarchical clustering

var <- subset(metrics, select = c(PopSize, CCSpop, TR, SR, AO, NFR, DFR, MA, BR, HF)) ## pull out just 'best score' for all metrics
var2 <- t(var) ## transpose the matrix so that the metrics will be in a row.  The values will then cluster by metric.
distVar <- dist(as.matrix(var2)) ## , method="manhattan")
clustVar <- hclust(distVar)
plot(clustVar)

## correlation analysis
library(corrgram)
corrgram(var)  ## visual depiction of relationships, blue=pos correlation, red=neg correlation, white=no correlation)
cor(var)
## run statistics on the correlations
library(Hmisc)
var <- as.matrix(var)
cor <- (rcorr(var, type="pearson")) ## assuming parametric assumptions are met...



