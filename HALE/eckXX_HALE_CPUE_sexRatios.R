### this script is used to test the sex ratios of the predators caught

library(plyr)
library(dplyr)
library(MASS)
library(ggplot2)
library(grid)

read.csv('~/WERC-SC/HALE/catch_11_traploc_baitTypes_predEvent_weeklyCatches.csv',
         stringsAsFactors = FALSE) -> weeklyCatches

### do Chi Sq test of all species 
## filter data to relevant values
weeklyCatches$Sex <- revalue(weeklyCatches$Sex, c("m"="M"))
# select variables to perfom test on
sexes <- c("F", "M") 
preds <- c("catCaught", "ratCaught", "mongooseCaught") # not including mouse data b/c sample size (thus frequencies) are too small

catch_sexRatio <- weeklyCatches %>% 
  filter(Sex %in% sexes & predEvent %in% preds)

## create a table of sex ratios for analysis
sexRatio <- table(catch_sexRatio$predEvent, catch_sexRatio$Sex)
# perform chi sq test
chisq.test(sexRatio) # outcome: X-squared = 99.378, df = 3, p-value < 2.2e-16 Warning message: Chi-squared approximation may be incorrect
# warning message suggests frequencies <5
chisq.test(sexRatio)$expected # yes, for catCaught(F) and mouse caught
# check accuracy of chi sq result with Fishers Exact Test
fisher.test(sexRatio) # p-value = 1.579e-15
# simulate sampling distribution using Monte Carlo methods
chisq.test(sexRatio, simulate.p.value = TRUE)


### do Chi Sq test of each species separately
## filter data to relevant values
cat <- c("catCaught") # not including mouse data b/c sample size (thus frequencies) are too small
rat <- c("ratCaught")
mongoose <- c("mongooseCaught")

## create a table of sex ratios for analysis
cat_sexRatio <- weeklyCatches %>% 
  filter(Sex %in% sexes & predEvent %in% cat)
c_sexRatio <- table(cat_sexRatio$predEvent, cat_sexRatio$Sex)

rat_sexRatio <- weeklyCatches %>% 
  filter(Sex %in% sexes & predEvent %in% rat)
r_sexRatio <- table(rat_sexRatio$predEvent, rat_sexRatio$Sex)

mongoose_sexRatio <- weeklyCatches %>% 
  filter(Sex %in% sexes & predEvent %in% mongoose)
m_sexRatio <- table(mongoose_sexRatio$predEvent, mongoose_sexRatio$Sex)

## perform chi sq test
chisq.test(c_sexRatio) 
chisq.test(r_sexRatio) 
chisq.test(m_sexRatio) 

### determine what percent of all predators were sexed
sexValues <- c("F", "M", "NR", "UNK")
predValues <- c("catCaught", "ratCaught", "mouseCaught", "mongooseCaught") 

all_sexRatio <- weeklyCatches %>% 
  filter(Sex %in% sexValues & predEvent %in% predValues) 
sexRatioTab <- table(all_sexRatio$predEvent, all_sexRatio$Sex)

### graph data
# create data table of values (there must be a better way to do this)
Input =(
  
  "Sex     Cat  Rat  Mongoose
    Male  15    1220       102
    Female  19    120       34   
    ")
sex.mat  <- as.matrix(read.table(textConnection(Input), header = TRUE, row.names = 1))
# graph in barplot
barplot(sex.mat, 
        beside=TRUE, 
        legend=TRUE, 
        space = c(0,0.5),
        col = c("lightblue", "darkred"),
        ylim=c(0, 1300),
        xlab="Predator Species",
        ylab="Sex Caught"
)


