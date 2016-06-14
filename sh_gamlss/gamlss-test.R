library(gamlss)
library(pscl)
library(MASS)
library(dplyr)
library(ggplot2)

# Read data
sh.data <- read.csv('sh_gamlss/TelemetryTransect17May2016_SOSH-PFSH-COMU.csv')

# Use June 2011 as sample
sh.sample <- sh.data %>%
  filter(Year == 2011,
         Month == 6) %>%
  dplyr::select(SOSHcount, 
                SOSH0610UD) %>%
  mutate(SOSHcount = as.integer(SOSHcount),
         SOSHpresent = SOSHcount > 0)
summary(sh.sample)

# Visualize presence as function of UD
ggplot(sh.sample,
       aes(x = SOSHpresent,
           y = SOSH0610UD)) +
  geom_violin() + 
  geom_boxplot(width = 0.1)

plot(sh.sample$SOSHcount, sh.sample$SOSHcount)

# Simple linear model
sh.lm <- lm(SOSHcount ~ SOSH0610UD, data = sh.sample)
plot(SOSHcount ~ SOSH0610UD, data = sh.sample)
abline(sh.lm)
summary(sh.lm)

# Negative binomial model
sh.nbm <- glm.nb(SOSHcount ~ SOSH0610UD, data = sh.sample)
plot(SOSHcount ~ SOSH0610UD, data = sh.sample)
abline(sh.nbm)
summary(sh.nbm)

# Zero-inflated negative binomial model
sh.zinbm <- zeroinfl(SOSHcount ~ SOSH0610UD, data = sh.sample, dist = 'negbin', EM = TRUE)
plot(SOSHcount ~ SOSH0610UD, data = sh.sample)
abline(sh.zinbm)
summary(sh.zinbm)

# NB vs ZINB
vuong(sh.nbm, sh.zinbm)

all(sh.nbm$y == sh.zinbm$y)
length(sh.nbm$y)
length(sh.zinbm$y)
data.frame(sh.nbm.y = sh.nbm$y,
           sh.zinbm.y = sh.zinbm$y) %>%
  mutate(row = row_number(),
         diff = abs(sh.nbm.y - sh.zinbm.y)) %>%
  group_by(diff > 0) %>%
  summarize(n())

sh.sample$SOSHcount %>% typeof
