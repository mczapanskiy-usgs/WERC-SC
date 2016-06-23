library(gamlss)
library(pscl)
library(MASS)
library(dplyr)
library(ggplot2)

# Read data
sh.data <- read.csv('sh_gamlss/TelemetryTransect17May2016_SOSH-PFSH-COMU.csv') %>%
  select(SOSHcount, Binarea, Month, FCPI, Dist200, SSTmean) %>%
  filter(Month > 2) %>% # exclude tiny January, February surveys
  mutate(Month = factor(Month)) %>%
  na.omit
head(sh.data)
summary(sh.data)

# Test model: SOSHcount as function of FCPI, Dist200, SSTmean (all three cubic splines) with Month as random variable 
# and log(Binarea) as offset
gamlssTest <- gamlss(SOSHcount ~ FCPI,
#                        offset(log(Binarea)) + random(Month) + 
#                        cs(FCPI) + cs(Dist200) + cs(SSTmean), 
                     data = sh.data, 
                     family = NBI)

d <- data.frame(x <- runif(100)) %>%
  mutate(y = sin(x * 2*pi) + runif(100))
gamlssTest <- gamlss(y ~ cs(x), data = d)
ggplot(mutate(d, y2 = predict(gamlssTest)),
       aes(x, y)) +
  geom_point() + 
  geom_line(aes(x, y2, group = 1))

d <- data.frame(x = runif(100)) %>%
  mutate(y = rpois(100, x))
gamlssTest <- gamlss(y ~ x, data = d, family = PO)
ggplot(mutate(d, y2 = predict(gamlssTest)), 
       aes(x, y)) +
  geom_point() +
  geom_line(aes(x, y2, group = 1))