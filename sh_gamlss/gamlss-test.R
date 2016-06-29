library(gamlss)
library(dplyr)

# Read data
sosh.data <- read.csv('sh_gamlss/TelemetryTransect17May2016_SOSH-PFSH-COMU.csv') %>%
  dplyr::select(SOSHcount, Binarea, Month, FCPI, Dist200, SSTmean) %>%
  filter(Month > 2) %>% # exclude tiny January, February surveys
  mutate(SOSHcount = as.integer(SOSHcount),
         Month = factor(Month)) %>%
  na.omit
head(sosh.data)
summary(sosh.data)

# Test model: SOSHcount as function of FCPI, Dist200, SSTmean (all three cubic splines) with Month as random variable 
# and log(Binarea) as offset
gamlssTest <- gamlss(SOSHcount ~ cs(FCPI) + cs(Dist200) + cs(SSTmean) +
                       offset(log(Binarea)) + random(Month),
                     data = sosh.data, 
                     family = NBI)

summary(gamlssTest)
plot(gamlssTest)
wp(gamlssTest)