library(gamlss)
library(dplyr)

# Read data
sosh.data <- read.csv('sh_gamlss/TelemetryTransect17May2016_SOSH-PFSH-COMU.csv') %>%
  dplyr::select(SOSHcount, Binarea, Month, FCPI, Dist200, SSTmean) %>%
  filter(Month > 2) %>% # exclude tiny January, February surveys
  mutate(Month = factor(Month)) %>%
  na.omit
head(sosh.data)
summary(sosh.data)

pfsh.data <- read.csv('sh_gamlss/TelemetryTransect17May2016_SOSH-PFSH-COMU.csv') %>%
  dplyr::select(PFSHcount, Binarea, Month, FCPI, Dist200, SSTmean) %>%
  filter(Month > 2) %>% # exclude tiny January, February surveys
  mutate(Month = factor(Month)) %>%
  na.omit
head(pfsh.data)
summary(pfsh.data)

sosh.gamlss <- gamlss(SOSHcount ~ FCPI +
                        offset(log(Binarea)) + random(Month) + 
                        cs(FCPI) + cs(Dist200) + cs(SSTmean),
                      data = sosh.data,
                      family = NBI)

# Test model: SOSHcount as function of FCPI, Dist200, SSTmean (all three cubic splines) with Month as random variable 
# and log(Binarea) as offset
gamlssTest <- gamlss(SOSHcount ~ FCPI,
#                        offset(log(Binarea)) + random(Month) + 
#                        cs(FCPI) + cs(Dist200) + cs(SSTmean), 
                     data = sosh.data, 
                     family = NBI)

rdist <- Vectorize(function(x) {
  result <- 0
  while(runif(1) <= x) result <- result + 1
  return(result)
})
d <- data.frame(x = runif(100) * .5) %>%
  mutate(y = rdist(x))

gamlssTest <- gamlss(y ~ x, data = d, family = PO)

View(sosh.data2)