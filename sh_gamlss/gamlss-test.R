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

# Create models using canonical discrete distributions.
# SOSHcount as function of FCPI, Dist200, SSTmean (all three cubic splines) with Month as random variable 
# and log(Binarea) as offset
discrete.dist <- c('PO', 'NBI', 'NBII', 'DEL', 'PIG', 'SI', 'SICHEL', 'ZIP', 'ZIP2')
all.models <- lapply(discrete.dist, function(dist) {
  result <- gamlss(SOSHcount ~ cs(FCPI) + cs(Dist200) + cs(SSTmean) +
                     offset(log(Binarea)) + random(Month),
                   data = sosh.data, 
                   family = get('dist'))
  if(!getElement(result, 'converged')) try(result <- refit(result))
  result
})

model.rankings <- sapply(all.models, function(model) c(family = model$family[1], # Distribution used
                                                    df = extractAIC(model)[1], # degrees of freedom
                                                    AIC = extractAIC(model)[2], # AIC value
                                                    converged = getElement(model, 'converged'))) %>% # whether model converged
  t %>%
  as.data.frame %>%
  arrange(AIC)

# Create models across surveys
# Use ZIP, which had the lowest AIC across all months
indiv.months <- lapply(unique(sosh.data$Month), function(month) filter(sosh.data, Month == month)) %>%
  lapply(function(month) {
    gamlss(SOSHcount ~ cs(FCPI) + cs(Dist200) + cs(SSTmean) +
             offset(log(Binarea)),
           data = sosh.data, 
           family = ZIP)
  })

summary(gamlssTest)
plot(gamlssTest)
wp(gamlssTest)