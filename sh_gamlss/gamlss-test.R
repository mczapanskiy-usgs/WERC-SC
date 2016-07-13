library(gamlss)
library(dplyr)
library(foreach)

# Read data
sosh.data <- read.csv('sh_gamlss/TelemetryTransect17May2016_SOSH-PFSH-COMU.csv') %>% 
  dplyr::select(SOSHcount, Binarea, Month, 
                Latitude, DistCoast, Dist200, DepCI,
                SSTmean, STD_SST, MEAN_Beaufort, L10CHLproxy, STD_CHL_log10, Watermass,
                L10CHLsurvey, CHLsurvanom, L10CHLsurvclim, FCPI) %>%
  filter(Month > 2) %>% # exclude tiny January, February surveys
  mutate(SOSHcount = as.integer(SOSHcount),
         Month = factor(Month)) %>%
  na.omit
head(sosh.data)
summary(sosh.data)

# Create models using canonical discrete distributions for each month.
# Geographic + oceanographic models
# geo = SOSHcount ~  cs(Latitude)+cs(DistCoast)+cs(Dist200)+cs(DepCI) + offset(log(Binarea))
# ocean = SOSHcount ~ cs(SSTmean)+cs(STD_SST)+cs(MEAN_Beaufort)+cs(L10CHLproxy)+cs(STD_CHL_log10)+Watermass+cs(L10CHLsurvey)+
#   cs(CHLsurvanom)+cs(L10CHLsurvclim)+cs(FCPI) + offset(log(Binarea))
discrete.dist <- c('PO', 'NBI', 'NBII', 'DEL', 'PIG', 'SI', 'SICHEL', 'ZIP', 'ZIP2')
months <- c(6, 7, 9, 10)
# For each discrete distribution...
monthly.geo.ocean.models <- foreach(dist = discrete.dist) %do% {
  # For each month's survey...
  foreach(month = months) %do% {
    print(sprintf('Distribution %s, month %i', dist, month))
    
    # Filter SOSH data to the month of interest
    month.data <- filter(sosh.data, Month == month)
    if(nrow(month.data) == 0) stop(sprintf('No data in month %i', month))
    
    # Fit geographic model
    print('Fitting geographic model')
    geo.model <- try(gamlss(SOSHcount ~ cs(Latitude) + cs(DistCoast) + cs(Dist200) + cs(DepCI) + offset(log(Binarea)),
                            data = month.data,
                            family = get('dist')))
    # Try one round of refitting if it doesn't converge
    if(class(geo.model) != 'try-error' && !getElement(geo.model, 'converged')) {
      print('Refitting geographic model')
      try(geo.model <- refit(geo.model))
    }
    
    # Fit oceanographic model
    print('Fitting oceanographic model')
    ocean.model <- try(gamlss(SOSHcount ~ cs(SSTmean) + cs(STD_SST) + cs(MEAN_Beaufort) + cs(L10CHLproxy) + 
                                cs(STD_CHL_log10) + Watermass + cs(L10CHLsurvey) + cs(CHLsurvanom) + 
                                cs(L10CHLsurvclim) + cs(FCPI) + offset(log(Binarea)),
                              data = month.data,
                              family = get('dist')))
    # Try one round of refitting if it doesn't converge
    if(class(ocean.model) != 'try-error' && !getElement(ocean.model, 'converged')) {
      print('Refitting oceanographic model')
      try(ocean.model <- refit(ocean.model))
    }
    
    list(dist = dist,
         month = month,
         geo.model = geo.model,
         ocean.model = ocean.model)
  }
}

model.rankings <- foreach(dist.list = monthly.geo.ocean.models, .combine = rbind) %do% {
  foreach(models = dist.list, .combine = rbind) %do% {
    data.frame(Distribution = models$dist,
               Month = models$month,
               GeoConverged = tryCatch(getElement(models$geo.model, 'converged'), error = function(e) NA),
               GeoEDF = tryCatch(extractAIC(models$geo.model)[1], error = function(e) NA),
               GeoAIC = tryCatch(extractAIC(models$geo.model)[2], error = function(e) NA),
               OceanConverged = tryCatch(getElement(models$ocean.model, 'converged'), error = function(e) NA),
               OceanEDF = tryCatch(extractAIC(models$ocean.model)[1], error = function(e) NA),
               OceanAIC = tryCatch(extractAIC(models$ocean.model)[2], error = function(e) NA))
  }
}

model.rankings %>%
  group_by(Month) %>%
  arrange(-GeoConverged, GeoAIC) %>% 
  slice(1:4)
model.rankings %>%
  group_by(Month) %>%
  arrange(-OceanConverged, OceanAIC) %>% 
  slice(1:4)