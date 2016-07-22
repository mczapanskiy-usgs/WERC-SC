library(gamlss)
library(dplyr)
library(foreach)
library(doParallel)

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

# Wrapper for gamlss. Error handling and refitting.
N_REFIT <- 5
try.fit <- function(formula, data, family, n_refit = N_REFIT) {
  # Try to fit model
  model <- try(gamlss(formula, data = data, family = family))
  fitAttempts <- 1
  
  # Keep refitting model until...
  while(fitAttempts <= n_refit &&             # ... we run out of attempts,
        class(model) != 'try-error' &&        # get an error,
        !getElement(model, 'converged')) {    # or model converges
    model <- try(refit(model))
    fitAttempts <- fitAttempts + 1
  }
  # Return fitting results. Hopefully it's a fitted model, may be a try-error
  model
}

# Register parallel backend for speed increase
nCores <- detectCores()
gamlssCl <- makeCluster(nCores)
registerDoParallel(gamlssCl)

# Create models using canonical discrete distributions for each month.
# Geographic + oceanographic models
# geo = SOSHcount ~  cs(Latitude)+cs(DistCoast)+cs(Dist200)+cs(DepCI) + offset(log(Binarea))
# ocean = SOSHcount ~ cs(SSTmean)+cs(STD_SST)+cs(MEAN_Beaufort)+cs(L10CHLproxy)+cs(STD_CHL_log10)+Watermass+cs(L10CHLsurvey)+
#   cs(CHLsurvanom)+cs(L10CHLsurvclim)+cs(FCPI) + offset(log(Binarea))
discrete.dist <- c('PO', 'NBI', 'NBII', 'DEL', 'PIG', 'SI', 'SICHEL', 'ZIP', 'ZIP2')
months <- c(6, 7, 9, 10)
# For each discrete distribution...
monthly.geo.ocean.models <- foreach(dist = discrete.dist,
                                    .packages = c('foreach',
                                                  'doParallel',
                                                  'gamlss',
                                                  'dplyr')) %dopar% {
  # For each month's survey...
  foreach(month = months) %dopar% {
    print(sprintf('Distribution %s, month %i', dist, month))
    
    # Filter SOSH data to the month of interest
    month.data <- filter(sosh.data, Month == month)
    if(nrow(month.data) == 0) stop(sprintf('No data in month %i', month))
    
    # Fit geographic model
    print('Fitting geographic model')
    geo.model <- try.fit(SOSHcount ~ cs(Latitude) + cs(DistCoast) + cs(Dist200) + cs(DepCI) + offset(log(Binarea)),
                         data = month.data,
                         family = get('dist'))
    
    # Fit oceanographic model
    print('Fitting oceanographic model')
    ocean.model <- try.fit(SOSHcount ~ cs(SSTmean) + cs(STD_SST) + cs(MEAN_Beaufort) + cs(L10CHLproxy) + 
                                cs(STD_CHL_log10) + as.factor(Watermass) + cs(L10CHLsurvey) + cs(CHLsurvanom) + 
                                cs(L10CHLsurvclim) + cs(FCPI) + offset(log(Binarea)),
                           data = month.data,
                           family = get('dist'))
    
    list(dist = dist,
         month = month,
         geo.model = geo.model,
         ocean.model = ocean.model)
  }
}

stopCluster(gamlssCl)

# Collect model fit parameters
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

# Sort models by month and quality of fit
model.rankings %>%
  filter(GeoConverged) %>%
  group_by(Month) %>%
  arrange(-GeoConverged, GeoAIC) %>% 
  slice(1:4)
model.rankings %>%
  filter(OceanConverged) %>%
  group_by(Month) %>%
  arrange(-OceanConverged, OceanAIC) %>% 
  slice(1:4)
