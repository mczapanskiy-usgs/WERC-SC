library(gamlss)
library(dplyr)
library(foreach)
library(doParallel)
library(MASS)

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
  foreach(month = months) %do% {
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
         data = month.data,
         geo.model = geo.model,
         ocean.model = ocean.model)
  }
}

stopCluster(gamlssCl)

# Utility function for accessing models
get.model <- function(dist = 'SI', month, predictors) {
  distIdx <- match(dist, discrete.dist)
  if(is.na(dist)) stop(sprintf('Distribution %s not found', dist))
  monthIdx <- match(month, months)
  if(is.na(month)) stop(sprintf('Month %i not found', month))
  if(predictors == 'geo') {
    monthly.geo.ocean.models[[distIdx]][[monthIdx]]$geo.model
  } else if (predictors == 'ocean') {
    monthly.geo.ocean.models[[distIdx]][[monthIdx]]$ocean.model
  } else {
    stop('Parameter predictors must be "geo" or "ocean"')
  }
}

# Collect model fit parameters

# Utility function for calculating AIC weight. vAIC is a vector of AIC values
calcAICw <- function(vAIC) {
  deltaAIC <- vAIC - min(vAIC, na.rm = TRUE)
  relLikelihood <- exp(-0.5 * deltaAIC)
  normalizingFactor <- sum(relLikelihood, na.rm = TRUE)
  relLikelihood / normalizingFactor
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

# Sort models by month and quality of fit
monthly.geo.ranks <- model.rankings %>%
  group_by(Month) %>%
  mutate(GeoDeltaAIC = GeoAIC - min(GeoAIC, na.rm = TRUE),
         GeoAICw = calcAICw(GeoAIC) %>% round(3)) %>%
  arrange(-GeoAICw) %>% 
  slice(1:4) %>% 
  ungroup %>%
  select(Distribution:GeoAICw)
monthly.ocean.ranks <- model.rankings %>%
  group_by(Month) %>%
  mutate(OceanDeltaAIC = OceanAIC - min(OceanAIC, na.rm = TRUE),
         OceanAICw = calcAICw(OceanAIC) %>% round(3)) %>%
  arrange(-OceanAICw) %>% 
  slice(1:4) %>%
  ungroup %>%
  select(Distribution, Month, OceanConverged:OceanAICw)

# Identify best available distribution by product of AIC weight across months
monthly.geo.ranks %>% 
  group_by(Distribution) %>%
  summarize(N = n(),
            meanAICw = mean(GeoAICw)) %>%
  ungroup %>%
  arrange(-N, -meanAICw)

monthly.ocean.ranks %>% 
  group_by(Distribution) %>%
  summarize(N = n(),
            meanAICw = mean(OceanAICw)) %>%
  ungroup %>%
  arrange(-N, -meanAICw)

# We see SI and SICHEL are the only two distributions to show up in both models' top 4 across months
# SI has the slightly higher mean AIC weight, so we'll go with that.

# Cull models using selected distribution
nCores <- detectCores()
gamlssCl <- makeCluster(nCores)
registerDoParallel(gamlssCl)

culled.models <- foreach(month = months[1],
                         .packages = c('gamlss',
                                       'dplyr')) %do% {
  # Filter SOSH data to the month of interest
  month.data <- filter(sosh.data, Month == month)
  
  # Function to recursively re-fit model with n-1 terms until quality degrades
  cull.vars <- function(model, var.list) {
    if(length(var.list) == 0) 
      stop('All variables culled')
    
    new.formula <- as.formula(sprintf('~ . - %s', var.list[1]))
    model2 <- try(update(model, new.formula))
    if(class(model2) == 'try-error')
      return(NULL)
    
    if(extractAIC(model)[2] < extractAIC(model2)[2]) 
      return(model)
    
    cull.vars(model2, var.list[-1])
  }
  
  # Cull geographic model
  # Re-run model (this is a workaround, dropterm doesn't work with models produced in the earlier foreach loop)
  geo.model <- try(gamlss(SOSHcount ~ cs(Latitude) + cs(DistCoast) + cs(Dist200) + cs(DepCI) + offset(log(Binarea)),
                          data = month.data,
                          family = SI,
                          control = gamlss.control(n.cyc = 100)))
  
  # Re-fit models with n-1 terms to gauge relative significance
  if(class(geo.model) != 'try-error') {
    geo.var.rank <- dropterm(geo.model, test = 'Chisq') %>% 
      mutate(Variable = row.names(.)) %>% 
      arrange(AIC) %>%
      filter(Variable != '<none>')
    
    best.geo.model <- cull.vars(geo.model, geo.var.rank$Variable)
  }
  
  # Repeat process with oceanographic model
  ocean.model <- try(gamlss(SOSHcount ~ cs(SSTmean) + cs(STD_SST) + cs(MEAN_Beaufort) + cs(L10CHLproxy) + 
                              cs(STD_CHL_log10) + as.factor(Watermass) + cs(L10CHLsurvey) + cs(CHLsurvanom) + 
                              cs(L10CHLsurvclim) + cs(FCPI) + offset(log(Binarea)),
                            data = month.data,
                            family = SI,
                            control = gamlss.control(n.cyc = 100)))
  
  if(class(ocean.model) != 'try-error') {
    ocean.var.rank <- dropterm(ocean.model, test = 'Chisq') %>% 
      mutate(Variable = row.names(.)) %>% 
      arrange(AIC) %>%
      filter(Variable != '<none>')
    
    best.ocean.model <- cull.vars(ocean.model, ocean.var.rank$Variable)
  }
  
  list(month = month,
       geo.var.rank = geo.var.rank,
       best.geo.model = best.geo.model,
       ocean.var.rank = ocean.var.rank,
       best.ocean.model = best.ocean.model)
}
                                                  
stopCluster(gamlssCl)



library(dplyr)
library(gamlss)
nCores <- detectCores()
gamlssCl <- makeCluster(nCores)
registerDoParallel(gamlssCl)
test <- foreach(s = unique(iris$Species)) %do% {
  species.data <- filter(iris, Species == s)
  model <- gamlss(Petal.Length ~ cs(Sepal.Length) + cs(Sepal.Width) + cs(Petal.Width), 
                  data = species.data, 
                  family = GA)
  best.model <- stepGAIC.CH(model)
  
  model.terms <- model %>% formula %>% terms.formula %>% attr('term.labels')
  best.model.terms <- best.model %>% formula %>% terms.formula %>% attr('term.labels')
  dropped.terms <- setdiff(model.terms, best.model.terms)
  
  list(species = s,
       best.model = best.model,
       dropped.terms = dropped.terms)
}
stopCluster(gamlssCl)
test.par
# [[1]]
# Df AIC LRT Pr(Chi)     Variable
# 1 NA  NA  NA      NA Sepal.Length
# 2 NA  NA  NA      NA  Sepal.Width
# 3 NA  NA  NA      NA Petal.Length
# 
# [[2]]
# Df AIC LRT Pr(Chi)     Variable
# 1 NA  NA  NA      NA Sepal.Length
# 2 NA  NA  NA      NA  Sepal.Width
# 3 NA  NA  NA      NA Petal.Length
# 
# [[3]]
# Df AIC LRT Pr(Chi)     Variable
# 1 NA  NA  NA      NA Sepal.Length
# 2 NA  NA  NA      NA  Sepal.Width
# 3 NA  NA  NA      NA Petal.Length

test.serial <- foreach(s = unique(iris$Species)) %do% {
  species.data <- filter(iris, Species == s)
  model <- gamlss(Petal.Length ~ Sepal.Length + Sepal.Width + Petal.Length, 
                  data = species.data, 
                  family = GA)
  var.rank <- dropterm(model, test = 'Chisq') %>%
    mutate(Variable = row.names(.)) %>% 
    arrange(AIC) %>%
    filter(Variable != '<none>')
  
  var.rank
}
test.serial
# [[1]]
# Df       AIC        LRT   Pr(Chi)     Variable
# 1  1 -31.66335 0.06406465 0.8001832  Sepal.Width
# 2  0 -29.72741 0.00000000        NA Petal.Length
# 3  1 -29.43731 2.29010516 0.1302011 Sepal.Length
# 
# [[2]]
# Df      AIC       LRT      Pr(Chi)     Variable
# 1  0 31.03608  0.000000           NA Petal.Length
# 2  1 33.81852  4.782442 2.875132e-02  Sepal.Width
# 3  1 56.00459 26.968510 2.067972e-07 Sepal.Length
# 
# [[3]]
# Df      AIC         LRT      Pr(Chi)     Variable
# 1  1 16.29265  0.08628226 7.689578e-01  Sepal.Width
# 2  0 18.20637  0.00000000           NA Petal.Length
# 3  1 77.14978 60.94341742 5.873901e-15 Sepal.Length

model <- gamlss(Petal.Length ~ cs(Sepal.Length) + cs(Sepal.Width) + cs(Petal.Length) + Species, 
                data = iris, 
                family = GA)
stepGAIC.CH(model)