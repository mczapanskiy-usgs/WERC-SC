library(gamlss)
library(dplyr)
library(foreach)
library(doParallel)
library(MASS)
library(ggplot2)

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

# Cull models using selected distribution (SI)

# Recursively drop parameters by largest decrease in AIC until model degrades
cull.model <- function(model) {
  aic <- extractAIC(model)[2]
    
  # Re-fit model to n-1 terms
  model.terms <- model %>% formula %>% terms
  term.labels <- attr(model.terms, 'term.labels')
  submodels <- foreach(i = seq(term.labels)) %do% {
    dropped <- term.labels[i]
    submodel <- try(update(model, reformulate(sprintf('. - %s', dropped))))
    list(dropped = dropped,
         submodel = submodel)
  }
  
  # Which submodels converged? Which submodel has the greatest decrease in AIC?
  drop.results <- foreach(i = seq(submodels), .combine = rbind) %do% {
    tryCatch(with(submodels[[i]], data.frame(dropped = dropped, 
                                             i = i,
                                             converged = submodel$converged,
                                             AIC = extractAIC(submodel)[2],
                                             deltaAIC = extractAIC(submodel)[2] - aic)),
             error = function(e) data.frame(dropped = NA,
                                            i = NA,
                                            converged = NA,
                                            AIC = NA,
                                            deltaAIC = NA))
  } %>%
    filter(converged) %>%
    arrange(deltaAIC)
  
  # If no submodels converge or if no submodels are an improvement, return the original model
  if(nrow(drop.results) == 0 || min(drop.results$deltaAIC, na.rm = TRUE) > 0) {
    return(model)
  } else {
    # Otherwise, repeat the process on the best submodel
    best.submodel <- submodels[[drop.results$i[1]]]$submodel
    return(cull.model(best.submodel))
  }
}

# Month 6 
month6 <- filter(sosh.data, Month == 6)

# Geo
geo6global <- gamlss(SOSHcount ~ cs(Latitude) + cs(DistCoast) + cs(Dist200) + cs(DepCI) + offset(log(Binarea)),
                     data = month6,
                     family = SI,
                     control = gamlss.control(n.cyc = 100))
geo6dropterm <- dropterm(geo6global, test = 'Chisq')
geo6culled <- cull.model(geo6global)
geo6dropped <- setdiff(terms(geo6global) %>% attr('term.labels'), terms(geo6culled) %>% attr('term.labels'))

# Ocean
ocean6global <- gamlss(SOSHcount ~ cs(SSTmean) + cs(STD_SST) + cs(MEAN_Beaufort) + cs(L10CHLproxy) + 
                         cs(STD_CHL_log10) + as.factor(Watermass) + cs(L10CHLsurvey) + cs(CHLsurvanom) + 
                         cs(L10CHLsurvclim) + cs(FCPI) + offset(log(Binarea)),
                       data = month6,
                       family = SI,
                       control = gamlss.control(n.cyc = 100))
ocean6dropterm <- dropterm(ocean6global, test = 'Chisq')
ocean6culled <- cull.model(ocean6global)
ocean6dropped <- setdiff(terms(ocean6global) %>% attr('term.labels'), terms(ocean6culled) %>% attr('term.labels'))

# Month 7 
month7 <- filter(sosh.data, Month == 7)

# Geo
geo7global <- gamlss(SOSHcount ~ cs(Latitude) + cs(DistCoast) + cs(Dist200) + cs(DepCI) + offset(log(Binarea)),
                     data = month7,
                     family = SI,
                     control = gamlss.control(n.cyc = 100))
geo7dropterm <- dropterm(geo7global, test = 'Chisq')
geo7culled <- cull.model(geo7global)
geo7dropped <- setdiff(terms(geo7global) %>% attr('term.labels'), terms(geo7culled) %>% attr('term.labels'))

# Ocean
ocean7global <- try(gamlss(SOSHcount ~ cs(SSTmean) + cs(STD_SST) + cs(MEAN_Beaufort) + cs(L10CHLproxy) + 
                             cs(STD_CHL_log10) + as.factor(Watermass) + cs(L10CHLsurvey) + cs(CHLsurvanom) + 
                             cs(L10CHLsurvclim) + cs(FCPI) + offset(log(Binarea)),
                           data = month7,
                           family = SI,
                           control = gamlss.control(n.cyc = 100)))
ocean7dropterm <- dropterm(ocean7global, test = 'Chisq')
ocean7culled <- cull.model(ocean7global)
ocean7dropped <- setdiff(terms(ocean7global) %>% attr('term.labels'), terms(ocean7culled) %>% attr('term.labels'))

# Month 9 
month9 <- filter(sosh.data, Month == 9)

# Geo
# For some reason month 9 geo model doesn't converge reliably
geo9global <- try.fit(SOSHcount ~ cs(Latitude) + cs(DistCoast) + cs(Dist200) + cs(DepCI) + offset(log(Binarea)),
                      data = month9,
                      family = SI)
geo9dropterm <- dropterm(geo9global, test = 'Chisq')
geo9culled <- cull.model(geo9global)
geo9dropped <- setdiff(terms(geo9global) %>% attr('term.labels'), terms(geo9culled) %>% attr('term.labels'))

# Ocean
ocean9global <- gamlss(SOSHcount ~ cs(SSTmean) + cs(STD_SST) + cs(MEAN_Beaufort) + cs(L10CHLproxy) + 
                         cs(STD_CHL_log10) + as.factor(Watermass) + cs(L10CHLsurvey) + cs(CHLsurvanom) + 
                         cs(L10CHLsurvclim) + cs(FCPI) + offset(log(Binarea)),
                       data = month9,
                       family = SI,
                       control = gamlss.control(n.cyc = 100))
ocean9dropterm <- dropterm(ocean9global, test = 'Chisq')
ocean9culled <- cull.model(ocean9global)
ocean9dropped <- setdiff(terms(ocean9global) %>% attr('term.labels'), terms(ocean9culled) %>% attr('term.labels'))

# Month 10 
month10 <- filter(sosh.data, Month == 10)

# Geo
geo10global <- gamlss(SOSHcount ~ cs(Latitude) + cs(DistCoast) + cs(Dist200) + cs(DepCI) + offset(log(Binarea)),
                     data = month10,
                     family = SI,
                     control = gamlss.control(n.cyc = 100))
geo10dropterm <- dropterm(geo10global, test = 'Chisq')
geo10culled <- cull.model(geo10global)
geo10dropped <- setdiff(terms(geo10global) %>% attr('term.labels'), terms(geo10culled) %>% attr('term.labels'))

# Ocean
ocean10global <- try(gamlss(SOSHcount ~ cs(SSTmean) + cs(STD_SST) + cs(MEAN_Beaufort) + cs(L10CHLproxy) + 
                             cs(STD_CHL_log10) + as.factor(Watermass) + cs(L10CHLsurvey) + cs(CHLsurvanom) + 
                             cs(L10CHLsurvclim) + cs(FCPI) + offset(log(Binarea)),
                           data = month10,
                           family = SI,
                           control = gamlss.control(n.cyc = 100)))
ocean10dropterm <- dropterm(ocean10global, test = 'Chisq')
ocean10culled <- cull.model(ocean10global)
ocean10dropped <- setdiff(terms(ocean10global) %>% attr('term.labels'), terms(ocean10culled) %>% attr('term.labels'))

# Geo variable rankings
geo.var.ranks <- mapply(function(df, month) {
  df %>% 
    mutate(Month = month,
           Variable = row.names(.), 
           AICw = calcAICw(AIC)) %>% 
    arrange(-AICw) %>%
    mutate(VarRank = row_number())
},
list(geo6dropterm,
     geo7dropterm,
     #geo9dropterm,
     geo10dropterm),
c(6, 7, 10),
SIMPLIFY = FALSE) %>%
  bind_rows 

geo.mean.ranks <- geo.var.ranks %>%
  group_by(Variable) %>%
  summarize(MeanRank = mean(VarRank)) %>%
  arrange(MeanRank, Variable)

ggplot(geo.var.ranks,
       aes(x = Variable,
           y = VarRank,
           color = factor(Month),
           group = factor(Month))) +
  geom_point(size = 3) +
  geom_line() +
  scale_x_discrete(limits = geo.mean.ranks$Variable)

# Ocean variable rankings
ocean.var.ranks <- mapply(function(df, month) {
  df %>% 
    mutate(Month = month,
           Variable = row.names(.), 
           AICw = calcAICw(AIC)) %>% 
    arrange(-AICw) %>%
    mutate(VarRank = row_number())
},
list(ocean6dropterm,
     ocean7dropterm,
     ocean9dropterm,
     ocean10dropterm),
c(6, 7, 9, 10),
SIMPLIFY = FALSE) %>%
  bind_rows 

ocean.mean.ranks <- ocean.var.ranks %>%
  group_by(Variable) %>%
  summarize(MeanRank = mean(VarRank)) %>%
  arrange(MeanRank, Variable)

ggplot(ocean.var.ranks,
       aes(x = Variable,
           y = VarRank,
           color = factor(Month),
           group = factor(Month))) +
  geom_point(size = 3) +
  geom_line() +
  scale_x_discrete(limits = ocean.mean.ranks$Variable)

culled.models <- foreach(month = months) %do% {
  # Filter SOSH data to the month of interest
  month.data <- filter(sosh.data, Month == month)
  
  # Cull geographic model
  # Re-run model (this is a workaround, stepGAIC.CH doesn't work with models produced in the earlier foreach loop)
  geo.model <- try(gamlss(SOSHcount ~ cs(Latitude) + cs(DistCoast) + cs(Dist200) + cs(DepCI) + offset(log(Binarea)),
                          data = month.data,
                          family = SI,
                          control = gamlss.control(n.cyc = 100)))
  
  # Use stepGAIC.CH to find best parsimonious model for geographic variables
  best.geo.model <- NULL
  dropped.geo.terms <- NULL
  delta.geo.AIC <- NULL
  if(class(geo.model) != 'try-error') {
    best.geo.model <- stepGAIC.CH(geo.model)
    
    geo.model.terms <- geo.model %>% formula %>% terms.formula %>% attr('term.labels')
    best.geo.model.terms <- best.geo.model %>% formula %>% terms.formula %>% attr('term.labels')
    dropped.geo.terms <- setdiff(geo.model.terms, best.geo.model.terms)
    
    delta.geo.AIC <- extractAIC(best.geo.model)[2] - extractAIC(geo.model)[2]
  }
  
  # Repeat process with oceanographic model
  ocean.model <- try(gamlss(SOSHcount ~ cs(SSTmean) + cs(STD_SST) + cs(MEAN_Beaufort) + cs(L10CHLproxy) + 
                              cs(STD_CHL_log10) + as.factor(Watermass) + cs(L10CHLsurvey) + cs(CHLsurvanom) + 
                              cs(L10CHLsurvclim) + cs(FCPI) + offset(log(Binarea)),
                            data = month.data,
                            family = SI,
                            control = gamlss.control(n.cyc = 100)))
  
  best.ocean.model <- NULL
  dropped.ocean.terms <- NULL
  delta.ocean.AIC <- NULL
  if(class(ocean.model) != 'try-error') {
    best.ocean.model <- stepGAIC.CH(ocean.model)
    
    ocean.model.terms <- ocean.model %>% formula %>% terms.formula %>% attr('term.labels')
    best.ocean.model.terms <- best.ocean.model %>% formula %>% terms.formula %>% attr('term.labels')
    dropped.ocean.terms <- setdiff(ocean.model.terms, best.ocean.model.terms)
    
    delta.ocean.AIC <- extractAIC(best.ocean.model)[2] - extractAIC(ocean.model)[2]
  }
  
  list(month = month,
       best.geo.model = best.geo.model,
       dropped.geo.terms = dropped.geo.terms,
       delta.geo.AIC = delta.geo.AIC,
       best.ocean.model = best.ocean.model,
       dropped.ocean.terms = dropped.ocean.terms,
       delta.ocean.AIC = delta.ocean.AIC)
}
