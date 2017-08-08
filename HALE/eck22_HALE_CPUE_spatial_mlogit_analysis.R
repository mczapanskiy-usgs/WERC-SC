## this script analyzes (graphically and statistically) spatial results of mlogit models
## seasonal and yearly effects on:
##   event types (pred, other, none)
##   predator events (rat, cat, mongoose)
## results from 'eck18_HALE_CPUE_spatial_analysisOfVar.R'

library(stats)
library(plyr)
library(dplyr)
library(tidyr)
library(data.table)
library(tidyr)
library(stringr)
library(ggplot2)
library(mosaic)

setwd("~/WERC-SC/HALE")

#### READ IN FITTED RESULTS FROM BEST FIT MODELS
# ## event type (predator, other, no event) uses base model w/ traplineYear = random effect, season + year = indiv. specif. was best fit
# lunar: 
read.csv(file = '~/WERC-SC/HALE/outputs/fitted_cpue_S_preds_eck18.csv',
         stringsAsFactors = FALSE) -> fitted_S_events
# weather: 
read.csv(file = '~/WERC-SC/HALE/outputs/fitted_cpue_S_events_eck18.csv',
         stringsAsFactors = FALSE) -> fitted_S_events