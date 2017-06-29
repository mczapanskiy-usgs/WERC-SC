## this script analyzes (graphically and statistically) spatial results of mlogit models
## seasonal and yearly effects on:
##   event types (pred, other, none)
##   predator events (rat, cat, mongoose)

library(stats)
library(plyr)
library(dplyr)
library(data.table)
library(tidyr)
library(stringr)
library(ggplot2)
library(mosaic)

setwd("~/WERC-SC/HALE")

#### READ IN FITTED RESULTS FROM BEST FIT MODELS
# event type (predator, other, no event) data - model w/ traplineYear = random effect, season + year = indiv. specif. was best fit
read.csv('~/WERC-SC/HALE/fitted_cpue_event_sub.csv',
         stringsAsFactors = FALSE) <- fitted_events
# pred event data (rat, cat, or mongoose) data model 23 w/ Trapline = random effect, season + year = indiv. specif. was best fit
read.csv('~/WERC-SC/HALE/fitted_cpue_model23.csv',
         stringsAsFactors = FALSE) -> fitted_preds

# create long versions of fitted results from mlogit model (variable = pred type, value = fitted CPUE probability)
fitted_events_lg <- melt(fitted_events, id.vars = c("Trapline", "Year", "Season"),
                        measure.vars = c("catCaught", "mongooseCaught", "ratCaught"))
fitted_preds_lg <- melt(fitted_preds, id.vars = c("Trapline", "Year", "Season"),
                       measure.vars = c("catCaught", "mongooseCaught", "ratCaught"))
dev.off()

#### SEASON
### use fitted results of mlogit model23 to graph fitted frequencies
fit_preds_season <- ggplot(fitted_preds_lg, aes(x=Season, y=value)) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = 'free') +
  scale_x_discrete(limits = c('Pre-laying', 'Incubation', 'Nestling', 'offSeason')) +
  theme_bw() +
  labs(x = 'Season', y = 'Probability of Events per Unit Effort')
fit_preds_season

#### YEAR
### use fitted results of mlogit model23 to graph fitted frequencies
fit_preds_year <- ggplot(fitted_preds_lg, aes(x=Year, y=value)) +
  geom_point(aes(color=Season)) +
  facet_wrap(~ variable, scales = 'free') +
  theme_bw() +
  labs(x = 'Year', y = 'Probability of Events per Unit Effort')
fit_preds_year

