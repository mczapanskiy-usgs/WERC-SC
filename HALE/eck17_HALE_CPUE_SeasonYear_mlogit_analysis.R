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

read.csv('~/WERC-SC/HALE/fitted_cpue_model23.csv',
         stringsAsFactors = FALSE) -> fitted_cpue
# create long version of fitted results from mlogit model (variable = pred type, value = fitted CPUE probability)
fitted_cpue_lg <- melt(fitted_cpue, id.vars = c("Trapline", "Year", "Season"),
                       measure.vars = c("catCaught", "mongooseCaught", "ratCaught"))


#### SEASON
### use fitted results of mlogit model23 to graph fitted frequencies
fit_preds_season <- ggplot(fitted_cpue_lg, aes(x=Season, y=value)) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = 'free') +
  scale_x_discrete(limits = c('Pre-laying', 'Incubation', 'Nestling', 'offSeason')) +
  theme_bw() +
  labs(x = 'Season', y = 'Probability of Events per Unit Effort')
fit_preds_season

aov_season_cat <- lm(catCaught ~ Season, data=fitted_cpue, y=offSeason)
summary(aov_season_cat)
anova(aov_season_cat)
aov_season_rat <- lm(ratCaught ~ Season, data=fitted_cpue)
summary(aov_season_rat)
anova(aov_season_rat)
aov_season_mongoose <- lm(mongooseCaught ~ Season, data=fitted_cpue)
summary(aov_season_mongoose)
anova(aov_season_mongoose)

#### YEAR
### use fitted results of mlogit model23 to graph fitted frequencies
fit_preds_year <- ggaov_year_cat <- aov(catCaught ~ Year, data=fitted_cpue)
summary(aov_year_cat)
aov_year_rat <- aov(ratCaught ~ Year, data=fitted_cpue)
summary(aov_year_rat)
aov_year_mongoose <- aov(mongooseCaught ~ Year, data=fitted_cpue)
summary(aov_year_mongoose)
plot(fitted_cpue_lg, aes(x=Year, y=value)) +
  geom_point(aes(color=Season)) +
  facet_wrap(~ variable, scales = 'free') +
  theme_bw() +
  labs(x = 'Year', y = 'Probability of Events per Unit Effort')
fit_preds_year



