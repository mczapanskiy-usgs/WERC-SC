## this script analyzes (graphically and statistically) spatial results of mlogit models
## seasonal and yearly effects on:
##   event types (pred, other, none)
##   predator events (rat, cat, mongoose)

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
# event type (predator, other, no event) data - model w/ traplineYear = random effect, season + year = indiv. specif. was best fit
read.csv('~/WERC-SC/HALE/fitted_cpue_event_sub.csv',
         stringsAsFactors = FALSE) -> fitted_events
# pred event data (rat, cat, or mongoose) data model 23 w/ Trapline = random effect, season + year = indiv. specif. was best fit
read.csv('~/WERC-SC/HALE/fitted_cpue_model23.csv',
         stringsAsFactors = FALSE) -> fitted_preds

# create long versions of fitted results from mlogit model (variable = pred type, value = fitted CPUE probability)
fitted_events_lg <- melt(fitted_events, id.vars = c("Trapline", "Year", "Season"),
                        measure.vars = c("noEvent", "predatorEvent", "otherEvent"))
fitted_preds_lg <- melt(fitted_preds, id.vars = c("Trapline", "Year", "Season"),
                       measure.vars = c("catCaught", "mongooseCaught", "ratCaught"))
dev.off()

#### SEASON
# graph fitted frequencies
fit_events_season <- ggplot(fitted_events_lg, aes(x=Season, y=value)) +
  geom_boxplot() +
  facet_wrap(~ variable) + #, scales = 'free') +
  scale_x_discrete(limits = c('Pre-laying', 'Incubation', 'Nestling', 'offSeason')) +
  theme_bw() +
  labs(x = 'Season', y = 'Probability of Events per Unit Effort')
fit_events_season

fit_preds_season <- ggplot(fitted_preds_lg, aes(x=Season, y=value)) +
  geom_boxplot() +
  facet_wrap(~ variable) + #, scales = 'free') +
  scale_x_discrete(limits = c('Pre-laying', 'Incubation', 'Nestling', 'offSeason')) +
  theme_bw() +
  labs(x = 'Season', y = 'Probability of Events per Unit Effort')
fit_preds_season
## tables of mean and SD fitted frequencies
# seasonal event probabilities
seasonEvents <- fitted_events %>% 
  group_by(Season) %>% 
  summarize(mNone = mean(noEvent, na.rm = T), sdNone = sd(noEvent, na.rm = T),
            mPred = mean(predatorEvent, na.rm = T), sdPred = sd(predatorEvent, na.rm = T),
            mOther = mean(otherEvent, na.rm = T), sdOther = sd(otherEvent, na.rm = T))
# seasonal predEvent probabilities
seasonPreds <- fitted_preds %>% 
  group_by(Season) %>% 
  summarize(mRat = mean(ratCaught, na.rm = T), sdRat = sd(ratCaught, na.rm = T),
            mCat = mean(catCaught, na.rm = T), sdCat = sd(catCaught, na.rm = T),
            mMong = mean(mongooseCaught, na.rm = T), sdMong = sd(mongooseCaught, na.rm = T))
# save tables
write.table(seasonEvents, file = '~/WERC-SC/HALE/outputs/seasonEvents.csv',
            row.names = FALSE)
write.table(seasonPreds, file = '~/WERC-SC/HALE/outputs/seasonPreds.csv',
            row.names = FALSE)


#### YEAR
## tables and figures of mean and SD fitted frequencies
# annual event probabilities
yearEvents <- fitted_events %>% 
  gather(eventType, eventProb, noEvent:otherEvent) %>%
  group_by(Year, eventType) %>% 
  summarize(meanProb = mean(eventProb, na.rm = TRUE), sdProb = sd(eventProb, na.rm = TRUE))
ggplot(yearEvents, aes(x=Year, y=meanProb, color = eventType)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = meanProb-sdProb, ymax=meanProb+sdProb), width = 0.5) +
  theme_bw()
# fit_preds_year <- ggplot(yearEvents, aes(x=Year, y=)) +
# geom_point(aes(color=Season)) +
#   theme_bw() +
#   labs(x = 'Year', y = 'Probability of Events per Unit Effort')
# fit_preds_year

# annual predEvent probabilities
# yearPreds <- fitted_preds %>% 
#   group_by(Year) %>% 
#   summarize(mRat = mean(ratCaught, na.rm = T), sdRat = sd(ratCaught, na.rm = T),
#             mCat = mean(catCaught, na.rm = T), sdCat = sd(catCaught, na.rm = T),
#             mMong = mean(mongooseCaught, na.rm = T), sdMong = sd(mongooseCaught, na.rm = T))
yearPreds <- fitted_preds %>% 
  gather(predType, predProb, ratCaught:mongooseCaught) %>%
  group_by(Year, predType) %>% 
  summarize(meanProb = mean(predProb, na.rm = TRUE), sdProb = sd(predProb, na.rm = TRUE))
ggplot(yearPreds, aes(x=Year, y=meanProb, color = predType)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = meanProb-sdProb, ymax=meanProb+sdProb), width = 0.5) +
  theme_bw()

## graph fitted frequencies
# fit_preds_year <- ggplot(fitted_preds_lg, aes(x=Year, y=value)) +
#   geom_point(aes(color=Season)) +
#   facet_wrap(~ variable) + #, scales = 'free') +
#   theme_bw() +
#   labs(x = 'Year', y = 'Probability of Events per Unit Effort')
# fit_preds_year
# 
# fit_events_year <- ggplot(fitted_events_lg, aes(x=Year, y=value)) + 
#   geom_point(aes(color=Season)) +
#   facet_wrap(~ variable) + #, scales = 'free') +
#   theme_bw() +
#   labs(x = 'Year', y = 'Probability of Events per Unit Effort')
# fit_events_year

# save tables
write.table(yearEvents, file = '~/WERC-SC/HALE/outputs/yearEvents.csv',
            row.names = FALSE)
write.table(yearPreds, file = '~/WERC-SC/HALE/outputs/yearPreds.csv',
            row.names = FALSE)
