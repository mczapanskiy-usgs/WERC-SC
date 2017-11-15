## this script analyzes (graphically and statistically) spatial results of mlogit models
## seasonal and yearly effects on:
##   event types (pred, other, none)
##   predator events (rat, cat, mongoose)
## results from 'eck12_HALE_CPUE_analysisOfVar' and 'eck13_HALE_CPUE_analysisOfVar_subsettedFinal'

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
read.csv('~/WERC-SC/HALE/fitted_cpue_caughts_model2.csv',
         stringsAsFactors = FALSE) -> fitted_preds
read.csv('~/WERC-SC/HALE/fitted_cpue_caughts_month_model9.csv',
         stringsAsFactors = FALSE) -> fitted_preds_month


# create long versions of fitted results from mlogit model (variable = pred type, value = fitted CPUE probability)
fitted_events_lg <- melt(fitted_events, id.vars = c("Trapline", "Year", "Season"),
                        measure.vars = c("noEvent", "predatorEvent", "otherEvent"))
fitted_preds_lg <- melt(fitted_preds, id.vars = c("Trapline", "Year", "Season"),
                       measure.vars = c("catCaught", "mongooseCaught", "ratCaught"))
fitted_preds_month_lg <- melt(fitted_preds_month, id.vars = c("Trapline", "Year", "Month"),
                        measure.vars = c("catCaught", "mongooseCaught", "ratCaught")) %>% 
  mutate(Month = as.factor(Month))
dev.off()

#### SEASON
# graph fitted frequencies
ggplot(fitted_events_lg, aes(x=Season, y=value)) +
  geom_boxplot() +
  facet_wrap(~ variable) + #, scales = 'free') +
  scale_x_discrete(limits = c('Pre-laying', 'Incubation', 'Nestling', 'offSeason')) +
  ylim(0, 1) +
  theme_bw() +
  labs(x = 'Season', y = 'Probability of Events Occurance')
ggsave(width = 8.5, height = 5, dpi=300, filename = "~/WERC-SC/HALE/outputs/fitSeasonEvents_eck17.pdf")

ggplot(fitted_preds_lg, aes(x=Season, y=value)) +
  geom_boxplot() +
  facet_wrap(~ variable) + #, scales = 'free') +
  scale_x_discrete(limits = c('Pre-laying', 'Incubation', 'Nestling', 'offSeason')) +
  theme_bw() +
  labs(x = 'Season', y = 'Probability of Predator Type Caught')
ggsave(width = 8.5, height = 5, dpi=300, filename = "~/WERC-SC/HALE/outputs/fitSeasonPreds_eck17.pdf")

ggplot(fitted_preds_month_lg, aes(x=Month, y=value)) +
  geom_boxplot() +
  facet_wrap(~ variable) + #, scales = 'free') +
  theme_bw() +
  labs(x = 'Month', y = 'Probability of Predator Type Caught') 
  # scale_x_discrete(limits = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec")) +
  # theme(axis.text.x = element_text(angle=60, hjust=1))
ggsave(width = 8.5, height = 5, dpi=300, filename = "~/WERC-SC/HALE/outputs/fitSeasonPreds_month_eck17.pdf")

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
fitYearEvents <- fitted_events %>% 
  gather(eventType, eventProb, noEvent:otherEvent) %>%
  group_by(Year, eventType) %>% 
  summarize(meanProb = mean(eventProb, na.rm = TRUE), sdProb = sd(eventProb, na.rm = TRUE))
ggplot(fitYearEvents, aes(x=Year, y=meanProb, color = eventType)) +
  geom_line(size = 1) +
  geom_point(size = 2, shape = 18) +
  geom_errorbar(aes(ymin = meanProb-sdProb, ymax=meanProb+sdProb), width = 0.5) +
  ylim(0, 1) +
  theme_bw() +
  labs(x = 'Year', y = 'Mean Probability of Event Occurance')
ggsave(width = 8.5, height = 5, dpi=300, filename = "~/WERC-SC/HALE/outputs/fitYearEvents.pdf")

# annual predEvent probabilities
fitYearPreds <- fitted_preds %>% 
  gather(predType, predProb, ratCaught:mongooseCaught) %>%
  group_by(Year, predType) %>% 
  summarize(meanProb = mean(predProb, na.rm = TRUE), sdProb = sd(predProb, na.rm = TRUE))
ggplot(fitYearPreds, aes(x=Year, y=meanProb, color = predType)) +
  geom_line(size = 1) +
  geom_point(size = 2, shape = 18) +
  geom_errorbar(aes(ymin = meanProb-sdProb, ymax=meanProb+sdProb), width = 0.5) +
  theme_bw() + 
  labs(x = 'Year', y = 'Mean Probability of Predator Type Caught')
ggsave(width = 8.5, height = 5, dpi=300, filename = "~/WERC-SC/HALE/outputs/fitYearPreds.pdf")

# save tables
write.table(yearEvents, file = '~/WERC-SC/HALE/outputs/yearEvents.csv',
            row.names = FALSE)
write.table(yearPreds, file = '~/WERC-SC/HALE/outputs/yearPreds.csv',
            row.names = FALSE)
# save graphs
