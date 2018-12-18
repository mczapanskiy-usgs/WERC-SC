## this script analyzes (graphically and statistically) spatial results of mlogit models
## seasonal and yearly effects on:
##   event types (pred, other, none)
##   predator events (rat, cat, mongoose)
## results from 'eck12.5_HALE_CPUE_analysisOfVar_weatherLunar'

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
## event type (predator, other, no event) uses base model w/ traplineYear = random effect, 
# coefficients: season, meanTmax
read.csv('~/WERC-SC/HALE/outputs/fitted_cpue_WL_events_yr_eck12.5.csv',
         stringsAsFactors = FALSE) -> fitted_WL_events
read.csv('~/WERC-SC/HALE/outputs/fitted_cpue_WL_events_mo_eck12.5.csv',
         stringsAsFactors = FALSE) -> fitted_WL_events_mo

## pred event data (rat, cat, or mongoose) uses base model w/ Trapline = random effect, season = indiv. specif. was best fit
read.csv('~/WERC-SC/HALE/outputs/fitted_cpue_WL_preds_yr_eck12.5.csv',
         stringsAsFactors = FALSE) -> fitted_WL_preds
read.csv('~/WERC-SC/HALE/outputs/fitted_cpue_WL_preds2_eck12.5.csv',
         stringsAsFactors = FALSE) -> fitted_WL_preds2


## create long versions of fitted results from mlogit model (variable = pred type, value = fitted CPUE probability)
fitted_WL_events_lg <- melt(fitted_WL_events, id.vars = c("Season", "total3monRain"),
                          measure.vars = c("noEvent", "predatorEvent", "otherEvent"))
fitted_WL_events_mo_lg <- melt(fitted_WL_events_mo, id.vars = c("Month", "total3monRain"),
                            measure.vars = c("noEvent", "predatorEvent", "otherEvent"))

fitted_WL_preds_lg <- melt(fitted_WL_preds, id.vars = c("Year", "Season", "meanTmax"),
                        measure.vars = c("catCaught", "mongooseCaught", "ratCaught"))
fitted_WL_preds2_lg <- melt(fitted_WL_preds2, id.vars = c("Year", "Season", "moon"),
                           measure.vars = c("catCaught", "mongooseCaught", "ratCaught"))


### EVENT (predator, other, none) ANALYSIS
rain_events <- ggplot(fitted_WL_events_lg, aes(total3monRain, value)) +
  geom_point(aes(colour = Season), size = 1, shape = 18) +
  facet_wrap(~ variable) + # , scales = 'free'
  labs(y = 'Proability of Event Type ', x = 'Total 3 Month Rainfall') +
  
  theme_bw()
rain_events
ggsave(width = 8.5, height = 5, dpi=300, filename = "~/WERC-SC/HALE/outputs/fitRain_events_eck19.pdf")

rain_events_mo <- ggplot(fitted_WL_events_mo_lg, aes(total3monRain, value)) +
  geom_point(aes(colour = Month), size = 1, shape = 18) +
  facet_wrap(~ variable) + # , scales = 'free'
  labs(y = 'Proability of Event Type ', x = 'Total 3 Month Rainfall') +
  
  theme_bw()
rain_events_mo
ggsave(width = 8.5, height = 5, dpi=300, filename = "~/WERC-SC/HALE/outputs/fitRain_events_mo_eck19.pdf")

### PREDATOR ANALYSIS
## MEAN MAXIMUM TEMPERATURE
temp_pred <- ggplot(fitted_WL_preds_lg, aes(meanTmax, value)) +
  geom_point(aes(colour = Season), size = 1, shape = 18) + # geom_density2d(aes(colour = Season)) + #
  facet_wrap(~ variable) +
  labs(x = 'Weekly Mean Maximum Temperature (°C)', y = 'Proability of Predator Type Caught') +
  ylim(c(0,1)) +
  theme_bw()
temp_pred
ggsave(width = 8.5, height = 5, dpi=300, filename = "~/WERC-SC/HALE/outputs/fitTempPreds_eck19.pdf")
## MOON
temp_pred <- ggplot(fitted_WL_preds2_lg, aes(moon, value)) +
  geom_point(aes(colour = Season), size = 1, shape = 18) + # geom_density2d(aes(colour = Season)) + #
  facet_wrap(~ variable) +
  labs(x = 'Weekly Moon Time (Mean Moon Illumination * Total Moon Time (seconds))', y = 'Proability of Predator Type Caught') +
  ylim(c(0,1)) +
  theme_bw()
temp_pred
ggsave(width = 8.5, height = 5, dpi=300, filename = "~/WERC-SC/HALE/outputs/fitMoonPreds_eck19.pdf")


# # annual predEvent probabilities
# fit_L_Preds <- fitted_L_preds %>%
#   gather(predType, predProb, catCaught:ratCaught) %>%
#   group_by(Year, predType) %>%
#   summarize(meanProb = mean(predProb, na.rm = TRUE), sdProb = sd(predProb, na.rm = TRUE))
# ggplot(fit_L_Preds, aes(x=Year, y=meanProb, color = predType)) +
#   geom_line(size = 1) +
#   geom_point(size = 2, shape = 18) +
#   geom_errorbar(aes(ymin = meanProb-sdProb, ymax=meanProb+sdProb), width = 0.5) +
#   theme_bw() +
#   labs(x = 'Year', y = 'Mean Probability of Predator Type Caught')
# ggsave(width = 8.5, height = 5, dpi=300, filename = "~/WERC-SC/HALE/outputs/fitYearPreds.pdf")


# #### SEASON
# # graph fitted frequencies
# ggplot(fitted_events_lg, aes(x=Season, y=value)) +
#   geom_boxplot() +
#   facet_wrap(~ variable) + #, scales = 'free') +
#   scale_x_discrete(limits = c('Pre-laying', 'Incubation', 'Nestling', 'offSeason')) +
#   ylim(0, 1) +
#   theme_bw() +
#   labs(x = 'Season', y = 'Probability of Events Occurance')
# ggsave(width = 8.5, height = 5, dpi=300, filename = "~/WERC-SC/HALE/outputs/fitSeasonEvents_eck17.pdf")
# 
# ## tables of mean and SD fitted frequencies
# # seasonal event probabilities
# seasonEvents <- fitted_events %>% 
#   group_by(Season) %>% 
#   summarize(mNone = mean(noEvent, na.rm = T), sdNone = sd(noEvent, na.rm = T),
#             mPred = mean(predatorEvent, na.rm = T), sdPred = sd(predatorEvent, na.rm = T),
#             mOther = mean(otherEvent, na.rm = T), sdOther = sd(otherEvent, na.rm = T))
# # seasonal predEvent probabilities
# seasonPreds <- fitted_preds %>% 
#   group_by(Season) %>% 
#   summarize(mRat = mean(ratCaught, na.rm = T), sdRat = sd(ratCaught, na.rm = T),
#             mCat = mean(catCaught, na.rm = T), sdCat = sd(catCaught, na.rm = T),
#             mMong = mean(mongooseCaught, na.rm = T), sdMong = sd(mongooseCaught, na.rm = T))
# # save tables
# write.table(seasonEvents, file = '~/WERC-SC/HALE/outputs/seasonEvents.csv',
#             row.names = FALSE)
# write.table(seasonPreds, file = '~/WERC-SC/HALE/outputs/seasonPreds.csv',
#             row.names = FALSE)
# 
# 
# #### YEAR
# ## tables and figures of mean and SD fitted frequencies
# # annual event probabilities
# fitYearEvents <- fitted_events %>% 
#   gather(eventType, eventProb, noEvent:otherEvent) %>%
#   group_by(Year, eventType) %>% 
#   summarize(meanProb = mean(eventProb, na.rm = TRUE), sdProb = sd(eventProb, na.rm = TRUE))
# ggplot(fitYearEvents, aes(x=Year, y=meanProb, color = eventType)) +
#   geom_line(size = 1) +
#   geom_point(size = 2, shape = 18) +
#   geom_errorbar(aes(ymin = meanProb-sdProb, ymax=meanProb+sdProb), width = 0.5) +
#   ylim(0, 1) +
#   theme_bw() +
#   labs(x = 'Year', y = 'Mean Probability of Event Occurance')
# ggsave(width = 8.5, height = 5, dpi=300, filename = "~/WERC-SC/HALE/outputs/fitYearEvents.pdf")
# 
# 
# # save tables
# write.table(yearEvents, file = '~/WERC-SC/HALE/outputs/yearEvents.csv',
#             row.names = FALSE)
# write.table(yearPreds, file = '~/WERC-SC/HALE/outputs/yearPreds.csv',
#             row.names = FALSE)
# # save graphs
