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
         stringsAsFactors = FALSE) -> fitted_S_preds
# weather: 
read.csv(file = '~/WERC-SC/HALE/outputs/fitted_cpue_S_events_eck18.csv',
         stringsAsFactors = FALSE) -> fitted_S_events

fitted_S_events_lg <- melt(fitted_S_events, id.vars = c("Season", "PctVeg", "majCoverType"),
                            measure.vars = c("noEvent", "predatorEvent", "otherEvent"))

fitted_S_preds_lg <- melt(fitted_S_preds, id.vars = c("Year", "Season", "Elevation"),
                           measure.vars = c("catCaught", "mongooseCaught", "ratCaught"))

fitted_S_preds_sum <- fitted_S_preds_lg %>% 
  group_by(variable, Elevation) %>% 
  summarise(aveProb = mean(value),
            sdProb = sd(value))


#### PREDS
## elevation
elev_preds_fitted <- ggplot(fitted_S_preds_lg, aes(Elevation, value)) +
  geom_jitter(aes(color = variable)) +
  # facet_grid(.~ majCoverType) +
  labs(y = 'Predicted Probability of Predator Events', x = 'Elevation') +
  scale_color_brewer(palette = "Set1") +
  theme_bw() ## + theme(axis.text.x = element_text(angle=60, hjust=1))
elev_preds_fitted
ggsave(width = 8.5, height = 5, dpi=300, filename = "~/WERC-SC/HALE/outputs/elev_preds_fitted_eck22.pdf")


elev_preds_fitted_ave <- ggplot(fitted_S_preds_sum, aes(Elevation, aveProb)) +
  geom_errorbar(aes(ymin = aveProb-sdProb, ymax=aveProb+sdProb, color = variable), width = 0.5) +
  geom_point(size = 2, shape = 18, aes(color = variable)) +
  labs(y = 'Mean Predicted Probability of Predator Events', x = 'Elevation', color = 'Predator Event') +
  theme_bw()
elev_preds_fitted_ave
ggsave(width = 8.5, height = 5, dpi=300, filename = "~/WERC-SC/HALE/outputs/elev_preds_fitted_ave_eck22.pdf")


#### EVENTS
## % veg
pctVeg_events_fitted <- ggplot(fitted_S_events_lg, aes(PctVeg, value)) +
  geom_line(size = 0.75, aes(color = variable, linetype = majCoverType)) + 
  facet_wrap(~ Season) +
  labs(y = 'Predicted Probability of Catch Events', x = 'Percent Vegetation Cover', color = 'Event Type', linetype = 'Veg Cover') +
  # scale_fill_manual(values = vegColors) +
  theme_bw() ## + theme(axis.text.x = element_text(angle=60, hjust=1))
pctVeg_events_fitted 
ggsave(width = 8.5, height = 5, dpi=300, filename = "~/WERC-SC/HALE/outputs/pctVeg_S_events_fitted_eck22.pdf")
# # summarize dataset by means and SDs
# fitted_S_events_means <- fitted_S_events_lg %>% 
#   group_by(value, variable, PctVeg) %>% 
#   summarize(meanPctVeg = mean(PctVeg, na.rm = TRUE), sdProb = sd(PctVeg, na.rm = TRUE)) %>% 
#   ungroup
# pctVeg_events_means <- ggplot(fitted_S_events_means, aes(PctVeg, majCoverType)) +
#   geom_line(size = 1) +
#   geom_point(size = 2, shape = 18, aes(color = variable)) + 
#   geom_errorbar(aes(ymin = meanProb-sdProb, ymax=meanProb+sdProb), width = 0.5) +
#   # facet_wrap(~ value) +
#   # labs(y = 'Predicted Probability of Catch Events', x = 'Percent Vegetation Cover') +
#   # scale_fill_manual(values = vegColors) +
#   theme_bw() 
# pctVeg_events_means 

## veg cover
vegCover_events_fitted <- ggplot(fitted_S_events_lg, aes(majCoverType, value)) +
  geom_boxplot(aes(color = variable)) +
  # facet_grid(.~ majCoverType) +
  labs(y = 'Predicted Probability of Catch Events', x = 'Major Vegetation Type') +
  scale_color_brewer(palette = "Set1") +
  theme_bw() ## + theme(axis.text.x = element_text(angle=60, hjust=1))
vegCover_events_fitted 
ggsave(width = 8.5, height = 5, dpi=300, filename = "~/WERC-SC/HALE/outputs/vegCover_events_fitted_eck22.pdf")

