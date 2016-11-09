max <- read.csv('~/../Desktop/temp/PFSH_QAQC-max.csv') %>% 
  mutate(observer = 'max')
rya <- read.csv('~/../Desktop/temp/PFSH_QAQC-ryan.csv') %>% 
  mutate(observer = 'rya')
jon <- read.csv('~/../Desktop/temp/PFSH_QAQC-jon.csv') %>% 
  mutate(observer = 'jon')

pfsh_qaqc_results <- rbind(max, rya, jon) %>%
  group_by(DeployID, DiveID) %>%
  summarize(valid = sum(ifelse(ValidDive == 't', 1, 0)) >= 2,
            plunge = any(PlungeErr),
            split = any(SplitErr))

pfsh_needs_review <- filter(pfsh_qaqc_results, !valid | plunge | split)

lapply(dir('dive_identification/4d_pfsh_dive_data/', full.names = TRUE), read.csv, stringsAsFactors = FALSE) %>%
  bind_rows %>%
  filter(!Reject) -> pfsh_dives

summarize(pfsh_dives,
          MeanDuration = mean(Duration),
          SdDuration = sd(Duration),
          MeanDepth = mean(MaxDepth),
          SdDepth = sd(MaxDepth))

ggplot(pfsh_dives,
       aes(Duration)) + 
  geom_density()

ggplot(pfsh_dives,
       aes(MaxDepth)) + 
  geom_density()

ggplot(pfsh_dives,
       aes(Duration, MaxDepth, color = factor(DeployID))) + 
  geom_jitter(size = 2)