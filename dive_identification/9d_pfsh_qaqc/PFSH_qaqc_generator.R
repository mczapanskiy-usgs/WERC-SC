dir('dive_identification/5d_pfsh_dive_plots/') %>%
  (function(s) substr(s, 1, nchar(s) - 4) ) %>%
  strsplit('_') %>%
  lapply(function(x) { x <- as.numeric(x); data.frame(DeployID = x[1], EventID = x[2], DiveID = x[3]) }) %>%
  rbind_all %>%
  mutate(Index = row_number(),
         ValidDive = NA,
         SurfCal = NA,
         PlungeErr = NA,
         SplitErr = NA,
         Eyes = FALSE) %>%
  write.csv('dive_identification/9d_pfsh_qaqc/www/PFSH_QAQC.csv',
            row.names = FALSE)