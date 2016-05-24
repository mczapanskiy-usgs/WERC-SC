dir('dive_identification/5c_wtsh_dive_plots/') %>%
  (function(s) substr(s, 1, nchar(s) - 4) ) %>%
  strsplit('_') %>%
  lapply(function(x) { x <- as.numeric(x); data.frame(DeployID = x[1], EventID = x[2], DiveID = x[3]) }) %>%
  rbind_all %>%
  mutate(ValidDive = NA,
         SurfCal = NA,
         PlungeErr = NA,
         Eyes = FALSE) %>%
  write.csv('dive_identification/QAQC/www/WTSH_QAQC.csv',
            row.names = FALSE)