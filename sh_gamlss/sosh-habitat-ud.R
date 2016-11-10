
formula.to.char <- function(model) {
  model %>% formula %>% terms.formula %>% attr('term.labels') %>% paste(collapse = ' + ')
}

# For all combinations of month, type, and scope...
model.summary.0 <- expand.grid(Month = c(6, 7, 9, 10),
                             type = c('geo', 'ocean', 'combined'),
                             scope = c('global', 'culled'),
                             stringsAsFactors = FALSE) %>%
  filter(!(type == 'combined' & scope == 'global')) %>% # There are no combined global models 
  mutate(Model = paste(type, scope)) %>%
  arrange(Month, type, scope)

model.summary <- foreach(row = iter(model.summary.0, by = 'row'), 
                         .combine = rbind) %do% {
    with(row, {
      # Fetch model
      model <- try(get(sprintf('%s%i%s', type, Month, scope)))
      
      # Verify model exists
      if(class(model)[1] == 'try-error')
        return(mutate(row, 
                      Formula = NA,
                      gAIC = NA,
                      EDF = NA))
      
      # Run model diagnostics
      mutate(row,
             Formula = formula.to.char(model),
             gAIC = extractAIC(model)[2],
             EDF = extractAIC(model)[1])
    })
  } %>%
  # Run model group diagnostics
  group_by(Month) %>%
  mutate(deltaGAIC = gAIC - min(gAIC, na.rm = TRUE),
         GAICw = calcAICw(gAIC)) %>%
  ungroup
