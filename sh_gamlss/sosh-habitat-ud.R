

fetch_model <- Vectorize(function(month, type, scope) {
  tryCatch(get(sprintf('%s%i%s', type, month, scope)), error = function(e) NA)
})

expand.grid(Month = c(6, 7, 9, 10),
            type = c('geo', 'ocean', 'combined'),
            scope = c('global', 'culled'),
            stringsAsFactors = FALSE) %>%
  filter(!(type == 'combined' & scope == 'global')) %>% # There are no combined global models 
  arrange(Month, type, scope)
  mutate(Model = paste(type, scope) ,# Name of model
         # Model itself (gets dropped before end)
         model = fetch_model)) 

