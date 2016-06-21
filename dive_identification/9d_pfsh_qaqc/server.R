# Read metadata
metadata <- read.csv('www/PFSH_metadata.csv',
                     stringsAsFactors = FALSE)

shinyServer(function(input, output, session) {
  # Choose next dive (haven't figure out how to put this in a function yet)
  divefocus <- diveData %>%
    filter(!Eyes) %>%
    arrange(DeployID, DiveID) %>%
    slice(1)
  dives$focus <- divefocus
  dives$selected <- divefocus$Index
  dives$focusMeta <- metadata[metadata$DeployID == divefocus$DeployID,]
  
  # Description of diving bird
  output$species <- renderText({ dives$focusMeta$Species })
  output$threshold <- renderText({ sprintf('Threshold %im', dives$focusMeta$threshold) })
  output$site <- renderText({ dives$focusMeta$Site })
  output$nest <- renderText({ paste('Nest', dives$focusMeta$NestNo) })
  output$band <- renderText({ paste('Band', dives$focusMeta$BandNo) })
  
  # Dive plot
  # Left panel: surface-corrected dive. Blue points are in dive, red are there for context
  # Right panel: green points are surface corrected, red are original
  output$divePlot <- renderImage({
    with(dives$focus,
         list(src = sprintf('www/5d_pfsh_dive_plots/%i_%i_%i.png', 
                            DeployID, 
                            EventID, 
                            DiveID)))
  })
  
  # Dive table
  output$diveTable <- DT::renderDataTable(dives$data,
                                          server = TRUE, 
                                          selection = list(mode = 'single',
                                                           target = 'row',
                                                           selected = dives$selected),
                                          options = list(
                                            order = list(list(1, 'asc'), list(3, 'asc')) # Sort by deploy id, dive id
                                          ))
  
  diveProxy <- dataTableProxy('diveTable')
  
  observe({input$selectRow,
    selectRows(proxy, as.numeric(input$selectRow))
  })
  
  # Responder for nextDive button
  observeEvent(input$nextDive, {
    # Read QAQC results
    qaqc <- dives$focus %>%
      mutate(ValidDive = input$validDive,
             SurfCal = input$surfCal,
             PlungeErr = any(input$errors == 'plunge'),
             SplitErr = any(input$errors == 'split'),
             Eyes = TRUE)
    
    # Update QAQC data and rewrite CSV file
    deployid <- dives$focus$DeployID
    diveid <- dives$focus$DiveID
    dives$data <<- dives$data %>%
      filter(DeployID != deployid | DiveID != diveid) %>%
      rbind(qaqc) %>%
      arrange(DeployID, DiveID)
    diveData <<- dives$data
    
    write.csv(dives$data,
              'www/PFSH_QAQC.csv',
              row.names = FALSE)
    
    # Choose next dive (haven't figure out how to put this in a function yet)
    divefocus <- diveData %>%
      filter(!Eyes) %>%
      arrange(DeployID, DiveID) %>%
      slice(1)
    dives$focus <- divefocus
    dives$selected <- divefocus$Index
    dives$focusMeta <- metadata[metadata$DeployID == divefocus$DeployID,]
    
    # Reset inputs
    if(dives$focus$Eyes == FALSE) {
      updateRadioButtons(session, 
                         'validDive', 
                         selected = defaultInput$validDive)
      updateRadioButtons(session, 
                         'surfCal', 
                         selected = defaultInput$surfCal)
      updateCheckboxGroupInput(session, 
                               'errors', 
                               selected = defaultInput$errors)
    } else {
      updateRadioButtons(session, 
                         'validDive', 
                         selected = dives$focus$ValidDive)
      updateRadioButtons(session, 
                         'surfCal', 
                         selected = dives$focus$SurfCal)
      selectErrs <- c(dives$focus$PlungeErr, dives$focus$SplitErr) %>% unlist
      errors <- c('plunge', 'split')[selectErrs]
      updateCheckboxGroupInput(session, 
                               'errors', 
                               selected = errors)
    }
  })
})