# Read metadata
metadata <- read.csv('www/WTSH_metadata.csv',
                     stringsAsFactors = FALSE)

shinyServer(function(input, output, session) {
  # Create a reactive structure for dive data as a whole and selected dive for QAQC (with metadata)
  dives <- reactiveValues(data = read.csv('www/WTSH_QAQC.csv'),
                          focus = NULL,
                          selected = NULL,
                          focusMeta = NULL)
  
  # Function for choosing the next dive
  choose.next.dive <- function() {
    observe({
      dives$focus <- dives$data %>%
        filter(!Eyes) %>%
        sample_n(1)
      dives$selected <- dives$focus$Index
      deployid <- dives$focus$DeployID
      dives$focusMeta <- filter(metadata, DeployID == deployid)
    })
  }
  
  # Choose first dive
  choose.next.dive()
  
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
         list(src = sprintf('www/5c_wtsh_dive_plots/%i_%i_%i.png', 
                            DeployID, 
                            EventID, 
                            DiveID)))
  })
  
  # Dive table
  output$diveTable <- DT::renderDataTable(dives$data, 
                                          server = TRUE, 
                                          selection = list(mode = 'single',
                                                           target = 'row',
                                                           selected = dives$selected))
  
  # Function for resetting inputs to defaults or (if previously seen) focus dives' settings
  reset.inputs <- function() {
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
  }
  
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
    dives$data <- dives$data %>%
      filter(DeployID != deployid | DiveID != diveid) %>%
      rbind(qaqc) %>%
      arrange(DeployID, DiveID)
    
    write.csv(dives$data,
              'www/WTSH_QAQC.csv',
              row.names = FALSE)
    
    # Choose next dive and reset inputs
    choose.next.dive()
    reset.inputs()
  })
})