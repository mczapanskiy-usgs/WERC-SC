library(shiny)
library(dplyr)

# Read metadata and dive data
metadata <- read.csv('www/WTSH_metadata.csv',
                     stringsAsFactors = FALSE)
dives <- read.csv('www/WTSH_QAQC.csv')

shinyServer(function(input, output, session) {
  # focusDive is the dive currently being QAQC'd
  focusDive <- reactive({
    input$nextDive # establish dependency on nextDive button
    dives %>%
      filter(!Eyes) %>%
      sample_n(1)
  })
  
  # Metadata for focusDive
  focusMeta <- reactive({
    deployid <- focusDive()$DeployID
    filter(metadata, DeployID == deployid)
  })
  
  # Description of diving bird
  output$species <- renderText({ focusMeta()$Species })
  output$site <- renderText({ focusMeta()$Site })
  output$nest <- renderText({ paste('Nest', focusMeta()$NestNo) })
  output$band <- renderText({ paste('Band', focusMeta()$BandNo) })
  
  # Dive plot
  # Left panel: surface-corrected dive. Blue points are in dive, red are there for context
  # Right panel: green points are surface corrected, red are original
  output$divePlot <- renderImage({
    with(focusDive(),
         list(src = sprintf('www/5c_wtsh_dive_plots/%i_%i_%i.png', 
                            DeployID, 
                            EventID, 
                            DiveID)))
  })
  
  # Responder for nextDive button
  observeEvent(input$nextDive, {
    # QAQC results
    qaqc <- focusDive() %>%
      mutate(ValidDive = input$validDive,
             SurfCal = input$surfCal,
             PlungeErr = any(input$errors == 'plunge'),
             SplitErr = any(input$errors == 'split'),
             Eyes = TRUE)
    
    deployid <- focusDive()$DeployID
    diveid <- focusDive()$DiveID
    
    # Reset inputs to default
    updateRadioButtons(session, 
                       'validDive', 
                       selected = 1)
#     updateRadioButtons(session, 
#                        'validDive', 
#                        selected = defaultInput$validDive)
#     updateRadioButtons(session, 
#                        'surfCal', 
#                        selected = defaultInput$surfCal)
#     updateCheckboxGroupInput(session, 
#                              'errors', 
#                              selected = defaultInput$errors)

    # Update dives
    dives <<- dives %>%
      filter(DeployID != deployid | DiveID != diveid) %>%
      rbind(qaqc) %>%
      arrange(DeployID, DiveID)
    
    # Re-write QAQC file
    write.csv(dives,
              'www/WTSH_QAQC.csv',
              row.names = FALSE)
  })
})