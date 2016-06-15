library(shiny)

shinyUI(fluidPage(
  
  # QAQC Options
  sidebarLayout(
    
    sidebarPanel(
      radioButtons('validDive',
                   'Valid dive?',
                   choices = list('True' = 1,
                                  'False' = 0,
                                  'Unclear' = -1),
                   selected = defaultInput$validDive),
      radioButtons('surfCal',
                  'Surface calibration:',
                  choices = list('Minimum Pressure' = 'min',
                                 'Median Pressure (-1:1m)' = 'med'),
                  selected = defaultInput$surfCal),
      checkboxGroupInput('errors',
                         'Errors?',
                         choices = c('Plunge' = 'plunge',
                                     'Split' = 'split'),
                         selected = defaultInput$errors),
      actionButton('nextDive',
                   'Next'),
      h3('Details:'),
      p(textOutput('species')),
      p(textOutput('site')),
      p(textOutput('nest')),
      p(textOutput('band')),
      width = 2
    ),
    
    # Dive plot
    mainPanel(
      imageOutput('divePlot')
    )
  )
))