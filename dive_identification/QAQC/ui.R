library(shiny)

shinyUI(fluidPage(
  
  # QAQC Options
  sidebarLayout(
    
    sidebarPanel(
      radioButtons('validDive',
                   'Valid dive?',
                   choices = list('True' = 1,
                                  'False' = 0,
                                  'Unclear' = -1)),
      radioButtons('surfCal',
                  'Surface calibration:',
                  choices = list('None' = 'none',
                                 'Minimum Pressure' = 'min',
                                 'Median Pressure (-1:1m)' = 'med'),
                                 selected = 'min'),
      radioButtons('plungeError',
                   'Plunge error?',
                   choices = list('False' = 0,
                                  'True' = 1),
                                  selected = 0),
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