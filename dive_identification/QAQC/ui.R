shinyUI(fluidPage(
  
  # QAQC Options
  sidebarLayout(
    
    sidebarPanel(
      radioButtons('validDive',
                   'Valid dive?',
                   choices = list('True' = 't',
                                  'False' = 'f',
                                  'Unclear' = 'u'),
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
      p(textOutput('threshold')),
      p(textOutput('site')),
      p(textOutput('nest')),
      p(textOutput('band')),
      p(textOutput('dive')),
      p(textOutput('progress')),
      width = 2
    ),
    
    # Dive plot
    mainPanel(imageOutput('divePlot'))
  )
))
