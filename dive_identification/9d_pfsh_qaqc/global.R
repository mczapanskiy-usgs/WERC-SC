library(shiny)
library(dplyr)

# Default inputs
defaultInput <- list(
  validDive = 't',
  surfCal = 'min',
  errors = character(0))
  
# Create a reactive structure for dive data as a whole and selected dive for QAQC (with metadata)
dives <- reactiveValues(data = read.csv('www/PFSH_QAQC.csv'),
                        focus = NULL,
                        selected = NULL,
                        focusMeta = NULL)

# Non-reactive mirror of dives
diveData <- read.csv('www/PFSH_QAQC.csv')