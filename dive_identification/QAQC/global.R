library(shiny)
library(dplyr)

# Default inputs
defaultInput <- list(
  validDive = 't',
  surfCal = 'min',
  errors = character(0))
  