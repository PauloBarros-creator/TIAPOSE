#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#sp <- 0

# you might have to install:
#install.packages("shinycssloaders")

library(shiny)
library(shinycssloaders)

source('shiny/ui.R', local = TRUE)
source('shiny/server.R')


# Run the application 
shinyApp(
  ui = my_ui,
  server = my_server
)

