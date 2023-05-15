#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#sp <- 0

library(shiny)
source("otim-2.R")
source('ui.R', local = TRUE)
source('server.R')


shinyApp(
  ui = my_ui,
  server = my_server
)

# Run the application 
shinyApp(ui = ui, server = server)