#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
source("otim.R")
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Otimização da Situação"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput(inputId = "year",
                      label = "Ano:",
                      choices = c("2019", "2020"),
                      selected = NULL),
          sliderInput(inputId = "week",
                      label = "Semana:",
                      min = 1,
                      max = 52,
                      value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
          verbatimTextOutput("textOutput")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Render output of function using renderPrint
    output$textOutput <- renderPrint({
      preparadas1 <- c(160, 8, 0, 52, 20, 0, 0)
      preparadas2 <- c(200, 200, 0, 0, 30, 0, 0)


      arm <- c(6, 3, 0, 1, 1, 0, 1)

      v1 <- c(2, 0, 0, 1, 0, 0, 0)
      v2 <- c(2, 1, 0, 0, 1, 0, 0)
      v3 <- c(2, 1, 0, 0, 0, 0, 0)

      s <- c(preparadas1,preparadas2,arm,v1,v2,v3)

      eval(s)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

