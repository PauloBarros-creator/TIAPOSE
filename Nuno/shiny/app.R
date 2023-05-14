#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
sp <- 0
source("otim-2.R")
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
                      value = 30),
          actionButton("chooseYear", "Selecionar Ano"),
          textInput('p1', 'Enter p1 (separado por vírgulas)', "0,1,2"),
          textInput('p2', 'Enter p2 (separado por vírgulas)', "0,1,2"),
          textInput('p3', 'Enter ARM (separado por vírgulas)', "0,1,2"),
          textInput('v1', 'Enter V1 (separado por vírgulas)', "0,1,2"),
          textInput('v2', 'Enter V2 (separado por vírgulas)', "0,1,2"),
          textInput('v3', 'Enter V3 (separado por vírgulas)', "0,1,2"),
          actionButton("execute", "Selecionar Plano"),
          
        ),

        # Show a plot of the generated distribution
        mainPanel(
          #verbatimTextOutput("yearOutput"),
          textOutput(outputId = "output"),
          textOutput(outputId = "output_f")
#          verbatimTextOutput("result")
          #uiOutput("moreInputs")
          #verbatimTextOutput("textOutput")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
  chooseYear <- function(week,year) {
    sp <- choose_year(year, week)
    texto <- paste0("   Vendas previstas de STELLA: ",sp[1:7],"\n   Vendas previstas de BUD: ",sp[8:14],"\n")
    return(texto)
  }
  
  plano <- function(week,year) {
      sp <- choose_year(year, week)
      p1 <- as.numeric(unlist(strsplit(as.character(input$p1),",")))
      p2 <- as.numeric(unlist(strsplit(as.character(input$p2),",")))
      arm <- as.numeric(unlist(strsplit(as.character(input$arm),",")))
      v1 <- as.numeric(unlist(strsplit(as.character(input$v1),",")))
      v2 <- as.numeric(unlist(strsplit(as.character(input$v2),",")))
      v3 <- as.numeric(unlist(strsplit(as.character(input$v3),",")))
      
      s <- c(p1,p2,arm,v1,v2,v3,sp)

      eval(s)
  }
  
  # observeEvent(input$chooseYear, {
  #   # Call chooseYear with the current input values
  #   result <- chooseYear(input$week, input$year)
  #   
  #   # Update the output with the result
  #   output$result <- renderPrint(result)
  # })
  
  result <- eventReactive(input$chooseYear, {
    chooseYear(input$week, input$year)
  })
  
  output$output <- renderText({
    result()
  })
  
  result_f <- eventReactive(input$execute, {
    plano(input$week, input$year)
  })
  
  output$output_f <- renderText({
    result_f()
  })
  
    # 
    # # Define a reactive expression to track the button click
    # showInputs <- reactive({
    #   input$chooseYear
    # })
    # 
    # # Render output of function using renderPrint
    # output$textOutput <- renderPrint({
    #   preparadas1 <- c(160, 8, 0, 52, 20, 0, 0)
    #   preparadas2 <- c(200, 200, 0, 0, 30, 0, 0)
    # 
    # 
    #   arm <- c(6, 3, 0, 1, 1, 0, 1)
    # 
    #   v1 <- c(2, 0, 0, 1, 0, 0, 0)
    #   v2 <- c(2, 1, 0, 0, 1, 0, 0)
    #   v3 <- c(2, 1, 0, 0, 0, 0, 0)
    # 
    #   s <- c(preparadas1,preparadas2,arm,v1,v2,v3)
    #   
    #   arm <- as.numeric(unlist(strsplit(input$arm,",")))
    #   v1 <- as.numeric(unlist(strsplit(input$v1,",")))
    #   v2 <- as.numeric(unlist(strsplit(input$v2,",")))
    #   v3 <- as.numeric(unlist(strsplit(input$v3,",")))
    # 
    #   eval(s)
    # })
    # 
    # # Show or hide the additional inputs based on the button click
    # output$moreInputs <- renderUI({
    #   if (showInputs()) {
    #     textInput('p1', 'Enter ARM (separado por vírgulas)', "0,1,2")
    #     textInput('p2', 'Enter ARM (separado por vírgulas)', "0,1,2")
    #     textInput('p3', 'Enter ARM (separado por vírgulas)', "0,1,2")
    #     textInput('v1', 'Enter V1 (separado por vírgulas)', "0,1,2")
    #     textInput('v2', 'Enter V2 (separado por vírgulas)', "0,1,2")
    #     textInput('v3', 'Enter V3 (separado por vírgulas)', "0,1,2")
    #   } else {
    #     NULL
    #   }
    # })
    # 
    # # Render the text output based on the inputs and button click
    # output$textOutput <- renderText({
    #   generateOutput(input$variable, input$week, input$moreInput1)
    # })
}

# Run the application 
shinyApp(ui = ui, server = server)

