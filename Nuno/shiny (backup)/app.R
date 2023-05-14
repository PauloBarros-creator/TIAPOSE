#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#sp <- 0
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
                      value = 30),
          actionButton("chooseYear", "Selecionar Ano"),
          textInput('p1', 'Enter p1 (separado por vírgulas)', "160,8,0,52,20,0,0"),
          textInput('p2', 'Enter p2 (separado por vírgulas)', "200,200,0,0,30,0,0"),
          textInput('p3', 'Enter ARM (separado por vírgulas)', "6,3,0,1,1,0,1"),
          textInput('v1', 'Enter V1 (separado por vírgulas)', "2,0,0,1,0,0,0"),
          textInput('v2', 'Enter V2 (separado por vírgulas)', "2,1,0,0,1,0,0"),
          textInput('v3', 'Enter V3 (separado por vírgulas)', "2,1,0,0,0,0,0"),
          actionButton("execute", "Selecionar Plano"),
          
        ),

        # Show a plot of the generated distribution
        mainPanel(
          verbatimTextOutput(outputId = "output"),
          verbatimTextOutput(outputId = "output_f")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
  chooseYear <- function(week,year) {
    sp <- choose_year(year, week)
    sp1 <- paste(sp[1:7], collapse = ",")
    sp2 <- paste(sp[8:14], collapse = ",")
    texto <- paste0("   Vendas previstas de STELLA: ",sp1,"\n   Vendas previstas de BUD: ",sp2,"\n")
    return(texto)
  }
  
  plano <- function(week,year) {
      sp <- choose_year(year, week)
      # nao funciona
      #p1 <- as.numeric(strsplit(as.character(input$p1), ",")[[1]])
      #p2 <- as.numeric(strsplit(as.character(input$p2), ",")[[1]])
      #arm <- as.numeric(strsplit(as.character(input$arm), ",")[[1]])
      #v1 <- as.numeric(strsplit(as.character(input$v1), ",")[[1]])
      #v2 <- as.numeric(strsplit(as.character(input$v2), ",")[[1]])
      #v3 <- as.numeric(strsplit(as.character(input$v3), ",")[[1]])
      
      p1 <- c(160, 8, 0, 52, 20, 0, 0)
      p2 <- c(200, 200, 0, 0, 30, 0, 0)
      arm <- c(6, 3, 0, 1, 1, 0, 1)
      v1 <- c(2, 0, 0, 1, 0, 0, 0)
      v2 <- c(2, 1, 0, 0, 1, 0, 0)
      v3 <- c(2, 1, 0, 0, 0, 0, 0)
      sp <- c(141, 154, 18, 102, 211, 69, 37, 0, 211, 172, 220,330, 39, 45, 125, 0)
      
      s <- c(p1,p2,arm,v1,v2,v3,sp)
      eval(s)
  }
  
  # print do ano
  result <- eventReactive(input$chooseYear, {
    chooseYear(input$week, input$year)
  })
  
  output$output <- renderText({
    result()
  })
  
  # print da previsao
  result_f <- eventReactive(input$execute, {
    capture.output(plano(input$week, input$year))
  })
  
  output$output_f <- renderPrint({
    result_f()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
