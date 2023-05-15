my_server <- function(input, output) {
  
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
    p1 <- as.numeric(strsplit(input$p1, ",")[[1]])
    p2 <- as.numeric(strsplit(input$p2, ",")[[1]])
    arm <- as.numeric(strsplit(input$arm, ",")[[1]])
    v1 <- as.numeric(strsplit(input$v1, ",")[[1]])
    v2 <- as.numeric(strsplit(input$v2, ",")[[1]])
    v3 <- as.numeric(strsplit(input$v3, ",")[[1]])
    # p1 <- c(160, 8, 0, 52, 20, 0, 0)
    # p2 <- c(200, 200, 0, 0, 30, 0, 0)
    # arm <- c(6, 3, 0, 1, 1, 0, 1)
    # v1 <- c(2, 0, 0, 1, 0, 0, 0)
    # v2 <- c(2, 1, 0, 0, 1, 0, 0)
    # v3 <- c(2, 1, 0, 0, 0, 0, 0)
    # sp <- c(141, 154, 18, 102, 211, 69, 37, 0, 211, 172, 220,330, 39, 45, 125, 0)
    
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
  
  # Define function to reset variables
  resetVariables <- function() {
    # Reset variables here
  }
  
  # Listen for click on reset button
  observeEvent(input$resetBtn, {
    resetVariables()
    # Reset pages here
  })
  
}