source("otim-2.R")
source("predict.R")

my_server <- function(input, output) {
  
  # variaveis globais
  sp1 <- c()
  sp2 <- c()
  
  predict <- function(model) {
    cat("Predicting with model ",model)
    sp1 <<- predict_ksvm_stella()
    sp2 <<- predict_ksvm_bud()
    texto <- paste("   Vendas previstas de STELLA: ",paste(sprintf("%.2f", sp1), collapse = ","),"\n   Vendas previstas de BUD: ",paste(sprintf("%.2f", sp2), collapse = ","),"\n")
    return(texto)
  }
  
  plano <- function(week,year) {
    # nao funciona
    # p1 <- as.numeric(strsplit(input$p1, ",")[[1]])
    # p2 <- as.numeric(strsplit(input$p2, ",")[[1]])
    # arm <- as.numeric(strsplit(input$arm, ",")[[1]])
    # v1 <- as.numeric(strsplit(input$v1, ",")[[1]])
    # v2 <- as.numeric(strsplit(input$v2, ",")[[1]])
    # v3 <- as.numeric(strsplit(input$v3, ",")[[1]])
    p1 <- c(160, 8, 0, 52, 20, 0, 0)
    p2 <- c(200, 200, 0, 0, 30, 0, 0)
    arm <- c(6, 3, 0, 1, 1, 0, 1)
    v1 <- c(2, 0, 0, 1, 0, 0, 0)
    v2 <- c(2, 1, 0, 0, 1, 0, 0)
    v3 <- c(2, 1, 0, 0, 0, 0, 0)
    
    s <- c(p1,p2,arm,v1,v2,v3,sp1,sp2)
    eval(s)
  }
  
  # print do modelo
  result <- eventReactive(input$chooseModel, {
    predict(input$model)
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