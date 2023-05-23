source("eval-print.R")
source("predict.R")

# para cada output existem 3 funcoes:
#   funcao principal, que define os calculos a serem apresentados (ex.: predict, plano)
#   funcao renderText/renderPrint, que está associada ao campo em que será apresentado o output e simplesmente o conecta à função eventReactive
#   funcao eventReactive, que aguarda que seja premido um botao (primeiro argumento) para executar a funcao principal.

my_server <- function(input, output) {
  
  # variaveis globais
  sp1 <- c()
  sp2 <- c()
  
  # PREVISAO
  predict <- function(model) {
    cat("Predicting with model ",model)
    sp1 <<- predict_ksvm_stella()
    sp2 <<- predict_ksvm_bud()
    texto <- paste("   Vendas previstas de STELLA: ",paste(sprintf("%.2f", sp1), collapse = " ; "),"\n   Vendas previstas de BUD: ",paste(sprintf("%.2f", sp2), collapse = " ; "),"\n")
    return(texto)
  }
  
  # print da previsao obtida
  result <- eventReactive(input$choosePred, {
    predict(input$pred-model)
  })
  
  output$output_previsao <- renderText({
    result()
  })
  
  
  # PLANO INICIAL
  plano <- function() {
    p1 <- as.numeric(unlist(strsplit(input$p1,",")))
    p1 <- as.numeric(unlist(strsplit(input$p1, ",")))
    p2 <- as.numeric(unlist(strsplit(input$p2, ",")))
    arm <- as.numeric(unlist(strsplit(input$arm, ",")))
    v1 <- as.numeric(unlist(strsplit(input$v1, ",")))
    v2 <- as.numeric(unlist(strsplit(input$v2, ",")))
    v3 <- as.numeric(unlist(strsplit(input$v3, ",")))
    
    s <- c(p1,p2,arm,v1,v2,v3,sp1,sp2)
    eval(s)
  }
  
  # print do plano
  result_plano <- eventReactive(input$choosePlan, {
    plano()
  })
  
  output$output_plano <- renderPrint({
    result_plano()
  })
  
  # OTIMIZACAO
  otimi <- function() {
    return(cat("not yet :)"))
  }
  
  # print da otimizacao
  result_otim <- eventReactive(input$chooseOtim, {
    otimi()
  })
  
  output$output_otim <- renderPrint({
    result_otim()
  })
  
}