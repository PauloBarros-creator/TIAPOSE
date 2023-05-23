source("pred/predict.R")
source("otim/otimizar.R")

# para cada output existem 3 funcoes:
#   funcao principal, que define os calculos a serem apresentados (ex.: predict, plano)
#   funcao renderText/renderPrint, que está associada ao campo em que será apresentado o output e simplesmente o conecta à função eventReactive
#   funcao eventReactive, que aguarda que seja premido um botao (primeiro argumento) para executar a funcao principal.

# variaveis globais
#sales_pred1 <- c()
#sales_pred2 <- c()

my_server <- function(input, output) {
  
  # PREVISAO
  do_prediction <- function(model) {
    cat("Predicting with model ",model)
    sales_pred1 <<- predict(model)
    sales_pred2 <<- predict(model)
    texto <- paste("   Vendas previstas de STELLA: ",paste(sprintf("%.2f", sales_pred1), collapse = " ; "),"\n   Vendas previstas de BUD: ",paste(sprintf("%.2f", sales_pred2), collapse = " ; "),"\n")
    return(texto)
  }
  
  # print da previsao obtida
  result_pred <- eventReactive(input$choosePred, {
    do_prediction(input$pred_model)
  })
  
  output$output_previsao <- renderText({
    result_pred()
  })
  
  
  # # PLANO INICIAL
  # plano <- function() {
  #   p1 <- as.numeric(unlist(strsplit(input$p1,",")))
  #   p1 <- as.numeric(unlist(strsplit(input$p1, ",")))
  #   p2 <- as.numeric(unlist(strsplit(input$p2, ",")))
  #   arm <- as.numeric(unlist(strsplit(input$arm, ",")))
  #   v1 <- as.numeric(unlist(strsplit(input$v1, ",")))
  #   v2 <- as.numeric(unlist(strsplit(input$v2, ",")))
  #   v3 <- as.numeric(unlist(strsplit(input$v3, ",")))
  #   
  #   s <- c(p1,p2,arm,v1,v2,v3,sales_pred1,sales_pred2)
  #   eval(s)
  # }
  # 
  # # print do plano
  # result_plano <- eventReactive(input$choosePlan, {
  #   plano()
  # })
  # 
  # output$output_plano <- renderPrint({
  #   result_plano()
  # })
  
  # OTIMIZACAO
  do_otim <- function(omodel) {
    cat("\nOptimizing with model ",omodel)
    cat("\n sales pred 1 ",sales_pred1)
    cat("\n sales pred 2 ",sales_pred2)
    result <- otimizar(omodel,sales_pred1,sales_pred2)
    return(result)
  }
  
  # print da otimizacao
  result_otim <- eventReactive(input$chooseOtim, {
    do_otim(input$otim_model)
  })
  
  output$output_otim <- renderPrint({
    result_otim()
  })
  
}