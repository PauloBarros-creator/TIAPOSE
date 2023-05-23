source("pred/predict.R")
source("otim/otimizar.R")

# para cada output existem 3 funcoes:
#   funcao principal, que define os calculos a serem apresentados (ex.: predict, plano)
#   funcao renderText/renderPrint, que está associada ao campo em que será apresentado o output e simplesmente o conecta à função eventReactive
#   funcao eventReactive, que aguarda que seja premido um botao (primeiro argumento) para executar a funcao principal.

# variaveis globais
sales_pred1 <- c()
sales_pred2 <- c()

my_server <- function(input, output) {
  
  # PREVISAO
  do_prediction <- function(model) {
    cat("Predicting with model ",model)
    sales_pred <- predict(model)
    split_sp <- split(sales_pred, rep(1:2, each = 7))
    sales_pred1 <<- split_sp[[1]]
    sales_pred2 <<- split_sp[[2]]
    
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
  
  # OTIMIZACAO
  do_otim <- function(omodel) {
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