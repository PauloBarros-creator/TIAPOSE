source("pred/predict.R")
source("otim/otimizar.R")
library(rminer)

# para cada output existem 3 funcoes:
#   funcao principal, que define os calculos a serem apresentados (ex.: predict, plano)
#   funcao renderText/renderPrint, que está associada ao campo em que será apresentado o output e simplesmente o conecta à função eventReactive
#   funcao eventReactive, que aguarda que seja premido um botao (primeiro argumento) para executar a funcao principal.

# variaveis globais
sales_pred1 <- c()
sales_pred2 <- c()
# PrevisaoStella <- c()
# PrevisaoBud <- c()
dfinal <- data.frame()
to_eval <- c()
result_eval <- c()
#dfinal2- data.frame()
#dfinal2$index <- NA

my_server <- function(input, output) {
  
  # PREVISAO
  do_prediction <- function(model) {
    cat("Predicting with model ",model)
    sales_pred <- predict(model)
    split_sp <- split(sales_pred, rep(1:2, each = 7))
    sales_pred1 <<- split_sp[[1]]
    sales_pred2 <<- split_sp[[2]]
    # PrevisaoStella <- sales_pred1
    # PrevisaoBud <- sales_pred2
    # texto <- paste("   Vendas previstas de STELLA: ",paste(sprintf("%.2f", sales_pred1), collapse = " ; "),"\n   Vendas previstas de BUD: ",paste(sprintf("%.2f", sales_pred2), collapse = " ; "),"\n")
    # return(texto)
    df <- as.data.frame(t(sales_pred1))
    df2 <- data.frame(t(sales_pred2))
    colnames(df) <- c("1","2","3","4","5","6","7")
    colnames(df2) <- c("1","2","3","4","5","6","7")
    rownames(df) <- "Previsoes.Stella"
    rownames(df2) <- "Previsoes.Bud"
    dfinal <<- rbind(df,df2)
    dfinal
  }

  # print da previsao obtida
  result_pred <- eventReactive(input$choosePred, {
    do_prediction(input$pred_model)
  })
  
  output$output_previsao <- renderTable({
    result_pred()
  }, rownames = TRUE)
  
  # PLOT PREVISAO
  do_prediction_plot <- function(sales_pred1,sales_pred2) {
    #mgraph(sales_pred1,sales_pred2,graph="REG",Grid=10,lty=1,col=c("black","blue"),main="Predictions",leg=list(pos="topright",leg=c("stella","bud")))
    #plot(sales_pred1,sales_pred2)
    dfinal2 <- data.frame(matrix(nrow = 7, ncol = 0))
    dfinal2 <- as.data.frame(t(dfinal))
    #dfinal2$index <- NA
    dfinal2$index <- 1:7
    ggplot(dfinal2, aes(x=index)) + 
      geom_line(aes(y = Previsoes.Stella), color = "darkred") + 
      geom_line(aes(y = Previsoes.Bud), color="steelblue", linetype="twodash") +
      geom_text(aes(label = Previsoes.Stella, y = Previsoes.Stella), color = "darkred", hjust = -0.1) +
      geom_text(aes(label = Previsoes.Bud, y = Previsoes.Bud), color = "steelblue", hjust = 1.1) +
      annotate("text", x = max(dfinal2$index) + 0.5, y = dfinal2$Previsoes.Stella[length(dfinal2$Previsoes.Stella)],
               label = "Previsoes.Stella", color = "darkred", hjust = 0) +
      annotate("text", x = max(dfinal2$index) + 0.5, y = dfinal2$Previsoes.Bud[length(dfinal2$Previsoes.Bud)],
               label = "Previsoes.Bud", color = "steelblue", hjust = 1)
  }
  # print do plot
  result_plot <- eventReactive(input$choosePlot, {
    do_prediction_plot(sales_pred1,sales_pred2)
  })
  
  output$output_prev_plot <- renderPlot({
    result_plot()
  })
  
  #OTIMIZACAO
  do_otim <- function(omodel) {
    result <- otimizar(omodel,sales_pred1,sales_pred2)
    
    # split_output <- split(result, rep(1:2, each = 1))
    # preparadas1 <- split_s[[1]]
    # preparadas2 <- split_s[[2]]
    to_eval <<- result[1:42]
    if(length(result)==43){
      cat("best solution:",to_eval,"evaluation function",result[43],"\n")
    }else{
      cat("best solution:",to_eval,"\n evaluation function",result[43],"\n iteration:",result[44],"\n")
    }
    #return(result)
  }

  # print da otimizacao
  result_otim <- eventReactive(input$chooseOtim, {
    do_otim(input$otim_model)
  })

  output$output_otim <- renderPrint({
    result_otim()
  })
  
  # ARM
  do_arm <- function() {
    cat("\n",to_eval,"\n")
    result_eval <- printeval(to_eval)
    print(result_eval)
    # SPLITTING CODE BY CHATGPT
    sp1 <<- result_eval[1:7]
    sp2 <<- result_eval[8:14]
    arm <<- result_eval[15:21]
    soma_arm <<- result_eval[22]
    v1 <<- result_eval[23:29]
    v2 <<- result_eval[30:36]
    v3 <<- result_eval[37:43]
    soma_v <<- result_eval[44]
    preparadas1 <<- result_eval[45:51]
    preparadas2 <<- result_eval[52:58]
    vendas1 <<- result_eval[59:65]
    vendas2 <<- result_eval[66:72]
    profit1 <<- result_eval[73]
    profit2 <<- result_eval[74]
    stock1 <<- result_eval[75:81]
    custo_stock1 <<- result_eval[82]
    stock2 <<- result_eval[83:89]
    custo_stock2 <<- result_eval[90]
    total_profit <<- result_eval[91]
    resources <<- result_eval[92]
    
    df_sp1 <- data.frame(t(sp1))
    df_sp2 <- data.frame(t(sp2))
    df_arm <- data.frame(t(arm))
    df_as <- data.frame(col1 = soma_arm, col2 = NA, col3 = NA, col4 = NA, col5 = NA, col6 = NA, col7 = NA)
    df_v1 <- data.frame(t(v1))
    df_v2 <- data.frame(t(v2))
    df_v3 <- data.frame(t(v3))
    df_vs <- data.frame(col1 = soma_v, col2 = NA, col3 = NA, col4 = NA, col5 = NA, col6 = NA, col7 = NA)
    df_p1 <- data.frame(t(preparadas1))
    df_p2 <- data.frame(t(preparadas2))
    df_vendas1 <- data.frame(t(vendas1))
    df_vendas2 <- data.frame(t(vendas2))
    df_profit1 <- data.frame(col1 = profit1, col2 = NA, col3 = NA, col4 = NA, col5 = NA, col6 = NA, col7 = NA)
    df_profit2 <- data.frame(col1 = profit2, col2 = NA, col3 = NA, col4 = NA, col5 = NA, col6 = NA, col7 = NA)
    df_stock1 <- data.frame(t(stock1))
    df_custostock1 <- data.frame(col1 = custo_stock1, col2 = NA, col3 = NA, col4 = NA, col5 = NA, col6 = NA, col7 = NA)
    df_stock2 <- data.frame(t(stock2))
    df_custostock2 <- data.frame(col1 = custo_stock2, col2 = NA, col3 = NA, col4 = NA, col5 = NA, col6 = NA, col7 = NA)
    df_totalprofit <- data.frame(col1 = total_profit, col2 = NA, col3 = NA, col4 = NA, col5 = NA, col6 = NA, col7 = NA)
    df_resources <- data.frame(col1 = resources, col2 = NA, col3 = NA, col4 = NA, col5 = NA, col6 = NA, col7 = NA)
    
    colnames(df_sp1) <- c("1","2","3","4","5","6","7")
    colnames(df_sp2) <- c("1","2","3","4","5","6","7")
    colnames(df_arm) <- c("1","2","3","4","5","6","7")
    colnames(df_as) <- c("1","2","3","4","5","6","7")
    colnames(df_v1) <- c("1","2","3","4","5","6","7")
    colnames(df_v2) <- c("1","2","3","4","5","6","7")
    colnames(df_v3) <- c("1","2","3","4","5","6","7")
    colnames(df_vs) <- c("1","2","3","4","5","6","7")
    colnames(df_p1) <- c("1","2","3","4","5","6","7")
    colnames(df_p2) <- c("1","2","3","4","5","6","7")
    colnames(df_vendas1) <- c("1","2","3","4","5","6","7")
    colnames(df_vendas2) <- c("1","2","3","4","5","6","7")
    colnames(df_profit1) <- c("1","2","3","4","5","6","7")
    colnames(df_profit2) <- c("1","2","3","4","5","6","7")
    colnames(df_stock1) <- c("1","2","3","4","5","6","7")
    colnames(df_stock2) <- c("1","2","3","4","5","6","7")
    colnames(df_custostock1) <- c("1","2","3","4","5","6","7")
    colnames(df_custostock2) <- c("1","2","3","4","5","6","7")
    colnames(df_totalprofit) <- c("1","2","3","4","5","6","7")
    colnames(df_resources) <- c("1","2","3","4","5","6","7")
    
    rownames(df_sp1) <- "Vendas previstas STELLA"
    rownames(df_sp2) <- "Vendas previstas BUD"
    rownames(df_arm) <- "Recursos de Armazenamento"
    rownames(df_as) <- "Custo Total dos Recursos de Armazenamento"
    rownames(df_v1) <- "Recursos de Transporte (V1)"
    rownames(df_v2) <- "Recursos de Transporte (V2)"
    rownames(df_v3) <- "Recursos de Transporte (V3)"
    rownames(df_vs) <- "Cuso Total dos Recursos de Transporte"
    rownames(df_p1) <- "Bebidas Preparadas STELLA"
    rownames(df_p2) <- "Bebidas Preparadas BUD"
    rownames(df_vendas1) <- "Vendas STELLA"
    rownames(df_vendas2) <- "Vendas BUD"
    rownames(df_profit1) <- "Lucro STELLA"
    rownames(df_profit2) <- "Lucro BUD"
    rownames(df_stock1) <- "Stock STELLA"
    rownames(df_stock2) <- "Stock BUD"
    rownames(df_custostock1) <- "Custo Stock STELLA"
    rownames(df_custostock2) <- "Custo Stock BUD"
    rownames(df_totalprofit) <- "LUCRO TOTAL"
    rownames(df_resources) <- "RECURSOS"
    
    dfeval <<- rbind(df_sp1,df_sp2,df_arm,df_as,df_v1,df_v2,df_v3,df_vs,df_p1,df_p2,df_vendas1,df_vendas2,df_profit1,df_profit2,df_stock1,df_stock2,df_custostock1,df_custostock2,df_totalprofit,df_resources)
    dfeval
  }

  # print da otimizacao
  result_arm <- eventReactive(input$choosePlan, {
    do_arm()
  })
  
  output$output_arm <- renderTable({
    result_arm()
  }, rownames = TRUE)
}