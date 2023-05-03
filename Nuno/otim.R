# Made in very small collaboration with ChatGPT
# duvidas (ver imagem):
#   para queservem os "valores estimados para venda"
#   por "elementos do plano" entende-se inputs do utilizador?
#   custo veiculos dá-me 489...
#   os arm e veiculos nao impactam o numero de bebidas disponiveis

#     SEJAM 
#      1 = stella
#      2 = bud

#variaveis globais

ganho1 <- 5.7
ganho2 <- 4.4

custo_stock <- 1
custo_arm <- 10
custo_v1 <- 40
custo_v2 <- 50
custo_v3 <- 53

sales_pred1 <- c(141, 154, 18, 102, 211, 69, 37, 0)
sales_pred2 <- c(211, 172, 330, 39, 45, 125, 0)

demand1 <- c(141, 27, 0, 52, 20, 0, 0)
demand2 <- c(200, 172, 28, 0, 30, 0, 0)

soma_arm <- 0
soma_v1 <- 0
soma_v2 <- 0
soma_v3 <- 0

# Define function to calculate profit
eval <- function(preparadas1, preparadas2, arm, v1, v2, v3) {
  # Initialize variables
  profit1 <- 0
  profit2 <- 0
  
  # cada elemento do vetor stock é relativo ao dia anterior. dai existirem 8
  #stock <- c(dia0,dia1,dia2,dia3,dia4,dia5,dia6,dia7)
  stock1 <- c(0, 0, 0, 0, 0, 0, 0, 0)
  stock2 <- c(0, 0, 0, 0, 0, 0, 0, 0)

  # Loop through each day
  for (i in 1:7) {
  
    
    # cerveja disponivel STELLA
    disponivel1 <- preparadas1[i] + stock1[i]
    
    # cerveja disponivel BUD
    disponivel2 <- preparadas2[i] + stock2[i]
    
    # STELLA ------------------
    # Calculate actual sales
    if (disponivel1 <= demand1[i]) {
      actual_sales1 <- disponivel1
    } else {
      actual_sales1 <- demand1[i]
      stock1[i+1] <- disponivel1 - demand1[i]
    }
    # Calculate profit
    profit1 <- profit1 + (actual_sales1 * ganho1)
    # -------------------------
    
    # BUD ---------------------
    # Calculate actual sales
    if (disponivel2 <= demand2[i]) {
      actual_sales2 <- disponivel2
    } else {
      actual_sales2 <- demand2[i]
      stock2[i+1] <- disponivel2 - demand2[i]
    }
    # Calculate profit
    profit2 <- profit2 + (actual_sales2 * ganho2)
    # -------------------------
    
    # Custos cumulativos
    #soma_arm <<- soma_arm + arm[i] * custo_arm
    #soma_v1 <<- soma_v1 + v1[i] * custo_v1
    #soma_v2 <<- soma_v2 + v2[i] * custo_v2
    #soma_v3 <<- soma_v3 + v3[i] * custo_v3
    
    # fim de semana hardcoded?????????????
    if (i == 2 || i == 3) {
      soma_arm <<- soma_arm + arm[i] * (custo_arm+5)
      soma_v1 <<- soma_v1 + v1[i] * (custo_v1+5)
      soma_v2 <<- soma_v2 + v2[i] * (custo_v2+5)
      soma_v3 <<- soma_v3 + v3[i] * (custo_v3+5)
    }else{
      soma_arm <<- soma_arm + arm[i] * custo_arm
      soma_v1 <<- soma_v1 + v1[i] * custo_v1
      soma_v2 <<- soma_v2 + v2[i] * custo_v2
      soma_v3 <<- soma_v3 + v3[i] * custo_v3
    }
    
  }
  
  total_profit <- profit1 + profit2 - soma_arm - soma_v1 - soma_v2 - soma_v3 - sum(stock1) - sum(stock2)
  
  resources <- sum(arm) + sum(v1) + sum(v2) + sum(v3)
  
  # resposta:
  cat("Valores estimados para venda: \n stella: ",sales_pred1,"\n    bud: ",sales_pred2,"\n")
  cat("    arm.: ",arm," > custo: ",soma_arm,
      "\n     v1: ",v1,
      "\n     v2: ",v2,
      "\n     v3: ",v3,
      "\ncusto veiculos: ",soma_v1+soma_v2+soma_v3,"\n")
  cat("bebidas empacotadas e distribuidas: \n  stella: ",preparadas1,"\n     bud: ",preparadas2,"\n")
  cat("- vendas stella:",demand1," > lucro: ",profit1,
      "\n-    vendas bud:",demand2," > lucro: ",profit2)
  cat("\n-  stock stella:",stock1," > custo: ",sum(stock1),
      "\n-     stock bud:",stock2," > custo: ",sum(stock2),"\n")

  
  cat("Lucro Final: R$", total_profit, "\n")
  
  cat("Recursos:  ", resources, "\n")
  
  return(total_profit)
}

# Usage:

preparadas1 <- c(160, 8, 0, 52, 20, 0, 0)
preparadas2 <- c(200, 200, 0, 0, 30, 0, 0)

arm <- c(6, 1, 0, 1, 1, 0, 1)

v1 <- c(2, 0, 0, 1, 0, 0, 0)
v2 <- c(2, 1, 0, 0, 1, 0, 0)
v3 <- c(2, 1, 0, 0, 0, 0, 0)

# profit <- 
eval(preparadas1, preparadas2, arm, v1,v2,v3)
# resources <- sum(arm) + sum(v1) + sum(v2) + sum(v3)
# 
# # resposta:
# cat("Valores estimados para venda: \n stella: ",sales_pred1,"\n    bud: ",sales_pred2,"\n")
# cat("    arm.: ",arm," > custo: ",soma_arm,
#     "\n     v1: ",v1,
#     "\n     v2: ",v2,
#     "\n     v3: ",v3,
#     "\ncusto veiculos: ",soma_v1+soma_v2+soma_v3,"\n")
# cat("- vendas stella:",demand1," > lucro: ",profit1,
#     "\n-     vendas bud:",demand2," > lucro: ",profi2)
# 
# cat("bebidas empacotadas e distribuidas: \n  stella: ",preparadas1,"\n     bud: ",preparadas2,"\n")
# 
# cat("The total profit is R$", profit, "\n")
# 
# cat("The total resources used was ", resources, "\n")
# 
#  stella
#     bud
#     arm
#      v1
