# Predict_Function.R

library(rminer) # access rminer functions -> CasesSeries, fit, lforecast, mmetric, mgraph, ...
library(openxlsx)


predict_ksvm_stella <- function() {
  
  # read data:
  d1=read.xlsx(xlsxFile="C:/Users/paulo/OneDrive/Ambiente de Trabalho/Uminho/TIAPOSE/TIAPOSE/bebidas.xlsx",sheet=1,skipEmptyRows=FALSE,colNames=TRUE,detectDates=TRUE)
  
  d1 = d1$STELLA
  d1 <- head(d1, - 14) # Remove last 14 lines - too many zeroes
  
  L=length(d1) # size of the time series
  K=7 # TS period (weekly!)
  
  Test=K # H, the number of multi-ahead steps, adjust if needed
  S=round(K/3) # step jump: set in this case to 2 days
  Runs=20 # number of growing window iterations, adjust if needed
  
  # forecast:
  W=(L-Test)-(Runs-1)*S # initial training window size for the ts space (forecast methods)
  
  # rminer:
  timelags=c(1:7) # domain knowledge for the 1,2,3,4,5,6,7 time lag selection
  D=CasesSeries(d1,timelags) # note: nrow(D) is smaller by max timelags than length(d1)
  W2=W-max(timelags) # initial training window size for the D space (CasesSeries, rminer methods)
  
  YR=diff(range(d1)) # global Y range, use the same range for the NMAE calculation in all iterations
  
  # rolling window to rminer models:
  for(b in 1:Runs)  # cycle of the incremental window training (growing window)
  {
    H=holdout(D$y,ratio=Test,mode="incremental",iter=b,window=W2,increment=S)   
  
    # code for rminer package methods, "ksvm:
    # note: the last training value is the same for dtr, namely:
    # print(dtr[length(dtr)])  
    # print(D[H$tr[length(H$tr)],]) # y is equal to previously shown value  
    M=fit(y~.,D[H$tr,],model="ksvm") # create forecasting model
    Pred=lforecast(M,D,start=(length(H$tr)+1),Test) # multi-step ahead forecasts
    #ev15[b]=mmetric(y=d1[H$ts],x=Pred,metric="NMAE",val=YR)
  }
  return(floor(Pred))
}

predict_ksvm_bud <- function() {
  
  # read data:
  d1=read.xlsx(xlsxFile="C:/Users/paulo/OneDrive/Ambiente de Trabalho/Uminho/TIAPOSE/TIAPOSE/bebidas.xlsx",sheet=1,skipEmptyRows=FALSE,colNames=TRUE,detectDates=TRUE)
  
  d1 = d1$BUD
  
  L=length(d1) # size of the time series
  K=7 # TS period (weekly!)
  
  Test=K # H, the number of multi-ahead steps, adjust if needed
  S=round(K/3) # step jump: set in this case to 2 days
  Runs=20 # number of growing window iterations, adjust if needed
  
  # forecast:
  W=(L-Test)-(Runs-1)*S # initial training window size for the ts space (forecast methods)
  
  # rminer:
  timelags=c(1:7) # domain knowledge for the 1,2,3,4,5,6,7 time lag selection
  D=CasesSeries(d1,timelags) # note: nrow(D) is smaller by max timelags than length(d1)
  W2=W-max(timelags) # initial training window size for the D space (CasesSeries, rminer methods)
  
  YR=diff(range(d1)) # global Y range, use the same range for the NMAE calculation in all iterations
  
  # rolling window to rminer models:
  for(b in 1:Runs)  # cycle of the incremental window training (growing window)
  {
    H=holdout(D$y,ratio=Test,mode="incremental",iter=b,window=W2,increment=S)   
    
    # code for rminer package methods, "ksvm:
    # note: the last training value is the same for dtr, namely:
    # print(dtr[length(dtr)])  
    # print(D[H$tr[length(H$tr)],]) # y is equal to previously shown value  
    M=fit(y~.,D[H$tr,],model="ksvm") # create forecasting model
    Pred=lforecast(M,D,start=(length(H$tr)+1),Test) # multi-step ahead forecasts
    #ev15[b]=mmetric(y=d1[H$ts],x=Pred,metric="NMAE",val=YR)
  }
  return(floor(Pred))
}
#cat("Predict Stella:")
#print(predict_ksvm_stella())
#cat("Predict Bud:")
#print(predict_ksvm_bud())

#variaveis globais

ganho1 <- 5.7
ganho2 <- 4.4

custo_stock <- 1
custo_arm <- 10
custo_v1 <- 40
custo_v2 <- 50
custo_v3 <- 53

sales_pred1 <- predict_ksvm_stella()
sales_pred2 <- predict_ksvm_bud()

soma_arm <- 0
soma_v1 <- 0
soma_v2 <- 0
soma_v3 <- 0

# Function to repair solution
repair <- function(s) {
  # splitting code by chatgpt
  split_s <- split(s, rep(1:6, each = 7))
  preparadas1 <- split_s[[1]]
  preparadas2 <- split_s[[2]]
  arm <- split_s[[3]]
  v1 <- split_s[[4]]
  v2 <- split_s[[5]]
  v3 <- split_s[[6]]
  
  # calculo dos maximos
  max_arm <- arm*72
  max_v <- v1*60 + v2*90 + v3*120
  
  #iniciacao das variaveis
  total_p <- preparadas1 + preparadas2
  p1_a <- preparadas1
  p2_a <- preparadas2
  p1_v <- preparadas1
  p2_v <- preparadas2
  
  # verificacao
  for (i in 1:7) {
    
    # arm
    if (0 <= total_p[i] && max_arm[i] >= total_p[i]) {
      p1_a[i] <- p1_a[i]
      p2_a[i] <- p2_a[i]
    } else {
      cat("AVISO: Plano com valor ilegal encontrado nos recursos de armazém, no dia ",i,"- A resolver...\n")
      p1_a[i] <- max_arm[i] / 2
      p2_a[i] <- max_arm[i] / 2
    }
    
    # v
    if (0 <= total_p[i] && max_v[i] >= total_p[i]) {
      p1_v[i] <- p1_v[i]
      p2_v[i] <- p2_v[i]
    } else {
      cat("AVISO: Plano com valor ilegal encontrado nos recursos de distribuição, no dia ",i,"- A resolver...\n")
      p1_v[i] <- max_v[i] / 2
      p2_v[i] <- max_v[i] / 2
    }
    
  }
  
  # cria o vetor final com o minimo de cada elemento. de maneira a respeitar semrpe as duas
  p1_f <- pmin(p1_a,p1_v)
  p2_f <- pmin(p2_a,p2_v)
  
  return (c(p1_f,p2_f,arm,v1,v2,v3))
}

# Define function to calculate profit
eval <- function(s) {
  
  # Repair proposed solution
  s <- repair(s)
  
  # splitting code by chatgpt
  split_s <- split(s, rep(1:6, each = 7))
  preparadas1 <- split_s[[1]]
  preparadas2 <- split_s[[2]]
  arm <- split_s[[3]]
  v1 <- split_s[[4]]
  v2 <- split_s[[5]]
  v3 <- split_s[[6]]
  
  # Initialize variables
  profit1 <- 0
  profit2 <- 0
  
  actual_sales1 <- c(0, 0, 0, 0, 0, 0, 0, 0)
  actual_sales2 <- c(0, 0, 0, 0, 0, 0, 0, 0)
  
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
    if (disponivel1 <= sales_pred1[i]) {
      actual_sales1[i] <- disponivel1
    } else {
      actual_sales1[i] <- sales_pred1[i]
      stock1[i+1] <- disponivel1 - sales_pred1[i]
    }
    # Calculate profit
    profit1 <- profit1 + (actual_sales1[i] * ganho1)
    # -------------------------
    
    # BUD ---------------------
    # Calculate actual sales
    if (disponivel2 <= sales_pred2[i]) {
      actual_sales2[i] <- disponivel2
    } else {
      actual_sales2[i] <- sales_pred2[i]
      stock2[i+1] <- disponivel2 - sales_pred2[i]
    }
    # Calculate profit
    profit2 <- profit2 + (actual_sales2[i] * ganho2)
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
  cat("- vendas stella:",head(actual_sales1,-1)," > lucro: ",profit1,
      "\n-    vendas bud:",head(actual_sales2,-1)," > lucro: ",profit2)
  cat("\n-  stock stella:",tail(stock1,-1)," > custo: ",sum(stock1),
      "\n-     stock bud:",tail(stock2,-1)," > custo: ",sum(stock2),"\n")
  
  
  cat("Lucro Final: R$", total_profit, "\n")
  
  cat("Recursos:  ", resources, "\n")
  
  return(total_profit)
}

# Usage:

preparadas1 <- c(160, 8, 0, 52, 20, 0, 0)
preparadas2 <- c(200, 200, 0, 0, 30, 0, 0)


arm <- c(6, 3, 0, 1, 1, 0, 1)

#arm <- c(1, 1, 0, 1, 1, 0, 1)

v1 <- c(2, 0, 0, 1, 0, 0, 0)
v2 <- c(2, 1, 0, 0, 1, 0, 0)
v3 <- c(2, 1, 0, 0, 0, 0, 0)

s <- c(preparadas1,preparadas2,arm,v1,v2,v3)

eval(s)