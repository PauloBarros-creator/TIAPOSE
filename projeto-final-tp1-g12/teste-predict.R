# Predict_Function.R

library(rminer) # access rminer functions -> CasesSeries, fit, lforecast, mmetric, mgraph, ...
library(openxlsx)


predict_ksvm_stella <- function() {
  
  # read data:
  #d1=read.xlsx(xlsxFile="C:/Users/paulo/OneDrive/Ambiente de Trabalho/Uminho/TIAPOSE/TIAPOSE/bebidas.xlsx",sheet=1,skipEmptyRows=FALSE,colNames=TRUE,detectDates=TRUE)
  d1=db
  
  d1 = d1$STELLA
  d1 <- head(d1, - 14) # Remove last 14 lines - too many zeroes
  
  L=length(d1) # size of the time series
  K=7 # TS period (weekly!)
  
  Test=K # H, the number of multi-ahead steps, adjust if needed
  S=round(K/3) # step jump: set in this case to 2 days
  print(S)
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
  print(Pred)
  return(Pred)
}

predict_ksvm_bud <- function() {
  
  # read data:
  #d1=read.xlsx(xlsxFile="C:/Users/paulo/OneDrive/Ambiente de Trabalho/Uminho/TIAPOSE/TIAPOSE/bebidas.xlsx",sheet=1,skipEmptyRows=FALSE,colNames=TRUE,detectDates=TRUE)
  d1=db
  
  d1 = d1$BUD
  
  L=length(d1) # size of the time series
  K=7 # TS period (weekly!)
  
  Test=K # H, the number of multi-ahead steps, adjust if needed
  S=round(K/3) # step jump: set in this case to 2 days
  print(S)
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
  print(Pred)
  return(Pred)
}