library(rminer) # access rminer functions -> CasesSeries, fit, lforecast, mmetric, mgraph, ...

xgboost_stella <- function() {
  
  d1 = db$STELLA
  
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
    
    M7=fit(y~.,D[H$tr,],model="xgboost") # create forecasting model
    PredR7=lforecast(M7,D,start=(length(H$tr)+1),Test) # multi-step ahead forecasts
    #ev7[b]=mmetric(y=d1[H$ts],x=PredR7,metric="NMAE",val=YR)
  }
  return(floor(PredR7))
}

xgboost_bud <- function() {
  
  d1 = db$BUD
  
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
    
    M7=fit(y~.,D[H$tr,],model="xgboost") # create forecasting model
    PredR7=lforecast(M7,D,start=(length(H$tr)+1),Test) # multi-step ahead forecasts
    #ev7[b]=mmetric(y=d1[H$ts],x=PredR7,metric="NMAE",val=YR)
  }
  return(floor(PredR7))
}