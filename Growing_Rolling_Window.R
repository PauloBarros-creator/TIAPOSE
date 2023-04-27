# Growing_Rolling_Window.R: script demonstration of growing window and rolling window evaluations.

library(forecast) # access forecast functions -> HoltWinters, forecast
library(rminer) # access rminer functions -> CasesSeries, fit, lforecast, mmetric, mgraph, ...
library(openxlsx)

# setwd() # adjust working directory if needed.

# read data:
cat("read beer time series:")
d1=read.xlsx(xlsxFile="C:/Users/paulo/OneDrive/Ambiente de Trabalho/Uminho/TIAPOSE/TIAPOSE/bebidas.xlsx",sheet=1,skipEmptyRows=FALSE,colNames=TRUE,detectDates=TRUE)
#d1 <- head(d1, - 14) # Remove last 14 lines - too many zeroes

#d1 = d1$STELLA
d1 = d1$BUD
summary(d1)

L=length(d1) # size of the time series
K=7 # TS period (weekly!)

print("incremental (growing) window training demonstration:")

Test=K # H, the number of multi-ahead steps, adjust if needed
S=round(K/2) # step jump: set in this case to 2 days
Runs=20 # number of growing window iterations, adjust if needed

# forecast:
W=(L-Test)-(Runs-1)*S # initial training window size for the ts space (forecast methods)

# rminer:
timelags=c(1:7) # domain knowledge for the 1,2,3,4,5,6,7 time lag selection
D=CasesSeries(d1,timelags) # note: nrow(D) is smaller by max timelags than length(d1)
W2=W-max(timelags) # initial training window size for the D space (CasesSeries, rminer methods)

YR=diff(range(d1)) # global Y range, use the same range for the NMAE calculation in all iterations

ev=vector(length=Runs) # error vector for "HoltWinters"
ev2=vector(length=Runs) # error vector for "mlpe"
ev3=vector(length=Runs) # error vector for "arima"
ev4=vector(length=Runs) # error vector for "NN"
ev5=vector(length=Runs) # error vector for "ets"
ev6=vector(length=Runs) # error vector for "lm"
ev7=vector(length=Runs) # error vector for "xgboost"
ev8=vector(length=Runs) # error vector for "cubist"
ev9=vector(length=Runs) # error vector for "mars"
ev10=vector(length=Runs) # error vector for "rvm"


# growing window to forecast models:
for(b in 1:Runs)  # cycle of the incremental window training (growing window)
{
  # code for the forecast package methods, HoltWinters:
  H=holdout(d1,ratio=Test,mode="incremental",iter=b,window=W,increment=S)   
  trinit=H$tr[1]
  dtr=ts(d1[H$tr],frequency=K) # create ts object, note that there is no start argument (for simplicity of the code)
  M=suppressWarnings(HoltWinters(dtr)) # create forecasting model, suppressWarnings removes warnings from HW method
  Pred=forecast(M,h=length(H$ts))$mean[1:Test] # multi-step ahead forecasts
  ev[b]=mmetric(y=d1[H$ts],x=Pred,metric="NMAE",val=YR)
  
  # code for the forecast package methods, Arima:
  H3=holdout(d1,ratio=Test,mode="incremental",iter=b,window=W,increment=S)   
  trinit=H3$tr[1]
  dtr=ts(d1[H3$tr],frequency=K) # create ts object, note that there is no start argument (for simplicity of the code)
  M3=suppressWarnings(auto.arima(dtr)) # create forecasting model, suppressWarnings removes warnings from arima method
  Pred3=forecast(M3,h=length(H3$ts))$mean[1:Test] # multi-step ahead forecasts
  ev3[b]=mmetric(y=d1[H$ts],x=Pred3,metric="NMAE",val=YR)
  
  # code for the forecast package methods, NN:
  H4=holdout(d1,ratio=Test,mode="incremental",iter=b,window=W,increment=S)   
  trinit=H4$tr[1]
  dtr=ts(d1[H4$tr],frequency=K) # create ts object, note that there is no start argument (for simplicity of the code)
  M4=suppressWarnings(nnetar(dtr,P=1,repeats=3)) # create forecasting model, suppressWarnings removes warnings from NN method
  Pred4=forecast(M4,h=length(H4$ts))$mean[1:Test] # multi-step ahead forecasts
  ev4[b]=mmetric(y=d1[H$ts],x=Pred4,metric="NMAE",val=YR)
  
  # code for the forecast package methods, ets:
  H5=holdout(d1,ratio=Test,mode="incremental",iter=b,window=W,increment=S)   
  trinit=H5$tr[1]
  dtr=ts(d1[H5$tr],frequency=K) # create ts object, note that there is no start argument (for simplicity of the code)
  M5=suppressWarnings(ets(dtr)) # create forecasting model, suppressWarnings removes warnings from ets method
  Pred5=forecast(M5,h=length(H5$ts))$mean[1:Test] # multi-step ahead forecasts
  ev5[b]=mmetric(y=d1[H$ts],x=Pred5,metric="NMAE",val=YR)
  
  cat("iter:",b,"TR from:",trinit,"to:",(trinit+length(H$tr)-1),"size:",length(H$tr),
      "TS from:",H$ts[1],"to:",H$ts[length(H$ts)],"size:",length(H$ts),
      "nmae:",ev[b],",",ev2[b],"\n")
  mgraph(d1[H$ts],Pred,graph="REG",Grid=10,col=c("black","blue", "green", "purple", "brown"),leg=list(pos="topleft",leg=c("target","HW pred.", "arima", "NN", "ets")))
  lines(Pred3,pch=19,cex=0.5,type="b",col="green")
  lines(Pred4,pch=19,cex=0.5,type="b",col="purple")
  lines(Pred5,pch=19,cex=0.5,type="b",col="brown")
  mpause() # wait for enter
  
} # end of cycle

# growing window to rminer models:
for(b in 1:Runs)  # cycle of the incremental window training (growing window)
{
  # code for rminer package methods, "lm":
  H6=holdout(D$y,ratio=Test,mode="incremental",iter=b,window=W2,increment=S)   
  # note: the last training value is the same for dtr, namely:
  print(dtr[length(dtr)])  
  print(D[H6$tr[length(H6$tr)],]) # y is equal to previously shown value  
  M6=fit(y~.,D[H6$tr,],model="lm") # create forecasting model
  Pred6=lforecast(M6,D,start=(length(H6$tr)+1),Test) # multi-step ahead forecasts
  ev6[b]=mmetric(y=d1[H$ts],x=Pred6,metric="NMAE",val=YR)
  
  # code for rminer package methods, "mlpe":
  H2=holdout(D$y,ratio=Test,mode="incremental",iter=b,window=W2,increment=S)   
  # note: the last training value is the same for dtr, namely:
  print(dtr[length(dtr)])  
  print(D[H2$tr[length(H2$tr)],]) # y is equal to previously shown value  
  M2=fit(y~.,D[H2$tr,],model="mlpe") # create forecasting model
  Pred2=lforecast(M2,D,start=(length(H2$tr)+1),Test) # multi-step ahead forecasts
  ev2[b]=mmetric(y=d1[H$ts],x=Pred2,metric="NMAE",val=YR)
  
  # code for rminer package methods, "xgboost":
  H7=holdout(D$y,ratio=Test,mode="incremental",iter=b,window=W2,increment=S)   
  # note: the last training value is the same for dtr, namely:
  print(dtr[length(dtr)])  
  print(D[H7$tr[length(H7$tr)],]) # y is equal to previously shown value  
  M7=fit(y~.,D[H7$tr,],model="xgboost") # create forecasting model
  Pred7=lforecast(M7,D,start=(length(H7$tr)+1),Test) # multi-step ahead forecasts
  ev7[b]=mmetric(y=d1[H$ts],x=Pred7,metric="NMAE",val=YR)
  
  # code for rminer package methods, "cubist":
  H8=holdout(D$y,ratio=Test,mode="incremental",iter=b,window=W2,increment=S)   
  # note: the last training value is the same for dtr, namely:
  print(dtr[length(dtr)])  
  print(D[H8$tr[length(H8$tr)],]) # y is equal to previously shown value  
  M8=fit(y~.,D[H8$tr,],model="cubist") # create forecasting model
  Pred8=lforecast(M8,D,start=(length(H8$tr)+1),Test) # multi-step ahead forecasts
  ev8[b]=mmetric(y=d1[H$ts],x=Pred8,metric="NMAE",val=YR)
  
  # code for rminer package methods, "mars:
  H9=holdout(D$y,ratio=Test,mode="incremental",iter=b,window=W2,increment=S)   
  # note: the last training value is the same for dtr, namely:
  print(dtr[length(dtr)])  
  print(D[H9$tr[length(H9$tr)],]) # y is equal to previously shown value  
  M9=fit(y~.,D[H9$tr,],model="mars") # create forecasting model
  Pred9=lforecast(M9,D,start=(length(H9$tr)+1),Test) # multi-step ahead forecasts
  ev9[b]=mmetric(y=d1[H$ts],x=Pred9,metric="NMAE",val=YR)
  
  # code for rminer package methods, "rvm:
  H10=holdout(D$y,ratio=Test,mode="incremental",iter=b,window=W2,increment=S)   
  # note: the last training value is the same for dtr, namely:
  print(dtr[length(dtr)])  
  print(D[H10$tr[length(H10$tr)],]) # y is equal to previously shown value  
  M10=fit(y~.,D[H10$tr,],model="rvm") # create forecasting model
  Pred10=lforecast(M10,D,start=(length(H10$tr)+1),Test) # multi-step ahead forecasts
  ev10[b]=mmetric(y=d1[H$ts],x=Pred10,metric="NMAE",val=YR)
  
  cat("iter:",b,"TR from:",trinit,"to:",(trinit+length(H$tr)-1),"size:",length(H$tr),
      "TS from:",H$ts[1],"to:",H$ts[length(H$ts)],"size:",length(H$ts),
      "nmae:",ev2[b],",",ev6[b],",",ev7[b],",",ev8[b],",",ev9[b],",",ev10[b],"\n")
  mgraph(d1[H$ts],Pred2,graph="REG",Grid=10,col=c("black","blue", "green", "purple", "red", "brown", "orange"),leg=list(pos="topleft",leg=c("target","lm", "mlpe", "xgboost", "cubist", "mars", "rvm")))
  lines(Pred6,pch=19,cex=0.5,type="b",col="green")
  lines(Pred7,pch=19,cex=0.5,type="b",col="purple")
  lines(Pred8,pch=19,cex=0.5,type="b",col="red")
  lines(Pred9,pch=19,cex=0.5,type="b",col="brown")
  lines(Pred10,pch=19,cex=0.5,type="b",col="orange")
  mpause() # wait for enter
}
# show median of ev, ev2 ... evn
cat("median NMAE values for HW and mlpe:\n")
cat("Holt-Winters median NMAE:",median(ev),"\n")
cat("mlpe median NMAE:",median(ev2),"\n")
mpause() # wait for enter

# rolling window to forecast models:
for(b in 1:Runs)  # cycle of the incremental window training (rolling window)
{
  # code for the forecast package methods, HoltWinters:
  H=holdout(d1,ratio=Test,mode="rolling",iter=b,window=W,increment=S)   
  trinit=H$tr[1]
  dtr=ts(d1[H$tr],frequency=K) # create ts object, note that there is no start argument (for simplicity of the code)
  M=suppressWarnings(HoltWinters(dtr)) # create forecasting model, suppressWarnings removes warnings from HW method
  Pred=forecast(M,h=length(H$ts))$mean[1:Test] # multi-step ahead forecasts
  ev[b]=mmetric(y=d1[H$ts],x=Pred,metric="NMAE",val=YR)
  
  # code for the forecast package methods, Arima:
  H3=holdout(d1,ratio=Test,mode="rolling",iter=b,window=W,increment=S)   
  trinit=H3$tr[1]
  dtr=ts(d1[H3$tr],frequency=K) # create ts object, note that there is no start argument (for simplicity of the code)
  M3=suppressWarnings(auto.arima(dtr)) # create forecasting model, suppressWarnings removes warnings from arima method
  Pred3=forecast(M3,h=length(H3$ts))$mean[1:Test] # multi-step ahead forecasts
  ev3[b]=mmetric(y=d1[H$ts],x=Pred3,metric="NMAE",val=YR)
  
  # code for the forecast package methods, NN:
  H4=holdout(d1,ratio=Test,mode="rolling",iter=b,window=W,increment=S)   
  trinit=H4$tr[1]
  dtr=ts(d1[H4$tr],frequency=K) # create ts object, note that there is no start argument (for simplicity of the code)
  M4=suppressWarnings(nnetar(dtr,P=1,repeats=3)) # create forecasting model, suppressWarnings removes warnings from NN method
  Pred4=forecast(M4,h=length(H4$ts))$mean[1:Test] # multi-step ahead forecasts
  ev4[b]=mmetric(y=d1[H$ts],x=Pred4,metric="NMAE",val=YR)
  
  # code for the forecast package methods, ets:
  H5=holdout(d1,ratio=Test,mode="rolling",iter=b,window=W,increment=S)   
  trinit=H5$tr[1]
  dtr=ts(d1[H5$tr],frequency=K) # create ts object, note that there is no start argument (for simplicity of the code)
  M5=suppressWarnings(ets(dtr)) # create forecasting model, suppressWarnings removes warnings from ets method
  Pred5=forecast(M5,h=length(H5$ts))$mean[1:Test] # multi-step ahead forecasts
  ev5[b]=mmetric(y=d1[H$ts],x=Pred5,metric="NMAE",val=YR)
  
  cat("iter:",b,"TR from:",trinit,"to:",(trinit+length(H$tr)-1),"size:",length(H$tr),
      "TS from:",H$ts[1],"to:",H$ts[length(H$ts)],"size:",length(H$ts),
      "nmae:",ev[b],",",ev2[b],"\n")
} # end of cycle

# show median of ev and ev2
cat("median NMAE values for HW and mlpe:\n")
cat("Holt-Winters median NMAE:",median(ev),"\n")
cat("mlpe median NMAE:",median(ev2),"\n")

# last iteration predictions:
mgraph(d1[H$ts],Pred,graph="REG",Grid=10,col=c("black","blue", "green", "purple", "brown"),leg=list(pos="topleft",leg=c("target","HW pred.", "arima", "NN", "ets")))
lines(Pred3,pch=19,cex=0.5,type="b",col="green")
lines(Pred4,pch=19,cex=0.5,type="b",col="purple")
lines(Pred5,pch=19,cex=0.5,type="b",col="brown")

# rolling window to rminer models:
for(b in 1:Runs)  # cycle of the incremental window training (rolling window)
{
  # code for rminer package methods, "lm":
  H6=holdout(D$y,ratio=Test,mode="incremental",iter=b,window=W2,increment=S)   
  # note: the last training value is the same for dtr, namely:
  print(dtr[length(dtr)])  
  print(D[H6$tr[length(H6$tr)],]) # y is equal to previously shown value  
  M6=fit(y~.,D[H6$tr,],model="lm") # create forecasting model
  Pred6=lforecast(M6,D,start=(length(H6$tr)+1),Test) # multi-step ahead forecasts
  ev6[b]=mmetric(y=d1[H$ts],x=Pred6,metric="NMAE",val=YR)
  
  # code for rminer package methods, "mlpe":
  H2=holdout(D$y,ratio=Test,mode="incremental",iter=b,window=W2,increment=S)   
  # note: the last training value is the same for dtr, namely:
  print(dtr[length(dtr)])  
  print(D[H2$tr[length(H2$tr)],]) # y is equal to previously shown value  
  M2=fit(y~.,D[H2$tr,],model="mlpe") # create forecasting model
  Pred2=lforecast(M2,D,start=(length(H2$tr)+1),Test) # multi-step ahead forecasts
  ev2[b]=mmetric(y=d1[H$ts],x=Pred2,metric="NMAE",val=YR)
  
  # code for rminer package methods, "xgboost":
  H7=holdout(D$y,ratio=Test,mode="incremental",iter=b,window=W2,increment=S)   
  # note: the last training value is the same for dtr, namely:
  print(dtr[length(dtr)])  
  print(D[H7$tr[length(H7$tr)],]) # y is equal to previously shown value  
  M7=fit(y~.,D[H7$tr,],model="xgboost") # create forecasting model
  Pred7=lforecast(M7,D,start=(length(H7$tr)+1),Test) # multi-step ahead forecasts
  ev7[b]=mmetric(y=d1[H$ts],x=Pred7,metric="NMAE",val=YR)
  
  # code for rminer package methods, "cubist":
  H8=holdout(D$y,ratio=Test,mode="incremental",iter=b,window=W2,increment=S)   
  # note: the last training value is the same for dtr, namely:
  print(dtr[length(dtr)])  
  print(D[H8$tr[length(H8$tr)],]) # y is equal to previously shown value  
  M8=fit(y~.,D[H8$tr,],model="cubist") # create forecasting model
  Pred8=lforecast(M8,D,start=(length(H8$tr)+1),Test) # multi-step ahead forecasts
  ev8[b]=mmetric(y=d1[H$ts],x=Pred8,metric="NMAE",val=YR)
  
  # code for rminer package methods, "mars:
  H9=holdout(D$y,ratio=Test,mode="incremental",iter=b,window=W2,increment=S)   
  # note: the last training value is the same for dtr, namely:
  print(dtr[length(dtr)])  
  print(D[H9$tr[length(H9$tr)],]) # y is equal to previously shown value  
  M9=fit(y~.,D[H9$tr,],model="mars") # create forecasting model
  Pred9=lforecast(M9,D,start=(length(H9$tr)+1),Test) # multi-step ahead forecasts
  ev9[b]=mmetric(y=d1[H$ts],x=Pred9,metric="NMAE",val=YR)
  
  # code for rminer package methods, "rvm:
  H10=holdout(D$y,ratio=Test,mode="incremental",iter=b,window=W2,increment=S)   
  # note: the last training value is the same for dtr, namely:
  print(dtr[length(dtr)])  
  print(D[H10$tr[length(H10$tr)],]) # y is equal to previously shown value  
  M10=fit(y~.,D[H10$tr,],model="rvm") # create forecasting model
  Pred10=lforecast(M10,D,start=(length(H10$tr)+1),Test) # multi-step ahead forecasts
  ev10[b]=mmetric(y=d1[H$ts],x=Pred10,metric="NMAE",val=YR)
  
  cat("iter:",b,"TR from:",trinit,"to:",(trinit+length(H$tr)-1),"size:",length(H$tr),
      "TS from:",H$ts[1],"to:",H$ts[length(H$ts)],"size:",length(H$ts),
      "nmae:",ev2[b],",",ev6[b],",",ev7[b],",",ev8[b],",",ev9[b],",",ev10[b],"\n")

} # end of cycle

# show median of ev and ev2
cat("median NMAE values for HW and mlpe:\n")
cat("Holt-Winters median NMAE:",median(ev),"\n")
cat("mlpe median NMAE:",median(ev2),"\n")

# last iteration predictions:
mgraph(d1[H$ts],Pred2,graph="REG",Grid=10,col=c("black","blue", "green", "purple", "red", "brown", "orange"),leg=list(pos="topleft",leg=c("target","lm", "mlpe", "xgboost", "cubist", "mars", "rvm")))
lines(Pred6,pch=19,cex=0.5,type="b",col="green")
lines(Pred7,pch=19,cex=0.5,type="b",col="purple")
lines(Pred8,pch=19,cex=0.5,type="b",col="red")
lines(Pred9,pch=19,cex=0.5,type="b",col="brown")
lines(Pred10,pch=19,cex=0.5,type="b",col="orange")