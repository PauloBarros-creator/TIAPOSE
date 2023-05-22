# Growing_Rolling_Window.R: script demonstration of growing window and rolling window evaluations.

library(forecast) # access forecast functions -> HoltWinters, forecast
library(rminer) # access rminer functions -> CasesSeries, fit, lforecast, mmetric, mgraph, ...
library(openxlsx)

# setwd() # adjust working directory if needed.

# read data:
cat("read beer time series:")
d1=read.xlsx(xlsxFile="C:/Users/paulo/OneDrive/Ambiente de Trabalho/Uminho/TIAPOSE/TIAPOSE/bebidas.xlsx",sheet=1,skipEmptyRows=FALSE,colNames=TRUE,detectDates=TRUE)
d1 <- head(d1, - 14) # Remove last 14 lines - too many zeroes

d1 = d1$STELLA
#d1 = d1$BUD
summary(d1)

L=length(d1) # size of the time series
K=7 # TS period (weekly!)

print("incremental (growing) window training demonstration:")

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
ev11=vector(length=Runs) # error vector for "mr"
ev12=vector(length=Runs) # error vector for "pcr"
ev13=vector(length=Runs) # error vector for "plsr"
ev14=vector(length=Runs) # error vector for "cppls"
ev15=vector(length=Runs) # error vector for "ksvm"
ev16=vector(length=Runs) # error vector for "mlp"
ev17=vector(length=Runs) # error vector for "ctree"
ev18=vector(length=Runs) # error vector for "dt"
ev19=vector(length=Runs) # error vector for "knn"
ev20=vector(length=Runs) # error vector for "cv.glmnet"
ev21=vector(length=Runs) # error vector for "randomForest"
ev22=vector(length=Runs) # error vector for "naive"

# growing window to forecast models:
for(b in 1:Runs)  # cycle of the incremental window training (growing window)
{
  H=holdout(d1,ratio=Test,mode="incremental",iter=b,window=W,increment=S)   
  trinit=H$tr[1]
  dtr=ts(d1[H$tr],frequency=K) # create ts object, note that there is no start argument (for simplicity of the code)
  
  # code for the forecast package methods, HoltWinters:  
  M=suppressWarnings(HoltWinters(dtr)) # create forecasting model, suppressWarnings removes warnings from HW method
  Pred=forecast(M,h=length(H$ts))$mean[1:Test] # multi-step ahead forecasts
  ev[b]=mmetric(y=d1[H$ts],x=Pred,metric="NMAE",val=YR)
  
  # code for the forecast package methods, Arima:
  M3=suppressWarnings(auto.arima(dtr)) # create forecasting model, suppressWarnings removes warnings from arima method
  Pred3=forecast(M3,h=length(H$ts))$mean[1:Test] # multi-step ahead forecasts
  ev3[b]=mmetric(y=d1[H$ts],x=Pred3,metric="NMAE",val=YR)
  
  # code for the forecast package methods, NN:
  M4=suppressWarnings(nnetar(dtr,P=1,repeats=3)) # create forecasting model, suppressWarnings removes warnings from NN method
  Pred4=forecast(M4,h=length(H$ts))$mean[1:Test] # multi-step ahead forecasts
  ev4[b]=mmetric(y=d1[H$ts],x=Pred4,metric="NMAE",val=YR)
  
  # code for the forecast package methods, ets:
  M5=suppressWarnings(ets(dtr)) # create forecasting model, suppressWarnings removes warnings from ets method
  Pred5=forecast(M5,h=length(H$ts))$mean[1:Test] # multi-step ahead forecasts
  ev5[b]=mmetric(y=d1[H$ts],x=Pred5,metric="NMAE",val=YR)
  
  cat("iter:",b,"TR from:",trinit,"to:",(trinit+length(H$tr)-1),"size:",length(H$tr),
      "TS from:",H$ts[1],"to:",H$ts[length(H$ts)],"size:",length(H$ts),
      "nmae:",ev[b],",",ev3[b],",",ev4[b],",",ev5[b],"\n")
  
  mgraph(d1[H$ts],Pred,graph="REG",Grid=10,col=c("black","blue", "green", "purple", "brown"),leg=list(pos="topleft",leg=c("target","HW pred.", "arima", "NN", "ets")))
  lines(Pred3,pch=19,cex=0.5,type="b",col="green")
  lines(Pred4,pch=19,cex=0.5,type="b",col="purple")
  lines(Pred5,pch=19,cex=0.5,type="b",col="brown")
  mpause() # wait for enter
  
} # end of cycle

# show median
cat("median NMAE values:\n")
cat("HoltWinters median NMAE:",median(ev),"\n")
cat("Arima median NMAE:",median(ev3),"\n")
cat("NN median NMAE:",median(ev4),"\n")
cat("ets median NMAE:",median(ev5),"\n")
mpause() # wait for enter

# growing window to rminer models part 1:
for(b in 1:Runs)  # cycle of the incremental window training (growing window)
{
  H=holdout(D$y,ratio=Test,mode="incremental",iter=b,window=W2,increment=S)   
  
  # code for rminer package methods, "naive":
  # note: the last training value is the same for dtr, namely:
  # print(dtr[length(dtr)])  
  # print(D[H$tr[length(H$tr)],]) # y is equal to previously shown value  
  M22=fit(y~.,D[H$tr,],model="naive") # create forecasting model
  Pred22=lforecast(M22,D,start=(length(H$tr)+1),Test) # multi-step ahead forecasts
  ev22[b]=mmetric(y=d1[H$ts],x=Pred22,metric="NMAE",val=YR)
  
  # code for rminer package methods, "lm":
  # note: the last training value is the same for dtr, namely:
  # print(dtr[length(dtr)])  
  # print(D[H$tr[length(H$tr)],]) # y is equal to previously shown value  
  M6=fit(y~.,D[H$tr,],model="lm") # create forecasting model
  Pred6=lforecast(M6,D,start=(length(H$tr)+1),Test) # multi-step ahead forecasts
  ev6[b]=mmetric(y=d1[H$ts],x=Pred6,metric="NMAE",val=YR)
  
  # code for rminer package methods, "mlpe":
  # note: the last training value is the same for dtr, namely:
  # print(dtr[length(dtr)])  
  # print(D[H$tr[length(H2$tr)],]) # y is equal to previously shown value  
  M2=fit(y~.,D[H$tr,],model="mlpe") # create forecasting model
  Pred2=lforecast(M2,D,start=(length(H$tr)+1),Test) # multi-step ahead forecasts
  ev2[b]=mmetric(y=d1[H$ts],x=Pred2,metric="NMAE",val=YR)
  
  # code for rminer package methods, "xgboost":
  # note: the last training value is the same for dtr, namely:
  # print(dtr[length(dtr)])  
  # print(D[H$tr[length(H$tr)],]) # y is equal to previously shown value  
  M7=fit(y~.,D[H$tr,],model="xgboost") # create forecasting model
  Pred7=lforecast(M7,D,start=(length(H$tr)+1),Test) # multi-step ahead forecasts
  ev7[b]=mmetric(y=d1[H$ts],x=Pred7,metric="NMAE",val=YR)
  
  # code for rminer package methods, "cubist":
  # note: the last training value is the same for dtr, namely:
  # print(dtr[length(dtr)])  
  # print(D[H$tr[length(H8$tr)],]) # y is equal to previously shown value  
  M8=fit(y~.,D[H$tr,],model="cubist") # create forecasting model
  Pred8=lforecast(M8,D,start=(length(H$tr)+1),Test) # multi-step ahead forecasts
  ev8[b]=mmetric(y=d1[H$ts],x=Pred8,metric="NMAE",val=YR)
  
  # code for rminer package methods, "mars:
  # note: the last training value is the same for dtr, namely:
  # print(dtr[length(dtr)])  
  # print(D[H$tr[length(H$tr)],]) # y is equal to previously shown value  
  M9=fit(y~.,D[H$tr,],model="mars") # create forecasting model
  Pred9=lforecast(M9,D,start=(length(H$tr)+1),Test) # multi-step ahead forecasts
  ev9[b]=mmetric(y=d1[H$ts],x=Pred9,metric="NMAE",val=YR)
  
  cat("iter:",b,"TR from:",trinit,"to:",(trinit+length(H$tr)-1),"size:",length(H$tr),
      "TS from:",H$ts[1],"to:",H$ts[length(H$ts)],"size:",length(H$ts),
      "nmae:",ev2[b],",",ev6[b],",",ev7[b],",",ev8[b],",",ev9[b],",",ev22[b],"\n")
  
  mgraph(d1[H$ts],Pred2,graph="REG",Grid=10,col=c("black","blue", "green", "purple", "red", "brown","grey"),leg=list(pos="topleft",leg=c("target","lm", "mlpe", "xgboost", "cubist", "mars", "naive")))
  lines(Pred6,pch=19,cex=0.5,type="b",col="green")
  lines(Pred7,pch=19,cex=0.5,type="b",col="purple")
  lines(Pred8,pch=19,cex=0.5,type="b",col="red")
  lines(Pred9,pch=19,cex=0.5,type="b",col="brown")
  lines(Pred22,pch=19,cex=0.5,type="b",col="grey")
  
  mpause() # wait for enter
}

# growing window to rminer models part 2:
for(b in 1:Runs)  # cycle of the incremental window training (growing window)
{
  H=holdout(D$y,ratio=Test,mode="incremental",iter=b,window=W2,increment=S)  
  
  # code for rminer package methods, "rvm:
  # note: the last training value is the same for dtr, namely:
  # print(dtr[length(dtr)])  
  # print(D[H$tr[length(H$tr)],]) # y is equal to previously shown value  
  M10=fit(y~.,D[H$tr,],model="rvm") # create forecasting model
  Pred10=lforecast(M10,D,start=(length(H$tr)+1),Test) # multi-step ahead forecasts
  ev10[b]=mmetric(y=d1[H$ts],x=Pred10,metric="NMAE",val=YR)
  
  # code for rminer package methods, "mr:
  # note: the last training value is the same for dtr, namely:
  # print(dtr[length(dtr)])  
  # print(D[H$tr[length(H$tr)],]) # y is equal to previously shown value  
  M11=fit(y~.,D[H$tr,],model="mr") # create forecasting model
  Pred11=lforecast(M11,D,start=(length(H$tr)+1),Test) # multi-step ahead forecasts
  ev11[b]=mmetric(y=d1[H$ts],x=Pred11,metric="NMAE",val=YR)
  
  # code for rminer package methods, "pcr:
  # note: the last training value is the same for dtr, namely:
  # print(dtr[length(dtr)])  
  # print(D[H$tr[length(H$tr)],]) # y is equal to previously shown value  
  M12=fit(y~.,D[H$tr,],model="pcr") # create forecasting model
  Pred12=lforecast(M12,D,start=(length(H$tr)+1),Test) # multi-step ahead forecasts
  ev12[b]=mmetric(y=d1[H$ts],x=Pred12,metric="NMAE",val=YR)
  
  # code for rminer package methods, "plsr:
  # note: the last training value is the same for dtr, namely:
  # print(dtr[length(dtr)])  
  # print(D[H$tr[length(H$tr)],]) # y is equal to previously shown value  
  M13=fit(y~.,D[H$tr,],model="plsr") # create forecasting model
  Pred13=lforecast(M13,D,start=(length(H$tr)+1),Test) # multi-step ahead forecasts
  ev13[b]=mmetric(y=d1[H$ts],x=Pred13,metric="NMAE",val=YR)
  
  # code for rminer package methods, "cppls:
  # note: the last training value is the same for dtr, namely:
  # print(dtr[length(dtr)])  
  # print(D[H$tr[length(H$tr)],]) # y is equal to previously shown value  
  M14=fit(y~.,D[H$tr,],model="cppls") # create forecasting model
  Pred14=lforecast(M14,D,start=(length(H$tr)+1),Test) # multi-step ahead forecasts
  ev14[b]=mmetric(y=d1[H$ts],x=Pred14,metric="NMAE",val=YR)
  
  # code for rminer package methods, "ksvm:
  # note: the last training value is the same for dtr, namely:
  # print(dtr[length(dtr)])  
  # print(D[H$tr[length(H$tr)],]) # y is equal to previously shown value  
  M15=fit(y~.,D[H$tr,],model="ksvm") # create forecasting model
  Pred15=lforecast(M15,D,start=(length(H$tr)+1),Test) # multi-step ahead forecasts
  ev15[b]=mmetric(y=d1[H$ts],x=Pred15,metric="NMAE",val=YR)
  
  cat("iter:",b,"TR from:",trinit,"to:",(trinit+length(H$tr)-1),"size:",length(H$tr),
      "TS from:",H$ts[1],"to:",H$ts[length(H$ts)],"size:",length(H$ts),
      "nmae:",ev10[b],",",ev11[b],",",ev12[b],",",ev13[b],",",ev14[b],",",ev15[b],"\n")
  
  mgraph(d1[H$ts],Pred10,graph="REG",Grid=10,col=c("black","blue", "purple", "red", "brown", "grey","pink"),leg=list(pos="topleft",leg=c("target","rvm", "mr", "pcr", "plsr", "cppls", "ksvm")))
  lines(Pred11,pch=19,cex=0.5,type="b",col="purple")
  lines(Pred12,pch=19,cex=0.5,type="b",col="red")
  lines(Pred13,pch=19,cex=0.5,type="b",col="brown")
  lines(Pred14,pch=19,cex=0.5,type="b",col="grey")
  lines(Pred15,pch=19,cex=0.5,type="b",col="pink")
  mpause() # wait for enter
}

# growing window to rminer models part 3:
for(b in 1:Runs)  # cycle of the incremental window training (growing window)
{
  H=holdout(D$y,ratio=Test,mode="incremental",iter=b,window=W2,increment=S)  
  
  # code for rminer package methods, "mlp:
  # note: the last training value is the same for dtr, namely:
  # print(dtr[length(dtr)])  
  # print(D[H$tr[length(H$tr)],]) # y is equal to previously shown value  
  M16=fit(y~.,D[H$tr,],model="mlp") # create forecasting model
  Pred16=lforecast(M16,D,start=(length(H$tr)+1),Test) # multi-step ahead forecasts
  ev16[b]=mmetric(y=d1[H$ts],x=Pred16,metric="NMAE",val=YR)
  
  # code for rminer package methods, "ctree:
  # note: the last training value is the same for dtr, namely:
  # print(dtr[length(dtr)])  
  # print(D[H$tr[length(H$tr)],]) # y is equal to previously shown value  
  M17=fit(y~.,D[H$tr,],model="ctree") # create forecasting model
  Pred17=lforecast(M17,D,start=(length(H$tr)+1),Test) # multi-step ahead forecasts
  ev17[b]=mmetric(y=d1[H$ts],x=Pred17,metric="NMAE",val=YR)
  
  # code for rminer package methods, "dt:
  # note: the last training value is the same for dtr, namely:
  # print(dtr[length(dtr)])  
  # print(D[H$tr[length(H$tr)],]) # y is equal to previously shown value  
  M18=fit(y~.,D[H$tr,],model="dt") # create forecasting model
  Pred18=lforecast(M18,D,start=(length(H$tr)+1),Test) # multi-step ahead forecasts
  ev18[b]=mmetric(y=d1[H$ts],x=Pred18,metric="NMAE",val=YR)
  
  # code for rminer package methods, "knn":
  # note: the last training value is the same for dtr, namely:
  # print(dtr[length(dtr)])  
  # print(D[H$tr[length(H$tr)],]) # y is equal to previously shown value  
  M19=fit(y~.,D[H$tr,],model="knn") # create forecasting model
  Pred19=lforecast(M19,D,start=(length(H$tr)+1),Test) # multi-step ahead forecasts
  ev19[b]=mmetric(y=d1[H$ts],x=Pred19,metric="NMAE",val=YR)
  
  # code for rminer package methods, "cv.glmnet":
  # note: the last training value is the same for dtr, namely:
  # print(dtr[length(dtr)])  
  # print(D[H$tr[length(H$tr)],]) # y is equal to previously shown value  
  M20=fit(y~.,D[H$tr,],model="cv.glmnet") # create forecasting model
  Pred20=lforecast(M20,D,start=(length(H$tr)+1),Test) # multi-step ahead forecasts
  ev20[b]=mmetric(y=d1[H$ts],x=Pred20,metric="NMAE",val=YR)
  
  # code for rminer package methods, "randomForest":
  # note: the last training value is the same for dtr, namely:
  # print(dtr[length(dtr)])  
  # print(D[H$tr[length(H$tr)],]) # y is equal to previously shown value  
  M21=fit(y~.,D[H$tr,],model="randomForest") # create forecasting model
  Pred21=lforecast(M21,D,start=(length(H$tr)+1),Test) # multi-step ahead forecasts
  ev21[b]=mmetric(y=d1[H$ts],x=Pred21,metric="NMAE",val=YR)
  
  cat("iter:",b,"TR from:",trinit,"to:",(trinit+length(H$tr)-1),"size:",length(H$tr),
      "TS from:",H$ts[1],"to:",H$ts[length(H$ts)],"size:",length(H$ts),
      "nmae:",ev16[b],",",ev17[b],",",ev18[b],",",ev19[b],",",ev20[b],",",ev21[b],"\n")
  
  mgraph(d1[H$ts],Pred16,graph="REG",Grid=10,col=c("black","blue", "purple", "red", "brown", "grey", "pink"),leg=list(pos="topleft",leg=c("target", "mlp", "ctree", "dt", "knn", "cv.glmnet", "randomForest")))
  lines(Pred17,pch=19,cex=0.5,type="b",col="purple")
  lines(Pred18,pch=19,cex=0.5,type="b",col="red")
  lines(Pred19,pch=19,cex=0.5,type="b",col="brown")
  lines(Pred20,pch=19,cex=0.5,type="b",col="grey")
  lines(Pred21,pch=19,cex=0.5,type="b",col="pink")
  
  mpause() # wait for enter
}

# show median
cat("median NMAE values:\n")
cat("lm median NMAE:",median(ev6),"\n")
cat("mlpe median NMAE:",median(ev2),"\n")
cat("xgboost median NMAE:",median(ev7),"\n")
cat("cubist median NMAE:",median(ev8),"\n")
cat("mars median NMAE:",median(ev9),"\n")
cat("rvm median NMAE:",median(ev10),"\n")
cat("mr median NMAE:",median(ev11),"\n")
cat("pcr median NMAE:",median(ev12),"\n")
cat("plsr median NMAE:",median(ev13),"\n")
cat("cppls median NMAE:",median(ev14),"\n")
cat("ksvm median NMAE:",median(ev15),"\n")
cat("mlp median NMAE:",median(ev16),"\n")
cat("ctree median NMAE:",median(ev17),"\n")
cat("dt median NMAE:",median(ev18),"\n")
cat("knn median NMAE:",median(ev19),"\n")
cat("cv.glmnet median NMAE:",median(ev20),"\n")
cat("randomForest median NMAE:",median(ev21),"\n")
cat("naive median NMAE:",median(ev22),"\n")
mpause() # wait for enter

# rolling window to forecast models:
for(b in 1:Runs)  # cycle of the incremental window training (rolling window)
{
  H=holdout(d1,ratio=Test,mode="rolling",iter=b,window=W,increment=S)   
  trinit=H$tr[1]
  dtr=ts(d1[H$tr],frequency=K) # create ts object, note that there is no start argument (for simplicity of the code)
  
  # code for the forecast package methods, HoltWinters:
  M=suppressWarnings(HoltWinters(dtr)) # create forecasting model, suppressWarnings removes warnings from HW method
  PredR=forecast(M,h=length(H$ts))$mean[1:Test] # multi-step ahead forecasts
  ev[b]=mmetric(y=d1[H$ts],x=PredR,metric="NMAE",val=YR)
  
  # code for the forecast package methods, Arima:
  M3=suppressWarnings(auto.arima(dtr)) # create forecasting model, suppressWarnings removes warnings from arima method
  PredR3=forecast(M3,h=length(H$ts))$mean[1:Test] # multi-step ahead forecasts
  ev3[b]=mmetric(y=d1[H$ts],x=PredR3,metric="NMAE",val=YR)
  
  # code for the forecast package methods, NN:
  M4=suppressWarnings(nnetar(dtr,P=1,repeats=3)) # create forecasting model, suppressWarnings removes warnings from NN method
  PredR4=forecast(M4,h=length(H$ts))$mean[1:Test] # multi-step ahead forecasts
  ev4[b]=mmetric(y=d1[H$ts],x=PredR4,metric="NMAE",val=YR)
  
  # code for the forecast package methods, ets:
  M5=suppressWarnings(ets(dtr)) # create forecasting model, suppressWarnings removes warnings from ets method
  PredR5=forecast(M5,h=length(H$ts))$mean[1:Test] # multi-step ahead forecasts
  ev5[b]=mmetric(y=d1[H$ts],x=PredR5,metric="NMAE",val=YR)
  
  cat("iter:",b,"TR from:",trinit,"to:",(trinit+length(H$tr)-1),"size:",length(H$tr),
      "TS from:",H$ts[1],"to:",H$ts[length(H$ts)],"size:",length(H$ts),
      "nmae:",ev[b],",",ev3[b],",",ev4[b],",",ev5[b],"\n")
} # end of cycle

# show median
cat("median NMAE values:\n")
cat("Holt-Winters median NMAE:",median(ev),"\n")
cat("Arima median NMAE:",median(ev3),"\n")
cat("NN median NMAE:",median(ev4),"\n")
cat("ets median NMAE:",median(ev5),"\n")

# last iteration predictions:
mgraph(d1[H$ts],PredR,graph="REG",Grid=10,col=c("black","blue", "green", "purple", "brown"),leg=list(pos="topleft",leg=c("target","HW pred.", "arima", "NN", "ets")))
lines(PredR3,pch=19,cex=0.5,type="b",col="green")
lines(PredR4,pch=19,cex=0.5,type="b",col="purple")
lines(PredR5,pch=19,cex=0.5,type="b",col="brown")

# rolling window to rminer models:
for(b in 1:Runs)  # cycle of the incremental window training (rolling window)
{
  H=holdout(D$y,ratio=Test,mode="incremental",iter=b,window=W2,increment=S)   
  
  # code for rminer package methods, "naive":
  # note: the last training value is the same for dtr, namely:
  # print(dtr[length(dtr)])  
  # print(D[H$tr[length(H$tr)],]) # y is equal to previously shown value  
  M22=fit(y~.,D[H$tr,],model="naive") # create forecasting model
  PredR22=lforecast(M22,D,start=(length(H$tr)+1),Test) # multi-step ahead forecasts
  ev22[b]=mmetric(y=d1[H$ts],x=PredR22,metric="NMAE",val=YR)
  
  # code for rminer package methods, "lm":
  # note: the last training value is the same for dtr, namely:
  # print(dtr[length(dtr)])  
  # print(D[H$tr[length(H$tr)],]) # y is equal to previously shown value  
  M6=fit(y~.,D[H$tr,],model="lm") # create forecasting model
  PredR6=lforecast(M6,D,start=(length(H$tr)+1),Test) # multi-step ahead forecasts
  ev6[b]=mmetric(y=d1[H$ts],x=PredR6,metric="NMAE",val=YR)
  
  # code for rminer package methods, "mlpe":
  # note: the last training value is the same for dtr, namely:
  # print(dtr[length(dtr)])  
  # print(D[H$tr[length(H2$tr)],]) # y is equal to previously shown value  
  M2=fit(y~.,D[H$tr,],model="mlpe") # create forecasting model
  PredR2=lforecast(M2,D,start=(length(H$tr)+1),Test) # multi-step ahead forecasts
  ev2[b]=mmetric(y=d1[H$ts],x=PredR2,metric="NMAE",val=YR)
  
  # code for rminer package methods, "xgboost":
  # note: the last training value is the same for dtr, namely:
  # print(dtr[length(dtr)])  
  # print(D[H$tr[length(H$tr)],]) # y is equal to previously shown value  
  M7=fit(y~.,D[H$tr,],model="xgboost") # create forecasting model
  PredR7=lforecast(M7,D,start=(length(H$tr)+1),Test) # multi-step ahead forecasts
  ev7[b]=mmetric(y=d1[H$ts],x=PredR7,metric="NMAE",val=YR)
  
  # code for rminer package methods, "cubist":
  # note: the last training value is the same for dtr, namely:
  # print(dtr[length(dtr)])  
  # print(D[H$tr[length(H8$tr)],]) # y is equal to previously shown value  
  M8=fit(y~.,D[H$tr,],model="cubist") # create forecasting model
  PredR8=lforecast(M8,D,start=(length(H$tr)+1),Test) # multi-step ahead forecasts
  ev8[b]=mmetric(y=d1[H$ts],x=PredR8,metric="NMAE",val=YR)
  
  # code for rminer package methods, "mars:
  # note: the last training value is the same for dtr, namely:
  # print(dtr[length(dtr)])  
  # print(D[H$tr[length(H$tr)],]) # y is equal to previously shown value  
  M9=fit(y~.,D[H$tr,],model="mars") # create forecasting model
  PredR9=lforecast(M9,D,start=(length(H$tr)+1),Test) # multi-step ahead forecasts
  ev9[b]=mmetric(y=d1[H$ts],x=PredR9,metric="NMAE",val=YR)
  
  # code for rminer package methods, "rvm:
  # note: the last training value is the same for dtr, namely:
  # print(dtr[length(dtr)])  
  # print(D[H$tr[length(H$tr)],]) # y is equal to previously shown value  
  M10=fit(y~.,D[H$tr,],model="rvm") # create forecasting model
  PredR10=lforecast(M10,D,start=(length(H$tr)+1),Test) # multi-step ahead forecasts
  ev10[b]=mmetric(y=d1[H$ts],x=PredR10,metric="NMAE",val=YR)
  
  # code for rminer package methods, "mr:
  # note: the last training value is the same for dtr, namely:
  # print(dtr[length(dtr)])  
  # print(D[H$tr[length(H$tr)],]) # y is equal to previously shown value  
  M11=fit(y~.,D[H$tr,],model="mr") # create forecasting model
  PredR11=lforecast(M11,D,start=(length(H$tr)+1),Test) # multi-step ahead forecasts
  ev11[b]=mmetric(y=d1[H$ts],x=PredR11,metric="NMAE",val=YR)
  
  # code for rminer package methods, "pcr:
  # note: the last training value is the same for dtr, namely:
  # print(dtr[length(dtr)])  
  # print(D[H$tr[length(H$tr)],]) # y is equal to previously shown value  
  M12=fit(y~.,D[H$tr,],model="pcr") # create forecasting model
  PredR12=lforecast(M12,D,start=(length(H$tr)+1),Test) # multi-step ahead forecasts
  ev12[b]=mmetric(y=d1[H$ts],x=PredR12,metric="NMAE",val=YR)
  
  # code for rminer package methods, "plsr:
  # note: the last training value is the same for dtr, namely:
  # print(dtr[length(dtr)])  
  # print(D[H$tr[length(H$tr)],]) # y is equal to previously shown value  
  M13=fit(y~.,D[H$tr,],model="plsr") # create forecasting model
  PredR13=lforecast(M13,D,start=(length(H$tr)+1),Test) # multi-step ahead forecasts
  ev13[b]=mmetric(y=d1[H$ts],x=PredR13,metric="NMAE",val=YR)
  
  # code for rminer package methods, "cppls:
  # note: the last training value is the same for dtr, namely:
  # print(dtr[length(dtr)])  
  # print(D[H$tr[length(H$tr)],]) # y is equal to previously shown value  
  M14=fit(y~.,D[H$tr,],model="cppls") # create forecasting model
  PredR14=lforecast(M14,D,start=(length(H$tr)+1),Test) # multi-step ahead forecasts
  ev14[b]=mmetric(y=d1[H$ts],x=PredR14,metric="NMAE",val=YR)
  
  # code for rminer package methods, "ksvm:
  # note: the last training value is the same for dtr, namely:
  # print(dtr[length(dtr)])  
  # print(D[H$tr[length(H$tr)],]) # y is equal to previously shown value  
  M15=fit(y~.,D[H$tr,],model="ksvm") # create forecasting model
  PredR15=lforecast(M15,D,start=(length(H$tr)+1),Test) # multi-step ahead forecasts
  ev15[b]=mmetric(y=d1[H$ts],x=PredR15,metric="NMAE",val=YR)

  # code for rminer package methods, "mlp:
  # note: the last training value is the same for dtr, namely:
  # print(dtr[length(dtr)])  
  # print(D[H$tr[length(H$tr)],]) # y is equal to previously shown value  
  M16=fit(y~.,D[H$tr,],model="mlp") # create forecasting model
  PredR16=lforecast(M16,D,start=(length(H$tr)+1),Test) # multi-step ahead forecasts
  ev16[b]=mmetric(y=d1[H$ts],x=PredR16,metric="NMAE",val=YR)
  
  # code for rminer package methods, "ctree:
  # note: the last training value is the same for dtr, namely:
  # print(dtr[length(dtr)])  
  # print(D[H$tr[length(H$tr)],]) # y is equal to previously shown value  
  M17=fit(y~.,D[H$tr,],model="ctree") # create forecasting model
  PredR17=lforecast(M17,D,start=(length(H$tr)+1),Test) # multi-step ahead forecasts
  ev17[b]=mmetric(y=d1[H$ts],x=PredR17,metric="NMAE",val=YR)
  
  # code for rminer package methods, "dt:
  # note: the last training value is the same for dtr, namely:
  # print(dtr[length(dtr)])  
  # print(D[H$tr[length(H$tr)],]) # y is equal to previously shown value  
  M18=fit(y~.,D[H$tr,],model="dt") # create forecasting model
  PredR18=lforecast(M18,D,start=(length(H$tr)+1),Test) # multi-step ahead forecasts
  ev18[b]=mmetric(y=d1[H$ts],x=PredR18,metric="NMAE",val=YR)
  
  # code for rminer package methods, "knn":
  # note: the last training value is the same for dtr, namely:
  # print(dtr[length(dtr)])  
  # print(D[H$tr[length(H$tr)],]) # y is equal to previously shown value  
  M19=fit(y~.,D[H$tr,],model="knn") # create forecasting model
  PredR19=lforecast(M19,D,start=(length(H$tr)+1),Test) # multi-step ahead forecasts
  ev19[b]=mmetric(y=d1[H$ts],x=PredR19,metric="NMAE",val=YR)
  
  # code for rminer package methods, "cv.glmnet":
  # note: the last training value is the same for dtr, namely:
  # print(dtr[length(dtr)])  
  # print(D[H$tr[length(H$tr)],]) # y is equal to previously shown value  
  M20=fit(y~.,D[H$tr,],model="cv.glmnet") # create forecasting model
  PredR20=lforecast(M20,D,start=(length(H$tr)+1),Test) # multi-step ahead forecasts
  ev20[b]=mmetric(y=d1[H$ts],x=PredR20,metric="NMAE",val=YR)
  
  # code for rminer package methods, "randomForest":
  # note: the last training value is the same for dtr, namely:
  # print(dtr[length(dtr)])  
  # print(D[H$tr[length(H$tr)],]) # y is equal to previously shown value  
  M21=fit(y~.,D[H$tr,],model="randomForest") # create forecasting model
  PredR21=lforecast(M21,D,start=(length(H$tr)+1),Test) # multi-step ahead forecasts
  ev21[b]=mmetric(y=d1[H$ts],x=PredR21,metric="NMAE",val=YR)
  
  cat("iter:",b,"TR from:",trinit,"to:",(trinit+length(H$tr)-1),"size:",length(H$tr),
      "TS from:",H$ts[1],"to:",H$ts[length(H$ts)],"size:",length(H$ts),
      "nmae:",ev2[b],",",ev6[b],",",ev7[b],",",ev8[b],",",ev9[b],ev10[b],",",ev11[b],",",ev12[b],",",ev13[b],",",ev14[b],",",ev15[b],ev16[b],",",ev17[b],",",ev18[b],",",ev19[b],",",ev20[b],",",ev21[b],"\n")

}

# show median
cat("lm median NMAE:",median(ev6),"\n")
cat("mlpe median NMAE:",median(ev2),"\n")
cat("xgboost median NMAE:",median(ev7),"\n")
cat("cubist median NMAE:",median(ev8),"\n")
cat("mars median NMAE:",median(ev9),"\n")
cat("rvm median NMAE:",median(ev10),"\n")
cat("mr median NMAE:",median(ev11),"\n")
cat("pcr median NMAE:",median(ev12),"\n")
cat("plsr median NMAE:",median(ev13),"\n")
cat("cppls median NMAE:",median(ev14),"\n")
cat("ksvm median NMAE:",median(ev15),"\n")
cat("mlp median NMAE:",median(ev16),"\n")
cat("ctree median NMAE:",median(ev17),"\n")
cat("dt median NMAE:",median(ev18),"\n")
cat("knn median NMAE:",median(ev19),"\n")
cat("cv.glmnet median NMAE:",median(ev20),"\n")
cat("randomForest median NMAE:",median(ev21),"\n")
cat("naive median NMAE:",median(ev22),"\n")

# last iteration predictions part 1:
mgraph(d1[H$ts],PredR2,graph="REG",Grid=10,col=c("black","blue", "green", "purple", "red", "brown", "orange","grey"),leg=list(pos="topleft",leg=c("target","lm", "mlpe", "xgboost", "cubist", "mars", "naive")))
lines(PredR6,pch=19,cex=0.5,type="b",col="green")
lines(PredR7,pch=19,cex=0.5,type="b",col="purple")
lines(PredR8,pch=19,cex=0.5,type="b",col="red")
lines(PredR9,pch=19,cex=0.5,type="b",col="brown")
lines(PredR22,pch=19,cex=0.5,type="b",col="grey")

# last iteration predictions part 2:
mgraph(d1[H$ts],PredR10,graph="REG",Grid=10,col=c("black","blue", "green", "purple", "red", "brown", "orange"),leg=list(pos="topleft",leg=c("target","rvm", "mr", "pcr", "plsr", "cppls", "ksvm")))
lines(PredR11,pch=19,cex=0.5,type="b",col="green")
lines(PredR12,pch=19,cex=0.5,type="b",col="purple")
lines(PredR13,pch=19,cex=0.5,type="b",col="red")
lines(PredR14,pch=19,cex=0.5,type="b",col="brown")
lines(PredR15,pch=19,cex=0.5,type="b",col="orange")

# last iteration predictions part 3:
mgraph(d1[H$ts],PredR16,graph="REG",Grid=10,col=c("black","blue", "green", "purple", "red", "brown", "orange"),leg=list(pos="topleft",leg=c("target","mlp", "ctree", "dt", "knn", "cv.glmnet", "randomForest")))
lines(PredR17,pch=19,cex=0.5,type="b",col="green")
lines(PredR18,pch=19,cex=0.5,type="b",col="purple")
lines(PredR19,pch=19,cex=0.5,type="b",col="red")
lines(PredR20,pch=19,cex=0.5,type="b",col="brown")
lines(PredR21,pch=19,cex=0.5,type="b",col="orange")