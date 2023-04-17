# Adapted from 3-passengers.R
# Univariate_1-step_ahead.R time series forecasting example that considers multi-step ahead forecasts.

library(openxlsx)
library(rminer)

cat("read beer time series:")
TS=read.xlsx(xlsxFile="/home/paulo/Desktop/TIAPOSE/bebidas.xlsx",sheet=1,skipEmptyRows=FALSE,colNames=TRUE,detectDates=TRUE)
#TS <- head(TS, - 14) # Remove last 14 lines - too many zeroes

#S = TS$STELLA
TS = TS$BUD
summary(TS)
K=7 # TS period (weekly!)
print("show graph")
tsdisplay(TS)
mpause()

L=length(TS)
NTS=K # number of predictions
H=NTS # from 1 to H ahead predictions

# --- this portion of code uses forecast library, which assumes several functions, such as forecast(), and uses a ts object 
# --- note: the forecast library works differently than rminer
# time series weekly object, frequency=K 
# this time series object only includes TRAIN (older) data:
LTR=L-H
# start means: year of 1949, 1st month (since frequency=K=12).
# according to the ts function documentation: frequency=7 assumes daily data, frequency=4 or 12 assumes quarterly and monthly data
TR=ts(TS[1:LTR],frequency=K) # start means: year of 2019, 1st month (since frequency=K=7). RETIREI ",start=c(2019,1)" Perguntar ao professor
# show the in-sample (training data) time series:
plot(TR)
print(TR)
mpause() # press enter

# target predictions:
Y=TS[(LTR+1):L]

# ----------//----------//----------//----------//----------

# holt winters forecasting method:
print("model> HoltWinters")
HW=HoltWinters(TR)
print(HW)
plot(HW)
print("show holt winters forecasts:")
# forecasts, from 1 to H ahead:
F=forecast(HW,h=H)
print(F)
Pred=F$mean[1:H] # HolWinters format

cat("MAE:",mmetric(Y,Pred,metric="MAE"),"\n")
cat("NMAE:",mmetric(Y,Pred,metric="NMAE",val=srange),"\n")
cat("RMSE:",mmetric(Y,Pred,metric="RMSE"),"\n")
cat("RRSE:",mmetric(Y,Pred,metric="RRSE"),"\n")

mgraph(Y,Pred,graph="REG",Grid=10,col=c("black","blue"),leg=list(pos="topleft",leg=c("target","HW pred.")))
mpause() # press enter

# ----------//----------//----------//----------//----------

# arima modeling:
print("model> auto.arima")
AR=auto.arima(TR)
print(AR) # ARIMA(3,0,1)(2,1,0)[12] 
print("show ARIMA forecasts:")
# forecasts, from 1 to H ahead:
F1=forecast(AR,h=H)
print(F1)
Pred1=F1$mean[1:H]

cat("MAE:",mmetric(Y,Pred1,metric="MAE"),"\n")
cat("NMAE:",mmetric(Y,Pred1,metric="NMAE",val=srange),"\n")
cat("RMSE:",mmetric(Y,Pred1,metric="RMSE"),"\n")
cat("RRSE:",mmetric(Y,Pred1,metric="RRSE"),"\n")

mgraph(Y,Pred1,graph="REG",Grid=10,col=c("black","blue"),leg=list(pos="topleft",leg=c("target"," ARIMA pred.")))
mpause() # press enter

# ----------//----------//----------//----------//----------

# NN from forecast:
print("model> nnetar")
NN1=nnetar(TR,P=1,repeats=3)
print(NN1)
F3=forecast(NN1,h=H)
Pred3=F3$mean[1:H] # HolWinters format

cat("MAE:",mmetric(Y,Pred3,metric="MAE"),"\n")
cat("NMAE:",mmetric(Y,Pred3,metric="NMAE",val=srange),"\n")
cat("RMSE:",mmetric(Y,Pred3,metric="RMSE"),"\n")
cat("RRSE:",mmetric(Y,Pred3,metric="RRSE"),"\n")

mgraph(Y,Pred3,graph="REG",Grid=10,col=c("black","blue"),leg=list(pos="topleft",leg=c("target","NN1 pred.")))
mpause() # press enter

# ----------//----------//----------//----------//----------

# ets from forecast:
print("model> ets")
ETS=ets(TR)
F4=forecast(ETS,h=H)
Pred4=F4$mean[1:H] # HolWinters format

cat("MAE:",mmetric(Y,Pred4,metric="MAE"),"\n")
cat("NMAE:",mmetric(Y,Pred4,metric="NMAE",val=srange),"\n")
cat("RMSE:",mmetric(Y,Pred4,metric="RMSE"),"\n")
cat("RRSE:",mmetric(Y,Pred4,metric="RRSE"),"\n")

mgraph(Y,Pred4,graph="REG",Grid=10,col=c("black","blue"),leg=list(pos="topleft",leg=c("target","ets pred.")))
mpause() # press enter

# ----------//----------//----------//----------//----------

# -- end of forecast library methods

# neural network modeling, via rminer:

# ----------//----------//----------//----------//----------

d=CasesSeries(TS,c(1:7)) # data.frame from time series (domain knowledge for the 1,2,3,4,5,6,7 time lag selection)
print(summary(d))
LD=nrow(d) # note: LD < L
hd=holdout(d$y,ratio=NTS,mode="order")

# ----------//----------//----------//----------//----------
# linear regression modeling ("lm"), via rminer:
print("model> mlpe (with t-1,t-2,t-3,t-4,t-5,t-6,t-7 -> t lags)")
NN2=fit(y~.,d[hd$tr,],model="mlpe")
# multi-step, from 1 to H ahead forecasts:
init=hd$ts[1] # or same as: init=LD-H+1
# for multi-step ahead prediction, the lforecast from rminer should be used instead of predict,
# since predict only performs 1-ahead predictions
F5=lforecast(NN2,d,start=hd$ts[1],horizon=H)
print(F5)
Pred5=F5

cat("MAE:",mmetric(Y,Pred5,metric="MAE"),"\n")
cat("NMAE:",mmetric(Y,Pred5,metric="NMAE",val=srange),"\n")
cat("RMSE:",mmetric(Y,Pred5,metric="RMSE"),"\n")
cat("RRSE:",mmetric(Y,Pred5,metric="RRSE"),"\n")

mgraph(Y,Pred5,graph="REG",Grid=10,col=c("black","blue"),leg=list(pos="topleft",leg=c("target","NN2 pred.")))
mpause() # press enter

# ----------//----------//----------//----------//----------

# ----------//----------//----------//----------//----------

# linear regression modeling ("lm"), via rminer:
print("model> lm (with t-1,t-2,t-3,t-4,t-5,t-6,t-7 -> t lags)")
LM=fit(y~.,d[hd$tr,],model="lm")
# multi-step, from 1 to H ahead forecasts:
init=hd$ts[1] # or same as: init=LD-H+1
# for multi-step ahead prediction, the lforecast from rminer should be used instead of predict,
# since predict only performs 1-ahead predictions
F6=lforecast(LM,d,start=hd$ts[1],horizon=H)
print(F6)
Pred6=F6

cat("MAE:",mmetric(Y,Pred6,metric="MAE"),"\n")
cat("NMAE:",mmetric(Y,Pred6,metric="NMAE",val=srange),"\n")
cat("RMSE:",mmetric(Y,Pred6,metric="RMSE"),"\n")
cat("RRSE:",mmetric(Y,Pred6,metric="RRSE"),"\n")

mgraph(Y,Pred6,graph="REG",Grid=10,col=c("black","blue"),leg=list(pos="topleft",leg=c("target","LM pred.")))
mpause() # press enter

# ----------//----------//----------//----------//----------

# ----------//----------//----------//----------//----------

# linear regression modeling ("xgboost"), via rminer:
print("model> xgboost (with t-1,t-2,t-3,t-4,t-5,t-6,t-7 -> t lags)")
XGBOOST=fit(y~.,d[hd$tr,],model="xgboost")
# multi-step, from 1 to H ahead forecasts:
init=hd$ts[1] # or same as: init=LD-H+1
# for multi-step ahead prediction, the lforecast from rminer should be used instead of predict,
# since predict only performs 1-ahead predictions
F7=lforecast(XGBOOST,d,start=hd$ts[1],horizon=H)
print(F7)
Pred7=F7

# show forecasting measures and graph:
cat("XGBOOST predictions:\n")
print(Pred7)
cat("MAE:",mmetric(Y,Pred7,metric="MAE"),"\n")
cat("NMAE:",mmetric(Y,Pred7,metric="NMAE",val=srange),"\n")
cat("RMSE:",mmetric(Y,Pred7,metric="RMSE"),"\n")
cat("RRSE:",mmetric(Y,Pred7,metric="RRSE"),"\n")

# graph: REG - simple Regression Plot
print("Graph with XGBOOST predictions (multi-ahead):")
mgraph(Y,Pred7,graph="REG",Grid=10,lty=1,col=c("black","blue"),main="Xgboost predictions",leg=list(pos="topright",leg=c("target","predictions")))

# ----------//----------//----------//----------//----------

# ----------//----------//----------//----------//----------

# linear regression modeling ("cubist"), via rminer:
print("model> cubist (with t-1,t-2,t-3,t-4,t-5,t-6,t-7 -> t lags)")
CUBIST=fit(y~.,d[hd$tr,],model="cubist")

# multi-step, from 1 to H ahead forecasts:
init=hd$ts[1] # or same as: init=LD-H+1
# for multi-step ahead prediction, the lforecast from rminer should be used instead of predict,
# since predict only performs 1-ahead predictions
F8=lforecast(CUBIST,d,start=hd$ts[1],horizon=H)
print(F8)
Pred8=F8

# show forecasting measures and graph:
cat("CUBIST predictions:\n")
print(PCUBIST)
cat("MAE:",mmetric(Y,Pred8,metric="MAE"),"\n")
cat("NMAE:",mmetric(Y,Pred8,metric="NMAE",val=srange),"\n")
cat("RMSE:",mmetric(Y,Pred8,metric="RMSE"),"\n")
cat("RRSE:",mmetric(Y,Pred8,metric="RRSE"),"\n")

# graph: REG - simple Regression Plot
print("Graph with CUBIST predictions (multi-ahead):")
mgraph(Y,Pred8,graph="REG",Grid=10,lty=1,col=c("black","blue"),main="Cubist predictions",leg=list(pos="topright",leg=c("target","predictions")))
# ----------//----------//----------//----------//----------

# ----------//----------//----------//----------//----------
# linear regression modeling ("mr"), via rminer:
print("model> mr (with t-1,t-2,t-3,t-4,t-5,t-6,t-7 -> t lags)")
MR=fit(y~.,d[hd$tr,],model="mr")

# multi-step, from 1 to H ahead forecasts:
init=hd$ts[1] # or same as: init=LD-H+1
# for multi-step ahead prediction, the lforecast from rminer should be used instead of predict,
# since predict only performs 1-ahead predictions
F9=lforecast(MR,d,start=hd$ts[1],horizon=H)
print(F9)
Pred9=F9

# show forecasting measures and graph:
cat("MR predictions:\n")
print(Pred9)
cat("MAE:",mmetric(Y,Pred9,metric="MAE"),"\n")
cat("NMAE:",mmetric(Y,Pred9,metric="NMAE",val=srange),"\n")
cat("RMSE:",mmetric(Y,Pred9,metric="RMSE"),"\n")
cat("RRSE:",mmetric(Y,Pred9,metric="RRSE"),"\n")

# graph: REG - simple Regression Plot
print("Graph with MR predictions (1-ahead):")
mgraph(Y,Pred9,graph="REG",Grid=10,lty=1,col=c("black","blue"),main="MR predictions",leg=list(pos="topright",leg=c("target","predictions")))
# ----------//----------//----------//----------//----------

# ----------//----------//----------//----------//----------
# fit a (MARS) with training data: 
MARS=fit(y~.,d[hd$tr,],model="mars")

# multi-step, from 1 to H ahead forecasts:
init=hd$ts[1] # or same as: init=LD-H+1
# for multi-step ahead prediction, the lforecast from rminer should be used instead of predict,
# since predict only performs 1-ahead predictions
F10=lforecast(MARS,d,start=hd$ts[1],horizon=H)
print(F10)
Pred10=F10

# show forecasting measures and graph:
cat("MARS predictions:\n")
print(Pred10)
cat("MAE:",mmetric(Y,Pred10,metric="MAE"),"\n")
cat("NMAE:",mmetric(Y,Pred10,metric="NMAE",val=srange),"\n")
cat("RMSE:",mmetric(Y,Pred10,metric="RMSE"),"\n")
cat("RRSE:",mmetric(Y,Pred10,metric="RRSE"),"\n")

# graph: MARS - simple Regression Plot
print("Graph with MR predictions (1-ahead):")
mgraph(Y,Pred10,graph="REG",Grid=10,lty=1,col=c("black","blue"),main="MARS predictions",leg=list(pos="topright",leg=c("target","predictions")))
# ----------//----------//----------//----------//----------

# ----------//----------//----------//----------//----------
# fit a  (CPPLS) with training data: 
CPPLS=fit(y~.,d[hd$tr,],model="cppls")

# multi-step, from 1 to H ahead forecasts:
init=hd$ts[1] # or same as: init=LD-H+1
# for multi-step ahead prediction, the lforecast from rminer should be used instead of predict,
# since predict only performs 1-ahead predictions
F11=lforecast(CPPLS,d,start=hd$ts[1],horizon=H)
print(F11)
Pred11=F11

# show forecasting measures and graph:
cat("CPPLS predictions:\n")
print(Pred11)
cat("MAE:",mmetric(Y,Pred11,metric="MAE"),"\n")
cat("NMAE:",mmetric(Y,Pred11,metric="NMAE",val=srange),"\n")
cat("RMSE:",mmetric(Y,Pred11,metric="RMSE"),"\n")
cat("RRSE:",mmetric(Y,Pred11,metric="RRSE"),"\n")

# graph: CPPLS - simple Regression Plot
print("Graph with CPPLS predictions (multi-ahead):")
mgraph(Y,Pred11,graph="REG",Grid=10,lty=1,col=c("black","blue"),main="CPPLS predictions",leg=list(pos="topright",leg=c("target","predictions")))
# ----------//----------//----------//----------//----------

# ----------//----------//----------//----------//----------
# fit a (RVM) with training data: 
RVM=fit(y~.,d[hd$tr,],model="rvm")

# multi-step, from 1 to H ahead forecasts:
init=hd$ts[1] # or same as: init=LD-H+1
# for multi-step ahead prediction, the lforecast from rminer should be used instead of predict,
# since predict only performs 1-ahead predictions
F12=lforecast(RVM,d,start=hd$ts[1],horizon=H)
print(F12)
Pred12=F12

# show forecasting measures and graph:
cat("RVM predictions:\n")
print(Pred12)
cat("MAE:",mmetric(Y,Pred12,metric="MAE"),"\n")
cat("NMAE:",mmetric(Y,Pred12,metric="NMAE",val=srange),"\n")
cat("RMSE:",mmetric(Y,Pred12,metric="RMSE"),"\n")
cat("RRSE:",mmetric(Y,Pred12,metric="RRSE"),"\n")

# graph: RVM - simple Regression Plot
print("Graph with MR predictions (multi-ahead):")
mgraph(Y,Pred12,graph="REG",Grid=10,lty=1,col=c("black","blue"),main="RVM predictions",leg=list(pos="topright",leg=c("target","predictions")))
# ----------//----------//----------//----------//----------