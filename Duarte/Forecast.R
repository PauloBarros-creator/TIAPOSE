# install.packages("openxlsx")
library(openxlsx)
library(forecast)
library(rminer)
TS=read.xlsx(xlsxFile="bebidas.xlsx",sheet=1,skipEmptyRows=FALSE,colNames=TRUE,detectDates=TRUE)
TS <- head(TS, - 14) # Remove last 14 lines - too many zeroes
# notas:
#TS=class(TS[,1]) # "Date" - R data type that handles dates
class(TS[,1])
summary(TS)

TS=TS$STELLA
K=7# TS period (weekly!)
print("show graph")
tsdisplay(TS)
mpause()


L=length(TS)
NTS=K # number of predictions
H=NTS # from 1 to H ahead predictions

# --- this portion of code uses forecast library, which assumes several functions, such as forecast(), and uses a ts object 
# --- note: the forecast library works differently than rminer
# time series monthly object, frequency=K 
# this time series object only includes TRAIN (older) data:
LTR=L-H
# start means: year of 1949, 1st month (since frequency=K=12).
# according to the ts function documentation: frequency=7 assumes daily data, frequency=4 or 12 assumes quarterly and monthly data
TR=ts(TS[1:LTR],frequency=K,start=c(2019,1,1)) # start means: year of 1949, 1st month (since frequency=K=12).
# show the in-sample (training data) time series:
plot(TR)
print(TR)
mpause() # press enter
# target predictions:
Y=TS[(LTR+1):L]

# holWinters from forecast:
print("model> holwinters")
HolW=ets(TR)
F4=forecast(HolW,h=H)
Pred=F4$mean[1:H] # HolWinters format
mgraph(Y,Pred4,graph="REG",Grid=10,col=c("black","blue"),leg=list(pos="topleft",leg=c("target","ets pred.")))
cat("MAE:",mmetric(Y,Pred,metric="MAE"),"\n")
mpause() # press enter

# arima modeling:
print("model> auto.arima")
AR=auto.arima(TR)
print(AR) # ARIMA(3,0,1)(2,1,0)[12] 
print("show ARIMA forecasts:")
# forecasts, from 1 to H ahead:
F1=forecast(AR,h=H)
print(F1)
Pred1=F1$mean[1:H]
mgraph(Y,Pred1,graph="REG",Grid=10,col=c("black","blue"),leg=list(pos="topleft",leg=c("target"," ARIMA pred.")))
cat("MAE:",mmetric(Y,Pred1,metric="MAE"),"\n")
mpause() # press enter

# NN from forecast:
print("model> nnetar")
NN1=nnetar(TR,P=1,repeats=3)
print(NN1)
F3=forecast(NN1,h=H)
Pred3=F3$mean[1:H] # HolWinters format
mgraph(Y,Pred3,graph="REG",Grid=10,col=c("black","blue"),leg=list(pos="topleft",leg=c("target","NN1 pred.")))
cat("MAE:",mmetric(Y,Pred3,metric="MAE"),"\n")
mpause() # press enter

# ets from forecast:
print("model> ets")
ETS=ets(TR)
F4=forecast(ETS,h=H)
Pred4=F4$mean[1:H] # HolWinters format
mgraph(Y,Pred4,graph="REG",Grid=10,col=c("black","blue"),leg=list(pos="topleft",leg=c("target","ets pred.")))
cat("MAE:",mmetric(Y,Pred4,metric="MAE"),"\n")
mpause() # press enter



# -- end of forecast library methods


cat("forecast library methods:\n")
cat("HW MAE:",mmetric(Y,Pred,metric="MAE"),"\n")
cat("AR MAE:",mmetric(Y,Pred1,metric="MAE"),"\n")
cat("NN1 MAE:",mmetric(Y,Pred3,metric="MAE"),"\n")
cat("ET MAE:",mmetric(Y,Pred4,metric="MAE"),"\n")

