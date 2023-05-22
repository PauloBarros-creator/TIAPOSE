# Weekly_Naive.R.

library(openxlsx)
library(rminer)
library(forecast)

cat("read beer time series:")
TS=read.xlsx(xlsxFile="C:/Users/paulo/OneDrive/Ambiente de Trabalho/Uminho/TIAPOSE/TIAPOSE/bebidas.xlsx",sheet=1,skipEmptyRows=FALSE,colNames=TRUE,detectDates=TRUE)
#TS <- head(TS, - 14) # Remove last 14 lines - too many zeroes

#TS = TS$STELLA
TS = TS$BUD
summary(TS)
K=7 # TS period (weekly!)

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
TR=ts(TS[1:LTR],frequency=K) # start means: year of 2019, 1st month (since frequency=K=7). RETIREI ",start=c(2019,1)" 
# show the in-sample (training data) time series:
print(TR)

ultima_semana <- tail(TR, 7)
print(ultima_semana)
# target predictions:
Y=TS[(LTR+1):L]
print(Y)

cat("NMAE:",mmetric(Y,ultima_semana,metric="NMAE"),"\n")