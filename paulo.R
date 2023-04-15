#Codigo_do_stor
library(openxlsx)
library(rminer)
library(forecast)

db=read.xlsx(xlsxFile="Desktop/Uminho/2º Semestre/TIAPOSE/TIAPOSE TP1-G12/projeto/bebidas.xlsx",sheet=1,skipEmptyRows=FALSE,colNames=TRUE,detectDates=TRUE)
#notas:
class(db[,5]) # "Date" - R data type that handles dates
summary(db)
dbrange=diff(range(db))
cat("range:",dbrange,"\n")
cat("size:",length(db),"\n")
plot(db,type="l",col="blue")
acf(db) # autocorrelation plot
mpause() # rminer function, requires user to press enter
db <- head(db, -21)
db=db[,5, drop=FALSE] # vector of numeric

K=7 # db period (weekly!)
print("show graph")
tsdisplay(db)
mpause()

L=length(db)
NTS=K # number of predictions
H=NTS # from 1 to H ahead predictions

# --- this portion of code uses forecast library, which assumes several functions, such as forecast(), and uses a ts object 
# --- note: the forecast library works differently than rminer
# time series monthly object, frequency=K 
# this time series object only includes TRAIN (older) data:
LTR=L-H
# start means: year of 1949, 1st month (since frequency=K=12).
# according to the ts function documentation: frequency=7 assumes daily data, frequency=4 or 12 assumes quarterly and monthly data
TR=ts(db[1:LTR],frequency=K,start=c(1949,1)) # start means: year of 1949, 1st month (since frequency=K=12).
# show the in-sample (training data) time series:
plot(db)
print(db)
mpause() # press enter

# target predictions:
Y=db[(LTR+1):L]

# holt winters forecasting method:
print("model> HoltWinters")
HW=HoltWinters(db)
print(HW)
plot(HW)
print("show holt winters forecasts:")
# forecasts, from 1 to H ahead:
F=forecast(HW,h=H)
print(F)
Pred=F$mean[1:H] # HolWinters format
mgraph(Y,Pred,graph="REG",Grid=10,col=c("black","blue"),leg=list(pos="topleft",leg=c("target","HW pred.")))
cat("MAE:",mmetric(Y,Pred,metric="MAE"),"\n")
mpause() # press enter

cat("forecast library methods:\n")
cat("HW MAE:",mmetric(Y,Pred,metric="MAE"),"\n")