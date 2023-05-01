# install.packages("openxlsx")
library(openxlsx)
library(forecast)
library(rminer)
db=read.xlsx(xlsxFile="bebidas.xlsx",sheet=1,skipEmptyRows=FALSE,colNames=TRUE,detectDates=TRUE)
db <- head(db, - 14) # Remove last 14 lines - too many zeroes
# notas:
#TS=class(TS[,1]) # "Date" - R data type that handles dates
#summary(TS)
class(db[,1]) # "Date" - R data type that handles dates
summary(db)

S = db$STELLA
NPRED=7 # number of predictions - Last 7 days
srange=diff(range(S)) # compute the range of S

# show S and some statistics:
print(S)
print(summary(S))
cat("range:",srange,"\n")
cat("size:",length(S),"\n")
plot(S,type="l",col="blue")
acf(S) # autocorrelation plot
#mpause() # rminer function, requires user to press enter

# CasesSeries: convert a single time series into a data.frame with inputs (...,lag2,lag1) and target output (y)
# selection of all 1 to 7 time lags:
D=CasesSeries(S,c(1:7)) # 7 time lags t-1,t-2,t-3,t-4,t-5,t-6,t-7-> t
print(summary(D))
print("Show TR and TS indexes:")
N=nrow(D) # number of D examples
NTR=N-NPRED
TR=1:NTR # training row elements of D (oldest elements), excluding last NPRED rows
TS=(NTR+1):N #  test row elements of D (more recent elements), total of NPRED rows
print("TR:")
print(TR)
print("TS:")
print(TS)

# neural network modeling, via rminer:
print("model> ctree (with t-7 lags)")
#d=CasesSeries(TS,c(1,12,13)) # data.frame from time series (domain knowledge for the 1,12,13 time lag selection)
print(summary(D))
LD=nrow(D) # note: LD < L
hd=holdout(D$y,ratio=NPRED,mode="order")
NN2=fit(y~.,D[hd$tr,],model="knn")
# multi-step, from 1 to H ahead forecasts:
init=hd$ts[1] # or same as: init=LD-H+1
# for multi-step ahead prediction, the lforecast from rminer should be used instead of predict,
# since predict only performs 1-ahead predictions
F5=lforecast(NN2,D,start=hd$ts[1],horizon=H)
print(F5)
Pred=F5
# store the output target into object Y
Y=D[TS,]$y # real observed values



# show forecasting measures and graph:
cat("naive predictions:\n")
print(Pred)
cat("MAE:",mmetric(Y,Pred,metric="MAE"),"\n")
cat("NMAE:",mmetric(Y,Pred,metric="NMAE",val=srange),"\n")
cat("RMSE:",mmetric(Y,Pred,metric="RMSE"),"\n")
cat("RRSE:",mmetric(Y,Pred,metric="RRSE"),"\n")

mgraph(Y,Pred,graph="REG",Grid=10,col=c("black","blue"),leg=list(pos="topleft",leg=c("target","ctree pred.")))
mpause() # press enter