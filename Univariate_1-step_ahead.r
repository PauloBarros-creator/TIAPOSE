# Adapted from 1-sunspots.R
# Univariate_1-step_ahead.R time series forecasting example that considers only 1-step ahead forecasts.

library(openxlsx)
library(rminer)
library(RSNNS) # library with several Neural Network (NN) models, including Elman
db=read.xlsx(xlsxFile="/home/paulo/Desktop/TIAPOSE/bebidas.xlsx",sheet=1,skipEmptyRows=FALSE,colNames=TRUE,detectDates=TRUE)
# db <- head(db, - 14) # Remove last 14 lines - too many zeroes
# notas:
class(db[,1]) # "Date" - R data type that handles dates
summary(db)

#S = db$STELLA
S = db$BUD
NPRED=7 # number of predictions - Last 7 days
srange=diff(range(S)) # compute the range of S

# lets scale the data to [0,1] range, using MAX=250 and MIN=0
MAX=1500;MIN=0;RANGE=(MAX-MIN)
SS=(S-MIN)/RANGE

H=holdout(S,ratio=NPRED,mode="order") # time ordered holdout split
print(H)
# train:
lags=7
DS=CasesSeries(SS,c(1:lags)) # 11 time lags t-1,t-2,t-3,t-4,t-5,t-6,t-7 -> t
print(summary(DS))
print("Show TR and TS indexes:")
N=nrow(DS) # number of D examples

HDS=holdout(DS$y,ratio=NPRED,mode="order")

cat("TR: from",HDS$tr[1]," to ",HDS$tr[length(HDS$tr)],"\n")
cat("TS: from",HDS$ts[1]," to ",HDS$ts[length(HDS$ts)],"\n")

inputs=DS[,1:lags]
output=DS[,(lags+1)]

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
print(D)
N=nrow(D) # number of D examples
NTR=N-NPRED
TR=1:NTR # training row elements of D (oldest elements), excluding last NPRED rows
TS=(NTR+1):N #  test row elements of D (more recent elements), total of NPRED rows
print("TR:")
print(TR)
print("TS:")
print(TS)

# We can start here as long as everything already exists in the environment.

# ----------//----------//----------//----------//----------
# fit a  (xgboost) with training data: 
XGBOOST=fit(y~.,D[TR,],model="xgboost",search="heuristic")

#1-ahead predictions:
print("Predictions (1-ahead):")
PXGBOOST=predict(XGBOOST,D[TS,])

# store the output target into object Y
Y=D[TS,]$y # real observed values

# show forecasting measures and graph:
cat("XGBOOST predictions:\n")
print(PXGBOOST)
cat("MAE:",mmetric(Y,PXGBOOST,metric="MAE"),"\n")
cat("NMAE:",mmetric(Y,PXGBOOST,metric="NMAE",val=srange),"\n")
cat("RMSE:",mmetric(Y,PXGBOOST,metric="RMSE"),"\n")
cat("RRSE:",mmetric(Y,PXGBOOST,metric="RRSE"),"\n")

# graph: REG - simple Regression Plot
print("Graph with XGBOOST predictions (1-ahead):")
mgraph(Y,PXGBOOST,graph="REG",Grid=10,lty=1,col=c("black","blue"),main="Xgboost predictions",leg=list(pos="topright",leg=c("target","predictions")))

# ----------//----------//----------//----------//----------

# ----------//----------//----------//----------//----------
# fit a  (cubist) with training data: 
CUBIST=fit(y~.,D[TR,],model="cubist",search="heuristic")

#1-ahead predictions:
print("Predictions (1-ahead):")
PCUBIST=predict(CUBIST,D[TS,])

# store the output target into object Y
Y=D[TS,]$y # real observed values

# show forecasting measures and graph:
cat("CUBIST predictions:\n")
print(PCUBIST)
cat("MAE:",mmetric(Y,PCUBIST,metric="MAE"),"\n")
cat("NMAE:",mmetric(Y,PCUBIST,metric="NMAE",val=srange),"\n")
cat("RMSE:",mmetric(Y,PCUBIST,metric="RMSE"),"\n")
cat("RRSE:",mmetric(Y,PCUBIST,metric="RRSE"),"\n")

# graph: REG - simple Regression Plot
print("Graph with CUBIST predictions (1-ahead):")
mgraph(Y,PCUBIST,graph="REG",Grid=10,lty=1,col=c("black","blue"),main="Cubist predictions",leg=list(pos="topright",leg=c("target","predictions")))
# ----------//----------//----------//----------//----------

# ----------//----------//----------//----------//----------
# fit a (LM) with training data: 
LM=fit(y~.,D[TR,],model="lm",search="heuristic")

#1-ahead predictions:
print("Predictions (1-ahead):")
PLM=predict(LM,D[TS,])

# store the output target into object Y
Y=D[TS,]$y # real observed values

# show forecasting measures and graph:
cat("LM predictions:\n")
print(PLM)
cat("MAE:",mmetric(Y,PLM,metric="MAE"),"\n")
cat("NMAE:",mmetric(Y,PLM,metric="NMAE",val=srange),"\n")
cat("RMSE:",mmetric(Y,PLM,metric="RMSE"),"\n")
cat("RRSE:",mmetric(Y,PLM,metric="RRSE"),"\n")

# graph: REG - simple Regression Plot
print("Graph with LM predictions (1-ahead):")
mgraph(Y,PLM,graph="REG",Grid=10,lty=1,col=c("black","blue"),main="LM predictions",leg=list(pos="topright",leg=c("target","predictions")))
# ----------//----------//----------//----------//----------

# ----------//----------//----------//----------//----------
# fit a  (MR) with training data: 
MR=fit(y~.,D[TR,],model="mr",search="heuristic")

#1-ahead predictions:
print("Predictions (1-ahead):")
PMR=predict(MR,D[TS,])

# store the output target into object Y
Y=D[TS,]$y # real observed values

# show forecasting measures and graph:
cat("MR predictions:\n")
print(PMR)
cat("MAE:",mmetric(Y,PMR,metric="MAE"),"\n")
cat("NMAE:",mmetric(Y,PMR,metric="NMAE",val=srange),"\n")
cat("RMSE:",mmetric(Y,PMR,metric="RMSE"),"\n")
cat("RRSE:",mmetric(Y,PMR,metric="RRSE"),"\n")
 
# graph: REG - simple Regression Plot
print("Graph with MR predictions (1-ahead):")
mgraph(Y,PMR,graph="REG",Grid=10,lty=1,col=c("black","blue"),main="MR predictions",leg=list(pos="topright",leg=c("target","predictions")))
# ----------//----------//----------//----------//----------

# ----------//----------//----------//----------//----------
# fit a  (MARS) with training data: 
MARS=fit(y~.,D[TR,],model="mars",search="heuristic")

#1-ahead predictions:
print("Predictions (1-ahead):")
PMARS=predict(MARS,D[TS,])

# store the output target into object Y
Y=D[TS,]$y # real observed values

# show forecasting measures and graph:
cat("MARS predictions:\n")
print(PMARS)
cat("MAE:",mmetric(Y,PMARS,metric="MAE"),"\n")
cat("NMAE:",mmetric(Y,PMARS,metric="NMAE",val=srange),"\n")
cat("RMSE:",mmetric(Y,PMARS,metric="RMSE"),"\n")
cat("RRSE:",mmetric(Y,PMARS,metric="RRSE"),"\n")

# graph: MARS - simple Regression Plot
print("Graph with MR predictions (1-ahead):")
mgraph(Y,PMARS,graph="REG",Grid=10,lty=1,col=c("black","blue"),main="MARS predictions",leg=list(pos="topright",leg=c("target","predictions")))
# ----------//----------//----------//----------//----------

# ----------//----------//----------//----------//----------
# fit a  (CPPLS) with training data: 
CPPLS=fit(y~.,D[TR,],model="cppls",search="heuristic")

#1-ahead predictions:
print("Predictions (1-ahead):")
PCPPLS=predict(CPPLS,D[TS,])

# store the output target into object Y
Y=D[TS,]$y # real observed values

# show forecasting measures and graph:
cat("CPPLS predictions:\n")
print(PCPPLS)
cat("MAE:",mmetric(Y,PCPPLS,metric="MAE"),"\n")
cat("NMAE:",mmetric(Y,PCPPLS,metric="NMAE",val=srange),"\n")
cat("RMSE:",mmetric(Y,PCPPLS,metric="RMSE"),"\n")
cat("RRSE:",mmetric(Y,PCPPLS,metric="RRSE"),"\n")

# graph: CPPLS - simple Regression Plot
print("Graph with MR predictions (1-ahead):")
mgraph(Y,PCPPLS,graph="REG",Grid=10,lty=1,col=c("black","blue"),main="CPPLS predictions",leg=list(pos="topright",leg=c("target","predictions")))
# ----------//----------//----------//----------//----------

# ----------//----------//----------//----------//----------
# fit a (RVM) with training data: 
RVM=fit(y~.,D[TR,],model="cppls",search="heuristic")

#1-ahead predictions:
print("Predictions (1-ahead):")
PRVM=predict(RVM,D[TS,])

# store the output target into object Y
Y=D[TS,]$y # real observed values

# show forecasting measures and graph:
cat("RVM predictions:\n")
print(PRVM)
cat("MAE:",mmetric(Y,PRVM,metric="MAE"),"\n")
cat("NMAE:",mmetric(Y,PRVM,metric="NMAE",val=srange),"\n")
cat("RMSE:",mmetric(Y,PRVM,metric="RMSE"),"\n")
cat("RRSE:",mmetric(Y,PRVM,metric="RRSE"),"\n")

# graph: RVM - simple Regression Plot
print("Graph with MR predictions (1-ahead):")
mgraph(Y,PRVM,graph="REG",Grid=10,lty=1,col=c("black","blue"),main="RVM predictions",leg=list(pos="topright",leg=c("target","predictions")))
# ----------//----------//----------//----------//----------

# ----------//----------//----------//----------//----------
# elman network: 1 input lag, 2 hidden layers with 5 and 3 hidden nodes, 1 output node:
EL=elman(inputs[HDS$tr,"lag1"],output[HDS$tr],size=c(5,3),learnFuncParams=c(0.1),maxit=200)
# show training error convergence:
#mpause("show EL training error convergence:")
plotIterativeError(EL)

# target
Y=S[H$ts] # real observed values

# only 1 input is used, predict wants a data.frame or matrix (10x1):
tinputs=data.frame(lag1=inputs[TS,"lag1"])

PEL=predict(EL,tinputs)
# rescale back to original space:
PEL=(PEL*RANGE)+MIN

# show forecasting measures and graph:
cat("EL predictions: ")
cat("MAE:",mmetric(Y,PEL,metric="MAE"),"\n")
cat("NMAE=",mmetric(Y,PEL,metric="NMAE",val=srange),"\n")
cat("RMSE:",mmetric(Y,PEL,metric="RMSE"),"\n")
cat("RRSE:",mmetric(Y,PEL,metric="RRSE"),"\n")

# graph:
print("Graph with NN predictions (1-ahead):")
plot(1:length(Y),Y,ylim=c(min(PEL,PNN,Y),max(PEL,PNN,Y)),type="b",col="black")
lines(PEL,type="b",col="blue",pch=2)
legend("topright",c("Sunspots","EL"),pch=c(1,2,3,3),col=c("black","blue"))
# ----------//----------//----------//----------//----------