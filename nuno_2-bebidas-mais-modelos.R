# Adapted from 1-sunspots.R

# install.packages("openxlsx")
library(openxlsx)
db=read.xlsx(xlsxFile="bebidas.xlsx",sheet=1,skipEmptyRows=FALSE,colNames=TRUE,detectDates=TRUE)
db <- head(db, - 14) # Remove last 14 lines - too many zeroes
# notas:
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

# We can start here as long as everything already exists in the environment.

# ----------//----------//----------//----------//----------
# fit a  (KSVM) with training data: 
KSVM=fit(y~.,D[TR,],model="ksvm",search="heuristic")

#1-ahead predictions:
print("Predictions (1-ahead):")
PKSVM=predict(KSVM,D[TS,])

# store the output target into object Y
Y=D[TS,]$y # real observed values

# show forecasting measures and graph:
cat("KSVM predictions:\n")
print(PKSVM)
cat("MAE:",mmetric(Y,PKSVM,metric="MAE"),"\n")
cat("NMAE:",mmetric(Y,PKSVM,metric="NMAE",val=srange),"\n")
cat("RMSE:",mmetric(Y,PKSVM,metric="RMSE"),"\n")
cat("RRSE:",mmetric(Y,PKSVM,metric="RRSE"),"\n")
# ----------//----------//----------//----------//----------

# ----------//----------//----------//----------//----------
# fit a  (LSSVM) with training data: 
LSSVM=fit(y~.,D[TR,],model="lssvm",search="heuristic")

#1-ahead predictions:
print("Predictions (1-ahead):")
PLSSVM=predict(LSSVM,D[TS,])

# store the output target into object Y
Y=D[TS,]$y # real observed values

# show forecasting measures and graph:
cat("LSSVM predictions:\n")
print(PLSSVM)
cat("MAE:",mmetric(Y,PLSSVM,metric="MAE"),"\n")
cat("NMAE:",mmetric(Y,PLSSVM,metric="NMAE",val=srange),"\n")
cat("RMSE:",mmetric(Y,PLSSVM,metric="RMSE"),"\n")
cat("RRSE:",mmetric(Y,PLSSVM,metric="RRSE"),"\n")
# ----------//----------//----------//----------//----------

# ----------//----------//----------//----------//----------
# fit a  (MLP) with training data: 
MLP=fit(y~.,D[TR,],model="mlp",search="heuristic")

#1-ahead predictions:
print("Predictions (1-ahead):")
PMLP=predict(MLP,D[TS,])

# store the output target into object Y
Y=D[TS,]$y # real observed values

# show forecasting measures and graph:
cat("MLP predictions:\n")
print(PMLP)
cat("MAE:",mmetric(Y,PMLP,metric="MAE"),"\n")
cat("NMAE:",mmetric(Y,PMLP,metric="NMAE",val=srange),"\n")
cat("RMSE:",mmetric(Y,PMLP,metric="RMSE"),"\n")
cat("RRSE:",mmetric(Y,PMLP,metric="RRSE"),"\n")
# ----------//----------//----------//----------//----------

# ----------//----------//----------//----------//----------
# fit a  (MLPE) with training data: 
MLPE=fit(y~.,D[TR,],model="mlpe",search="heuristic")

#1-ahead predictions:
print("Predictions (1-ahead):")
PMLPE=predict(MLPE,D[TS,])

# store the output target into object Y
Y=D[TS,]$y # real observed values

# show forecasting measures and graph:
cat("MLPE predictions:\n")
print(PMLPE)
cat("MAE:",mmetric(Y,PMLPE,metric="MAE"),"\n")
cat("NMAE:",mmetric(Y,PMLPE,metric="NMAE",val=srange),"\n")
cat("RMSE:",mmetric(Y,PMLPE,metric="RMSE"),"\n")
cat("RRSE:",mmetric(Y,PMLPE,metric="RRSE"),"\n")
# ----------//----------//----------//----------//----------

# ----------//----------//----------//----------//----------
# fit a  (MLPE) with training data: 
RF=fit(y~.,D[TR,],model="randomForest",search="heuristic")

#1-ahead predictions:
print("Predictions (1-ahead):")
PRF=predict(RF,D[TS,])

# store the output target into object Y
Y=D[TS,]$y # real observed values

# show forecasting measures and graph:
cat("RF predictions:\n")
print(PRF)
cat("MAE:",mmetric(Y,PRF,metric="MAE"),"\n")
cat("NMAE:",mmetric(Y,PRF,metric="NMAE",val=srange),"\n")
cat("RMSE:",mmetric(Y,PRF,metric="RMSE"),"\n")
cat("RRSE:",mmetric(Y,PRF,metric="RRSE"),"\n")
# ----------//----------//----------//----------//----------

# ----------//----------//----------//----------//----------
# fit a  (MLPE) with training data: 
RF=fit(y~.,D[TR,],model="naive",search="heuristic")

#1-ahead predictions:
print("Predictions (1-ahead):")
PRF=predict(RF,D[TS,])

# store the output target into object Y
Y=D[TS,]$y # real observed values

# show forecasting measures and graph:
cat("RF predictions:\n")
print(PRF)
cat("MAE:",mmetric(Y,PRF,metric="MAE"),"\n")
cat("NMAE:",mmetric(Y,PRF,metric="NMAE",val=srange),"\n")
cat("RMSE:",mmetric(Y,PRF,metric="RMSE"),"\n")
cat("RRSE:",mmetric(Y,PRF,metric="RRSE"),"\n")
# ----------//----------//----------//----------//----------
