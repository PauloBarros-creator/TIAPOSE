# install.packages("vars") # if needed, install
# install.packages("fpp2") # if needed, install
library(vars)
library(rminer)
library(forecast)
library(openxlsx)

# load the Canada data:
# data(Canada) # Quarter data
# print(summary(Canada))
Canada=read.xlsx(xlsxFile="bebidas.xlsx",sheet=1,skipEmptyRows=FALSE,colNames=TRUE,detectDates=TRUE)
Canada <- head(Canada, - 14) # Remove last 14 lines - too many zeroes
class(Canada[,1]) # "Date" - R data type that handles dates

K=7 # seasonal frequency: 4 time periods per year
LTS=K #  1 year, used for the forecasting range, thus 4 forecasts
dia=Canada[,2] #dia da semana
prod=Canada[,3] # productivity (precipitação)
rw=Canada[,4]   # real wage (temperatura maxima)

# prod = Canada$DIA_SEMANA
# rw=Canada$PRECIPITACAO

hd=holdout(prod,ratio=LTS,mode="order") # simple ordered holdout train and test split, rminer function

cdata=cbind(prod,rw)
mtr=ts(cdata[hd$tr,],frequency=K) # TS training object, uses forecast library mode!
Y=cdata[hd$ts,] # target values

### Recipe for VAR.
# note: here the default type="const" value is assumed. there are other options, see: help(VAR)
LAGMAX=4*K # 4*K. Also default lags.pt=16 of serial.test
# p lag order selection for a multi-variate VAR model with 2 time series:
# VARselect(mtr,lag.max=LAGMAX,type="const")[["selection"]]
vselect=VARselect(mtr,lag.max=LAGMAX,type="const")
# R. Hyndman book recipe:
# The R output shows the lag length selected by each of the information criteria available in the vars package. 
# There is a large discrepancy between the VAR(5) selected by the AIC and the VAR(1) selected by the BIC -> SC(n). 
# This is not unusual. As a result we first fit a VAR(1), as selected by the BIC.
# o is the initial order
omin=as.numeric(vselect$selection[3]) # BIC
omax=as.numeric(vselect$selection[1]) # AIC
# Adapted recipe of R. Hyndman book:
stop=FALSE
pvalueref=0.10
o=omin
while(!stop)
{
  mvar=VAR(mtr,p=o,type="const")
  # Portmanteau Test (asymptotic)
  st=serial.test(mvar,lags.pt=LAGMAX,type="PT.asymptotic")
  pvalue=as.numeric(st$serial$p.value)
  cat("order:",o,"pvalue:",pvalue,"\n")
  if(pvalue> pvalueref) stop=TRUE
  else if( (o+1)==omax) {stop=TRUE;o=o+1} 
  else o=o+1
}
# set final VAR model
mvar=VAR(mtr,p=o,type="const")

# get multi-step ahead forecasts
F1=forecast(mvar,h=LTS) # similar to the forecast library function, multi-step ahead forecasts
Pred1=as.numeric(F1$forecast$prod$mean)
Pred2=as.numeric(F1$forecast$rw$mean)
Pred3=as.numeric(F1$forecast$dia$mean)

fshow=function(Y,Pred1,Pred2,Pred3,method,name1,name2,name3)
{
  par(mfrow = c(1, 2, 3)) # two graphs inside a plot
  # note: in this example, NMAE is normalized to the range of only the Y prediction period.
  # since Y only includes 4 periods, the NMAE % values are often high.
  # an alternative would be to use mtr range values or cdata full data range values. 
  yrange1=diff(range(Y[,1]));yrange2=diff(range(Y[,2]))
  nmae=round(mmetric(Y[,1],Pred1,metric="NMAE",val=yrange1),1)
  cor=round(mmetric(Y[,1],Pred1,metric="COR"),digits=2)
  main=paste(method," ",name1," (NMAE=",nmae,"%, COR=",cor,")",sep="")
  mgraph(Y[,1],Pred1,main=main,graph="REG",Grid=10,col=c("black","blue"),leg=list(pos="topleft",leg=c("target","VAR pred.")))
  
  nmae=round(mmetric(Y[,2],Pred2,metric="NMAE",val=yrange2),1)
  cor=round(mmetric(Y[,2],Pred2,metric="COR"),digits=2)
  main=paste(method," ",name2," (NMAE=",nmae,"%, COR=",cor,")",sep="")
  mgraph(Y[,2],Pred2,main=main,graph="REG",Grid=10,col=c("black","blue"),leg=list(pos="topleft",leg=c("target","VAR pred.")))
  
  nmae=round(mmetric(Y[,3],Pred3,metric="NMAE",val=yrange2),1)
  cor=round(mmetric(Y[,3],Pred3,metric="COR"),digits=2)
  main=paste(method," ",name3," (NMAE=",nmae,"%, COR=",cor,")",sep="")
  mgraph(Y[,3],Pred3,main=main,graph="REG",Grid=10,col=c("black","blue"),leg=list(pos="topleft",leg=c("target","VAR pred.")))
}

fshow(Y,Pred1,Pred2,Pred3,"VAR","precipitação","temperatura")
mpause()

### Recipe for ARIMAX
# generate 2 ARIMAX models, one for prod (1) and other for rw (2)
marimax1=auto.arima(mtr[,1],xreg=mtr[,2]) # prod
marimax2=auto.arima(mtr[,2],xreg=mtr[,1]) # rw
marimax3=auto.arima(mtr[,3],xreg=mtr[,1]) # rw

# need to get estimates/forecasts for the xreg argument used in forecast, 
# in this case, a simpler auto.arima is used:
marima1=auto.arima(mtr[,1]) # 
marima2=auto.arima(mtr[,2]) # 
marima3=auto.arima(mtr[,3]) # 
xreg1=as.numeric(forecast(marima1,h=LTS)$mean)
xreg2=as.numeric(forecast(marima2,h=LTS)$mean)
xreg3=as.numeric(forecast(marima3,h=LTS)$mean)

# now that external values for the forecasting period are "known" (xreg), lets compute the
# final ARIMAX forecasts:
F21=forecast(marimax1,h=LTS,xreg=xreg2)
F22=forecast(marimax2,h=LTS,xreg=xreg1)
F23=forecast(marimax3,h=LTS,xreg=xreg3)

Pred21=as.numeric(F21$mean)
Pred22=as.numeric(F22$mean)
Pred23=as.numeric(F23$mean)

fshow(Y,Pred21,Pred22,Pred23,"ARIMAX","precipitação","temperatura")
mpause()

### Recipe for 2 "entwined" Machine Learning models:
# in this recipe, 2 mlpe models are combined, each with 4 time lags:
D1=CasesSeries(mtr[,1],c(1,2,3,4))
D2=CasesSeries(mtr[,2],c(1,2,3,4))
D3=CasesSeries(mtr[,3],c(1,2,3,4))

# in this simple recipe, last external variable lag is fed into training set:
D1F=cbind(xlag1=D2$lag1,D1)
D2F=cbind(xlag1=D1$lag1,D2)
D3F=cbind(xlag1=D1$lag1,D3)

# fit 2 mlpe models:
N1=fit(y~.,D1F,model="mlpe")
N2=fit(y~.,D2F,model="mlpe")
N3=fit(y~.,D3F,model="mlpe")

# get iterative predictions:
xex1=D1F[nrow(D1F),] # last example
xex2=D2F[nrow(D2F),] # last example
xex3=D3F[nrow(D3F),] # last example

Pred31=vector(length=LTS)
Pred32=vector(length=LTS)
Pred33=vector(length=LTS)
# note: the following code performs a multi-step ahead forecasting 
# but is not very general, it only works for the adopted: xreg lag1 + c(1,2,3,4) input time lags
for(i in 1:LTS)
{ 
  p1=predict(N1,xex1)
  p2=predict(N2,xex2)
  p3=predict(N3,xex3)
  Pred31[i]=p1
  Pred32[i]=p2
  Pred33[i]=p3
  if(i<LTS) # update xexamples
  {
    xex1[1]=p2 # external var
    # slide xexample
    xex1[2: (length(xex1)-2) ] = xex1[4:(length(xex1)-1) ] # 4 lags + y
    xex1[length(xex1)-1]=p1 # prediction
    
    xex2[1]=p1 # external var
    xex2[2: (length(xex2)-2) ] = xex2[4:(length(xex2)-1) ] # 4 lags + y
    xex2[length(xex2)-1]=p2 # prediction
    
    xex3[1]=p3 # external var
    xex3[2: (length(xex3)-2) ] = xex3[4:(length(xex3)-1) ] # 4 lags + y
    xex3[length(xex3)-1]=p3 # prediction
  }
}

fshow(Y,Pred31,Pred32,Pred33,"2MLP","precipitação","temperatura")