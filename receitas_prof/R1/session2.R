# slide:
# setwd("/Users/pcortez/AULAS/MIEGSI/SAIN/R")
wine=read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv",sep=";",header=TRUE)
#fires=read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/forest-fires/forestfires.csv",sep=",",header=TRUE)
fires=read.table("forestfires.csv",sep=",",header=TRUE) # se forestfires.csv estiver na pasta de trabalho atual

# slide:
edit(fires)
summary(fires)
x=fires$area
hist(x,col="gray") # histograma
lx=log(x+1)
hist(lx,col="gray") # histograma
y=cut(x,c(-1,100,Inf),c("small","large")) # factor
plot(y)
t=table(y); print(t)
pie(t) # pie chart

# slide:
pdf("pie.pdf") # criar ficheiro pdf
pie(t)
dev.off() # fechar pdf

# slide:
temp=fires$temp # temperatura
boxplot(temp) # assumindo que há outliers
boxplot(temp,range=0) # sem outliers
rh=fires$RH
boxplot(temp,rh) # duas variaveis

# slide:
plot(fires)
plot(fires$DMC,fires$DC,xlab="DMC",ylab="DC",main="fires")
cor(fires$DMC,fires$DC)
cor(fires[,5:13])

# slide:
# exemplo melhorado:
plot(fires$DMC,fires$DC,
        xlab="DMC",ylab="DC",main="fires",
        pch=19,col="blue")
LM=lm(DC~DMC,fires) # regressao linear
print(LM)
abline(LM$coefficients) # ver recta

# slide:
plot(fires$X,fires$Y,col="red",pch=19,
        cex=3*fires$area/max(fires$area)) 

# slide:
summary(fires)
T=fires$temp
cat(mean(T),median(T),sd(T),max(T),min(T)) 
t.test(fires$temp)
cat(mean(fires$temp),"+-",
       mean(fires$temp)-t.test(fires$temp)$conf.int[1])

# slide:
Aug=which(fires$month=="aug")
Feb=which(fires$month=="feb")
Min=min(length(Aug),length(Feb))
ta=sample(fires$temp[Aug],Min) # mesmo num. amostras 
tf=sample(fires$temp[Feb],Min) # na comparacao
boxplot(tf,ta)
t.test(tf,ta)
cat("p-value:",t.test(tf,ta)$p.value)

# slide:
hist(wine$quality,breaks=7,col="red")
wine3=wine
wine3$quality=cut(wine$quality,c(0,5,6,10),c("bad", "medium","good"))
summary(wine3$quality)
# gravar novo ficheiro csv:
write.table(wine3,"winequality-white3.csv",row.names=FALSE,sep=",")
