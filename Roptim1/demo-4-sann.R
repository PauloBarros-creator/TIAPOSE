source("../Roptim1/hill.R")
# dimension
D=42

# evaluation function:
sphere=function(x) sum(x^2)

# hill climbing search
N=1000 # 100 searches
REPORT=N/10 # report results
lower=rep(0,D) # lower bounds, defined before optim call
#upper=rep(10.4,D) #  upper bounds, defined before optim call
upper=getUpperLimit(c(sales_pred1,sales_pred2)) #  upper bounds

# slight change of a real par under a normal u(0,0.5) function:
rchange2=function(par) # change for hclimbing
{ hchange(par,lower=lower,upper=upper,rnorm,mean=0,sd=0.5,round=FALSE) }
 
cat("Simulated Annealing search sphere D=",D,"(iters=",N,")\n")
CSANN=list(maxit=N,temp=5,trace=TRUE)
SA=optim(par=rep(-10.4,D),fn=sphere,method="SANN",gr=rchange2,control=CSANN)
cat("best solution:",SA$par,"evaluation function",SA$value,"\n")
