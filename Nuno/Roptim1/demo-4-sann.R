source("hill.R")
# dimension
D=42

#sphere=function(x) eval(x)

# hill climbing search
N=10000 # 100 searches
REPORT=N/10 # report results
lower=rep(0,D) # lower bounds, defined before optim call

sales_pred1 <- predict_ksvm_stella()
sales_pred2 <- predict_ksvm_bud()
upper= getUpperLimit(c(sales_pred1,sales_pred2))

# slight change of a real par under a normal u(0,0.5) function:
rchange2=function(par) # change for hclimbing
{ hchange(par,lower=lower,upper=upper,rnorm,mean=0,sd=0.5,round=FALSE) }
 
cat("Simulated Annealing search sphere D=",D,"(iters=",N,")\n")
CSANN=list(maxit=N,temp=5,trace=TRUE,fnscale = -1)
SA=optim(par=upper/2,fn=sphere,method="SANN",gr=rchange2,control=CSANN)
cat("best solution:",SA$par,"evaluation function",SA$value,"\n")
