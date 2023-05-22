source("hill.R") #  hclimbing is defined here
#source("eval.R")
#source("Predict_Function.R")
#source("getUpperLimit.R")
#source("repair.R")

# evaluation function:
#sphere=function(x) eval(x)

# hill climbing search
N=10000 # searches
REPORT=N/20 # report results


#set.seed(125)
# slight change of a real par under a normal u(0,0.5) function:
rchange1=function(par,lower,upper) # change for hclimbing
{ hchange(par,lower=lower,upper=upper,rnorm,mean=0,sd=0.25,round=FALSE) }

# simple example: D=5
D=42 # dimension
lower=rep(0,D) # lower bounds
#upper=rep(10.4,D) #  upper bounds
sales_pred1 <- predict_ksvm_stella()
sales_pred2 <- predict_ksvm_bud()
upper= getUpperLimit(c(sales_pred1,sales_pred2))
cat("hill climbing search sphere D=",D,"(iters=",N,")\n")
# initial solution: 
s0=upper/2 # one extreme point, could be a random point
HC=hclimbing(par=s0,fn=eval,change=rchange1,lower=lower,upper=upper,type="max",
             control=list(maxit=N,REPORT=REPORT,digits=2))
cat("best solution:",HC$sol,"evaluation function",HC$eval,"\n")
