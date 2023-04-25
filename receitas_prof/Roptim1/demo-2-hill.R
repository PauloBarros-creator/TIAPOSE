source("hill.R") #  hclimbing is defined here

# evaluation function:
sphere=function(x) sum(x^2)

# hill climbing search
N=1000 # searches
REPORT=N/20 # report results


#set.seed(125)
# slight change of a real par under a normal u(0,0.5) function:
rchange1=function(par,lower,upper) # change for hclimbing
{ hchange(par,lower=lower,upper=upper,rnorm,mean=0,sd=0.25,round=FALSE) }

# simple example: D=5
D=5 # dimension
lower=rep(-10.4,D) # lower bounds
upper=rep(10.4,D) #  upper bounds
cat("hill climbing search sphere D=",D,"(iters=",N,")\n")
# initial solution: 
s0=rep(-10.4,D) # one extreme point, could be a random point
HC=hclimbing(par=s0,fn=sphere,change=rchange1,lower=lower,upper=upper,type="min",
             control=list(maxit=N,REPORT=REPORT,digits=2))
cat("best solution:",HC$sol,"evaluation function",HC$eval,"\n")
