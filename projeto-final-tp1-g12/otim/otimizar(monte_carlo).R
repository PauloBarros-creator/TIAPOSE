source("blind.R") # fsearch is defined here
source("montecarlo.R") # mcsearch is defined here
source("teste-predict.R") #função predict stella
source("getUpperLimit.R")

# dimension
D=42

# evaluation function:
source("eval.R")

sales_pred1 <- predict_ksvm_stella()
sales_pred2 <- predict_ksvm_bud()

N=1000 # number of searches
# monte carlo search with D=2 and x in [-10.4,10.4]
lower=rep(0,D)
upper=getUpperLimit(c(sales_pred1,sales_pred2)) #  upper bounds
MC=mcsearch(fn=eval,lower=lower,upper=upper,N=N,type="max")
cat("Best Solution:",MC$sol,"\n","Evaluation Function:",MC$eval,"(found at iteration:",MC$index,")\n")
