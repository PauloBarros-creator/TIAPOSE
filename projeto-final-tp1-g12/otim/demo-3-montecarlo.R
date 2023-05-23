source("blind.R") # fsearch is defined here
source("montecarlo.R") # mcsearch is defined here

# dimension
D=42

# evaluation function:
#sphere=function(x) sum(x^2)

N=10000 # number of searches
# monte carlo search with D=2 and x in [-10.4,10.4]
lower=rep(0,D) # lower bounds
sales_pred1 <- ksvm_stella()
sales_pred2 <- ksvm_bud()
upper=getUpperLimit(c(sales_pred1,sales_pred2)) #  upper bounds
MC=mcsearch(fn=eval,lower=lower,upper=upper,N=N,type="max")
cat("best solution:",floor(MC$sol),"evaluation function",floor(MC$eval)," (found at iteration:",floor(MC$index),")\n")
#cat("detailed plan:\n")
#printeval(MC$sol)