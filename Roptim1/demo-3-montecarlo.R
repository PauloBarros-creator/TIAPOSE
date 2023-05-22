source("blind.R") # fsearch is defined here
source("montecarlo.R") # mcsearch is defined here


# dimension
D=42

# evaluation function:
# sphere=function(x) eval(x)

preparadas1 <- c(160, 8, 0, 52, 20, 0, 0)
preparadas2 <- c(200, 200, 0, 0, 30, 0, 0)


arm <- c(6, 3, 0, 1, 1, 0, 1)

v1 <- c(2, 0, 0, 1, 0, 0, 0)
v2 <- c(2, 1, 0, 0, 1, 0, 0)
v3 <- c(2, 1, 0, 0, 0, 0, 0)

#min_s <- c(c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0))
s <- c(preparadas1,preparadas2,arm,v1,v2,v3)

#N=100000 # number of searches
N=1000 # number of searches
# monte carlo search with D=2 and x in [-10.4,10.4]
lower=rep(0,D) # lower bounds
upper=rep(250,D) #  upper bounds
MC=mcsearch(fn=eval,lower=lower,upper=upper,N=N,type="max")
cat("best solution:",MC$sol,"evaluation function",MC$eval," (found at iteration:",MC$index,")\n")