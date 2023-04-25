source("blind.R") # fsearch is defined here
source("grid.R") #  gsearch is defined here
 
# simple sphere evaluation function:
sphere=function(x) sum(x^2)

# 1st example: dimension D=2
D=2
lower=rep(-10.4,D) # lower bounds
upper=rep(10.4,D) #  upper bounds
# grid search 10x10 search: length(seq(-10.4,10.4,by=2.1))== 10:
range=upper[1]-lower[1]
nsearches=10 # searches per dimension
jump=range/(nsearches-1) # 10 searches per dimension
step=rep(jump,D) # 10 searches per dimension
iter=nsearches^D
cat("grid search sphere D=",D,"step=",step,"(iters=",iter,")\n")
GS=gsearch(fn=sphere,lower=lower,upper=upper,step=step,type="min")
cat("best solution:",GS$sol,"evaluation function",GS$eval," (found at iteration:",GS$index,")\n")

# 2nd example: dimension D=5
D=5
lower=rep(-10.4,D) # lower bounds
upper=rep(10.4,D) #  upper bounds
nsearches=10 # searches per dimension
jump=range/(nsearches-1) # 10 searches per dimension
step=rep(jump,D) # 10 searches per dimension
iter=nsearches^D
cat("grid search sphere D=",D,"step=",step,"(iters=",iter,")\n")
GS=gsearch(fn=sphere,lower=lower,upper=upper,step=step,type="min")
cat("best solution:",GS$sol,"evaluation function",GS$eval," (found at iteration:",GS$index,")\n")