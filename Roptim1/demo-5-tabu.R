# if needed, install this package:
# install.packages("tabuSearch")
library(tabuSearch)
source("otim.R")

# global variables (can be used inside the functions):
# dimension
D=42 # 2 parameters x1 and x2
BITS=1 # bits per parameter, sets the real value precision and affects the solution size
LIM=250 # upper and lower bound
Low=-LIM
Up=LIM

# return real value within [Low,Up] using the binary x (vector of 0 or 1):
bin2real=function(x) # x is vector of bits
{ x=paste(x,collapse="") # x is now string 
  n=strtoi(x,base=2)
  return (Low+(Up-Low)*n/(2^BITS-1))
}

# evaluation function:
K=D*LIM^2 # global variable
#K=10000000000
# note1: tabuSearch maximizes the evaluation function, thus we maximize K - the function, which is equivalent
#  to minimize the function.
# note2: tabuSearch only works with non negative evaluation function values, thus K was computed as the maximum possible value.
bsphere=function(x) 
{ #x1=x[1:BITS];x2=x[(BITS+1):length(x)]
  #xr=c(bin2real(x1),bin2real(x2)) 
  profit <- eval(x)
  #return (K-sum(xr^2)) # tabuSeach maximizes
  return (profit)
}

N=100 # number of iterations
cat("tabu search sphere D=",D,"(iters=",N,")\n")
size=D*BITS # solution size is D*BITS
#s=c(rep(0,BITS),rep(1,BITS))
s <- c(c(160, 8, 0, 52, 20, 0, 0),c(200, 200, 0, 0, 30, 0, 0),c(6, 3, 0, 1, 1, 0, 1),c(2, 0, 0, 1, 0, 0, 0),c(2, 1, 0, 0, 1, 0, 0),c(2, 1, 0, 0, 0, 0, 0))

s=tabuSearch(size,iters=N,objFunc=bsphere,config=s,verbose=TRUE)
b=which.max(s$eUtilityKeep) # best index
bs=s$configKeep[b,]
cat("best solution:",bin2real(bs[1:BITS]),bin2real(bs[(BITS+1):length(bs)]),"evaluation function",K-s$eUtilityKeep[b],"\n")
cat("solution in binary space:",bs,"\n")