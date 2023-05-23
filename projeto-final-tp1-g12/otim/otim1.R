source("blind.R") # fsearch is defined here
source("montecarlo.R") # mcsearch is defined here
source("hill.R") #  hclimbing is defined here

# dimension
D=42

lower=rep(0,D) # lower bounds

N=1000 # number of searches
REPORT=N/20 # report results

# a interface vai enviar as sales pred obtidas atraves da interface
otimizar <- function(metodo,sales_pred1,sales_pred2) {
  
  upper=getUpperLimit(c(sales_pred1,sales_pred2)) # limite superior
  
  funcao <- paste("otim_", metodo, sep = "")
  return(get(funcao)())
}

#set.seed(125)
# slight change of a real par under a normal u(0,0.5) function:
rchange1=function(par,lower,upper) # change for hclimbing
{ hchange(par,lower=lower,upper=upper,rnorm,mean=0,sd=0.25,round=FALSE) }

otim_hill <- function(upper) {
  cat("hill climbing search sphere D=",D,"(iters=",N,")\n")
  # initial solution: 
  s0=upper/2 # one extreme point, could be a random point
  HC=hclimbing(par=s0,fn=eval,change=rchange1,lower=lower,upper=upper,type="max",
               control=list(maxit=N,REPORT=REPORT,digits=2))
  cat("best solution:",HC$sol,"evaluation function",HC$eval,"\n")
}

otim_montecarlo <- function(upper) {
  # monte carlo search with D=2 and x in [-10.4,10.4]
  MC=mcsearch(fn=eval,lower=lower,upper=upper,N=N,type="max")
  cat("best solution:",MC$sol,"evaluation function",MC$eval," (found at iteration:",MC$index,")\n")
  cat("detailed plan:\n")
  printeval(MC$sol)
}

otim_sann <- function(upper) {
  
  
}