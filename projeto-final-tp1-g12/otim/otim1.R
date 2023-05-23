source("blind.R") # fsearch is defined here
source("montecarlo.R") # mcsearch is defined here
source("hill.R") #  hclimbing is defined here

# dimension
D=42

lower=rep(0,D) # lower bounds

N=1000 # number of searches
REPORT=N/20 # report results

s0=c(rep(sample(0:max_previsto_stella +max_previsto_stella*0.5, 1),7),
     rep(sample(0:max_previsto_bud + max_previsto_bud*0.5,1),7),
     rep(sample(0:72, 1),7),rep(sample(0:60, 1),7),rep(sample(0:90, 1),7),rep(sample(0:120, 1),7))

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

otim_hill <- function(upper,s0) {
  #cat("hill climbing search sphere D=",D,"(iters=",N,")\n")
  # initial solution: 
  #s0=upper/2 # one extreme point, could be a random point
  HC=hclimbing(par=s0,fn=eval,change=rchange1,lower=lower,upper=upper,type="max",
               control=list(maxit=N,REPORT=REPORT,digits=2))
  #cat("best solution:",HC$sol,"evaluation function",HC$eval,"\n")
  output <- character()
  output <- c(output,cat("hill climbing search sphere D=",D,"(iters=",N,")\n"))
  output <- c(output,cat("best solution:",HC$sol,"evaluation function",HC$eval,"\n"))
  outputfinal<-paste(output,"\n")
}

otim_montecarlo <- function(upper) {
  # monte carlo search with D=2 and x in [-10.4,10.4]
  MC=mcsearch(fn=eval,lower=lower,upper=upper,N=N,type="max")
  #cat("best solution:",MC$sol,"evaluation function",MC$eval," (found at iteration:",MC$index,")\n")
  #cat("detailed plan:\n")
  #printeval(MC$sol)
  output <- character()
  output <- c(output,cat("best solution:",MC$sol,"evaluation function",MC$eval," (found at iteration:",MC$index,")\n"))
  outputfinal<-paste(output,"\n")
}

otim_sann <- function(upper,s0){
  rchange2=function(par) # change for hclimbing
  { hchange(par,lower=lower,upper=upper,rnorm,mean=0,sd=0.5,round=TRUE) }
  
  #cat("Simulated Annealing search D=",D,"(iters=",N,")\n")
  CSANN=list(maxit=N,temp=2000,trace=TRUE, fnscale= -1)
  SA=optim(par=s0,fn=eval,method="SANN",gr=rchange2,control=CSANN)

  #cat("Best Solution:",SA$par,"\n","Evaluation Function",SA$value,"\n")
  output <- character()
  output <- c(output,cat("Simulated Annealing search D=",D,"(iters=",N,")\n"))
  output <- c(output,cat("Best Solution:",SA$par,"\n","Evaluation Function",SA$value,"\n"))
  outputfinal<-paste(output,"\n")
}

otim_sann <- function(upper) {
  
  
}