source("otim/blind.R") # fsearch is defined here
source("otim/montecarlo.R") # mcsearch is defined here
source("otim/hill.R") #  hclimbing is defined here

# dimension
Dim=42

lower=rep(0,Dim) # lower bounds

N=1000 # number of searches
REPORT=N/20 # report results

# instanciar solucao inicial para poder ser alterada dentro de funcoes
#s0 <- lower

# a interface vai enviar as sales pred obtidas atraves da interface
otimizar <- function(metodo,sales_pred1,sales_pred2) {
  
  # calculo de uma solucao inicial aleatoria
  max_previsto_stella = max(sales_pred1)
  max_previsto_bud = max(sales_pred2)
  s0 <- c(rep(sample(0:max_previsto_stella +max_previsto_stella*0.5, 1),7),
          rep(sample(0:max_previsto_bud + max_previsto_bud*0.5,1),7),
          rep(sample(0:0, 1),7),rep(sample(0:0, 1),7),rep(sample(0:0, 1),7),rep(sample(0:0, 1),7))

  upper <- getUpperLimit(c(sales_pred1,sales_pred2)) # limite superior

  if(metodo == "hill"){
    result<-otim_hill(upper,s0,sales_pred1,sales_pred2)
  }else if(metodo=="montecarlo"){
    result<-otim_montecarlo(upper,sales_pred1,sales_pred2)
  }else{
    result<-otim_sann(upper,s0,sales_pred1,sales_pred2)
  }
  return(result)
}

otim_hill <- function(upper,s0,sales_pred1,sales_pred2) {
  
  #cat("\n",sales_pred1,"\n",sales_pred2,"\n")
  # slight change of a real par under a normal u(0,0.5) function:
  rchange1=function(par,lower,upper) # change for hclimbing
  { hchange(par,lower=lower,upper=upper,rnorm,mean=0,sd=0.25,round=FALSE) }
  
  HC=hclimbing(par=s0,fn=eval,change=rchange1,lower=lower,upper=upper,type="max",
               control=list(maxit=N,REPORT=REPORT,digits=2))
  #cat("best solution:",HC$sol,"evaluation function",HC$eval,"\n")
  #output <- character()
  #output <- c(output,cat("hill climbing search sphere Dim=",Dim,"(iters=",N,")\n"))
  #output <- c(output,cat("best solution:",HC$sol,"evaluation function",HC$eval,"\n"))
  #outputfinal<-paste(output,"\n")
  output<-c(HC$sol,HC$eval)
  return(output)
}

otim_montecarlo <- function(upper,sales_pred1,sales_pred2) {
  # monte carlo search with Dim=2 and x in [-10.4,10.4]
  MC=mcsearch(fn=eval,lower=lower,upper=upper,N=N,type="max")
  #cat("best solution:",MC$sol,"evaluation function",MC$eval," (found at iteration:",MC$index,")\n")
  #cat("detailed plan:\n")
  #printeval(MC$sol)
  # output <- character()
  # output <- c(output,cat("best solution:",MC$sol,"\n evaluation function",MC$eval,"\n (found at iteration:",MC$index,")\n"))
  # outputfinal<-paste(output,"\n")
  output<-c(MC$sol,MC$eval,MC$index)
  return(output)
}

otim_sann <- function(upper,s0,sales_pred1,sales_pred2){
  
  rchange2=function(par) # change for hclimbing
  { hchange(par,lower=lower,upper=upper,rnorm,mean=0,sd=0.5,round=TRUE) }
  
  #cat("Simulated Annealing search Dim=",Dim,"(iters=",N,")\n")
  CSANN=list(maxit=N,temp=2000,trace=TRUE, fnscale= -1)
  SA=optim(par=s0,fn=eval,method="SANN",gr=rchange2,control=CSANN)

  # cat("Best Solution:",SA$par,"\n","Evaluation Function",SA$value,"\n")
  # output <- character()
  # output <- c(output,cat("Simulated Annealing search Dim=",Dim,"(iters=",N,")\n"))
  # output <- c(output,cat("Best Solution:",SA$par,"\n","Evaluation Function",SA$value,"\n"))
  # outputfinal<-paste(output,"\n")
  
  output<-c(SA$par,SA$value)
  return(output)
}