source("C:/Users/paulo/OneDrive/Ambiente de Trabalho/Uminho/TIAPOSE/TIAPOSE/Paulo/hill.R") #  hclimbing is defined here
source("C:/Users/paulo/OneDrive/Ambiente de Trabalho/Uminho/TIAPOSE/TIAPOSE/Paulo/Predict_Function.R") #  eval is defined here

# hill climbing search
N=6000 # searches
REPORT=N/20 # report results

#set.seed(125)
# slight change of a real par under a normal u(0,0.5) function:
rchange1=function(par,lower,upper) # change for hclimbing
{ hchange(par,lower=lower,upper=upper,rnorm,mean=0,sd=0.25,round=TRUE) }

# slight change of a real par under a normal u(0,0.5) function:
rchange2=function(par) # change for hclimbing
{ hchange(par,lower=lower,upper=upper,rnorm,mean=0,sd=0.5,round=TRUE) }

max_previsto_stella = max(predict_ksvm_stella())
max_previsto_bud = max(predict_ksvm_stella())

D=42 # dimension
lower=rep(0,D) # lower bounds
upper=c(rep(2*max_previsto_stella,7),rep(2*max_previsto_bud,7),rep(72,7),rep(60,7),rep(90,7),rep(120,7)) #  upper bounds

eval1=function(s) return(-eval(s))
s0=c(rep(sample(0:max_previsto_stella +max_previsto_stella*0.5, 1),7),rep(sample(0:max_previsto_bud + max_previsto_bud*0.5,1),7),rep(sample(0:72, 1),7),rep(sample(0:60, 1),7),rep(sample(0:90, 1),7),rep(sample(0:120, 1),7)) # one extreme point, could be a random point
print(s0)

hill <- function() {
  # hill climbing:
  #Runs=10
  
  #cat("Hill Climbingg (example with",Runs,"different seeds/runs): \n")
  
  #best= -Inf # - infinity
  
   #for(i in 1:Runs)
  #{
  cat("hill climbing search D=",D,"(iters=",N,")\n")
  # initial solution: 
  HC=hclimbing(par=s0,fn=eval1,change=rchange1,lower=lower,upper=upper,type="max",
               control=list(maxit=N,REPORT=REPORT,digits=0))
  #L=-eval(HC$par)
  #if(L>best) { BESTSA=HC; best=L;}
  #}
  cat("best solution:",HC$sol,"evaluation function",HC$eval,"\n")
}

sann <- function(){
  
  # Simulated Annealing:
  #Runs=10
  
  #cat("Simulated Annealing (example with",Runs,"different seeds/runs): \n")
  
  #best= -Inf # - infinity
  #for(i in 1:Runs)
  #{
  #cat("Simulated Annealing search D=",D,"(iters=",N,")\n")
    # initial solution: 
  cat("Simulated Annealing search D=",D,"(iters=",N,")\n")
  CSANN=list(maxit=N,temp=2000,trace=TRUE)
  SA=optim(par=s0,fn=eval1,method="SANN",gr=rchange2,control=CSANN)
  #L=-eval(SA$par)
  #if(L>best) { BESTSA=SA; best=L;}
  #s0 = s0[i+41:length(s0) ]
  #}
  cat("best solution:",SA$par,"evaluation function",SA$value,"\n")
}

Nelder <- function(){
  
  cat("Nelder-Mead search D=",D,"(iters=",N,")\n")
  CSANN=list(maxit=N,temp=2000,trace=TRUE)
  NelderM=optim(par=s0,fn=eval1,method="Nelder-Mead",gr=rchange2,control=CSANN)

  cat("best solution:",NelderM$par,"evaluation function",NelderM$value,"\n")
}

BFGS <- function(){
  
  cat("BFGS search D=",D,"(iters=",N,")\n")
  CSANN=list(maxit=N,temp=2000,trace=TRUE)
  BFGS1=optim(par=s0,fn=eval1,method="BFGS",gr=rchange2,control=CSANN)
  
  cat("best solution:",BFGS1$par,"evaluation function",BFGS1$value,"\n")
}

CG <- function(){
  
  cat("CG search D=",D,"(iters=",N,")\n")
  CSANN=list(maxit=N,temp=2000,trace=TRUE)
  CG1=optim(par=s0,fn=eval1,method="CG",gr=rchange2,control=CSANN)
  
  cat("best solution:",CG1$par,"evaluation function",CG1$value,"\n")
}

LBFGSB <- function(){
  
  cat("L-BFGS-B search D=",D,"(iters=",N,")\n")
  CSANN=list(maxit=N,temp=2000,trace=TRUE)
  LBFGSB1=optim(par=s0,fn=eval1,method="L-BFGS-B",gr=rchange2,control=CSANN)

  cat("best solution:",LBFGSB1$par,"evaluation function",LBFGSB1$value,"\n")
}

#Brent <- function(){
  
#  cat("Brent search D=",D,"(iters=",N,")\n")
#  CSANN=list(maxit=N,temp=2000,trace=TRUE)
#  Brent1=optim(par=s0,fn=eval1,method="Brent",gr=rchange2,control=CSANN)
  
#  cat("best solution:",Brent1$par,"evaluation function",Brent1$value,"\n")
#}

print(hill())
print(sann())
print(Nelder())
print(BFGS())
print(CG())
print(LBFGSB())
#print(Brent())