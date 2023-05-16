source("C:/Users/paulo/OneDrive/Ambiente de Trabalho/Uminho/TIAPOSE/TIAPOSE/Paulo/hill.R") #  hclimbing is defined here
source("C:/Users/paulo/OneDrive/Ambiente de Trabalho/Uminho/TIAPOSE/TIAPOSE/Paulo/Predict_Function.R") #  eval is defined here

# hill climbing search
N=1000 # searches
REPORT=N/20 # report results

#set.seed(125)
# slight change of a real par under a normal u(0,0.5) function:
rchange1=function(par,lower,upper) # change for hclimbing
{ hchange(par,lower=lower,upper=upper,rnorm,mean=0,sd=0.25,round=FALSE) }


hill <- function() {
  
  max_previsto_stella = max(predict_ksvm_stella())
  max_previsto_bud = max(predict_ksvm_stella())
  
  D=42 # dimension
  lower=rep(0,D) # lower bounds
  upper=c(rep(max_previsto_stella,7),rep(max_previsto_bud,7),rep(72,7),rep(60,7),rep(90,7),rep(120,7)) #  upper bounds
  print(upper)
  cat("hill climbing search sphere D=",D,"(iters=",N,")\n")
  # initial solution: 
  s0=rep(0,D) # one extreme point, could be a random point
  HC=hclimbing(par=s0,fn=eval,change=rchange1,lower=lower,upper=upper,type="max",
               control=list(maxit=N,REPORT=REPORT,digits=0))
  cat("best solution:",HC$sol,"evaluation function",HC$eval,"\n")
}

print(hill())