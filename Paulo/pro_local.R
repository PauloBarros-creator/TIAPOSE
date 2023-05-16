source("C:/Users/paulo/OneDrive/Ambiente de Trabalho/Uminho/TIAPOSE/TIAPOSE/Paulo/hill.R") #  hclimbing is defined here
source("C:/Users/paulo/OneDrive/Ambiente de Trabalho/Uminho/TIAPOSE/TIAPOSE/Paulo/Predict_Function.R") #  eval is defined here

# hill climbing search
N=1000 # searches
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
upper=c(rep(max_previsto_stella,7),rep(max_previsto_bud,7),rep(72,7),rep(60,7),rep(90,7),rep(120,7)) #  upper bounds

eval1=function(s) -eval(s)

hill <- function() {
  # hill climbing:
  Runs=10
  
  cat("Hill Climbingg (example with",Runs,"different seeds/runs): \n")
  
  best= -Inf # - infinity
  
  s0=c(rep(sample(0:max_previsto_stella, Runs),7),rep(sample(0:max_previsto_bud, Runs),7),rep(sample(0:72, Runs),7),rep(sample(0:60, Runs),7),rep(sample(0:90, Runs),7),rep(sample(0:120, Runs),7)) # one extreme point, could be a random point
  print(s0)
   for(i in 1:Runs)
  {
  cat("hill climbing search D=",D,"(iters=",N,")\n")
  # initial solution: 
  HC=hclimbing(par=s0[i],fn=eval1,change=rchange1,lower=lower,upper=upper,type="max",
               control=list(maxit=N,REPORT=REPORT,digits=0))
  L=-eval(HC$par)
  if(L>best) { BESTSA=HC; best=L;}
  }
  cat("best solution:",BESTSA$sol,"evaluation function",BESTSA$eval,"\n")
}

sann <- function(){
  
  # hill climbing:
  Runs=10
  
  cat("Hill Climbingg (example with",Runs,"different seeds/runs): \n")
  
  best= -Inf # - infinity
  s0 =c(rep(sample(0:max_previsto_stella, Runs),6),rep(sample(0:max_previsto_bud, Runs),7),rep(sample(0:72, Runs),7),rep(sample(0:60, Runs),7),rep(sample(0:90, Runs),7),rep(sample(0:120, Runs),7)) # one extreme point, could be a random point
  s0 = matrix(c(rep(sample(0:max_previsto_stella, Runs),6),rep(sample(0:max_previsto_bud, Runs),7),rep(sample(0:72, Runs),7),rep(sample(0:60, Runs),7),rep(sample(0:90, Runs),7),rep(sample(0:120, Runs),7)), nrow=70, ncol=10) # one extreme point, could be a random point
  print(s0) 
  print(c(s0[142]))
  for(i in 1:Runs)
  {
  cat("hill climbing search D=",D,"(iters=",N,")\n")
    # initial solution: 
  cat("Simulated Annealing search D=",D,"(iters=",N,")\n")
  CSANN=list(maxit=N,temp=5,trace=TRUE)
  SA=optim(par=s0[i:i+41],fn=eval1,method="SANN",gr=rchange2,control=CSANN)
  L=-eval(SA$par)
  if(L>best) { BESTSA=SA; best=L;}
  s0 = s0[i+41:length(s0) ]
  }
  cat("best solution:",BESTSA$par,"evaluation function",BESTSA$value,"\n")
}


print(hill())
print(sann())