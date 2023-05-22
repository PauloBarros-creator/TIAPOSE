source("blind.R") # fsearch is defined here
source("montecarlo.R") # mcsearch is defined here
source("teste-predict.R") #função predict stella
source("getUpperLimit.R")
source("eval.R")
source("hill.R")

N=1000 # searches
REPORT=N/10 # report results

sales_pred1 = predict_ksvm_stella()
sales_pred2 = predict_ksvm_bud()

max_previsto_stella = max(predict_ksvm_stella())
max_previsto_bud = max(predict_ksvm_bud())

D=42 # dimension
lower=rep(0,D) # lower bounds
upper=getUpperLimit(c(sales_pred1,sales_pred2)) #  upper bounds

# slight change of a real par under a normal u(0,0.5) function:
rchange2=function(par) # change for hclimbing
{ hchange(par,lower=lower,upper=upper,rnorm,mean=0,sd=0.5,round=TRUE) }

eval1=function(s) return(eval(s))
sann <- function(){
  
  s0=c(rep(sample(0:max_previsto_stella +max_previsto_stella*0.5, 1),7),
       rep(sample(0:max_previsto_bud + max_previsto_bud*0.5,1),7),
       rep(sample(0:72, 1),7),rep(sample(0:60, 1),7),rep(sample(0:90, 1),7),rep(sample(0:120, 1),7))
  cat("Simulated Annealing search D=",D,"(iters=",N,")\n")
  CSANN=list(maxit=N,temp=2000,trace=TRUE, fnscale= -1)
  SA=optim(par=s0,fn=eval1,method="SANN",gr=rchange2,control=CSANN)
  #L=-eval(SA$par)
  #if(L>best) { BESTSA=SA; best=L;}
  #s0 = s0[i+41:length(s0) ]
  #}
  cat("Best Solution:",SA$par,"\n","Evaluation Function",SA$value,"\n")
  
}
sann()