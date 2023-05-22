source("hill.R") #  hclimbing is defined here
source("montecarlo.R") # mcsearch is defined here
source("teste-predict.R") #função predict stella
source("getUpperLimit.R")

# dimension
D=42

# evaluation function:
source("eval.R")

sales_pred1 <- predict_ksvm_stella()
sales_pred2 <- predict_ksvm_bud()

max_previsto_stella = max(predict_ksvm_stella())
max_previsto_bud = max(predict_ksvm_bud())


N=1000 # number of searches
REPORT=N/20 # report results

# monte carlo search with D=2 and x in [-10.4,10.4]
lower=rep(0,D)
upper=getUpperLimit(c(sales_pred1,sales_pred2)) #  upper bounds

rchange1=function(par,lower,upper) # change for hclimbing
{ hchange(par,lower=lower,upper=upper,rnorm,mean=0,sd=0.25,round=FALSE) }

s0=c(rep(sample(0:max_previsto_stella +max_previsto_stella*0.5, 1),7),rep(sample(0:max_previsto_bud + max_previsto_bud*0.5,1),7),rep(sample(0:72, 1),7),rep(sample(0:60, 1),7),rep(sample(0:90, 1),7),rep(sample(0:120, 1),7))



cat("hill climbing search D=",D,"(iters=",N,")\n")
# initial solution: 
HC=hclimbing(par=s0,fn=eval,change=rchange1,lower=lower,upper=upper,type="max",
             control=list(maxit=N,REPORT=REPORT,digits=2))
#L=-eval(HC$par)
#if(L>best) { BESTSA=HC; best=L;}
#}
cat("best solution:",HC$sol,"evaluation function",HC$eval,"\n")