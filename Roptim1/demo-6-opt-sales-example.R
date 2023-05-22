source("otim.R")

# --------- Product Sales example ----------
evali=function(x) -eval(x)


# simulated annealing:
Runs=10

cat("Simulated Annealing (example with",Runs,"different seeds/runs): \n")
x=sample(1:1000,42)
y=sample(1:1000,Runs)

init = c(c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0),c(0,0,0,0,0,0,0))

best= -Inf # - infinity
for(i in 1:Runs)
{
 sa= optim(c(x[i],y[i]),fn=evali,method="SANN",control=list(maxit=6000, temp=2000, trace=FALSE))
 L=eval(sa$par)
 cat("execution:",i," solution:",round(sa$par)," profit:",L,"\n")
 if(L>best) { BESTSA=sa; best=L;}
}
cat(">> Solution: ",round(BESTSA$par),"profit:",eval(BESTSA$par)," sales:",sales(BESTSA$par),"\n")
