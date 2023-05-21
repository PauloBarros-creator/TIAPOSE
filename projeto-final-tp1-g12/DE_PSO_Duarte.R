source("blind.R") # fsearch is defined here
source("montecarlo.R") # mcsearch is defined here
source("teste-predict.R") #função predict stella
source("getUpperLimit.R")

#librarias
library(DEoptim)
library(pso)

# dimension
D=42

# evaluation function:
source("eval.R")

sales_pred1 <- predict_ksvm_stella()
sales_pred2 <- predict_ksvm_bud()

N=1000 # number of searches

# Função de avaliação
eval_fn <- function(s) {
  eval(s)
}

lower=rep(0,D)
upper=getUpperLimit(c(sales_pred1,sales_pred2)) #  upper bounds

# Execução do algoritmo DE
result_de <- DEoptim(eval_fn, lower = lower, upper = upper)

# Melhor solução encontrada pelo DE
best_solution_de <- result_de$optim$bestmem

# Avaliação da melhor solução
best_profit_de <- eval_fn(best_solution_de)

#------------------------------------------------------------------------------------------

# Execução do algoritmo PSO
result_pso <- psoptim(lower = lower, upper = upper, fn = eval_fn, control = list(trace = FALSE), par = s)

# Melhor solução encontrada pelo PSO
best_solution_pso <- result_pso$par

# Avaliação da melhor solução
best_profit_pso <- eval_fn(best_solution_pso)


# Resultados DE
cat("Melhor solução encontrada: ", best_solution_de, "\n")
cat("Lucro da melhor solução: R$", best_profit_de, "\n")

# Resultados pso 
cat("Melhor solução encontrada: ", best_solution_pso, "\n")
cat("Lucro da melhor solução: R$", best_profit_pso, "\n")