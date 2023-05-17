#Código adaptado do ChatGPT

library(GA)
source("C:/Users/paulo/OneDrive/Ambiente de Trabalho/Uminho/TIAPOSE/TIAPOSE/Paulo/Predict_Function.R") #  eval is defined here

max_previsto_stella = max(predict_ksvm_stella())
max_previsto_bud = max(predict_ksvm_stella())

D=42 # dimension
lower=rep(0,D) # lower bounds
upper=c(rep(100*max_previsto_stella,7),rep(100*max_previsto_bud,7),rep(72,7),rep(60,7),rep(90,7),rep(120,7)) #  upper bounds

# Configurar o algoritmo AG
config <- gaControl(functions = eval, fitness = eval, minFitness = FALSE, 
                    popSize = 7, maxiter = 100, pcrossover = 0.8, 
                    pmutation = 0.1)

# Executar o AG
resultado <- ga(type = "real-valued", fitness = eval,
                lower = lower, upper = upper, 
                control = config)

# Imprimir os resultados
print(resultado@solution)
print(resultado@fitnessValue)