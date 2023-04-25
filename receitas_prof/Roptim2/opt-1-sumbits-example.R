# you need to install this package: install.packages("genalg")
library(genalg)

# string is a vector of binary 0 or 1 values
# rbga.bin always performs a minimization task, thus the sum of bits ( sum(string) )is transformed 
# into a minimization task using: K - eval(S):
evaluate=function(string=c()) 
{ return ( length(string) - sum(string)) }

nbits=24

iter=20
cat("rbga.bin sum of bits demo (nbits=",nbits,"):\n")
# genetic algorithm for a binary representation with a size of 24 bits, each bit is 0 or 1:
bga= rbga.bin(size=nbits,popSize=10,mutationChance=0.01,zeroToOneRatio=1,elitism=1,evalFunc=evaluate,iter=iter)
# visual example of the evolution of the rbga.bin optimization
plot(bga)
bindex=which.min(bga$evaluations)
cat("best solution:",bga$population[bindex,],"evaluation function",bga$evaluations[bindex],"\n")