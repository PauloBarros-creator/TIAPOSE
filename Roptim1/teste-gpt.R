# if needed, install this package:
# install.packages("tabuSearch")
library(tabuSearch)

# global variables (can be used inside the functions):
# dimension
D <- 42 # Total number of elements in the vectors
BITS <- 1 # Each element is represented by 1 bit (0 or 1)
LIM <- 250 # Upper and lower bound for each element (assumed to be in the range [0, 1])
Low <- -LIM
Up <- LIM

# return real value within [Low,Up] using the binary x (vector of 0 or 1):
bin2real <- function(x) {
  x <- paste(x, collapse = "") # x is now a string 
  n <- strtoi(x, base = 2)
  return(Low + (Up - Low) * n / (2^BITS - 1))
}

# evaluation function:
K <- -Inf # Minimum possible value for the profit

evaluate_solution <- function(x) {
  # Call your original evaluation function here to calculate the profit
  profit <- eval(x)
  
  return_value <- ifelse(is.finite(profit), -profit, -Inf)
  return(return_value)
}
# 
N <- 100 # number of iterations
cat("tabu search profit maximization (iters=", N, ")\n")
size <- D * BITS # solution size is D * BITS
#s <- rep(0, size) # initial configuration

s <- c(c(160, 8, 0, 52, 20, 0, 0),c(200, 200, 0, 0, 30, 0, 0),c(6, 3, 0, 1, 1, 0, 1),c(2, 0, 0, 1, 0, 0, 0),c(2, 1, 0, 0, 1, 0, 0),c(2, 1, 0, 0, 0, 0, 0))

s <- tabuSearch(size = size, iters = N, objFunc = evaluate_solution, config = s, verbose = TRUE)

b <- which.max(s$eUtilityKeep) # best index
bs <- s$configKeep[b,]
cat("best solution: ", bs, "\n")
cat("profit: ", -s$eUtilityKeep[b], "\n")
