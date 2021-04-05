source("C:/Users/User/Documents/Programming/Maths/Project Euler/Useful.R")

current.num = 9

current.dimension = 3

current.diags = c(1, 3, 5, 7, 9)

current.primes = c(3, 5, 7)

while (length(current.primes) > length(current.diags)/10) {
  current.dimension = current.dimension + 2
  for (i in 1:4) {
    current.num = current.num + current.dimension - 1
    current.diags = c(current.diags, current.num)
    if (is.prime(current.num)) {
      current.primes = c(current.primes, current.num)
    }
  }
  cat("Size: ", current.dimension, " - ", 100*length(current.primes)/length(current.diags), "%", "\n")
  
}