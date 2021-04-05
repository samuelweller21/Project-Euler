source("C:/Users/User/Documents/Project Euler/Useful.R")

max = 1000000
consec = 4
prev = 0

primes = gen.primes(max)

fast.prime.factors = function(x) {
  for (p in primes) {
    if (x %% p == 0) {
      if (x/p == 1) {
        return (p)
      } else {
        factors = c(p, fast.prime.factors(x/p))
      }
      break
    }
  }
  return(factors)
}

for (i in 2:max) {
  if (100*i/max > prev + 1) {
    prev = round(100*i/max,0)
    cat(prev, "%", "\n")
  }
  if (length(unique(fast.prime.factors(i))) == consec) {
    for (j in 1:(consec-1)) {
      if (length(unique(fast.prime.factors(i + j))) != consec) {
        break
      }
      if (j == (consec-1)) {
        cat(i, "looks okay?", "\n")
      }
    }
  }
}
