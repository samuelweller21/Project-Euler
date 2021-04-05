source("C:/Users/User/Documents/Project Euler/Useful.R")

vec = gen.primes(ceiling(1000000/21))
results = data.frame(Value = NA, Length = NA)

prev = 0
l = length(vec)

for (i in 1:l) {
  if (100*i/l > prev + 0.1) {
    prev = round(100*i/l,1)
    cat(prev, "%", "\n")
  }
  for (j in 0:(length(vec)-i)) {
    v = sum(vec[i:(i+j)])
    if ((v < 1000000) & (is.prime(v))) {
      results[nrow(results)+1,]$Value = v
      results[nrow(results),]$Length = j+1
    }
  }
}
results = results[-1,]

