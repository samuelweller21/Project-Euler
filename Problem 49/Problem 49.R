source("C:/Users/User/Documents/Project Euler/Useful.R")

gen.primes.four = function(x) {
  if (x == 2) {
    return (2)
  } else if (x < 2) {
    return(NA)
  } else {
    vec = rep(1009,1)
    for (i in 1010:x) {
      if (is.prime(i) == T) {
        vec[length(vec) + 1] = i
      }
    }
    return(vec)
  }
}

primes = gen.primes.four(10000)
vecs = list()
piles = list()
index = 1

for (i in 1:length(primes)) {
  vecs[[i]] = num.to.vec(primes[i])
}

piles[[1]] = vecs[[1]]

for (i in 2:length(vecs)) {
  for (j in 1:index) {
    if (all(vecs[[i]] %in% piles[[j]])) {
      piles[[j]] = c(piles[[j]], vecs[[i]])
      break
    }
    if (j == index) {
      piles[[index + 1]] = vecs[[i]]
      index = index + 1
    }
  }
}

final = subset(piles, lengths(piles) >= 12)
#109
for (f in 1:length(final)) {
  l = length(final[[f]])/4
  this.vec = rep(NA, l)
  for (i in 1:l) {
    this.vec[i] = 1000*final[[f]][(1+(i-1)*4):(1+(i-1)*4)] + 
      100*final[[f]][(2+(i-1)*4):(2+(i-1)*4)] + 
      10*final[[f]][(3+(i-1)*4):(3+(i-1)*4)] + 
      final[[f]][(4+(i-1)*4):(4+(i-1)*4)]
  }
  diffs = 0
  for (a in 1:l) {
    for (b in 1:l) {
      diffs[length(diffs) + 1] = abs(this.vec[a] - this.vec[b])
    }
  }
  diffs = subset(diffs, diffs != 0)
  for (i in 1:length(diffs)) {
    if (sum(diffs[i] == diffs) >= 6) {
      if (diffs[i] == 3330) {
        cat("look at", f, "in final with diff", diffs[i], "\n")
      }
    }
  }
}

