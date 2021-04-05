pentagonal = function(m) {
  #Returns a vector containing all trangle numbers less than n
  n = 1
  pent = NA
  while (T) {
    if (n*(n+1)/2 < m) {
      pent[n] = n*(3*n-1)/2
      n = n + 1
    } else {
      return(pent)
    }
  }
}

max = 100000000

pent = pentagonal(max)

for (p in 1:length(pent)) {
  to.check = subset(pent, pent < pent[p])
  for (c in 1:length(to.check)) {
    if (any((pent[p] - pent[c]) == pent)) {
      if (any((pent[p] + pent[c]) == pent)) {
        cat(pent[p] - pent[c], "\n")
      }
    }
  }
}