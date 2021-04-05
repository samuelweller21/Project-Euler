triangle = function(m) {
  #Returns a vector containing all trangle numbers less than n
  n = 1
  triag = NA
  while (T) {
    if (n*(n+1)/2 < m) {
      triag[n] = n*(n+1)/2
      n = n + 1
    } else {
      return(triag)
    }
  }
}

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

hexagonal = function(m) {
  #Returns a vector containing all trangle numbers less than n
  n = 1
  hex = NA
  while (T) {
    if (n*(n+1)/2 < m) {
      hex[n] = n*(2*n-1)
      n = n + 1
    } else {
      return(hex)
    }
  }
}

max = 2000000000

triag = triangle(max)
pent = pentagonal(max)
hex = hexagonal(max)

for (t in triag) {
  if (any(t == pent)) {
    if (any(t == hex)) {
      cat(t, "\n")
    }
  }
}