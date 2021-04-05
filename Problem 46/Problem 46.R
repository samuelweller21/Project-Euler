#####
#Helpers
#####

gen.primes = function(x) {
  if (x == 2) {
    return (2)
  } else if (x < 2) {
    return(NA)
  } else {
    vec = rep(2,1)
    for (i in 3:x) {
      if (is.prime(i) == T) {
        vec[length(vec) + 1] = i
      }
    }
    return(vec)
  }
}

is.prime = function(x) {
  if (x == 2) {
    return (T)
  } else if (any(x %% 2:max(2,floor(sqrt(x))) == 0)) {
    return (F)
  } else {
    return (T)
  }
}

#####
#Code
#####

max = 10000

for (i in 1:max) {
  if (!is.prime(i) & (i %% 2 != 0)) {
    primes = gen.primes(i)
    for (p in 1:length(primes)) {
      if (sqrt((i - primes[p])/2) %% 1 == 0) {
        #cat(i, "is fine", "\n") #Remove for easier finding
        break
      }
      if(p == length(primes)) {
        cat(i, "is NOT fine", "\n")
      }
    }
  }
}

