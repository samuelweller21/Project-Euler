source("C:/Users/User/Documents/Project Euler/Useful.R")

is.pandigital9 = function(x) {
  vec = num.to.vec(x)
  if (length(vec) != length(unique(vec))) {
    return (FALSE)
  }
  if ((sum(vec %in% digits) == 9) & (!(0 %in% vec))) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

num.to.vec = function(x) {
  length = nchar(toString(x))
  out = rep(NA, length)
  for (i in 1:length) {
    out[i] = as.integer(substr(toString(x),i,i))
  }
  return(out)
}

concatenated.product = function(k, n) {
  # Returns concatenated product of k and (1,2,...,n)
  
  prods = rep(NA, n) 
  
  for (i in 1:n) {
    prods[i] = k * i
  }
  
  prod.string = ""
  
  for (i in 1:n) {
    prod.string = paste0(prod.string, toString(prods[i]))
  }
  
  return(as.integer(prod.string))
}

all = 0

for (k in 1:98765) {
  if (is.pandigital9(concatenated.product(k,2)) == TRUE) {
    all[length(all) + 1] = concatenated.product(k,2)
  }
}

for (k in 1:987) {
  if (is.pandigital9(concatenated.product(k,3)) == TRUE) {
    all[length(all) + 1] = concatenated.product(k,3)
  }
}

for (k in 1:987) {
  if (is.pandigital9(concatenated.product(k,4)) == TRUE) {
    all[length(all) + 1] = concatenated.product(k,4)
  }
}

for (k in 1:98) {
  if (is.pandigital9(concatenated.product(k,5)) == TRUE) {
    all[length(all) + 1] = concatenated.product(k,5)
  }
}

for (k in 1:98) {
  if (is.pandigital9(concatenated.product(k,6)) == TRUE) {
    all[length(all) + 1] = concatenated.product(k,6)
  }
}

for (k in 1:98) {
  if (is.pandigital9(concatenated.product(k,7)) == TRUE) {
    all[length(all) + 1] = concatenated.product(k,7)
  }
}

for (k in 1:98) {
  if (is.pandigital9(concatenated.product(k,8)) == TRUE) {
    all[length(all) + 1] = concatenated.product(k,8)
  }
}

for (k in 1:9) {
  if (is.pandigital9(concatenated.product(k,9)) == TRUE) {
    all[length(all) + 1] = concatenated.product(k,9)
  }
}

cat(max(all), "\n")