to.base.complex = function(v, b1 = 10, b2 = 2) {
  #Reverse vec for ease
  v = rev(v)
  
  #First convert to base 10
  sum = 0
  for (i in 1:length(v)) {
    sum = sum + v[i]*b1^(i-1)
  }
  
  cat(sum, "\n")
  
  #Then convert to base b2
  max = floor(log(sum, base = b2))
  out = rep(NA, max + 1)
  for (i in max:0) {
    out[(max - i + 1)] = (sum - (sum %% (b2^i))) / b2^i
    sum = sum - out[(max - i + 1)]*(b2^i)
  }
  return(out)
}

to.base = function(x, b = 2) {
  max = floor(log(x, base = b))
  out = rep(NA, max + 1)
  for (i in max:0) {
    out[(max - i + 1)] = (x - (x %% (b^i))) / b^i
    x = x - out[(max - i + 1)]*(b^i)
  }
  return(out)
}

is.palindrome = function(x) {
  as.vec = to.base(x,10)
  if (length(as.vec) == 1) {
    return(T)
  }
  if (length(as.vec) %% 2 == 0) {
    v1 = as.vec[1:(length(as.vec)/2)]
    v2 = rev(as.vec[((length(as.vec)/2)+1):length(as.vec)])
    if (all(v1==v2)) {
      return (T)
    }
  } else {
    v1 = as.vec[1:(floor(length(as.vec)/2))]
    v2 = rev(as.vec[(ceiling(length(as.vec)/2)+1):length(as.vec)])
    if (all(v1==v2)) {
      return(T)
    }
  }
  return(F)
}

is.palindrome.complex = function(v) {
  if (length(v) == 1) {
    return(T)
  }
  if (length(v) %% 2 == 0) {
    v1 = v[1:(length(v)/2)]
    v2 = rev(v[((length(v)/2)+1):length(v)])
    if (all(v1==v2)) {
      return (T)
    }
  } else {
    v1 = v[1:(floor(length(v)/2))]
    v2 = rev(v[(ceiling(length(v)/2)+1):length(v)])
    if (all(v1==v2)) {
      return(T)
    }
  }
  return(F)
}

sum = 0
prev = 0
for (i in 1:1000000) {
  if (100*i/1000000 > prev + 1) {
    prev = 100*i/1000000
    cat(prev, "%", "\n")
  }
  if (is.palindrome(i) && is.palindrome.complex(to.base(i,b=2))) {
    sum = sum + i
  }
}
