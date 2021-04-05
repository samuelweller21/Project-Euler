#A collection of useful functions for Project Euler
#
# Some edits may have made previous problems not run

# source("C:/Users/User/Documents/Programming/Maths/Project Euler/Useful.R")

###- generate.cycle -###
#
# Returns the first l digits of 1/d

generate.cycle = function(d, l = 10) {
  n = 1
  cycle = rep(NA, l)
  for (i in 1:l) {
    r = (10*n) %% d
    k = (10*n - r)/d
    n = 10*n - k*d
    cycle[i] = k
  }
  return(cycle)
}

###- find.cycle.length -###
#
# Returns the length of the shorthand form of recurring decimals

find.cycle.length = function(cycle, debug = T) {
  if (cycle[length(cycle)] == 0) {
    return (-1)
  } else {
    for (j in 1:(length(cycle)-1)) {
      for (i in 0:(length(cycle)-2)) {
        short.cycle = cycle[j:(j+i)]
        if (debug == T) {
          cat("j", " - ", j, "\n")
          cat("i", " - ", i, "\n")
        }
        rem = (length(cycle) - j + 1) %% length(short.cycle)
        rep = (length(cycle) - j + 1 - rem)/length(short.cycle)
        if (rem == 0) {
          if( j != 1) {
            actual.cycle = c(cycle[1:(j-1)], rep(short.cycle,rep))
          } else {
            actual.cycle = rep(short.cycle,rep)
          }
        } else {
          if (j != 1) {
            actual.cycle = c(cycle[1:(j-1)], rep(short.cycle, rep), short.cycle[1:rem])
          } else {
            actual.cycle = c(rep(short.cycle, rep), short.cycle[1:rem])
          }
        }
        if (debug == T) {
          print(actual.cycle)
          print(cycle)
        }
        if (all(actual.cycle == cycle)) {
          if (i > (length(cycle)/2)) {
            
          } else {
            return(length(short.cycle))
          }
        }
      }
    }
  }
}

###- to.base.complex -###
#
# Converts a vector in base b1 to a vector in base b2

to.base.complex = function(v, b1 = 10, b2 = 2) {
  #Reverse vec for ease
  v = rev(v)
  
  #First convert to base 10
  sum = 0
  for (i in 1:length(v)) {
    sum = sum + v[i]*b1^(i-1)
  }
  
  #cat(sum, "\n")
  
  #Then convert to base b2
  if (sum == 0) {
    return(0)
  }
  max = floor(log(sum, base = b2))
  out = rep(NA, max + 1)
  for (i in max:0) {
    out[(max - i + 1)] = (sum - (sum %% (b2^i))) / b2^i
    sum = sum - out[(max - i + 1)]*(b2^i)
  }
  return(out)
}

###- to.base -###
#
# Converts x (in base 10) into base b as a vector

to.base = function(x, b = 2) {
  max = floor(log(x, base = b))
  out = rep(NA, max + 1)
  for (i in max:0) {
    out[(max - i + 1)] = (x - (x %% (b^i))) / b^i
    x = x - out[(max - i + 1)]*(b^i)
  }
  return(out)
}

###- is.palindrome -###
#
# Checks to see if number is a palindrome (same forward and back)

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

###- is.palindrome.complex -###
#
# Checks to see if a vector is the same forward/back

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

###- pentagonal -###
#
# Returns a vector containing all pentagonal numbers less than m

pentagonal = function(m) {
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

###- hexagonal -###
#
# Returns a vector containing all hexagonal numbers less than m

hexagonal = function(m) {
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

###- triangle -###
#
# Returns a vector containing all triangle numbers less than m

triangle = function(m) {
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

###- gen.primes -###
#
# Generates all primes less than x

gen.primes = function(x, progress.bar = F, progress.gap = 0.1) {
  if (x == 2) {
    return (2)
  } else if (x < 2) {
    return(NA)
  } else {
    vec = rep(2,1)
    prev = 0
    for (i in 3:x) {
      if (progress.bar == T) {
        if (100*i/x > prev + progress.gap) {
          prev = round(100*i/x,1)
          cat(prev, "%", "\n")
        }
      }
      if (is.prime(i) == T) {
        vec[length(vec) + 1] = i
      }
    }
    return(vec)
  }
}

###- fast.gen.primes -###
#
# Faster gen.primes for large x

fast.gen.primes = function(x) {
  prev = 0
  if (x < 2) {
    return (2)
  } else {
    A = rep(TRUE, x-1)
    for (i in 2:floor(sqrt(x))) {
      for (j in 0:floor((x - (i^2))/i)) {
        A[i^2+j*i-1] = FALSE
      }
    }
  }
  return (which(A %in% TRUE)+1)
}

###- is.prime -###
#
# Returns if x is prime

is.prime = function(x) {
  if (x == 2) {
    return (T)
  } else if (any(x %% 2:max(2,floor(sqrt(x))) == 0)) {
    return (F)
  } else {
    return (T)
  }
}

###- eval -###
#
# Returns i^i modulus m

eval = function(i, m) {
  sum = i
  for (j in 1:(i-1)) {
    sum = sum*i
    sum = sum %% m
  }
  return(sum)
}

###- is.pandigital9 -###
#
# Returns if a number is a 1-9 pandigital number

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

###- num.to.vec -###
#
# Returns x as a vector of each of its digits

num.to.vec = function(x) {
  length = nchar(format(x, scientific = F))
  if (is.na(length)) {
    cat(x, "\n")
    System.sleep(100)
  }
  out = rep(NA, length)
  for (i in 1:length) {
    out[i] = as.integer(substr(format(x, scientific = F),i,i))
  }
  return(out)
}

###- num.to.vec.fast -###
#
# Faster num to vec without string conversion

num.to.vec.fast = function(x) {
  i = 0
  while (x >= (10^i)) {
    i = i + 1
  }
  vec = rep(NA,i)
  for (j in (i-1):0) {
    vec[i + 1 - (j+1)] = (x - (x %% 10^j))/(10^j)
    x = x - vec[i + 1 - (j+1)]*10^j
  }
  return (vec)
}

###- vec.to.num -###
#
# Turns a vector into a number

vec.to.num = function(x) {
  
  total = 0
  
  for (i in 1:length(x)) {
    total = total + x[i] * 10^(length(x)-i)
  }
  
  return(total)
  
}

###- concatenated.product -###
#
# Returns the concatenated.product of k and (1,2,...,n)

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

###- prime.factors -###
#
# Returns all prime factors of x

prime.factors = function(x) {
  for (p in gen.primes(x)) {
    if (x %% p == 0) {
      if (x/p == 1) {
        return (p)
      } else {
        factors = c(p, prime.factors(x/p))
      }
      break
    }
  }
  return(factors)
}

###- gcd -###
#
# Returns the greatest common denominator of two numbers

gcd = function(a,b) {
  
  if (a == 0) {
    return (b)
  } else if (b == 0) {
    return (a)
  } else {
    r = a %% b
    return (gcd(b, r))
  }
  
}

###- reverse -###
#
# Reverses a number (1st digit = last) e.g. 123 -> 321

reverse = function(x) {
  vec = num.to.vec(x)
  rev = rep(NA, length(vec))
  for (i in 1:length(vec)) {
    rev[i] = vec[length(vec)-i+1]
  }
  return(vec.to.num(rev))
}

###- reverse.vec -###
#
# Reverses a vector c(1,2,3) -> c(3,2,1)

reverse.vec = function(x) {
  rev = rep(NA, length(x))
  for (i in 1:length(x)) {
    rev[i] = x[length(x)-i+1]
  }
  return(rev)
}

###- is.palindrome.string -###
#
# Checks if a string is a palindrome

is.palindrome.string = function(x) {
  return(is.palindrome.complex(string.to.vec(x)))
}

###- string.to.vec -###
#
# Returns a string as a vector of characters

string.to.vec = function(x) {
  vec = rep(NA, nchar(x))
  for (i in 1:nchar(x)) {
    vec[i] = substr(x, i, i)
  }
  return (vec)
}

###- vec.to.string -###
#
# Returns a string from a vector of characters

vec.to.string = function(x) {
  string = ""
  for (i in 1:length(x)) {
    string = paste(string, x[i], sep = "")
  }  
  return (string)
}

###- reverse.string -###
#
# Reverses a string

reverse.string = function(x) {
  return (vec.to.string(reverse.vec(string.to.vec(x))))
}

###- to.base.byte -###
#
# Returns to base 2 with leading 0 s.t. it fills a byte

to.base.byte = function(x) {
  vec = to.base(x)
  vec = reverse.vec(vec)
  missing_zeroes = 8 - length(vec)
  while (missing_zeroes > 0) {
    vec[length(vec)+1] = 0
    missing_zeroes = missing_zeroes - 1
  }
  return(reverse.vec(vec))
}

###- numerical.xor -###
#
# Returns xor of two numbers e.g. 65 XOR 42 = 107

numerical.xor = function(a,b) {
  if (a == 0) {
    return(b)
  } else if (b == 0) {
    return (a)
  }
  a.vec = to.base.byte(a)
  b.vec = to.base.byte(b)
  c.vec = as.numeric(xor(a.vec, b.vec))
  return (vec.to.num(to.base.complex(c.vec,2,10)))
}

###- euler totient 1 to n -###
#
# Returns phi(x) for x in 1:n

phi.to.n = function(n) {
  phi = 0:(n-1)
  for (i in 3:)
}

###
###
###- Vectors -###
###
###
###


###- Alphabet -###
#
# The alphabet in Upper Case

alphabet = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z")

###- Digits.s -###
#
# Digits 1 through to 9 as strings

digits.s = c("1", "2", ",3,", "4", "5", "6", "7", "8", "9")

###- Digits0.s -####
#
# Digits 0 through 9 as strings

digits0.s = c("0", "1", "2", ",3,", "4", "5", "6", "7", "8", "9")

###- Digits -###
#
# Digits 1 through to 9

digits = c(1,2,3,4,5,6,7,8,9)

###- Digits0 -####
#
# Digits 0 through 9

digits0 = c(0,1,2,3,4,5,6,7,8,9)