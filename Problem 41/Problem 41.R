source("C:/Users/User/Documents/Project Euler/Useful.R")

#Nine Digits

nums = 9:1

for (a in nums) {
  for (b in subset(nums, !(nums %in% c(a)))) {
    for (c in subset(nums, !(nums %in% c(a, b)))) {
      for (d in subset(nums, !(nums %in% c(a, b, c)))) {
        for (e in subset(nums, !(nums %in% c(a, b, c, d)))) {
          for (f in subset(nums, !(nums %in% c(a, b, c, d, e)))) {
            for (g in subset(nums, !(nums %in% c(a, b, c, d, e, f)))) {
              for (h in subset(nums, !(nums %in% c(a, b, c, d, e, f, g)))) {
                for (i in subset(nums, !(nums %in% c(a, b, c, d, e, f, g, h)))) {
                    num = 10^8*a + 10^7*b + 10^6*c + 10^5*d + 10^4*e + 10^3*f + 10^2*g + 10^1*h + i
                    if (is.prime(num)) {
                      cat(num, "\n")
                    }
                }
              }
            }
          }
        }
      }
    }
  }
}

#Eight Digits

nums = 8:1

for (a in nums) {
  for (b in subset(nums, !(nums %in% c(a)))) {
    for (c in subset(nums, !(nums %in% c(a, b)))) {
      for (d in subset(nums, !(nums %in% c(a, b, c)))) {
        for (e in subset(nums, !(nums %in% c(a, b, c, d)))) {
          for (f in subset(nums, !(nums %in% c(a, b, c, d, e)))) {
            for (g in subset(nums, !(nums %in% c(a, b, c, d, e, f)))) {
              for (h in subset(nums, !(nums %in% c(a, b, c, d, e, f, g)))) {
                num = 10^7*a + 10^6*b + 10^5*c + 10^4*d + 10^3*e + 10^2*f + 10^1*g + h
                if (is.prime(num)) {
                  cat(num, "\n")
                }
              }
            }
          }
        }
      }
    }
  }
}

#Seven Digits

nums = 7:1

for (a in nums) {
  for (b in subset(nums, !(nums %in% c(a)))) {
    for (c in subset(nums, !(nums %in% c(a, b)))) {
      for (d in subset(nums, !(nums %in% c(a, b, c)))) {
        for (e in subset(nums, !(nums %in% c(a, b, c, d)))) {
          for (f in subset(nums, !(nums %in% c(a, b, c, d, e)))) {
            for (g in subset(nums, !(nums %in% c(a, b, c, d, e, f)))) {
              num = 10^6*a + 10^5*b + 10^4*c + 10^3*d + 10^2*e + 10^1*f + g
              if (is.prime(num)) {
                cat(num, "\n")
              }
            }
          }
        }
      }
    }
  }
}

#Six Digits

nums = 6:1

for (a in nums) {
  for (b in subset(nums, !(nums %in% c(a)))) {
    for (c in subset(nums, !(nums %in% c(a, b)))) {
      for (d in subset(nums, !(nums %in% c(a, b, c)))) {
        for (e in subset(nums, !(nums %in% c(a, b, c, d)))) {
          for (f in subset(nums, !(nums %in% c(a, b, c, d, e)))) {
            num = 10^5*a + 10^4*b + 10^3*c + 10^2*d + 10^1*e + f
            if (is.prime(num)) {
              cat(num, "\n")
            }
          }
        }
      }
    }
  }
}

#Five Digits

nums = 5:1

for (a in nums) {
  for (b in subset(nums, !(nums %in% c(a)))) {
    for (c in subset(nums, !(nums %in% c(a, b)))) {
      for (d in subset(nums, !(nums %in% c(a, b, c)))) {
        for (e in subset(nums, !(nums %in% c(a, b, c, d)))) {
          num = 10^4*a + 10^3*b + 10^2*c + 10^1*d + e
          if (is.prime(num)) {
            cat(num, "\n")
          }
        }
      }
    }
  }
}

#Four Digits

nums = 4:1

for (a in nums) {
  for (b in subset(nums, !(nums %in% c(a)))) {
    for (c in subset(nums, !(nums %in% c(a, b)))) {
      for (d in subset(nums, !(nums %in% c(a, b, c)))) {
        num = 10^3*a + 10^2*b + 10^1*c + d
        if (is.prime(num)) {
          cat(num, "\n")
        }
      }
    }
  }
}


# source("C:/Users/User/Documents/Project Euler/Useful.R")
# 
# primes = fast.gen.primes(99999)
# 
# pan.primes = 0
# 
# prev = 0
# 
# for (p in 1:length(primes)) {
#   if (100*p/length(primes) > prev + 0.1) {
#     prev = round(100*p/length(primes),1)
#     cat(prev, "\n")
#   }
#   str = as.String(primes[p])
#   len = nchar(str)
#   str.vec = rep(NA, len)
#   for (l in 1:len) {
#     str.vec[l] = substr(str, l, l)
#   }
#   if (sum(str.vec %in% digits) == 9) {
#       pan.primes[length(pan.primes) + 1] = p
#   }
# }
# 
# pan.primes = pan.primes[-1]
# 
# cat(max(pan.primes), "\n")