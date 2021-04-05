
max.p = 1000

num = rep(0, max.p)

for (p in 1:max.p) {
  bound = floor(p/(2+2*sqrt(2)))
  #cat(bound, "\n")
  if (bound != 0) {
    for (a in 1:bound) {
      b = (p^2-2*p*a)/(2*p-2*a)
      if (floor(b) == b) {
        num[p] = num[p] + 1
      }
    }
  }
}

pos = match(max(num), num)