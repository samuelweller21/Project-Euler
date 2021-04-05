#Problem 71
library(Rmpfr)

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

#Use a lower bound of 428571/1000000 since this is < 3/7 and reduced proper

precision = 100

best_so_far = mpfr(428571/1000000,precision)

old_progress = 0

#Only need to check the last few as these will be the most precise (at least statistically)

for (d in 990000:1000000) {
  progress = d/10000
  if (progress > (old_progress + 1)) {
    cat(progress, "%", "\n")
    old_progress = progress
  }
  #Lower bound
  lb = floor(d*asNumeric(best_so_far))
  ub = ceiling(3/7*d)
  for (n in lb:ub) {
    if (n/d < 3/7) {
      if (gcd(n,d) == 1) {
        num = mpfr(n, precision)
        den = mpfr(d, precision)
        new = num/den
        if (new > best_so_far) {
          best_so_far = new
          cat(n, " / ", d, " = ")
          print(best_so_far)
        }
      }
    }
  }
}
