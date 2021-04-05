source("C:/Users/User/Documents/Programming/Maths/Project Euler/Useful.R")

iterate = function(x) {
  vec = num.to.vec.fast(x)
  sum = 0
  for (i in 1:length(vec)) {
    sum = sum + vec[i]^2
  }
  return (sum)
  
}

tm = 10000000

book = rep(NA,tm)
book[1] = 1
book[89] = 89
progress = 0
old_progress = 0
for (i in 1:tm) {
  progress = 100*i/tm
  if (progress > old_progress + 0.1) {
    cat(progress, "%", "\n")
    old_progress = progress
  }
  j = i
  while (TRUE) {
    j = iterate(j)
    if (!is.na(book[j])) {
      book[i] = book[j]
      break
    }
  }
}

sum(book == 89)
