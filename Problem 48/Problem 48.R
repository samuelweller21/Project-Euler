eval = function(i, m) {
  #Evaluates i^i modulus m
  sum = i
  for (j in 1:(i-1)) {
   sum = sum*i
   sum = sum %% m
  }
  return(sum)
}

sum = 0
m = 10^10

for (i in 1:1000) {
  sum = sum + eval(i, m)
  sum = sum %% m
}

cat(sum, "\n")