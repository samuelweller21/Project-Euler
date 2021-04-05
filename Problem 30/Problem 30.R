max = 200000

return.sum = function(x, p) {
  sum = 0
  for (j in 1:nchar(toString(i))) {
    l = as.integer(substr(toString(i), j,j))^p
    sum = sum + l
  }
  return (sum)
}

big.sum = 0

for (i in 1:max) {
  sum = return.sum(i,5)
  if (sum == i) {
    cat(i, "\n")
    big.sum = big.sum + sum
  }
}

#Minus 1

cat("Total: ", big.sum, "\n")