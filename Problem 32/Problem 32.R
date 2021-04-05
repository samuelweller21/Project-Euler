
v = c("1", "2", "3", "4", "5", "6", "7", "8", "9")

max = 10000
sum = 0

for (i in 1:max) {
  for (m in 2:(i/2)) {
    if (i %% m == 0) {
      n = i / m
      #Form vector
      vec = rep(NA, nchar(toString(m)) + nchar(toString(n)) + nchar(toString(i)))
      
      for (l in 1:nchar(toString(m))) {
        vec[l] = substr(toString(m), l, l)
      }
      for (l in 1:nchar(toString(n))) {
        vec[l + nchar(toString(m))] = substr(toString(n), l, l)
      }
      for (l in 1:nchar(toString(i))) {
        vec[l + nchar(toString(m)) + nchar(toString(n))] = substr(toString(i), l, l)
      }
      if (all(length(unique(vec)) == length(vec))) {
        if (sum(vec %in% v) == 9) {
          cat(toString(m), "x", toString(n), "=", toString(i), "\n")
          sum = sum + i
          break
        }
      }
    }
  }
}
cat("Total:", sum, "\n")