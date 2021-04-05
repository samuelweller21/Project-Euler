source("C:/Users/User/Documents/Project Euler/Useful.R")

str = ""

for (i in 1:200000) {
  str = paste0(str, toString(i))
}

prod = 1

for (i in 1:7) {
  prod = prod * as.integer(substr(str, 1*10^(i-1), 1*10^(i-1)))
}

cat(prod, "\n")
