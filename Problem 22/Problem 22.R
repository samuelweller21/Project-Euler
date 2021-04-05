setwd("C:/Users/User/Documents/Project Euler/Problem 22")
file = read.csv("Problem 22 File 2.csv", stringsAsFactors = F)

lookup = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z")

sum = 0

for (i in 1:length(file$Name)) {
  word = 0
  if (is.na(file$Name[i])) {
    word = match("N", lookup) + match("A", lookup)
  } else {
    for (l in 1:nchar(file$Name[i])) {
      word = word + match(substr(x = file$Name[i], start = l, stop = l), lookup)
    }
  }
  sum = sum + word*i
}