source("C:/Users/User/Documents/Project Euler/Useful.R")

words = read.csv("C:/Users/User/Documents/Project Euler/Problem 42/words.csv", stringsAsFactors = F, header = F)$V1

triangles = triangle(max(nchar(words))*26)

total = 0

for (w in 1:length(words)) {
  sum = 0
  for (l in 1:nchar(words[w])) {
    if (is.na(match(substr(words[w],l,l),alphabet))) {
      cat(substr(words[w],l,l), "\n")
    }
    sum = sum + match(substr(words[w],l,l),alphabet)
  }
  if (sum(sum %in% triangles) == 1) {
    total = total + 1
  }
}

cat(total, "\n")