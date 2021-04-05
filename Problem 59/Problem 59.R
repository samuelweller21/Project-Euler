file = "C:\\Users\\User\\Documents\\Programming\\Maths\\Project Euler\\Problem 59\\ciphertext.csv"

source("C:/Users/User/Documents/Programming/Maths/Project Euler/Useful.R")

library(gtools)

file = file(file)

text = read.csv(file)
text = as.numeric(text$Code)

possible.messages = rep(NA, 21952)
good.messages = c()
good.indicator = c("Hello", "hello", "and")
k=1
i = 1
old_progress = 0

for (a in 97:122) {
  for (b in 97:122) {
    for (c in 97:122) {
      progress = 100*i/21952
      if (progress > old_progress + 0.1) {
        cat(progress, "%", "\n")
        old_progress = progress
      }
      small.vec = c(a,b,c)
      key = rep(small.vec, length(text))
      key = key[1:length(text)]
      decryption = rep(NA, length(key))
      for (j in 1:length(key)) {
        #cat("i: ", i, " - ", text[j], " - ", key[j], "\n")
        decryption[j] = numerical.xor(text[j], key[j])
      }
      possible.messages[i] = vec.to.string(chr(decryption))
      for (l in 1:length(good.indicator)) {
        if (grepl(good.indicator[l], possible.messages[i])) {
          good.messages[k] = possible.messages[i]
          cat("Found a good message: ", "\n", good.messages[k], "\n", "I've stored it in good.messages at ", k, "\n")
          k = k + 1
          break
        }
      }
      i = i + 1
    }
  }
}

write.csv(good.messages, "C:\\Users\\User\\Documents\\Programming\\Maths\\Project Euler\\Problem 59\\output.csv")

vec = string.to.vec(message)
sum = 0
for (i in 1:length(vec)) {
  sum = sum + asc(vec[i])
}
print(sum)

