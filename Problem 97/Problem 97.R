library(gmp)

num = 1

for (i in 1:7830457) {
  num = (num * 2) %% (10^10)
}
num = (num * 28433) + 1
num = num %% 10^10
print(num)