
library(gmp)

next_num = function(a,b) {
  return (2*b+a)
}

next_denom = function(a,b) {
  return (a+b)
}

nums = rep(as.bigz(0), 1000)
denoms = rep(as.bigz(0), 1000)
est = rep(as.bigq(0), 1000)

nums[1] = as.bigz(3)
denoms[1] = as.bigz(2)

for (i in 2:1000) {
  nums[i] = next_num(nums[i-1],denoms[i-1])
  denoms[i] = next_denom(nums[i-1],denoms[i-1])
  est[i] = div.bigq(nums[i], denoms[i])
}

s_nums = nchar(as.character(nums))
s_denoms = nchar(as.character(denoms))

sum(s_nums > s_denoms)

plot(est)

print(sum(s_nums > s_denoms))

