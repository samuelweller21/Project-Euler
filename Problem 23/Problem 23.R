abundent.nums = 12

actual = 28123

for (i in 13:actual) {
  
  div.sum = 0
  for (d in 1:(i-1)) {
    if (i %% d == 0) {
      div.sum = div.sum + d
    }
  }
  
  if (div.sum > i) {
    abundent.nums[length(abundent.nums) + 1] = i
  }
  
}

abundent.sums = 24

for (i in 1:length(abundent.nums)) {
  for (j in i:length(abundent.nums)) {
    abundent.sums[length(abundent.sums) + 1] = abundent.nums[i] + abundent.nums[j]
  }
}

abundent.sums.unique = unique(abundent.sums)

not.a.sum = 0

for (i in 1:actual) {
  if (!(i %in% abundent.sums.unique)) {
    not.a.sum = not.a.sum + i
  }  
}

