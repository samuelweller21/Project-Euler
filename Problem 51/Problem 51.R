source("C:/Users/User/Documents/Project Euler/Useful.R")

library(combinat)

#Master function

try.function = function(digits, family) {
  x = digits
  
  #Generate all permutations it can be in
  #Iterate over number of digits remaining the same
  perm.list = list()
  for (i in 1:(x-1)) {
    perms = c(rep(0, i), rep(1, x - i))
    perms2 = permn(perms)
    perm.list = c(perm.list, perms2)
    perm.list = unique(perm.list)
  }
  
  #Now we iterate over our choice of permutations
  
  for (i in 1:length(perm.list)) {
    
    cat(i, " of ", length(perm.list), "\n")
    
    #Generate all x digit numbers
    nums = gen.primes(10^x-1)
    nums = subset(nums, nums >= 10^(x-1))
    
    while (length(nums) >= 1) {
      
      #cat(j, " of ", length(nums), "\n")
      
      family_size = 0
      failures = nums[1]
      
      for (k in 0:9) {
        new = (number.inserter(nums[1], k, perm.list[[i]]))
        if (new %in% nums) {
          family_size = family_size + 1
          failures = c(failures, new)
        }
        
      }
     
      if (family_size >= family) {
        
        cat("We got em!", "\n")
        cat("With:", "\n")
        print(nums[1])
        print(perms2[i])
        break
        
      } else {
        #Remove failures
        nums = nums[!(failures == nums)]
      }
       
    }
    
  }
  
}

number.inserter = function(j, k, perm) {
  
  #Takes a multi digit number j, a permutation perm and a digit k and replaces the 
  #1's of the number and turns them into the digit k
  
  vec = num.to.vec(j)
  
  for (i in 1:length(perm)) {
    
    if (perm[i] == 1) {
      vec[i] = k
    }
    
  }
  
  return(vec.to.num(vec))
  
}

try.function(digits = 7, family = 8)



