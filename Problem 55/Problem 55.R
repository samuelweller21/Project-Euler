library(gmp)
source("C:/Users/User/Documents/Programming/Maths/Project Euler/Useful.R")

counter = 0

for (i in 1:9999) {
  
  num = as.bigz(i)
  reversed.string = reverse.string(as.character(num))
  #Check for leading 0
  while (substr(reversed.string, 1,1) == "0") {
    reversed.string = substr(reversed.string,2,nchar(reversed.string))
  }
  reversed.big = as.bigz(reversed.string)
  num = num + reversed.big
  j = 1
  
  while (j < 50) {
    #cat("i: ", i, "\n")
    #print(as.character(num))
    if (is.palindrome.string(as.character(num))) {
      break
    }
    reversed.string = reverse.string(as.character(num))
    
    #Check for leading 0
    while (substr(reversed.string, 1,1) == "0") {
      reversed.string = substr(reversed.string,2,nchar(reversed.string))
    }
    
    reversed.big = as.bigz(reversed.string)
    num = num + reversed.big
    j = j + 1
  }
  
  if (j == 50) {
    counter = counter + 1
    cat("i: ", i, " - ", as.character(num), "\n")
  }
  
}
