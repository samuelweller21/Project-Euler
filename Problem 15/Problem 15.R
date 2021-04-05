my.recursion = function(a, b, c) {
  if (a == 1) {
    return (b + 1)
  } else if (b == 1) {
    return (2 + a - 1)
  } else if (a < c && b < c) {
    return(global.table[a,b])
  } else{
    return (my.recursion(a-1,b, c) + my.recursion(a,b-1, c))
  }
}

global.table = matrix(c(2,3,3,6), nrow = 2, ncol = 2)

size = 10

for (s in 1:size) {
  table = matrix(NA, nrow = s, ncol = s)
  
  for (i in 1:s) {
    for (j in 1:s) {
      table[i,j] = my.recursion(i,j,s)
    }
  }
  
  assign("global.table", table, envir = .GlobalEnv)
}
