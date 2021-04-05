stringtonum = function(x) {
  sum = 0
  for (i in 1:nchar(x)) {
      sum = sum + as.numeric(substr(x, i, i))*(10^(nchar(x) - i))
  }
  return(sum)
}

x = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")

library(combinat)

perms = permn(x)
perms.collate = rep(NA, length(perms))
for (i in 1:length(perms)) {
  string = ""
  perm = perms[[i]]
  for (j in 1:length(perm)) {
    string = paste(string,perm[j], sep = "")
  }
  perms.collate[i] = stringtonum(string)
}

perms.collate = perms.collate[order(perms.collate)]