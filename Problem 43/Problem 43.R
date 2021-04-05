n = 1
pal = NA
nums = c("0","1","2","3","4","5","6","7","8","9")
for (a in nums) {
  cat(a, "\n")
  for (b in subset(nums, !(nums %in% c(a)))) {
    for (c in subset(nums, !(nums %in% c(a,b)))) {
      for (d in subset(nums, !(nums %in% c(a,b,c)))) {
        for (e in subset(nums, !(nums %in% c(a,b,c,d)))) {
          for (f in subset(nums, !(nums %in% c(a,b,c,d,e)))) {
            for (g in subset(nums, !(nums %in% c(a,b,c,d,e,f)))) {
              for (h in subset(nums, !(nums %in% c(a,b,c,d,e,f,g)))) {
                for (i in subset(nums, !(nums %in% c(a,b,c,d,e,f,g,h)))) {
                  for (j in subset(nums, !(nums %in% c(a,b,c,d,e,f,g,h,i)))) {
                    pal[n] = paste0(a,b,c,d,e,f,g,h,i,j)
                    n = n + 1
                  } 
                } 
              } 
            } 
          } 
        } 
      } 
    } 
  }
}

sum = 0
prev = 0

for (i in 1:length(pal)) {
  if (100*i/length(pal) > prev + 0.1) {
    prev = 100*i/length(pal)
    cat(prev, "%", "\n")
  }
  if (as.double(paste0(substr(pal[i],2,2), substr(pal[i],3,3), substr(pal[i],4,4))) %% 2 == 0) {
    if (as.double(paste0(substr(pal[i],3,3), substr(pal[i],4,4), substr(pal[i],5,5))) %% 3 == 0) {
      if (as.double(paste0(substr(pal[i],4,4), substr(pal[i],5,5), substr(pal[i],6,6))) %% 5 == 0) {
        if (as.double(paste0(substr(pal[i],5,5), substr(pal[i],6,6), substr(pal[i],7,7))) %% 7 == 0) {
          if (as.double(paste0(substr(pal[i],6,6), substr(pal[i],7,7), substr(pal[i],8,8))) %% 11 == 0) {
            if (as.double(paste0(substr(pal[i],7,7), substr(pal[i],8,8), substr(pal[i],9,9))) %% 13 == 0) {
              if (as.double(paste0(substr(pal[i],8,8), substr(pal[i],9,9), substr(pal[i],10,10))) %% 17 == 0) {
                sum = sum + as.double(pal[i])
              }
            }
          }
        }
      }
    }
  }
}

cat(sum, "\n")