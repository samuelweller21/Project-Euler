source("C:/Users/User/Documents/Project Euler/Useful.R")

for (a in 1:9) {
  for (b in 0:9) {
    for (c in 1:9) {
      for (d in 0:9) {
        if ((10*a+b) < (10*c + d)) {
          if (a == c) {
            if (d != 0) {
              if (((10*a+b)/(10*c + d)) == (b/d)) {
                if (!(b == 0 && d == 0)) { 
                  cat(10*a+b, "/", 10*c+d)
                }
              }
            }
          } else if (a == d) {
            if (c != 0) {
              if (((10*a+b)/(10*c + d)) == (b/c)) {
                if (!(b == 0 && d == 0)) { 
                  cat(10*a+b, "/", 10*c+d, "\n")
                }
              } 
            }
          } else if (b == c) {
            if (d != 0) {
              if (((10*a+b)/(10*c + d)) == (a/d)) {
                if (!(b == 0 && d == 0)) { 
                  cat(10*a+b, "/", 10*c+d, "\n")
                }
              } 
            }
          } else if (b == d) {
            if (c != 0) {
              if (((10*a+b)/(10*c + d)) == (a/c)) {
                if (!(b == 0 && d == 0)) { 
                  cat(10*a+b, "/", 10*c+d, "\n")
                }
              } 
            }
          }
        }
      }
    }
  }
}