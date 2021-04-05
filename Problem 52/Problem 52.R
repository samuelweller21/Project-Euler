source("C:/Users/User/Documents/Project Euler/Useful.R")

limit = 10000000

for (i in 1:limit) {
  
  if (length(num.to.vec(6*i)) > length(num.to.vec(i))) {
    next
  }
  
  #Convert to vecs
  i.vec = num.to.vec(i)
  i.vec2 = num.to.vec(2*i)
  i.vec3 = num.to.vec(3*i)
  i.vec4 = num.to.vec(4*i)
  i.vec5 = num.to.vec(5*i)
  i.vec6 = num.to.vec(6*i)
  
  #Order
  i.vec = i.vec[order(i.vec)]
  i.vec2 = i.vec2[order(i.vec2)]
  i.vec3 = i.vec3[order(i.vec3)]
  i.vec4 = i.vec4[order(i.vec4)]
  i.vec5 = i.vec5[order(i.vec5)]
  i.vec6 = i.vec6[order(i.vec6)]
  
 if (sum(i.vec == i.vec2) == length(i.vec)) {
   if (sum(i.vec == i.vec3) == length(i.vec)) {
     if (sum(i.vec == i.vec4) == length(i.vec)) {
       if (sum(i.vec == i.vec5) == length(i.vec)) {
         if (sum(i.vec == i.vec6) == length(i.vec)) {
           print(i)
         }
       }
     }
   }
 } 
}