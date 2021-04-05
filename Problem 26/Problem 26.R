
debug = T

generate.cycle = function(d, l = 10) {
  n = 1
  cycle = rep(NA, l)
  for (i in 1:l) {
    r = (10*n) %% d
    k = (10*n - r)/d
    n = 10*n - k*d
    cycle[i] = k
  }
  return(cycle)
}

find.cycle.length = function(cycle, debug = T) {
  if (cycle[length(cycle)] == 0) {
    return (-1)
  } else {
    for (j in 1:(length(cycle)-1)) {
      for (i in 0:(length(cycle)-2)) {
        short.cycle = cycle[j:(j+i)]
        if (debug == T) {
          cat("j", " - ", j, "\n")
          cat("i", " - ", i, "\n")
        }
        rem = (length(cycle) - j + 1) %% length(short.cycle)
        rep = (length(cycle) - j + 1 - rem)/length(short.cycle)
        if (rem == 0) {
          if( j != 1) {
            actual.cycle = c(cycle[1:(j-1)], rep(short.cycle,rep))
          } else {
            actual.cycle = rep(short.cycle,rep)
          }
        } else {
          if (j != 1) {
            actual.cycle = c(cycle[1:(j-1)], rep(short.cycle, rep), short.cycle[1:rem])
          } else {
            actual.cycle = c(rep(short.cycle, rep), short.cycle[1:rem])
          }
        }
        if (debug == T) {
          print(actual.cycle)
          print(cycle)
        }
        if (all(actual.cycle == cycle)) {
          if (i > (length(cycle)/2)) {
            
          } else {
            return(length(short.cycle))
          }
        }
      }
    }
  }
}

recurrence = rep(NA, 999)
maximum.recurrence = 10000

for (i in 1:length(recurrence)) {
  recurrence[i] = find.cycle.length(generate.cycle(i,maximum.recurrence), debug = F)
  cat(100*i/999, "%", "\n")
  if (recurrence[i] > 100) {
    cat(i, " - ", recurrence[i], "\n")
  }
}
plot(recurrence)
