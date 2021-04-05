library(stringr)
words.letters = c("ONE", "TWO", "THREE", "FOUR", "FIVE", "SIX", "SEVEN", "EIGHT", "NINE")
words.letters2 = c("ONE", "TWO", "THREE", "FOUR", "FIVE", "SIX", "SEVEN", "EIGHT", "NINE", 
             "TEN", "ELEVEN", "TWELVE", "THIRTEEN", "FOURTEEN", "FIFTEEN", "SIXTEEN", "SEVENTEEN", "EIGHTEEN", "NINETEEN")
words.tens = c("NULL", "TWENTY", "THIRTY", "FORTY", "FIFTY", "SIXTY", "SEVENTY", "EIGHTY", "NINETY")

#Writes numbers up to 1,000
write.num = function(x) {
  
  if (x > 999) {
    cat("ONE THOUSAND", "\n");
  } else if (x > 99) {
    num = as.integer(substr(toString(x), 1, 1))
    cat(words.letters[num], " HUNDRED ")
    if (!(x %% 100 == 0)) {
      cat("AND ")
    }
    if (x %% 100 > 19) {
      cat(words.tens[as.integer(substr(toString(x), 2, 2))], " ")
      cat(words.letters[as.integer(substr(toString(x), 3, 3))], "\n")
    } else {
      cat(words.letters2[as.integer(substr(toString(x), 2, 3))], "\n")
    }
  } else if (x > 19) {
      cat(words.tens[as.integer(substr(toString(x), 1, 1))], " ")
      cat(words.letters[as.integer(substr(toString(x), 2, 2))], "\n")
  } else {
      cat(words.letters2[x], "\n")
  }
}

#Returns string of number

num.to.string = function(x) {
  temp = ""
  if (x > 999) {
    temp = paste0(temp, paste0("ONE THOUSAND"))
  } else if (x > 99) {
    num = as.integer(substr(toString(x), 1, 1))
    temp = paste0(temp, paste0(words.letters[num], " HUNDRED "))
    if (!(x %% 100 == 0)) {
      temp = paste0(temp, paste0("AND "))
    }
    if (x %% 100 > 19) {
      temp = paste0(temp, paste0(words.tens[as.integer(substr(toString(x), 2, 2))], " "))
      temp = paste0(temp, paste0(words.letters[as.integer(substr(toString(x), 3, 3))], ""))
    } else {
      temp = paste0(temp, paste0(words.letters2[as.integer(substr(toString(x), 2, 3))], ""))
    }
  } else if (x > 19) {
    temp = paste0(temp, paste0(words.tens[as.integer(substr(toString(x), 1, 1))], " "))
    temp = paste0(temp, paste0(words.letters[as.integer(substr(toString(x), 2, 2))], ""))
  } else {
    temp = paste0(temp, paste0(words.letters2[x], ""))
  }
  return(temp)
}

#Writes numbers up to 1,000 and counts the letters
write.num.total = function(x, running_total) {
  
  if (x > 999) {
    #cat("ONE THOUSAND", "\n");
    running_total = running_total + 11
  } else if (x > 99) {
    num = as.integer(substr(toString(x), 1, 1))
    cat(words.letters[num], " HUNDRED ")
    running_total = running_total + nchar(words.letters[as.integer(substr(toString(x), 1,1))])
    if (!(x %% 100 == 0)) {
      #cat("AND ")
      running_total = running_total + 3
    }
    if (x %% 100 > 19) {
      #cat(words.tens[as.integer(substr(toString(x), 2, 2))], " ")
      running_total = running_total + nchar(words.tens[as.integer(substr(toString(x), 2, 2))])
      #cat(words.letters[as.integer(substr(toString(x), 3, 3))], "\n")
      running_total = running_total + nchar(words.letters[as.integer(substr(toString(x), 3, 3))])
    } else {
      #cat(words.letters2[as.integer(substr(toString(x), 2, 3))], "\n")
      running_total = running_total + nchar(words.letters2[as.integer(substr(toString(x), 2, 3))])
    }
  } else if (x > 19) {
    #cat(words.tens[as.integer(substr(toString(x), 1, 1))], " ")
    running_total = running_total + nchar(words.tens[as.integer(substr(toString(x), 1, 1))])
    #cat(nchar(words.tens[as.integer(substr(toString(x), 1, 1))]), "\n")
    #cat(words.letters[as.integer(substr(toString(x), 2, 2))], "\n")
    running_total = running_total + ifelse(identical(numeric(0), nchar(words.letters[as.integer(substr(toString(x), 2, 2))])), 0,  nchar(words.letters[as.integer(substr(toString(x), 2, 2))]))
    #cat(nchar(words.letters[as.integer(substr(toString(x), 2, 2))]), "\n")
  } else {
    #cat(words.letters2[x], "\n")
    running_total = running_total + nchar(words.letters2[x])
  }
  return (running_total)
}

# for (i in 1:1000) {
#   write.num(i)
#   write.table(write.num(i))
# }

running_total = 0
vec = ""
for (i in 1:1000) {

  running_total = write.num.total(i, running_total)

  # cat(i, ": ", running_total, "\n")
  # cat(num.to.string(i))
  
  vec[length(vec)+1] = num.to.string(i)

}

#print(vec)

#Now count how many letters ... 

running_total = 0
total_spaces = 0
for (i in 1:length(vec)) {
  
  vec <- vec
  running_total = running_total + nchar(vec[i])
  total_spaces = total_spaces + str_count(vec[i], " ")
  
}

total = running_total - total_spaces
total



