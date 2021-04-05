library(stringr)

file = file("C:\\Users\\User\\Documents\\Programming\\Maths\\Project Euler\\Problem 67\\Problem 67.txt")

lines = readLines(file)
i = 100
#Convert to vec

current_line = lines[i]
empty_locations = str_locate(current_line, " ")[1,1]
vec_of_line = c(as.numeric(str_sub(current_line, 1, empty_locations-1)))
current_line = str_sub(current_line, empty_locations + 1, nchar(current_line))
empty_locations = str_locate(current_line, " ")[1,1]
while (!is.na(empty_locations)) {
  vec_of_line = c(vec_of_line, as.numeric(str_sub(current_line, 1, empty_locations-1)))
  current_line = str_sub(current_line,empty_locations + 1, nchar(current_line))
  empty_locations = str_locate(current_line, " ")[1,1]
}
vec_of_line = c(vec_of_line, as.numeric(current_line))

#Generate a new vector of length 1 less with the max of adjacent numbers

max_line = rep(NA, length(vec_of_line))
for (i in 1:length(max_line)) {
  max_line[i] = max(vec_of_line[i], vec_of_line[i+1])
}
i = 99
vec_of_line = c()

while (i > 1) {
  print(i)
  #Convert to vec
  
  current_line = lines[i]
  empty_locations = str_locate(current_line, " ")[1,1]
  vec_of_line = c(as.numeric(str_sub(current_line, 1, empty_locations-1)))
  current_line = str_sub(current_line, empty_locations + 1, nchar(current_line))
  empty_locations = str_locate(current_line, " ")[1,1]
  while (!is.na(empty_locations)) {
    vec_of_line = c(vec_of_line, as.numeric(str_sub(current_line, 1, empty_locations-1)))
    current_line = str_sub(current_line,empty_locations + 1, nchar(current_line))
    empty_locations = str_locate(current_line, " ")[1,1]
  }
  vec_of_line = c(vec_of_line, as.numeric(current_line))
 
  #Generate a new row with old max line values added
  
  for (j in 1:length(vec_of_line)) {
    vec_of_line[j] = vec_of_line[j] + max_line[j]
  }
  
  for (j in 1:length(max_line)) {
    max_line[j] = max(vec_of_line[j], vec_of_line[j+1])
  }
  
  i = i - 1
  print(max(vec_of_line+59))
  vec_of_line = c()
}



