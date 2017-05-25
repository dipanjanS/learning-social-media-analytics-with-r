
## looping constructs

# for loop
for (i in 1:5){
  cat(paste(i," "))
}

sum <- 0
for (i in 1:10){
  sum <- sum + i
}
sum

# while loop
n <- 1
while (n <= 5){
  cat(paste(n, " "))
  n <- n + 1
}

# repeat loop
i <- 1
repeat{
  cat(paste(i, " "))
  if (i >= 5){
    break  # break out of the infinite loop
  }
  i <- i + 1
}



## conditional constructs

# using if
num = 10
if (num == 10){
  cat('The number was 10')
}

# using if-else
num = 5
if (num == 10){
  cat('The number was 10')
} else{
  cat('The number was not 10')
}

# using if-else if-else
if (num == 10){
  cat('The number was 10')
} else if (num == 5){
  cat('The number was 5')
} else{
  cat('No match found')
}

# using ifelse(...) function
ifelse(num == 10, "Number was 10", "Number was not 10")

# using switch(...) function
for (num in c("5","10","15")){
  cat(
    switch(num,
           "5" = "five",
           "7" = "seven",
           "10" = "ten",
           "No match found"
    ), "\n")
}













