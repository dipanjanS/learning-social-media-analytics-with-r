
## built-in functions
sqrt(7)

mean(1:5)

sum(1:5)

sqrt(1:5)

runif(5)

rnorm(5)


## user-defined functions

# define the function
square <- function(data){
  return (data^2)
}

# inspect function components
environment(square)

body(square)

# execute the function on data
square(1:5)

square(12)





