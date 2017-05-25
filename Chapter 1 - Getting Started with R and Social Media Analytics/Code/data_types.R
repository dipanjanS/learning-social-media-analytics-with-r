
# typecasting and checking data types
n <- c(3.5, 0.0, 1.7, 0.0)
typeof(n)

is.numeric(n)

is.double(n)

is.integer(n)

as.integer(n)

as.logical(n)


# complex numbers
comp <- 3 + 4i
typeof(comp)


# factoring nominal variables
size <- c(rep('large', 5), rep('small', 5), rep('medium', 3))
size

size <- factor(size)
size

summary(size)










