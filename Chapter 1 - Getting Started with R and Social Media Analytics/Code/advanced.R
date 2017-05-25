
## apply

# creating a 4x4 matrix
mat <- matrix(1:16, nrow=4, ncol=4)

# view the matrix
mat

# row sums
apply(mat, 1, sum)

rowSums(mat)

# row means
apply(mat, 1, mean)

rowMeans(mat)

# col sums
apply(mat, 2, sum)

colSums(mat)

# col means
apply(mat, 2, mean)

colMeans(mat)

# row quantiles
apply(mat, 1, quantile, probs=c(0.25, 0.5, 0.75))


## lapply

# create and view a list of elements
l <- list(nums=1:10, even=seq(2,10,2), odd=seq(1,10,2))
l

# use lapply on the list
lapply(l, sum)


## sapply

# create and view a sample list
l <- list(nums=1:10, even=seq(2,10,2), odd=seq(1,10,2))
l

# observe differences between lapply and sapply
lapply(l, mean)

typeof(lapply(l, mean))

sapply(l, mean)

typeof(sapply(l, mean))


## tapply

data <- 1:30
data

groups <- gl(3, 10)
groups

tapply(data, groups, sum)

tapply(data, groups, sum, simplify = FALSE)


## mapply

list(rep(1,4), rep(2,3), rep(3,2), rep(4,1))

mapply(rep, 1:4, 4:1)


