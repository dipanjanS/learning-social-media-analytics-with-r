
## vectors
1:5

c(1,2,3,4,5)

seq(1,5)

seq_len(5)

# assigning two vectors to variables
x <- 1:5
y <- c(6,7,8,9,10)

# operating on vectors
x + y

sum(x)

mean(x)

x * y

sqrt(x)

# indexing and slicing
y[2:4]

y[c(2,3,4)]

# naming vector elements
names(x) <- c("one", "two", "three", "four", "five")
x


## arrays

# create a three-dimensional array
three.dim.array <- array(
  1:12,    # input data
  dim = c(2, 2, 3),   # dimensions
  dimnames = list(    # names of dimensions
    c("row1", "row2"),
    c("col1", "col2"),
    c("first.set", "second.set", "third.set")
  )
)

# view the array
three.dim.array


## matrices

# create a matrix
mat <- matrix(
  1:12,   # data
  nrow = 4,  # num of rows
  ncol = 3,  # num of columns
  byrow = TRUE  # fill the elements row-wise
)

# view the matrix
mat

# initialize matrices
m1 <- matrix(
  1:9,   # data
  nrow = 3,  # num of rows
  ncol = 3,  # num of columns
  byrow = TRUE  # fill the elements row-wise
)
m2 <- matrix(
  10:18,   # data
  nrow = 3,  # num of rows
  ncol = 3,  # num of columns
  byrow = TRUE  # fill the elements row-wise
)

# matrix addition
m1 + m2

# matrix transpose
t(m1)

# matrix product
m1 %*% m2


## lists

# create sample list
list.sample <- list(
  nums = seq.int(1,5),
  languages = c("R", "Python", "Julia", "Java"),
  sin.func = sin
)

# view the list
list.sample

# accessing individual list elements
list.sample$languages

list.sample$sin.func(1.5708)

# initializing two lists
l1 <- list(nums = 1:5)
l2 <- list(
  languages = c("R", "Python", "Julia"),
  months = c("Jan", "Feb", "Mar")
)

# check lists and their type
l1

typeof(l1)

# concatenating lists
l3 <- c(l1, l2)
l3

# converting list back to a vector
v1 <- unlist(l1)
v1

typeof(v1)


## data frames

# create data frame
df <- data.frame(
  name = c("Wade", "Steve", "Slade", "Bruce"),
  age = c(28, 85, 55, 45),
  job = c("IT", "HR", "HR", "CS")
)

# view the data frame
df

# examine data frame properties
class(df)

str(df)

rownames(df)

colnames(df)

dim(df)

# initialize two data frames
emp.details <- data.frame(
  empid = c('e001', 'e002', 'e003', 'e004'),
  name = c("Wade", "Steve", "Slade", "Bruce"),
  age = c(28, 85, 55, 45)
)
job.details <- data.frame(
  empid = c('e001', 'e002', 'e003', 'e004'),
  job = c("IT", "HR", "HR", "CS")
)

# view data frames
emp.details

job.details

# binding and merging data frames
cbind(emp.details, job.details)

merge(emp.details, job.details, by='empid')

# subsetting data frame
subset(emp.details, age > 50)






















