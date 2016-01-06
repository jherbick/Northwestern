
# PREDICT 422 - MACHINE LEARNING LABS


#----------
# CHAPTER 2
#----------

# CREATE A VECTOR
x <- c(1,2,3,4,5)

# Length check
length(x)

# Addition
x <- c(1,6,2)
y <- c(1,4,3)

x+y

# List of objects
ls()

# Remove unwanted objects
rm(x,y)

# Remove all objects at once
rm(list=ls())

# Creating a matrix (creates column matrices by default)
x <- matrix(data = c(1,2,3,4), nrow = 2, ncol = 2)

# To fill matrix by rows
y <- matrix(c(1,2,3,4), 2, 2, byrow = TRUE)

# Square Root (can use on vector or matrix)
z <- sqrt(y)

# Exponents / powers (can use fractions and negative numbers)
zz <- y^2

# Generate a vector of random normal variables (first arg is sample size, n)
# By default, rnorm generates standard normal random variables with a mean=0 and standard dev=1.
# But these values can be altered w mean and sd arguments.
x <- rnorm(50)
y <- x+rnorm(50,mean=50, sd=.1)

# Compute correlation
cor(x,y)

# Set seed to be able to reproduce a random generation
set.seed(1303)
z <- rnorm(50)

# Mean, variance, standard deviation
# square root of variance = standard deviation
# square (^2) of standard deviation gives variance
set.seed(3)
y <- rnorm(100)
mean(y)

var(y)

sqrt(var(y))

sd(y)

(sd(y))^2


# Plotting / graphing
# See ggplot2 notes
x <- rnorm(100)
y <- rnorm(100)
plot(x,y)

plot(x,y, xlab="x axis", ylab = "y axis", main = "TITLE")


# Create a sequence of numbers
x <- seq(1,10)

# Creates a sequence of 10 equally spaced numbers between 0 and 1
seq(0, 1, length=10)

# Shorthand for seq(3,11) for integers
3:11


# Contour Plot, 3 dimensions
# 1. need vector of x values
# 2. need vector of y values
# 3. need a matrix corresponding to the z value for each x,y coordinate.

x <- seq(-pi, pi, length=50)
y <- x
f <- outer(x,y,function(x,y)cos(y)/(1+x^2)) # This creates a 50x50 matrix

contour(x,y,f)
contour(x,y,f, nlevels=45, add=T)

fa <- (f-t(f))/2

contour(x,y,fa,nlevels=15)

# Contour plot with color as third dimension
# This is a heatmap
image(x,y,fa)

# 3 dimensional plot
persp(x, y, fa)

persp(x, y, fa, theta=30)
persp(x, y, fa, theta=30, phi=20)
persp(x, y, fa, theta=30, phi=70)
persp(x, y, fa, theta=30, phi=40)


# Indexing / selecting part of a matrix or vector

A <- matrix(1:16, 4, 4)

A[2,3]   # row, column



