
# Version Control
# C:/R/Git/cmd is the command line directory for running Git command line prompts

# Running files, parts of files
# Highlight a selection and press Ctrl+Enter
# To run an entire file, press Ctrl+Shift+S

#------------------------------------------------
# Read in a .csv file
# Read in the full abalone dataset
fileLocation <- "C:/Users/James R. Herbick/Documents/Northwestern/PREDICT 401/Report 1/abalone.csv"
fullDataSet <- read.table (file = fileLocation, header = TRUE, sep = " ")

# Validate full dataset per Prof. Martin instructions
str(fullDataSet)

# Write a .csv file from a data.frame
d <- data.frame(sampleDataSet, VOLUME=volume)
# Create new dataset with the VOLUME attribute
write.csv(d, "C:/Users/James R. Herbick/Documents/Northwestern/PREDICT 401/Report 1/volume.csv", row.names=F)


#-------------------------------------------------------
# assigning values to variables:
# R does not require variable types to be declared
# variables cannot start with a number or an underscore
# variable names are case sensitive
x <- 2
assign("x", 2)

# Removing a variable
rm(x)

# Four main types of data in R:
# numeric (similar to float or double), character, Date, logical
# Checking the class of a variable
class(x)
is.numeric(x)
is.integer(y)

# assign an integer to a variable, not the default of numeric
y <- 3L

# Characters and factors are the two data types for words
# Characters are case sensitive
z <- "James R. Herbick"

# Finds the length of a string
nchar(z)

# Assigns a factor to a variable
# These are used for categorical variables.
y <- factor("data")

# Dates and Times, Date and POSIXct.
# Date stores only the date, the other stores date and time.
# Stored as numeric values (days or seconds) since 1/1/1970
# Use the lubridate and chron packages for date manipulation
date1 <- as.Date("2015-01-02")
date2 <- as.POSIXct("2015-01-02 09:18")

#Logicals.  True=1, False=0
is.logical(x)

# does 2 equal 3?
2 == 3

# does 2 not equal 3?
2 != 3


#---------------------------------------------------------
# Vectors are a collection of data, all of the same type.
# Vectors cannot be of mixed types.
# Operations are applied to each element of a vector automatically, 
# without the need to loop through the vector.

x <- c(1,2,3,4,5)
x * 3

# raise to a power
x^2

# Find the square root of the vector
sqrt(x)

# Shortcut to create a range of numbers from 1 to 10
1:10

# If two vectors are of equal length, you can perform operations on the two, element 
# by element

a <- 1:10
b <- -5:4

a + b
# Can do the same with multiplication, raise to a power (^) etc.

# Comparisons also work on vectors.  Result is TRUE or FALSE for each element of the vector.
a <= 5

# Use all to check if all elements of a vector meet a criteria
# Use any to check if any of the elements of a vector meet a criteria
all(a < 5)
any(a < 5)

# Retrieving elements of a vector
a[1]     # retrieves the first element from vector a
a[1:3]   # retrieves consecutive elements from vector a
a[c(1,4)]# retrieves non consecutive elements from vector a

# You can provide names to elements of a vector in 2 ways:
c(one = "a", two = "b")

# Or, create a vector, then create the names for each element
w <- 1:3
names(w) <- c("a", "b", "c")

# Factor vectors
# Convert a vector to a factor vector
# The levels of a factor are the unique values of that factor variable
q2 <- c(1:10)
q2Factor <- as.factor(q2)

# Setting the ordered argument to TRUE creates an ordered factor
factor(x=c("High School", "College", "Masters", "Doctorate"), levels=c("High School", "College", "Masters", "Doctorate"), ordered=TRUE)

# Two special vectors
letters  # all lowercase letters a-z
LETTERS  # all uppercase letters a-z


#-------------------------------------------------
# Calling functions
# More difficult functions, you can enter arguments in order, or
# use argument name=
mean(x)   # takes a vector as an argument

# To get help on functions
?mean
?'+'

# If you only know part of a function use apropos
apropos("mea")


# Missing Data
# R has two types of missing data: NA and NULL
# NA is for missing data
# Test each element of a vector for missingness
is.na(z)

# For handling missing data look at mi, mice, and Amelia packages

# NULL is the absence of anything.  NULL cannot exist within a vector
# Test for NULL
is.NULL(d)


#---------------------------------------
# Advanced data structures
# data.frame, matrix, list, array
# data.frame is like an excel spreadsheet, has columns and rows
# each column of a data.frame is a vector
x <- 10:8
y <- -4:-2
q <- c("Hockey", "Football", "soccer")
theDF <- data.frame(x,y,q)

# Assign names to the columns
theDF <- data.frame(First=x, Second=y, Sport=q)

# Check the number of rows and columns
nrow(theDF)
ncol(theDF)
dim(theDF)  # number of rows and columns of the data.frame

# Check the column names of the data.frame
names(theDF)

# Check and assign row names of a data.frame
rownames(theDF)
rownames(theDF) <- c("One", "Two", "Three")

# Print just a few rows of a data.frame to the screen
head(theDF)
head(theDF, n=7)
tail(theDF)

class(theDF)  # tells you the class type

# ** Accessing columns of a data.frame **
theDF$Sport   # pick a column by name
theDF[,c("First", "Sport")]  # get multiple columns by name
theDF[,3]     # specify no row, 3rd column
theDF[2,]     # specify the second row, all columns
theDF[2,3]    # specify the value from the second row, third column
theDF[2,1:2]  # picks the second row, first and second columns

# To ensure the return value is a single-column data.frame
# use the drop=FALSE option
theDF[,3,drop=FALSE]   # no rows specified, 3rd column

# Calculate on base percentage
# with allows us to specify columns of a data.frame without using
# the data.frame name each time
baseball$OBP <- with(baseball, (h+bb+hbp)/(ab+bb+hbp+sf))



#----------------------------------------
# Lists, store any number of items of any data type
list(1,2,3,"four")

# Create a single element list where the only element is a vector
list(c(1,2,3))

# Create a 2-element list, first element is a vector, second is a vector
(list3 <- list(c(1,2,3), 3:7))  # enclosing in () prints the list

### look into this more


#----------------------------------------
# Matrices
# Every single element must be the same type, normally numeric
# Matrices act similar to vectors with element by element addition, subtraction, etc.
# The nrow, ncol, and dim functions work just like for data.frames

# creates a 5x2 matrix (5 rows, 2 columns)
a <- matrix(1:10, nrow=5)
nrow(a)
ncol(a)
dim(a)

# transpose a matrix
t(a)

# matrix multiplication
a %*% b  # don't know if we need the %%% signs


#---------------------------------------------
# Plotting using ggplot2
# ~ separating values indicates that you are viewing say price against carat
# where price is the y value and carat is the x value
# plot(price~carat, data=diamonds)  this is a base plot, not ggplot2
# Most important component of ggplot2 is aes which determines the data that gets
# mapped to which axis

# Saves the basis of a ggplot to a variable.  Then can add layers on top
# Breaks RATE up by the TYPE attribute, creates boxplots
g <- ggplot(fullDataSet, aes(x = TYPE, y = RATE))
g + geom_boxplot(outlier.colour="blue")



#-------------------------------------
# Group Manipulation
# plyr
# ddply - takes a data.frame, splits it according to some variables,
# performs a desired action, and returns a data.frame

# Sets the sf column to 0 for any row where year < 1954
# This uses baseball data from the plyr package
baseball$sf[baseball$year < 1954] <- 0

# Only keeps players with at least 50 at bats for a season
baseball2 <- baseball[baseball$ab >= 50, ]

# Calculate on base percentage
# with allows us to specify columns of a data.frame without using
# the data.frame name each time
baseball$OBP <- with(baseball, (h+bb+hbp)/(ab+bb+hbp+sf))

# aggregate
aggregate(price ~ cut,diamonds, each(mean,median))


#------------------------------------------
# Simple Linear Regression

# Read in a .csv file
# Read in the full dataset
fileLocation <- "C:/Users/James R. Herbick/Documents/Northwestern/R Notes/Triola Data Sets/FOOT.csv"
fullDataSet <- read.table (file = fileLocation, header = TRUE, sep = ",")

# Plot the regression line using data for data in table 10-1 Triola (used full data set)
ggplot(fullDataSet, aes(x=Shoe.Print, y=Height)) + geom_point() + geom_smooth(method="lm") + labs(x="Shoe Print", y="Height")

# Calculate regression statistics
# First argument (Height) is the response, Second argument is the predictor
HeightLM <- lm(Height ~ Shoe.Print, data=fullDataSet)
HeightLM

# Show additional statistics
summary(HeightLM)






# Writing data to text / SAS datastep
# Need data to be a dataframe
# Writes data to the R working directory
a <- data.frame(beer)
write.foreign(a, "test.txt", "testcode.sas", package="SAS")








