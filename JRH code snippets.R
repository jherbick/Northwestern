
# Version Control
# C:/R/Git/cmd is the command line directory for running Git command line prompts

# Running files, parts of files
# Highlight a selection and press Ctrl+Enter
# To run an entire file, press Ctrl+Shift+S

# assigning values to variables:
# R does not require variable types to be declared
# variables cannot start with a number or an underscore
# variable names are case sensitive
x <- 2
assign("x", 2)

# assign an integer to a variable, not the default of numeric
y <- 3L

# Removing a variable
rm(x)

# Checking the class of a variable
class(x)
is.numeric(x)
is.integer(y)

# Characters and factors are the two data types for words
# Characters are case sensitive
z <- "James R. Herbick"

# Finds the length of a string
nchar(z)

# Dates and Times, Date and POSIXct.
# Date stores only the date, the other stores date and time.
# Stored as numeric values since 1/1/1970
# Use the lubridate and chron packages for date manipulation
date1 <- as.Date("2015-01-02")
date2 <- as.POSIXct("2015-01-02 09:18")

#Logicals.  True=1, False=0
is.logical(x)

# does 2 equal 3?
2 == 3

# does 2 not equal 3?
2 != 3

# Vectors are a collection of data, all of the same type.
# Vectors cannot be of mixed types.
# Operations are applied to each element of a vector automatically, 
# without the need to loop through the vector.

x <- c(1,2,3,4,5)
x * 3

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


# Read in a .csv file
# Read in the full abalone dataset
fileLocation <- "C:/Users/James R. Herbick/Documents/Northwestern/PREDICT 401/Report 1/abalone.csv"
fullDataSet <- read.table (file = fileLocation, header = TRUE, sep = " ")

# Validate full dataset per Prof. Martin instructions
str(fullDataSet)







