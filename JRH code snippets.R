
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
 





