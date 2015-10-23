
# Version Control
# C:/R/Git/cmd is the command line directory for running Git command line prompts

# Running files, parts of files
# Highlight a selection and press Ctrl+Enter
# To run an entire file, press Ctrl+Shift+S

#------------------------------------------------

# ----------------
# DATA ACQUISITION
# ----------------

# Read in a .csv file
# Read in the full abalone dataset
fileLocation <- "C:/Users/James R. Herbick/Documents/Northwestern/PREDICT 401/Report 1/abalone.csv"
fullDataSet <- read.table (file = fileLocation, header = TRUE, sep = ",", stringsAsFactors=TRUE)

# Validate full dataset per Prof. Martin instructions
# Gives metadata of an object
str(fullDataSet)

# Write a .csv file from a data.frame
write.csv(d, "C:/Users/James R. Herbick/Documents/Northwestern/PREDICT 401/Report 1/volume.csv", row.names=F)

# Create new dataset with the VOLUME attribute
d <- data.frame(sampleDataSet, VOLUME=volume)


# Saving data structures to a file
save(x,y,z, file="jrh.RData")

# Loading a saved data file
load("jrh.RData")

# -------------------
# VARIABLE ASSIGNMENT
# -------------------

# assigning values to variables:
# R does not require variable types to be declared
# variables cannot start with a number or an underscore
# variable names are case sensitive

x <- 2
assign("x", 2)

# Removing a variable
rm(x)


# ----------
# DATA TYPES
# ----------
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


# ----
# MATH
# ----

# Calculus
d(expression(expx^2)), "x")  # derivative

integrate(function(x) x^2, 0, 1) #integral




# -------
# VECTORS
# -------
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

#Natural log
log(x)

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


# -------
# FACTORS
# -------
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


# ------------
# MISSING DATA
# ------------
# R has two types of missing data: NA and NULL
# NA is for missing data
# Test each element of a vector for missingness
is.na(z)

# For handling missing data look at mi, mice, and Amelia packages

# NULL is the absence of anything.  NULL cannot exist within a vector
# Test for NULL
is.NULL(d)


# ------------------------
# Advanced data structures
# ------------------------

# ----------
# DATA FRAME
# ----------
# data.frame, matrix, list, array
# data.frame is like an excel spreadsheet, has columns and rows
# each column of a data.frame is a vector
# -------------------------------------------------------------
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
theDF[,3]     # specify no row, 3rd column (entire 3rd column)
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



# -----
# LISTS
# -----
# store any number of items of any data type
list(1,2,3,"four")

# Create a single element list where the only element is a vector
list(c(1,2,3))

# Create a 2-element list, first element is a vector, second is a vector
(list3 <- list(c(1,2,3), 3:7))  # enclosing in () prints the list

### look into this more


# --------
# MATRICES
# --------
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
a %*% b  # don't know if we need the %%% signs, I think you do.


# -----
# Dates
# -----

# Try to create Date field as a Date datatype, from character
fullDataSet$Date <- as.Date(fullDataSet$Date, format="%m/%d/%Y")


# -------------
# VISUALIZATION
# -------------

#-----------------------
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



# ------------------
# GROUP MANIPULATION
# ------------------
# plyr package
# ddply - takes a data.frame, splits it according to some variables,
# performs a desired action, and returns a data.frame
# -------------------------------------------------------------------

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

# Sort dataset (works, but date field has to be specified with year first format)
library(stats)
fullDataSet <- fullDataSet[order(fullDataSet$Date,decreasing=FALSE),]


# --------------------------------------
# Sampling, train / test splits, sorting
# --------------------------------------

# Take a random sample
#----------------------------------

# Create index variable for selecting rows from abalone.csv
set.seed(123)
index <- sample(1:nrow(fullDataSet), 500)
# View the indexes created
str(index)
# Pull out the random sample from abalone.csv
abaloneSample <- fullDataSet[index,]

# Save sample to a file
write.csv(abaloneSample, "C:/Users/James R. Herbick/Documents/Northwestern/PREDICT 401/Report 1/mydata.csv", row.names=T)


# -------
# Sorting
# -------
# Sort dataset (works, but date field has to be specified with year first format)
fullDataSet <- fullDataSet[order(fullDataSet$Date,decreasing=FALSE),]

# Sort randomly
set.seed(12345)
# creates new randomized dataset with 1000 random numbers
credit_rand <- credit[order(runif(1000)),]


# -------------------------
# Exploratory Data Analysis
# -------------------------

# Give structure / characteristics of an object
str()

# Lists attributes of an object
attributes()

# Lists the variables of an object... I think....
ls()


# Numerical Data
# --------------
# Show common summary statistics for numeric data
# five number summary
summary(HeightLM)
# Get summary statistics for several numeric variables at once
summary(usedcars[c("price", "mileage")])

# Return the mean, median
mean()
median()

# Span between minimum and maximum values
# Range displays min and max values
range()

# Range and diff give the actual range of the data
diff(range(usedcars$price))

# Interquartile range = middle 50% of data Q1-Q3
IQR()

# Quantiles, default gives five number summary
quantile()

# Specify quantiles
quantile(usedcars$price, probs=c(0.01, 0.99))

# Evenly spaced quantiles
quantile(usedcars$price, seq(from=0, to=1, by=.20))


# Categorical Data
# ----------------

# Create frequency tables (could feed into a histogram, etc.)
table()

# Percentages / proportions of items in a table
x <- table()
prop.table(x)

# Explore crosstable for SAS type tables
CrossTable(x=usedcars$model, y=usedcars$conservative)





# -----------------
# Linear Regression
# -----------------

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

# Provides coefficients, etc.
summary(HeightLM)

# Provides fit statistics (MAE, CV, etc.)
CV(HeightLM)






# ---------------------------------------
# Writing data to text / SAS datastep
# Need data to be a dataframe
# Writes data to the R working directory
# ---------------------------------------
a <- data.frame(beer)
write.foreign(a, "test.txt", "testcode.sas", package="SAS")



# ----------------
# TIME SERIES DATA
# ----------------

# USEFUL COMMANDS
# ---------------
# - COPY TO CLIPBOARD THE DELIMITED DATA FILE
# - THEN TYPE IN THE DATA....
MYDATA=READ.DELIM("CLIPBOARD")

# COMMAND THAT MAKES THE VARIABLES AVAILABLE IN THE WORKSPACE....
# CAN ADDRESS THESE VARIABLES JUST BY TYPING THEM IN....
# I THINK FOR DATAFRAMES... ALLOWS YOU TO REFERENCE COLUMNS W/O $
ATTACH (MYDATA) 

# - rnorm - returns random numbers between 1 and 100


# ---------------
# Useful Packages
# ---------------
library(forecast)   # for forecasts
library(quantmod)   # download financial timeseries data
library(ResourceSelection)   # visualizations, many dimensions
library(moments)    # skewness, kurtosis, normality
library(car)   # transformations
library(rgl)  # FOR 3D PLOTTING
library(x12)  # for X-12 ARIMA decomposition... link to US Census 


# READING DATA

# Use this function to read time series data in.  Assumes that data for
# successive time points is in a simple text file with one column.
# The skip parameter states to skip the first 3 lines of the file.
kings <- scan("http://robjhyndman.com/tsdldata/misc/kings.dat",skip=3)

# Then need to create / store data in a time series object.
# frequency parameter defines granularity of data collection....
# frequency=12 means monthly data, frequency=4 means quarterly data.
# The start parameter specifies the first year and first interval of the data.
# This states that the first year of data collection was 1946, first month.
kingstimeseries <- ts(kings, frequency=12, start=c(1946,1))






# PLOTTING DATA

# Plot a time series object
plot.ts(kingstimeseries)

# COMMAND... PROVIDES SCATTER PLOT
pairs() 

# tabulates.... frequencies... COUNTS
table()  


# LAYOUT TELLS R WHERE GRAPHS WILL GO ON THE SCREEN
layout(matrix(c(1,2,3,4, nrow=2))
         
# How to get multiple plots in the same window
par(mfrow=c(2,2))  #  I think does a 2x2 matrix of plots


# Forecast using benchmark methods, requires forecast package
# Average Method
library(forecast)
plot(meanf(HOG, 20), main="Mean Forecasts for Harley Davidson")

# Naive Method
plot(naive(HOG, 20), main="Naive Forecasts for Harley Davidson")

# Drift Method
plot(rwf(HOG,20, drift=TRUE), main="Drift Forecasts for Harley Davidson")


# LINEAR REGRESSION
# -----------------

fit$residuals  # (where fit is the model)(gives residuals)
fit$fitted.values  #(equals the fitted values)
summary(fit$residuals)    # where fit is the linear regression model....

# forecast an lm model on new data
forecast()


# FITS THE BEST FIT REGRESSION LINE FOR THAT MODEL
# adds a straight line through current plot
abline()
abline(fit, col="red")

# car package - for variable transformations
# result is lambda... ideal transformation
# to reverse a power transform raise value to the 1/lambda power
powerTransform()


# LIKELIHOOD RATIO TEST... BASED ON CHI SQUARED DISTRIBUTION
# IF P VALUE IS SMALL, REJECT NULL HYPOTHESIS 
testTransform(MYT, MYT$LAMBDA)

# FIT STATISTICS FOR LM MODEL
CV()


# Notes from Doc Larry
# I like to look at many variables simultaneously.  Doing so requires visibility in two or 
# more dimensions.  One of the libraries I use for this is "ResourceSelection".  I then 
# generate scatterplot matrices, histograms, correlations, and bivariate density plots
# (contours) on one chart.  This is quite useful, so I thought I would share.  Enjoy!
library(ResourceSelection)
kdepairs(USairpollution)


# ----------------------------------------------------------------------
# DECOMPOSING TIME SERIES INTO TREND, SEASONAL, AND IRREGULAR COMPONENTS
# ----------------------------------------------------------------------
# Smoothing method to id the trend component... simple moving average
# Requires TTR package, n=order or span of the moving average
kingstimeseriesSMA3 <- SMA(kingstimeseries,n=3)


# Classical Decomposition (into components)
# ** (not recommended) **
# -----------------------------------------
# Returns a list with "seasonal", "trend", and "random" components
y <- decompose (x, type=c("additive", "multiplicative"))
plot (y)


# Currently no R package for x-12 ARIMA, but this package
# connects to US Census software
library(x12)


# STL Decomposition (** very roubust and versitle)
# ------------------------------------------------
# These options control how rapidly trend and season components can change
# small values allow more rapid changes.
# t.window = trend window
# s.window = season window
fit <- stl(elecequip, t.window=15, s.window="periodic", robust=TRUE)
plot(fit)


# Forecasting with STL Decomposition
# ----------------------------------
fit <- stl(elecequip, t.window=15, s.window="periodic", robust=TRUE)
eeadj <- seasadj(fit)  # seasonally adjusts the data
# plots a naive forecast of the seasonally adjusted data
plot(naive(eeadj), ylab="New Orders Index", main="Naive forecasts of seasonally adjusted data")

# I guess this is forecasting the deconstructed data
# using the naive method for the seasonal part....
fcast <- forecast(fit, method="naive")  # fit is the deconstructed data
plot(fcast, ylab="New Orders Index")













# Seasonally adjusting data
# Take the original data and subtract the seasonal component
y <- decompose(x)
z <- x - y$seasonal
plot(z)


# Moving Averages
# ---------------

# Plot original data, with red trend component using a moving average
plot(elecsales)
lines(ma(elecsales,5), col="red")   # m=5... i.e. how many points to average


# Calculate the moving average values
beer2 <- window(ausbeer, start=1992)
ma4 <- ma(beer2, order=4, centre=FALSE)
ma2x4 <- ma(beer2, order=4, centre=TRUE)  # centered moving average of order 4




# Convert timeseries data into a dataframe
# From Melissa
data<-data.frame(Y=as.matrix(fancy), date=as.Date(as.yearmon(time(fancy))))


# EXPONENTIAL SMOOTHING

# Simple exponential smoothing (SES)
# Smoothing is controlled by the alpha parameter (which is between 0 and 1)
# Alpha values close to 0 mean that recent observations are given little weight
# Use the HoltWinters function for SES, set both parameters to FALSE.
# Only makes forecasts for the in sample, original dataset....
rainseriesforecasts <- HoltWinters(rainseries, beta=FALSE, gamma=FALSE)

# use start parameter as below to specify the intial value for the level.
HoltWinters(rainseries, beta=FALSE, gamma=FALSE, l.start=23.56)

# To get the forecasts use this (stored in $fitted attribute)
rainseriesforecasts$fitted

# Plot original time series against the forecasts
plot(rainseriesforecasts)

# Get the Sum Squared Errors for in sample forecasts
rainseriesforecasts$SSE

# Use this to forecast beyond the time period in the sample data (i.e. on new observations)
# Requires the forecast package.
# as its first argument (input), you pass it the predictive model
# that you have already fitted using the HoltWinters() function.
# h=how many further time points out to make predictions for
rainseriesforecasts2 <- forecast.HoltWinters(rainseriesforecasts, h=8)

# Plot forecasts
plot.forecast(rainseriesforecasts2)

# Get residuals, calculate a correlogram 
acf(rainseriesforecasts2$residuals, lag.max=20)


# ARIMA
auto.arima()    # p. 227 in book




# ----------------
# Machine Learning
# ----------------

# Packages: Rweka











