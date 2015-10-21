
# --------
# HOMEWORK
# --------

# list available data in the fma package
data(package="fma")

# ---------
# Chapter 2
# ---------

library(fma)
data(package="fma")

# ------------
# Question 2.1
# ------------

# Australian Unemployement - Explore the data
str(dole)
summary(dole)
head(dole)

# Plot the data
plot(dole, main="Monthly Unemployment in Australia", xlab="Year", ylab="Unemployement")

# Plot Australian Unemployment data with Box-Cox transformation
lambda <- BoxCox.lambda(dole)  # 0.3290922
plot(BoxCox(dole, 0.3290922), main="Australian Unemployment", xlab="Year", ylab="Brick Production")




# US deaths - Explore the data
str(usdeaths)
summary(usdeaths)

# Plot the data
plot(usdeaths, main="Monthly Accidental Deaths in US", xlab="Year", ylab="Deaths")

# Plot US deaths data with Box-Cox transformation
lambda1 <- BoxCox.lambda(usdeaths)  # -0.03363775
plot(BoxCox(usdeaths, -0.03363775), main="US Deaths", xlab="Year", ylab="Brick Production")


# Quarterly Brick Production
str(bricksq)
summary(bricksq)

# Plot the data
plot(bricksq, main="Quarterly Brick Production", xlab="Year", ylab="Brick Production")

# Plot brick data with Box-Cox transformation
lambda2 <- BoxCox.lambda(bricksq)  # 0.2548929
plot(BoxCox(bricksq, 0.2548929), main="Quarterly Brick Production", xlab="Year", ylab="Brick Production")




# ------------
# Question 2.2
# ------------

str(dowjones)

# Plot the data
plot(dowjones, main="Dow Jones Index", xlab="Period", ylab="Level")

# Drift method forecasts
dj <- rwf(dowjones, 10, drift=TRUE)
plot(dj, main="Drift Forecasts for Dow Jones", xlab="Period", ylab="Level")
lines(c(1,78), c(110.94, 121.23), type="l")
par(col="red")

# Draws a line between the first and last observation of Dow Jones
# Not needed with revised code above.
# plot(dowjones[c(1,78)], type="l")

# Naive Method
dj2 <- naive(dowjones, 10)
plot(dj2, main="Naive Forecasts for Dow Jones", xlab="Period", ylab="Level")
par(col="blue")

# Average Method
library(forecast)

plot(meanf(dowjones, 10), main="Average Forecasts for Dow Jones", xlab="Period", ylab="Level")


# ------------
# Question 2.3
# ------------

str(ibmclose)

# Plot the data
plot(ibmclose, main="IBM Closing Stock Price", xlab="Period", ylab="Price ($)")

# Split data into train / test split
ibm_train <- window(ibmclose, start=1, end=300)
ibm_test  <- window(ibmclose, start=301)


# Forecast using benchmark methods
# Average Method
ibm1 <- meanf(ibm_train, 20)
# Provides in-sample / training set fit statistics
accuracy(ibm1)  
# Provides both training and test dataset fit statistics
accuracy(ibm1, ibm_test)

plot(ibm1, main="Average Method Forecasts - IBM", xlab="Period", ylab="Price ($)")


# Naive Method
ibm2 <- naive(ibm_train, 20)
# Provides both training and test dataset fit statistics
accuracy(ibm2, ibm_test)

plot(ibm2, main="Naive Method Forecasts - IBM", xlab="Period", ylab="Price ($)")


# Drift Method
ibm3 <- rwf(ibm_train,20, drift=TRUE)
# Provides both training and test dataset fit statistics
accuracy(ibm3, ibm_test)

plot(ibm3, main="Drift Method Forecasts - IBM", xlab="Period", ylab="Price ($)")


# ------------
# Question 2.4
# ------------

str(hsales)
head(hsales)

plot(hsales, main="New Single-Family House Sales US", xlab="Period", ylab="Sales")


# Split data into train / test split
hsales_train <- window(hsales, start=1973, end=1993)
hsales_test <- window(hsales, start=1994)


# Forecast using benchmark methods
# Average Method
hsales1 <- meanf(hsales_train, 20)
# Provides both training and test dataset fit statistics
accuracy(hsales1, hsales_test)

plot(hsales1, main="Average Method Forecasts - Home Sales", xlab="Year", ylab="Count")


# Naive Method
hsales2 <- naive(hsales_train, 20)
# Provides both training and test dataset fit statistics
accuracy(hsales2, hsales_test)

plot(hsales2, main="Naive Method Forecasts - Home Sales", xlab="Year", ylab="Count")


# Drift Method
hsales3 <- rwf(hsales_train,20, drift=TRUE)
accuracy(hsales3, hsales_test)

plot(hsales3, main="Drift Method Forecasts - Home Sales", xlab="Year", ylab="Count")


# ----------
# Chapter 4
# ----------

# Question 4.1

#EDA
econsumption
class(econsumption)

plot(Mwh ~temp, data=econsumption, main="Temperature vs. Energy Consumption")


# Fit Regression Model
model1 <- lm(Mwh ~ temp, data=econsumption)
model1


# Residual Plot
plot(residuals(model1) ~ temp, data=econsumption, main=" Residuals vs. temp")
plot1 <- ggplot(data=econsumption, aes(x=1, y=temp)) + geom_boxplot() + ggtitle("Boxplot for temp")
plot1


# Forecast
fcast <- forecast(model1, newdata=data.frame(temp=c(10,35)))
plot(fcast, main="Forecasts for Energy Consumption", ylab="Mwh", xlab="temp")
fcast


# Question 4.2

# Update dataset
fix(olympic)

# Create scatterplot using ggplot2
library(ggplot2)
plot2 <- ggplot(data=olympic, aes(x=Year, y=time)) + geom_point() + ggtitle("Best Times vs. Year")
plot2

# Split data into train / test split
# Do it this way, because it is a dataframe, not timeseries object
olympic_train <- olympic[c(1:23),]
olympic_test <- olympic[c(24:26),]

# Fit Regression Model
model2 <- lm(time ~ Year, data=olympic_train)
summary(model2)
plot(model2)


# Residual Plot
plot(residuals(model2) ~ Year, data=olympic_train, main=" Residuals vs. Year")


# Forecast
fcast2 <- forecast(model2, newdata=data.frame(Year=c(2000, 2004, 2008, 2012)))

plot(fcast2, main="Forecast Times for Men's 400", ylab="Time", xlab="Year")

fcast2

# Check model accuracy
accuracy(fcast2, olympic_test)


# ---------
# CHAPTER 5
# ---------
library(fpp)

# Question 5.1

#a
plot(fancy, main="Plot of Fancy dataset")

#c
log.fancy <- log(fancy)
plot(log.fancy, main="Plot of Log-Fancy dataset")

# **** use log.fancy ???? ****
# From Melissa
data <- data.frame(Y=as.matrix(fancy), date=as.Date(as.yearmon(time(fancy))))

data2 <- data.frame(Y=as.numeric(log.fancy), date=as.Date(as.yearmon(time(log.fancy))))


# My code, subsetting month and year as separate variables
# Creates a dummy variable, x, 1=March, 0=anything else.
# This signifies the surfing festival held in March each year.
data2$x <- ifelse(substr(as.yearmon(time(fancy)), 1, 3)=="Mar", 1, 0)

# Create factor variable for only the month of the year (seasonality component)
data2$month <- as.factor(months(data2$date, abbreviate = TRUE))


# Fit the model
model1 <- tslm(log.fancy ~ trend + season)
summary(model1)

# This seems to work... need to understand if it is really doing
# what I want it to....
model2 <- lm(Y ~ date + x + month, data=data2)

summary(model2)

#d
plot(model2)
plot(data2$date, model2$residuals, main="model2 - Residuals v. Time")


#e
boxplot(model2$residuals ~ data2$month, main="Boxplot of model2 Residuals by Month")


#g

#h - not working... yet
a <- as.Date("1994-01-01":"1996-12-01")
d <- data.frame()
fcast2 <- forecast(model2, newdata=)





# ---------------
# Midterm Project
# ---------------

# -------------------
# 1. Data Acquisition
# -------------------

# Read in a .csv file
fileLocation_Total <- "C:/Users/James R. Herbick/Documents/Northwestern/PREDICT 413_TIME_SERIES/Midterm Project/Smoking_Adults_Total.csv"
fileLocation_Gender <- "C:/Users/James R. Herbick/Documents/Northwestern/PREDICT 413_TIME_SERIES/Midterm Project/Smoking_Adults_Gender.csv"

fullDataSet_Total <- read.table (file = fileLocation_Total, header = TRUE, sep = ",", stringsAsFactors=FALSE)
fullDataSet_Gender <- read.table (file = fileLocation_Gender, header = TRUE, sep = ",", stringsAsFactors=TRUE)


# Sort dataset (works, but date field has to be specified with year first format)
# library(stats)
# fullDataSet <- fullDataSet[order(fullDataSet$Date,decreasing=FALSE),]


# Create time series object
smoke_Total <- ts(fullDataSet_Total$Percentage.Smokers, frequency=1, start=c(1984,1))

# Can I make a timeseries object out of the gender data?
# Looks like this may be working... set frequency=2
smoke_Gender <- ts(fullDataSet_Gender$Percentage.of..Current.Smokers, frequency=2, start=c(1984,1))


# ----------------------------
# 2. Exploratory Data Analysis
# ----------------------------

# Plot overall (Total) percentage of smokers over time
plot(smoke_Total, main="Percentage of Smokers - Total, 1984-2013", ylab="% Smokers")

# Plot smokers by gender over time
plot(smoke_Gender, main="Percentage of Smokers - Gender, 1984-2013", ylab="% Smokers")

# Scatterplot of % of smokers vs year, color coded by gender, male vs female
library(ggplot2)
g <- ggplot(fullDataSet_Gender, aes(x = Year, y = Percentage.of..Current.Smokers))
g + geom_point(aes(color=Gender, title="Percent Smokers by Gender"))


# -------------------
# 3. Data Preparation
# -------------------

# Create step variable for smoker definition expansion in 1996
fullDataSet_Gender$x <- ifelse(fullDataSet_Gender$Year<1996,0,1)


# Split data into train / test split
# Do it this way, because it is a dataframe, not timeseries object
# This is for the linear regression model
smoke_train <- fullDataSet_Gender[c(1:48),]
smoke_test <- fullDataSet_Gender[c(49:60),]

# Pick which one of these that works for the gender data....
# This is for timeseries object.
smoke_gender_train <- window(smoke_Gender, start=1984, end=2007)
smoke_gender_test <- window(smoke_Gender, start=2008)

# Splits the total smoking data that was in a ts object
smoke_total_train <- window(smoke_Total, start=1984, end=2007)
smoke_total_test <- window(smoke_Total, start=2008)


# ---------------
# 4. Build Models
# ---------------

# Drift Method
library(forecast)

# This is drift method on the TOTAL data
# smoke_drift_model <- rwf(smoke_total_train,10, drift=TRUE)
# accuracy(smoke_drift_model, smoke_total_test)
# plot(smoke_drift_model, main="Drift Method Forecasts (Total) - Percentage Smokers", xlab="Year", ylab="*100=%")

# This is the drift method on the GENDER data...
# Compares directly to the linear regression approach
smoke_drift_model_gender <- rwf(smoke_gender_train,10, drift=TRUE)
accuracy(smoke_drift_model_gender, smoke_gender_test)
plot(smoke_drift_model_gender, main="Drift Method Forecasts (Gender) - Percentage Smokers", xlab="Year", ylab="*100=%")


# --------------------------------
# Fit Regression Model
# --------------------------------

# Multiple regression model (.90 adjusted R squared)
smoke_reg_model <- lm(Percentage.of..Current.Smokers ~ Gender + Year + x, data=smoke_train)
summary(smoke_reg_model)
plot(smoke_reg_model)

# Time series regression with trend and season components (.50 adjusted R squared)
smoke_reg_model_ts <- tslm(smoke_gender_train ~ trend + season)
summary(smoke_reg_model_ts)


# ------------------------
# 5. Check Goodness of Fit
# ------------------------







# ----------------
# Regression Model
# ----------------


# Forecast
smoke_fcast <- forecast(smoke_reg_model, newdata=smoke_test)

# Only works for a single predictor
# plot(smoke_fcast, main="Forecast Smoking Percentages ", ylab="*100= %", xlab="Year")

smoke_fcast

CV(smoke_reg_model)

# Check model accuracy
accuracy(smoke_fcast, smoke_test)

# ------

# Forecast
smoke_fcast2 <- forecast(smoke_reg_model_ts, newdata=smoke_gender_test)

# Check model accuracy
accuracy(smoke_fcast2, smoke_gender_test)




# ---------------
# Check residuals
# ---------------

# Drift Method
# Want these to be a scatterplot.
plot(smoke_drift_model_gender$residuals, type="p", main="Drift Model Residuals")
# plot(smoke_drift_model_gender$fitted,smoke_drift_model_gender$residuals, type="p")

Acf(smoke_drift_model_gender$residuals, main="ACF of Drift Method Residuals")


# Multiple Linear Regression

Acf(smoke_fcast$residuals, main="ACF of Multiple Regression Residuals")





# -----------------
# DISCUSSION BOARDS
# -----------------

# -----------------------
# Week 2 Discussion Board (not best code to use for example)
# -----------------------

# Read in a .csv file
# Read in the full Harley Davidson dataset
fileLocation <- "C:/Users/James R. Herbick/Documents/Northwestern/PREDICT 413_TIME_SERIES/WEEK 2/table2.csv"
fullDataSet <- read.table (file = fileLocation, header = TRUE, sep = ",", stringsAsFactors=FALSE)


# Sort dataset (works, but date field has to be specified with year first format)
# fullDataSet <- fullDataSet[order(fullDataSet$Date,decreasing=FALSE),]


# Create time series object
HOG <- ts(fullDataSet$Close, frequency=1, start=c(1/2/2013,1))

# Plot the original data
plot(HOG)

# Forecast using benchmark methods, requires forecast package
# Average Method

library(forecast)

plot(meanf(HOG, 20), main="Mean Forecasts for Harley Davidson")

# Naive Method
plot(naive(HOG, 20), main="Naive Forecasts for Harley Davidson")

# Drift Method
plot(rwf(HOG,20, drift=TRUE), main="Drift Forecasts for Harley Davidson")



# -----------------------
# Week 4 Discussion Board
# -----------------------

# ---------------
# 1. Acquire Data
# ---------------

# Read in the obesity by state dataset
fileLocation <- "C:/Users/James R. Herbick/Documents/Northwestern/PREDICT 413_TIME_SERIES/WEEK 4/obesity data.csv"
fullDataSet <- read.table (file = fileLocation, header = TRUE, sep = ",", stringsAsFactors=TRUE)

# Validate full dataset per Prof. Martin instructions
# Gives metadata of an object
str(fullDataSet)


# Read in the obesity by age dataset
fileLocation2 <- "C:/Users/James R. Herbick/Documents/Northwestern/PREDICT 413_TIME_SERIES/WEEK 4/obesity data age.csv"
fullDataSetAge <- read.table (file = fileLocation2, header = TRUE, sep = ",", stringsAsFactors=TRUE)

# Validate full dataset per Prof. Martin instructions
# Gives metadata of an object
str(fullDataSetAge)


# ----------------------------
# 2. Exploratory Data Analysis
# ----------------------------

# Scatterplot of % obesity vs state in the US
library(ggplot2)
g <- ggplot(fullDataSet, aes(x = Location, y = Value))
g + geom_point(aes(color=Location))


# Scatterplot of % obesity vs year and age
library(ggplot2)
g <- ggplot(fullDataSetAge, aes(x = Year, y = Percent))
g + geom_point(aes(color=Age.Group))

# Test correlation between age and % obsese
cor(fullDataSetAge$Year, fullDataSetAge$Percent)


# -------------
# 3. Split Data
# -------------

train <- fullDataSetAge[1:44, ]
test <- fullDataSetAge[45:68, ]
    


# ---------------
# 4. Build models
# ---------------

# First argument (Height) is the response, Second argument is the predictor
obesity.Simple <- lm(Percent ~ Age.Group, data=train)
obsesity.Simple
summary(obsesity.Simple)

obesity.Multiple <- lm(Percent ~ Age.Group + Year, data=train)


# -----------------------
# 5. Test models' Accuracy
# -----------------------

# Simple Regression Model
# -----------------------
fcast.Simple <- forecast(obesity.Simple, newdata=test)

# check residuals



# Check model accuracy
accuracy(fcast.Simple, test)


# Multiple Regression Model
# -----------------------
fcast.Multiple <- forecast(obesity.Multiple, newdata=test)


# check residuals

# Residuals vs predictors
plot(train$Age.Group, residuals(obesity.Multiple), xlab="Age Group")

plot(train$Year, residuals(obesity.Multiple), xlab="Year")





# Check model accuracy
accuracy(fcast.Multiple, test)


# -----------------------
# WEEK 5 DISCUSSION BOARD
# -----------------------

# ----------------
# 1.  Acquire Data
# ----------------

# Read in a .csv file
# Read in the ABT stock data
fileLocation <- "C:/Users/James R. Herbick/Documents/Northwestern/PREDICT 413_TIME_SERIES/WEEK 5/ABT.csv"
fullDataSet <- read.table (file = fileLocation, header = TRUE, sep = ",", stringsAsFactors=FALSE)

# Try to create Date field as a Date datatype
fullDataSet$Date <- as.Date(fullDataSet$Date, format="%m/%d/%Y")

# Add month and year attributes to use in multiple regression model
fullDataSet$month <- as.factor(months(fullDataSet$Date, abbreviate = TRUE))
fullDataSet$year <- as.numeric(format(fullDataSet$Date,'%Y'))


# Create timeseries object for tslm
abt.ts <- ts(fullDataSet$Adj.Close, frequency=12, start=c(2010,10))



# -------------------------
# 2, Exploratory Data Analysis
# -------------------------

plot(abt.ts, main="ABT stock", ylab="Adj. Close")



# ---------------------
# 3. Train / Test Split
# ---------------------

# For dataframe
ABT_train <- fullDataSet[c(1:48),]
ABT_test <- fullDataSet[c(49:60),]

start1 <- as.Date("2010-10-19", format="%Y-%m-%d")
end1 <- as.Date("2014-09-02", format="%Y-%m-%d")

# For timeseries object
ABT_train_ts <- window(abt.ts, start=2010, end=2015)
ABT_test_ts <- window(abt.ts, start=2015)


# ---------------
# 4. Build Models
# ---------------

library(forecast)
# Trend and Seasonality using tslm
model1 <- tslm(ABT_train_ts ~ trend + season)


# Try with true multiple linear regression model
# This works, just not sure it is the right way to go


model2 <- lm(Adj.Close ~ year + month, data=ABT_train)


# -------------------------------
# 5. Check model fit and accuracy
# -------------------------------

# Model 1 tslm model
# ------------------
# Fit statistics on training data
summary(model1)
CV(model1)

# Forecast accuracy on test data
fcast1 <- forecast(model1, newdata=ABT_test_ts)
summary(fcast1)

plot(fcast1, main="fcast1 Plot")
points(ABT_test_ts, type="p")  # this isn't working ... to show actual data...

plot(fcast1$residuals, type="p", main="Residuals of fcast1")

Acf(fcast1$residuals, main="ACF for fcast1 Residuals")


# Model 2, multiple linear regression
# -----------------------------------

summary(model2)
CV(model2)


fcast2 <- forecast(model2, newdata=ABT_test)

summary(fcast2)

