
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

# ------------
# Question 5.1
# ------------

# ------
# part a
# ------
plot(fancy, main="Plot of Fancy dataset")

# ------
# part c
# ------

# Take logarithms of original dataset
log.fancy <- log(fancy)
plot(log.fancy, main="Plot of Log-Fancy dataset")

# Turn timeseries object into dataframe, to add dummy variables.
# From Melissa

# original data
data <- data.frame(Y=as.matrix(fancy), date=as.Date(as.yearmon(time(fancy))))

# log of original data
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
# check this code to make sure it is doing the right thing...
boxplot(model2$residuals ~ data2$month, main="Boxplot of model2 Residuals by Month")


#g - durbin watson statistic

# Durbin-Watson test for lag-1 autocorrelation
# Symmetric around 2, if no autocorrelation
dwtest(model2, alt="two.sided")

# Just checking the ACF for visual on autocorrelation
acf(model2$residuals)




#h - not working... yet
# a <- as.Date("1994-01-01":"1996-12-01")
# d <- data.frame()

# - COPY TO CLIPBOARD THE DELIMITED DATA FILE
# - THEN TYPE IN THE DATA....
z=read.delim(file="clipboard", header=TRUE, sep="\t")

# Try to create Date field as a Date datatype, from character
z$date <- as.Date(z$date, format="%m/%d/%Y")

# Forecast... is this working??? Looks like it is.....
fcast2 <- forecast(model2, newdata=z)

plot(data2$Y ~ data2$date, type="l")
lines(z$date, fcast2$mean, col="red")  # works but plot doesn't extend ....

# Plots the forecast (log scale)
plot(fcast2$mean ~ z$date, type="l")


# part i - (reverse transformation)

# Grab the point forecasts and the upper / lower data
fcast2.predictions <- data.frame(fcast2)

# Reverse the transformation
fcast2.reversed <- exp(fcast2.predictions)


# ------------
# Question 5.2
# ------------

library(fpp)
library(ggplot2)
data(package="fma")  # check out the datasets available

texasgas$City <- c("Amarillo", "Borger", "Dalhart", "Shamrock", "Royalty", "Texarkana", "Corpus Christi", "Palestine", "Marhall", "Iowa Park", "Palo Pinto", "Millsap", "Memphis", "Granger", "Llano", "Brownsville", "Mercedes", "Karnes City", "Mathis", "La Pryor")

# a.
attach(texasgas)
g <- ggplot(texasgas, aes(x = price, y = consumption))
g + geom_point() + labs(title="Natural Gas for Texas Towns")
g + geom_point() + labs(title="Natural Gas for Texas Towns") + geom_line()


# c.

# model1
exp.texasgas <- data.frame(price=exp(price),consumption=exp(consumption), City)
model1 <- lm(exp.texasgas$consumption ~ exp.texasgas$price)
summary(model1)
CV(model1)
plot(model1$residuals)

# model1b (take exponent only of price)
model1b <- lm(texasgas$consumption ~ exp(texasgas$price))
summary(model1b)
CV(model1b)
plot(model1b$residuals)



# model2
texasgas$p.less.60 <- as.factor(ifelse(texasgas$price<=60, 1, 0))
texasgas$P1 <- ifelse(texasgas$price<=60, texasgas$price, 0)

texasgas$p.larger.60 <- as.factor(ifelse(texasgas$price>60, 1, 0))
texasgas$P2 <- ifelse(texasgas$price>60, texasgas$price, 0)

attach(texasgas)
# 0 + as first terms removes the intercept from the model
model2 <- lm(consumption ~ 0 + p.less.60 + P1 + p.larger.60 + P2)
summary(model2)
CV(model2)
plot(model2$residuals)


# model3
texasgas$price.square <- texasgas$price^2

attach(texasgas)
model3 <- lm(consumption ~ price + price.square)
summary(model3)
CV(model3)
plot(model3$residuals)


# e)
new.data <- data.frame(price=c(40, 60, 80, 100, 120), consumption=c(NA, NA, NA, NA, NA))

new.data$p.less.60 <- as.factor(ifelse(new.data$price<=60, 1, 0))
new.data$P1 <- ifelse(new.data$price<=60, new.data$price, 0)

new.data$p.larger.60 <- as.factor(ifelse(new.data$price>60, 1, 0))
new.data$P2 <- ifelse(new.data$price>60, new.data$price, 0)


pred1 <- forecast(model2, newdata=new.data)
plot(texasgas$consumption ~ texasgas$price)
points(new.data$price, pred1$mean, col="purple")
lines(new.data$price, pred1$mean, col="red")

# f
plot(new.data$price, pred1$mean)
lines(new.data$price, pred1$upper[,2], col="blue")
lines(new.data$price, pred1$lower[,2], col="blue")


# g

cor(texasgas$price, texasgas$price.square)


# ---------
# CHAPTER 6
# ---------

# 6.1





# 6.2

#a
plot(plastics, main="Plastics Timeseries", xlab="year number", ylab="sales (000s)")

# b
classical <- decompose(plastics, type="multiplicative")
plot(classical)

# d
season.adj <- seasadj(classical)
plot(season.adj)


#e

plastics2 <- plastics
plastics2[12] <- plastics2[12] + 500

classical2 <- decompose(plastics2, type="multiplicative")

season.adj2 <- seasadj(classical2)
plot(season.adj2)


#f

plastics3 <- plastics
plastics4 <- plastics

plastics3[30] <- plastics3[30] + 500

classical3 <- decompose(plastics3, type="multiplicative")

season.adj3 <- seasadj(classical3)
plot(season.adj3)


plastics4[57] <- plastics4[57] + 500

classical4 <- decompose(plastics4, type="multiplicative")

season.adj4 <- seasadj(classical4)
plot(season.adj4)


# g

pred1 <- rwf(season.adj, h=24, drift=TRUE)
plot(pred1)



# h

# Do I have to do a random walk with drift on the seasonal component....
pred2 <- rwf(classical$seasonal, h=24, drift=TRUE)

# but, I want to add back in season for the predicted values....
# is this right????
original2 <- pred2$mean*pred1$mean

# This gets me the original timeseries... this is correct
original1 <- classical$seasonal*season.adj


# ---------
# CHAPTER 7
# ---------

# QUESTION 7.1
# ------------

# a
str(books)
head(books)
plot(books, xlab="days", main="Daily Book Sales")


#b

# PAPERBACK
# ---------
# small alpha gives more weight to past observations
# large alpha gives more weight to recent observations
fit1 <- ses(books[,1], alpha=0.25, h=4, level=c(80, 95), initial="simple")
fit2 <- ses(books[,1], alpha=0.5, h=4, level=c(80, 95), initial="simple")
fit3 <- ses(books[,1], alpha=0.75, h=4, level=c(80, 95), initial="simple")


# How to get multiple plots in the same window
par(mfrow=c(1,3))  #  I think does a 2x2 matrix of plots

plot(fit1, main="Alpha = 0.25")
plot(fit2, main="Alpha = 0.5")
plot(fit3, main="Alpha = 0.75")


SSE <- c(sum(fit1$residuals^2), sum(fit2$residuals^2), sum(fit3$residuals^2))
alpha <- c(0.25, 0.5, 0.75)
df1 <- data.frame(SSE, alpha)

# Saves the basis of a ggplot to a variable.  Then can add layers on top
# Breaks RATE up by the TYPE attribute, creates boxplots
library(ggplot2)
g <- ggplot(df1, aes(x = alpha, y = SSE))
g + geom_point(color = "purple", size=4)


# HARDCOVER
# ---------

# small alpha gives more weight to past observations
# large alpha gives more weight to recent observations
fit1b <- ses(books[,2], alpha=0.25, h=4, level=c(80, 95), initial="simple")
fit2b <- ses(books[,2], alpha=0.5, h=4, level=c(80, 95), initial="simple")
fit3b <- ses(books[,2], alpha=0.75, h=4, level=c(80, 95), initial="simple")


# How to get multiple plots in the same window
par(mfrow=c(1,3))  

plot(fit1b, main="Alpha = 0.25")
plot(fit2b, main="Alpha = 0.5")
plot(fit3b, main="Alpha = 0.75")


SSEb <- c(sum(fit1b$residuals^2), sum(fit2b$residuals^2), sum(fit3b$residuals^2))
alphab <- c(0.25, 0.5, 0.75)
df1b <- data.frame(SSEb, alphab)

# Saves the basis of a ggplot to a variable.  Then can add layers on top
# Breaks RATE up by the TYPE attribute, creates boxplots
library(ggplot2)
g2 <- ggplot(df1b, aes(x = alphab, y = SSEb))
g2 + geom_point(color = "purple", size=4)






# c

# PAPERBACK
# ---------
fit4 <- ses(books[,1], h=4, level=c(80, 95), initial="simple")
fit4$model

par(mfrow=c(1,1))
plot(fit4)


# HARDCOVER
# ---------
fit4b <- ses(books[,2], h=4, level=c(80, 95), initial="simple")
fit4b$model

par(mfrow=c(1,1))
plot(fit4b)





# d

# PAPERBACK
# ---------
fit5 <- ses(books[,1], h=4, level=c(80, 95), initial="optimal")
fit5$model

par(mfrow=c(1,1))
plot(fit5)


# HARDCOVER
# ---------
fit5b <- ses(books[,2], h=4, level=c(80, 95), initial="optimal")
fit5b$model

par(mfrow=c(1,1))
plot(fit5b)


# QUESTION 7.2
# ------------










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


# Experiment with timeseries decomposition
# Seasonal data
# Returns a list with "seasonal", "trend", and "random" components
y <- decompose (abt.ts, type=c("additive", "multiplicative"))
plot (y)


# ---------------------
# 3. Train / Test Split
# ---------------------

# For dataframe
ABT_train <- fullDataSet[c(1:48),]
ABT_test <- fullDataSet[c(49:60),]


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


# -----------------------
# WEEK 6 DISCUSSION BOARD
# -----------------------

# Continue with week 5 data....


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

# Plot the timeseries
plot(abt.ts, main="ABT stock", ylab="Adj. Close")


# Timeseries decomposition
# ------------------------
# Classical decomposition (* NOT RECOMMENDED APPROACH *
# Returns a list with "seasonal", "trend", and "random" components

classical.decomp <- decompose (abt.ts, type=c("additive", "multiplicative"))
plot (classical.decomp)


# Since data is monthly, can look at X12 ARIMA decomposition.
# But, no R package, check in SAS?


# STL decomposition

stl.decomp <- stl(abt.ts, t.window=10, s.window="periodic", robust=TRUE)
plot(stl.decomp)

# Trend / Cycle component along with original time series
plot(abt.ts, col="gray", main="ABT Adjusted Closing Price", ylab="Adj Closing Price", xlab="")
lines(stl.decomp$time.series[,2], col="red", ylab="Trend")


# Plot the seasonally adjusted data
library(forecast)
plot(abt.ts, col="gray", main="ABT Adjusted Closing Price", ylab="Adj Closing Price", xlab="")
lines(seasadj(stl.decomp), col="red", ylab="Seasonally Adjusted")




# Analyze additive vs multiplicative decomposition.
# lambda... ideal transformation
# to reverse a power transform raise value to the 1/lambda power

# Use BoxCox to determine proper lambda value.
lambda2 <- BoxCox.lambda(abt.ts)  # 0.3688217

# Plot transformed timeseries data
plot(BoxCox(abt.ts, lambda2), main="ABT data with BoxCox Transform")

# Raise original data to the lambda power to get transformed data...
# This matches the plot above... so this is working.
abt.ts.boxcox <- (abt.ts^0.3688217-1)/0.3688217
plot(abt.ts.boxcox, main="ABT data with manual BoxCox Transform")


stl.decomp.boxcox <- stl(abt.ts.boxcox, t.window=10, s.window="periodic", robust=TRUE)
plot(stl.decomp.boxcox)











# ---------------------
# 3. Train / Test Split
# ---------------------

# For dataframe
ABT_train <- fullDataSet[c(1:48),]
ABT_test <- fullDataSet[c(49:60),]


# For timeseries object
ABT_train_ts <- window(abt.ts, start=2010, end=2015)
ABT_test_ts <- window(abt.ts, start=2015)




# -----------------------
# WEEK 7 DISCUSSION BOARD
# -----------------------

# Continue with week 5 data....

# ----------------
# 1.  Acquire Data
# ----------------

# Read in a .csv file
# Read in the ABT stock data
fileLocation <- "C:/Users/James R. Herbick/Documents/Northwestern/PREDICT 413_TIME_SERIES/WEEK 5/ABT.csv"
fullDataSet <- read.table (file = fileLocation, header = TRUE, sep = ",", stringsAsFactors=FALSE)

# Try to create Date field as a Date datatype
fullDataSet$Date <- as.Date(fullDataSet$Date, format="%m/%d/%Y")

# Create timeseries object
abt.ts <- ts(fullDataSet$Adj.Close, frequency=12, start=c(2010,10))


# --------------------
# Holt-Winters Methods
# --------------------

# Create the forecasts
library(forecast)
fit1 <- hw(abt.ts, seasonal="additive")
fit2 <- hw(abt.ts, seasonal="multiplicative")

# Plot the forecasts, with original data
plot(fit2, ylab="ABT Adj Close", plot.conf=FALSE, type="o", fcol="white", xlab="Year")
lines(fitted(fit1), col="red", lty=2)
lines(fitted(fit2), col="green", lty=2)
lines(fit1$mean, type="o", col="red")
lines(fit2$mean, type="o", col="green")
legend("topleft", lty=1, pch=1, col=1:3, c("data", "Holt Winters' Additive", "Holt Winters' Multiplicative"))


# Plot the components of each model

states <- cbind(fit1$model$states[,1:3], fit2$model$states[,1:3])
colnames(states) <- c("level", "slope", "seasonal", "level", "slope", "seasonal")
plot(states, xlab="Year")








