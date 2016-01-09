
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

# QUESTION 1
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


# QUESTION 2
# ------------

# a

# Paperback
# used alpha calcualted above
fit10 <- holt(books[,1], alpha=0.2125, beta=0.25, initial="simple", h=4)
fit10$model

plot(fit10)


# hardcover
# used alpha calcualted above
fit10b <- holt(books[,2], alpha=0.3473, beta=0.25, initial="simple", h=4)
fit10b$model

plot(fit10b)



# b

# SES
accuracy(fit1)
accuracy(fit1b)


# Holt
accuracy(fit10)
accuracy(fit10b)


# c

# SES
fit1
fit1b

# Holt
fit10
fit10b


# Manual calculation for paperback using SES
x <- books[30,1]*.25

y <- 0
t <- 1

for (i in 29:1)
{
    y <- y + (books[i,1])*(.25)*(.75)^t
    t <- t+1
    print(y)
}

x+y


# Manual calculation for hardcover using SES
x <- books[30,2]*.25

y <- 0
t <- 1

for (i in 29:1)
{
    y <- y + (books[i,2])*(.25)*(.75)^t
    t <- t+1
    print(y)
}

x+y


# Manual calculation for paperback using Holt


L0 <- 199
B0 <- -27

alpha <- 0.2125
beta <- 0.25
e <- c(0)

e[1] <- 27
lt[1] <- L0 + B0 + alpha*e[1]
bt[1] <- B0 + alpha*beta*e[1]


yhat[1] <- lt1 + 1*bt1


for (i in 2:30)
{
    
    e[i] <- books[i,1] - fit10$fitted[i]
    lt[i] <- lt[i-1] + bt[i-1] + alpha*e[i]
    bt[i] <- bt[i-1] + alpha*beta*e[i]
   
    yhat[i] <- lt[i] + i*bt[i] 
}


# Manual calculation for hardcover using Holt


L0 <- 139
B0 <- -11

alpha <- 0.3473
beta <- 0.25
e <- c(0)

e[1] <- 11
lt[1] <- L0 + B0 + alpha*e[1]
bt[1] <- B0 + alpha*beta*e[1]


yhat[1] <- lt1 + 1*bt1


for (i in 2:30)
{
    
    e[i] <- books[i,2] - fit10b$fitted[i]
    lt[i] <- lt[i-1] + bt[i-1] + alpha*e[i]
    bt[i] <- bt[i-1] + alpha*beta*e[i]
    
    yhat[i] <- lt[i] + i*bt[i] 
}






# ----------
# Question 3
# -----------

# a)        
plot(ukcars, main="ukcars Dataset Plot")    


# b)
# STL decomposition
uk.stl <- stl(ukcars, t.window=15, s.window="periodic", robust=TRUE)
plot(uk.stl, main="STL Decomposition of ukcars")

# obtain seasonally adjusted data
uk.seasadj <- seasadj(uk.stl)
plot(uk.seasadj, main="Seasonally Adjusted ukcars")


# c)

# 2 year forecasts with Holt-Winters additive damped method
uk.fcast1 <- hw(uk.seasadj, h=8, damped=TRUE, seasonal="additive")
plot(uk.fcast1)

# reseasonalize the data


# d)
uk.fcast2 <- holt(uk.seasadj, h=8, alpha=0.8, beta=0.2, initial="simple")
plot(uk.fcast2)


# e)
uk.fcast3 <- ets(uk.seasadj)


# f)

# g)


# Question 4
# ----------

# a)
plot(visitors, main="Visitors Dataset")

# b)
# 2 year forecasts with Holt-Winters multiplicative method
visit.fcast1 <- hw(visitors, h=24, seasonal="multiplicative")
plot(visit.fcast1)

# d)
# Holt-Winters multiplicative damped method
visit.fcast2 <- hw(visitors, h=24, seasonal="multiplicative", damped=TRUE)
plot(visit.fcast2)

# e)

# f)

# Holt-Winters multiplicative
visit.fcast3 <- hw(visitors, h=24, seasonal="multiplicative")
plot(visit.fcast3$residuals, type="p", main="Holt-Winters Mult Residuals") # scatterplot of residuals.
plot(visit.fcast3)

# ETS model
visit.fcast4 <- ets(visitors)
plot(visit.fcast4$residuals, type="p", main="ETS Residuals") # scatterplot of residuals.
plot(visit.fcast4)

# Box-Cox Transformation of original dataset
visit.lambda <- BoxCox.lambda(visitors)  # 0.2775249
visit.transform <- BoxCox(visitors, 0.2775249)
plot(visit.transform, main="BoxCox Transformed Visitors Dataset")

# Additive ETS, Box-Cox transformed series
visit.fcast5 <- ets(visit.transform, model="AAA", damped=FALSE, additive.only = TRUE)
plot(visit.fcast5$residuals, type="p", main="Additive ETS Residuals - BoxCox Transform") # scatterplot of residuals.
plot(visit.fcast5)

# Seasonal Naive, Box-Cox transformed series
# which model do I use here???

# STL decomposition applied to BoxCox, followed by ETS
visit.stl <- stl(visit.transform, t.window=15, s.window="periodic", robust=TRUE)
plot(visit.stl, main="STL Decomposition of visitors")

# Ensure 1 plot per window
par(mfrow=c(1,1))  

# obtain seasonally adjusted data
visit.seasadj <- seasadj(visit.stl)
plot(visit.seasadj, main="Seasonally Adjusted visitors")

# ETS model
visit.fcast6 <- ets(visit.seasadj)
plot(visit.fcast6$residuals, type="p", main="ETS Residuals, STL Decomposed - BoxCox Transform") # scatterplot of residuals.
plot(visit.fcast6)





# ---------------------------------------------------------
# ** QUESTION 7.3  - wrong question from hardcover book. **
# ---------------------------------------------------------

str(eggs)
eggs
plot(eggs, main="Price of Eggs 1900 - 1993")


# regular and exponential, not damped
# gives weights to distant observations
egg1 <- holt(eggs, alpha=0.25, beta=0.25, initial="simple", h=100)
# had to add na.remove=true
egg2 <- holt(eggs, alpha=0.25, beta=0.25, initial="simple", h=100, exponential=TRUE, na.remove(TRUE))

# damped
egg3 <- holt(eggs, alpha=0.25, beta=0.25, initial="simple", h=100, damped = TRUE)
egg4 <- holt(eggs, alpha=0.25, beta=0.25, initial="simple", h=100, exponential=TRUE, damped = TRUE, na.remove(TRUE))


# change alpha parameter values
# weights most recent observations the heaviest
# regular and exponential, not damped
egg5 <- holt(eggs, alpha=0.8, beta=0.25, initial="simple", h=100)
egg6 <- holt(eggs, alpha=0.8, beta=0.25, initial="simple", h=100, exponential=TRUE, na.remove(TRUE))

# change alpha parameter damped
egg7 <- holt(eggs, alpha=0.8, beta=0.25, initial="simple", h=100, damped = TRUE)
egg8 <- holt(eggs, alpha=0.8, beta=0.25, initial="simple", h=100, exponential=TRUE, damped = TRUE, na.remove(TRUE))


# change beta parameter values
# increases the trend component
# regular and exponential, not damped
egg9 <- holt(eggs, alpha=0.25, beta=0.75, initial="simple", h=100)
egg10 <- holt(eggs, alpha=0.25, beta=0.75, initial="simple", h=100, exponential=TRUE)

# not working
# damped
#egg11 <- holt(eggs, alpha=0.25, beta=0.75, initial="simple", h=100, damped=TRUE)
#egg12 <- holt(eggs, alpha=0.25, beta=0.75, initial="simple", h=100, exponential=TRUE, damped=TRUE)


# change both alpha and beta parameters
egg13 <- holt(eggs, alpha=0.8, beta=0.75, initial="simple", h=100)
egg14 <- holt(eggs, alpha=0.8, beta=0.75, initial="simple", h=100, exponential=TRUE, na.remove(TRUE))

# damped
egg15 <- holt(eggs, alpha=0.8, beta=0.75, initial="simple", h=100, damped = TRUE)
egg16 <- holt(eggs, alpha=0.8, beta=0.75, initial="simple", h=100, exponential=TRUE, damped = TRUE)


# not working (first two rows work)
a <- data.frame(accuracy(egg1))
a <- rbind(a, accuracy(egg2))
rownames(a) <- c("egg1", "egg2")
# accuracy(egg3), accuracy(egg4), accuracy(egg5), accuracy(egg6), accuracy(egg7), accuracy(egg8), accuracy(egg9), accuracy(egg10), accuracy(egg13), accuracy(egg14), accuracy(egg15), accuracy(egg16))
# rownames(a) <- c("egg1", "egg2", "egg3", "egg4", "egg5", "egg6", "egg7", "egg8", "egg9", "egg10", "egg13", "egg14", "egg15", "egg16")


accuracy(egg1)
accuracy(egg2)
accuracy(egg3)
accuracy(egg4)
accuracy(egg5)
accuracy(egg6)
accuracy(egg7)
accuracy(egg8)
accuracy(egg9)
accuracy(egg10)
accuracy(eegg13)
accuracy(egg14) 
accuracy(egg15)
accuracy(egg16)



# ---------
# CHAPTER 8
# ---------
library(fpp)

# QUESTION 5
# ----------

# a) 
y <- ts(numeric(100))
e <- rnorm(100)
for (i in 2:100)
    y[i] <- 0.6*y[i-1] + e[i]

# b)

# plot part a
plot(y, main="0.6")

# change value to 0.2
y <- ts(numeric(100))
for (i in 2:100)
    y[i] <- 0.2*y[i-1] + e[i]

plot(y, main="0.2")

# change value to 0.9
y <- ts(numeric(100))
for (i in 2:100)
    y[i] <- 0.9*y[i-1] + e[i]

plot(y, main="0.9")


# c)
# build MA(1) model
y2 <- ts(numeric(200))
e2 <- rnorm(200)
for (i in 2:200)
    y2[i] <- e2[i] + 0.6*e2[i-1]


# d)
plot(y2, main="MA(1) - 0.6")

y2 <- ts(numeric(200))
e2 <- rnorm(200)
for (i in 2:200)
    y2[i] <- e2[i] + 0.2*e2[i-1]

plot(y2, main="MA(1) - 0.2")


y2 <- ts(numeric(200))
e2 <- rnorm(200)
for (i in 2:200)
    y2[i] <- e2[i] + 0.9*e2[i-1]

plot(y2, main="MA(1) - 0.9")


# e)

y4 <- ts(numeric(100))
e4 <- rnorm(100)
for (i in 2:100)
    y4[i] <- 0.6*y4[i-1] + e4[i] + e4[i] + 0.6*e4[i-1]



# f)

y3 <- ts(numeric(100))
e3 <- rnorm(100)
for (i in 3:100)
    y3[i] <- 0.8*y3[i-1] + 0.3*y3[i-2] + e3[i]


# g)

plot(y3, main="AR(2) Model")

plot(y4, main="ARMA(1,1) Model")



# ----------
# QUESTION 6
# ----------

# a)
# Plot the data
plot(wmurders, main="wmurders dataset")

# Determine if and the number of differencing needed
# Run tests for differencing
adf.test(wmurders, alternative = "stationary")
kpss.test(wmurders)

ndiffs(wmurders)   # 2

# difference data
diff2<- diff(wmurders, differences=2)

# plot diff2 acf and pacf
tsdisplay(diff2, main="2 Differences")





# d)
manual <- Arima(wmurders, order = c(1,2,0), include.constant = TRUE)
par(mfrow=c(1,1))
plot(manual$residuals, type="p", main="ARIMA(1,2,0) residuals")

accuracy(manual)


# e)
fcast1 <- forecast(manual, h=3)
plot(fcast1)


# g)
x <- auto.arima(wmurders, seasonal=FALSE)
plot(x$residuals, type="p", main="ARIMA(1,2,1) residuals")

accuracy(x)


# ----------
# QUESTION 7
# ----------

# a)
plot(austourists, main="Austrialian Tourists")

# b,c)
tsdisplay(austourists)

# d)
tour.diff1 <- diff(austourists, 4)
tsdisplay(tour.diff1)

tour.2 <- Arima(austourists, order=c(0,1,0), seasonal=c(1,1,0))
accuracy(tour.2)


# e)
tour.1 <- auto.arima(austourists)
accuracy(tour.1)


# ----------
# QUESTION 8
# ----------

plot(usmelec)

# a)
plot(usmelec, main="US Elec Data w/ 12-Month MA")
lines(ma(usmelec, order=12), col="red")

# b)
lambda <- BoxCox.lambda(usmelec)  # -0.4772402
elec.trans <- BoxCox(usmelec, lambda)
plot(elec.trans, main="Transformed Elec Data")

# c)
# perform tests for stationary data
z <- adf.test(elec.trans, alternative = "stationary")
z2 <- kpss.test(elec.trans)

# determine number of differences needed
num.diff <- ndiffs(elec.trans)
elec.trans.diff <- diff(elec.trans, differences=1)
plot(elec.trans.diff, main="1 Diff of BoxCox Transformed Data")


# d)
tsdisplay(elec.trans.diff)
mod1 <- Arima(elec.trans.diff, order=c(0,0,0), seasonal = c(0,0,4))

mod2 <- auto.arima(elec.trans.diff)  # doesn't look like it's working



    
    
    
    
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




# -------------
# FINAL PROJECT
# -------------

# ----------------
# 1.  ACQUIRE DATA
# ----------------

# Try reading in John's new file....
# ----------------------------------
# Read in a .csv file
# Need to convert date to date format for the dataframe
fileLocation <- "C:/Users/James R. Herbick/Documents/Northwestern/PREDICT 413_TIME_SERIES/Final Project/John/Final_Data.csv"
fullDataSet <- read.table (file = fileLocation, header = TRUE, sep = ",", stringsAsFactors=FALSE)




# ---------------------------------

# Read in a .csv file
# Need to convert date to date format for the dataframe
# fileLocation <- "C:/Users/James R. Herbick/Documents/Northwestern/PREDICT 413_TIME_SERIES/Final Project/John/environment_baseline.csv"
# fullDataSet <- read.table (file = fileLocation, header = TRUE, sep = ",", stringsAsFactors=FALSE)


# Create time series object for untransformed CO2 emissions.
CO2.ts.raw <- ts(fullDataSet$EmissionsDataRaw, frequency=12, start=c(1973,1))

# Create time series object for transformed CO2 emissions by population.
CO2.ts <- ts(fullDataSet$EmissionsData, frequency=12, start=c(1973,1))


# ------
# 2. EDA
# ------

plot(CO2.ts.raw, main="CO2 Emissions - Residential")
plot(CO2.ts, main="Transformed CO2 Emissions")


# ---------------------------
# 3. TRAIN / VAL / TEST SPLIT
# ---------------------------

# 50%, 25%, 25%
# train <- ts(co2.ts.pop[1:243], frequency=12, start=c(1973,1))
# val <- ts(co2.ts.pop[244:364], frequency=12, start=c(1993,4))
# test <- ts(co2.ts.pop[365:486], frequency=12, start=c(2003,5))

# Use John's Split
# Split time series into training (51.85%), valiation (24.69%) and test sets (23.46%).
CO2.train <- window(CO2.ts, start=c(1973, 01), end=c(1993, 12), frequency=12)
CO2.validation <- window(CO2.ts, start=c(1994, 01), end=c(2003, 12), frequency=12)
CO2.test <- window(CO2.ts, start=c(2004, 01), end=c(2013, 06), frequency=12)



# ---------------
# 4. BUILD MODELS
# ---------------

# BASELINE - Drift method / random walk forecast
ranwalk <- rwf(CO2.train, h=12, drift=TRUE)
plot(ranwalk, main="Baseline Random Walk Forecast with Drift")
accuracy(ranwalk)


# SEASONAL DECOMPOSITION - STL Decomposition
# Looks like seasonality with non-linear trend....
CO2.stl <- stl(CO2.train, t.window=13, s.window="periodic", robust=TRUE)
plot(CO2.stl)




# XREG WITH EXTERNAL DATA, LAGGED ????











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


# -----------------
# WEEK 8 DISCUSSION
# -----------------

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


# ---------------------
# 2. Train / Test Split
# ---------------------

# For dataframe
ABT_train <- fullDataSet[c(1:48),]
ABT_test <- fullDataSet[c(49:60),]


# For timeseries object
ABT_train_ts <- window(abt.ts, start=2010, end=2015)
ABT_test_ts <- window(abt.ts, start=2015)


# ---------------
# 3. BUILD MODELS
# ---------------

library(forecast)
fit <- auto.arima(ABT_train_ts, seasonal=FALSE)

# plot forecast
plot(forecast(fit, h=10))


# -----------
# 4. TEST FIT
# -----------

par(mfrow=c(1,2))
Acf(ABT_train_ts)
Pacf(ABT_train_ts)


# -----------------
# WEEK 9 DISCUSSION
# -----------------

# Build 3 models on ABT, use 80/20 data split for train / test


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


# ---------------------
# 2. Train / Test Split
# ---------------------

# For dataframe
ABT_train <- fullDataSet[c(1:48),]
ABT_test <- fullDataSet[c(49:60),]


# For timeseries object
ABT_train_ts <- window(abt.ts, start=2010, end=2015)
ABT_test_ts <- window(abt.ts, start=2015)


# ------
# 3. EDA
# ------

# Plot original timeseries
plot(abt.ts, main="ABT stock", ylab="Adj. Close")

# Seasonally decompose training data using STL
abt.stl <- stl(ABT_train_ts, t.window=10, s.window="periodic", robust=TRUE)
plot(abt.stl, main="STL Decomp of ABT Training Data")


# -----------------------
# 4. BUILD / CHOSE MODELS
# -----------------------

# Model 1 - STL Decomposition
fcast1 <- forecast(abt.stl, method="ets", h=12)
plot(fcast1, main="Model 1: STL Decomp using ETS Method")


# Model 2 - auto.arima
library(forecast)
fit2 <- auto.arima(ABT_train_ts, seasonal=TRUE)
fit2$aicc  #  170.6206

# plot forecast
fcast2 <- forecast(fit2, h=12)  # ARIMA(1,1,0) with drift
plot(fcast2, main="Model 2: auto.arima w/ Seasonality")


# Model 3 - manually selected ARIMA

# Step 1- transformation / data stationary?
# No significant fluctuations in variance, will not perform transformations
# Determine if data is stationary and number of differences
adf.test(ABT_train_ts, alternative="stationary")
kpss.test(ABT_train_ts)

# Step 2 - difference the data
ndiffs(ABT_train_ts)  #1

# Doesn't seem to be an overwhelming seasonal component, so just 1 reg diff
abt.diff <- diff(ABT_train_ts)

# Step 3 - Review ACF / PACF plots
tsdisplay(abt.diff)

# Step 4 - Build model and tweak alternatives
# Check AICc
fit3 <- Arima(ABT_train_ts, order = c(0,1,0), seasonal = c(0,0,0))
fit3$aicc   # 175.0459

fit4 <- Arima(ABT_train_ts, order = c(1,1,1), seasonal = c(0,0,0))
fit4$aicc   # 179.0017

fit5 <- Arima(ABT_train_ts, order = c(0,1,0), seasonal = c(1,0,0))
fit5$aicc   # 177.0852

fit6 <- Arima(ABT_train_ts, order = c(1,1,0), seasonal = c(1,0,0))
fit6$aicc   # 178.8849

fit7 <- Arima(ABT_train_ts, order = c(0,1,0), seasonal = c(0,0,0), include.constant = TRUE)
fit7$aicc   # 171.216


# -----------
# 4. TEST FIT
# -----------

# Model 1
accuracy(fcast1, ABT_test_ts)

# Model 2
accuracy(fcast2, ABT_test_ts)
fit2$aicc  #  170.6206

# Model 7
# plot forecast
fcast3 <- forecast(fit7, h=12)  # ARIMA(0,1,0) with constant
plot(fcast2, main="Model 7: Manual Arima w/ Constant")
accuracy(fcast3, ABT_test_ts)
fit7$aicc   # 171.216





