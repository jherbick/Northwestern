
# PREDICT 422 - MACHINE LEARNING LABS
# ISLR BOOK


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

# Select multiple rows or columns using vectors
A[c(1,3),c(2,4)]

A[1:3,2:4]   # rows 1:3, columns 2:4

A[1:2,]   # rows 1:2, all columns

A[ , 1:2] # all rows, columns 1:2

A[-c(1,3),]   # all rows EXCEPT 1 and 3, all columns

# ID the number of rows, columns in a matrix or other object
dim(A)


# Loading / Writing Data

read.table()
write.table()

# Read in a .csv file
# Read in the full Auto dataset
# Use na.strings argument to id missing values
fileLocation <- "C:/Users/James R. Herbick/Documents/Northwestern/PREDICT 422_Machine Learning/ISLR datasets/Auto.csv"
# Doesn't work on this dataset for some reason, but read.csv does.....
fullDataSet <- read.table (file = fileLocation, header = TRUE, sep = ",", stringsAsFactors=FALSE, na.strings = "?")

fullDataSet <- read.csv (file = fileLocation, header = TRUE, sep = ",", stringsAsFactors=FALSE, na.strings = "?")


# Validate full dataset per Prof. Martin instructions
# Gives metadata of an object
str(fullDataSet)

# Write R dataframes / datasets to a .csv
write.csv(Boston, file = "C:/Users/James R. Herbick/Documents/Northwestern/PREDICT 422_Machine Learning/ISLR datasets/Boston.csv")
write.csv(Carseats, file = "C:/Users/James R. Herbick/Documents/Northwestern/PREDICT 422_Machine Learning/ISLR datasets/Carseats.csv")


# View data in a spreadsheet like window
fix(fullDataSet)

# Remove rows / observations with missing data
fullDataSet=na.omit(fullDataSet)

# Check variable names
names(fullDataSet)

plot(fullDataSet$cylinders, fullDataSet$mpg)

# Make columns / variables in a dataframe available by name only
attach(fullDataSet)
plot(cylinders, mpg)


# Convert a variable to a factor (categorical variable)
fullDataSet$cylinders=as.factor(fullDataSet$cylinders)

# Create boxplots when x axis is a factor / categorical variable
plot(cylinders, mpg)

# Histograms
hist(mpg, col="red")

# Scatterplot matrix
pairs(fullDataSet)  # not working... because of factor??
# Use only a subset of the dataframe's variables
pairs(~ mpg + displacement + horsepower + weight, fullDataSet)  

# Identify() allows you to mark a point and see the value
# Need to mark the point, hit esc, and then esc again.....
# the third argument tells what value to print for the point.
plot(horsepower, mpg)
identify(horsepower, mpg, name)


#----------
# CHAPTER 3
#----------

# Install a package
install.packages("ISLR")

# Load a package
library(MASS)
library(ISLR)

# Show dataset in a pop up window
fix(Boston)

# Show variable names
names(Boston)

# Find out more about the dataset
?Boston

# Simple linear regression
attach(Boston)
fit1 <- lm(medv~lstat)

# View info about the model
fit1
summary(fit1)

# Find out what other data exists in the model
names(fit1)

# View the coefficients
fit1$coefficients
coef(fit1)

# Obtain confidence intervals for the model coefficients
confint(fit1)

# Predict new values with the model (data that wasn't used in training)
# First uses confidence intervals, second uses prediction intervals.
predict1 <- predict(fit1, data.frame(lstat <- (c(5, 10, 15))), interval = "confidence")

predict1 <- predict(fit1, data.frame(lstat <- (c(5, 10, 15))), interval = "prediction")


# Plot dependent and independent data, and regression line.
plot(Boston$lstat, Boston$medv)
# col= is the line color, lwd= is the line weight, pch= different plotting symbols
abline(fit1, col="red", lwd=3)

# abline() can be used to draw any line with an
# intercept of a and a slope of b
abline(a, b)

# Various plotting variations
plot(Boston$lstat, Boston$medv, col="red")
plot(Boston$lstat, Boston$medv, pch=20)
plot(Boston$lstat, Boston$medv, pch="+")
plot(1:20, 1:20, pch=1:20)


# Plots from linear regression
# Standard plots with the model
par(mfrow=c(2,2))
plot(fit1)

# Others
residuals()  # model's residuals
rstudent()   # model's studentized residuals

# Plot fitted values vs. residuals 
plot(predict(fit1), residuals(fit1))
# Plot fitted values vs. studentized residuals
plot(predict(fit1), rstudent(fit1))

# Leverage statisitics
plot(hatvalues(fit1))
which.max(hatvalues(fit1))  # index of the largest leverage statistic


# Multiple linear regression
# --------------------------

fit2 <- lm(medv~lstat + age, data=Boston)

fit2
summary(fit2)

# Multiple regression using all independent variables
fit3 <- lm(medv~., data=Boston)
summary(fit3)


# what individual element of the linear regression summary are available?
?summary.lm

# Access individual components of the linear regression summary
summary(fit3)$r.sq  # r squared
summary(fit3)$sigma # RSE (residual standard error)

# Variation Inflation Factors (VIFs)
library(car)
vif(fit3)

# Remove a predictor variable from a full model (use all but a few variables)
fit4 <- lm(medv~.-age, data=Boston)
summary(fit4)

# or use the update() function
fit4 <- update(fit3, ~.-age)  # remove age variable from previous model


# Interaction terms
# * includes individual items and the interaction term
# lstat:age would only include the interaction term
fit5 <- lm(medv~lstat*age, data=Boston)
summary(fit5)


# Non linear transformations of the predictors
# Need the I() function to ensure that the standard form of ^2 is used
fit6 <- lm(medv ~ lstat + I(lstat^2), data=Boston)
summary(fit6)


# How much better is non linear fit over linear fit?
# H0: both models fit the data equally well.
anova(fit1, fit6)

par(mfrow=c(2,2))
plot(fit6)

# Higher order polynomial orders for linear regression
# use poly()
fit7 <- lm(medv~poly(lstat,5), data=Boston)
summary(fit7)

# Log transformations
fit8 <- lm(medv~log(rm), data=Boston)
summary(fit8)


# Qualitative Predictors (need to be setup as Factors)
# ---------------------------------------------------
library(ISLR)
fix(Carseats)
names(Carseats)
str(Carseats)

# Linear regression with all predictors and 2 interaction terms
car.fit1 <- lm(Sales~. + Income:Advertising + Price:Age, data=Carseats)
summary(car.fit1)

# See dummy variable coding for a qualitative variable
attach(Carseats)
contrasts(ShelveLoc)

# Learn about other contrasts and how to use them
?contrasts


# Writing Functions
# -----------------

# Define the function
LoadLibraries <- function(){
    library(ISLR)
    library(MASS)
    print("The libraries have been loaded.")
}

# View details of the function
LoadLibraries

# Call the function
LoadLibraries()


# ---------
# CHAPTER 5
# ---------


# Chaper 5 Lab: Cross-Validation and the Bootstrap

# ---------------------------
# The Validation Set Approach
# ---------------------------

library(ISLR)
set.seed(1)
# pick 196 out of 392 random numbers (Auto has 392 obs)
# this just picks 196 random integers between 1:392
train=sample(392,196)  

# subset option takes just the obs from the train variable.
# subset = optional vector specifying a subset of observations to use in the fitting process
lm.fit=lm(mpg~horsepower,data=Auto,subset=train)

# Calculate MSE for test set observations
attach(Auto)
# This is y - yhat squared and the mean of all those ... MSE
mean((mpg-predict(lm.fit,Auto))[-train]^2)

# Estimate test error for polynomial and cubic regressions
lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)

lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)


# Perform same model fitting on a different training set.
set.seed(2)
train=sample(392,196)

lm.fit=lm(mpg~horsepower,subset=train)
mean((mpg-predict(lm.fit,Auto))[-train]^2)

# horsepower squared
lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)

# horsepower cubed
lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)


# ------------------------------
# Leave-One-Out Cross-Validation
# ------------------------------

# glm function with no family argument performs linear regression.
glm.fit=glm(mpg~horsepower,data=Auto)
coef(glm.fit)
# These are equal
lm.fit=lm(mpg~horsepower,data=Auto)
coef(lm.fit)

# adds up individual MSEs and averages (formula 5.1 in book)
library(boot)
glm.fit=glm(mpg~horsepower,data=Auto)
cv.err=cv.glm(Auto,glm.fit)
cv.err$delta

# This calculates the cv error for regressions using horsepower raised to 1 thru 5 power
# rep repeats the value 0, 5 times
cv.error=rep(0,5)
for (i in 1:5){
    glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
    cv.error[i]=cv.glm(Auto,glm.fit)$delta[1]
}
cv.error


# -----------------------
# k-Fold Cross-Validation
# -----------------------

# Must be doing k fold validation on each model. Uses 10 folds.
set.seed(17)
cv.error.10=rep(0,10)
for (i in 1:10){
    glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
    cv.error.10[i]=cv.glm(Auto,glm.fit,K=10)$delta[1]
}
cv.error.10


# -------------
# The Bootstrap
# -------------

# Portfolio data has X, and Y attributes.
# Formula 5.6
alpha.fn=function(data,index){
    X=data$X[index]
    Y=data$Y[index]
    return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}
# Run this function to calculate alpha based on all 100 observations.
alpha.fn(Portfolio,1:100)

# This runs the function taking a random sample from Portfolio with replacement.
# This takes one random sample
set.seed(1)
alpha.fn(Portfolio,sample(100,100,replace=T))

# This automates up above, and runs it 1000 times.
boot(Portfolio,alpha.fn,R=1000)


# Estimating the Accuracy of a Linear Regression Model

# Return the coefficients of a linear regression model
boot.fn=function(data,index)
    return(coef(lm(mpg~horsepower,data=data,subset=index)))

# Run the function
boot.fn(Auto,1:392)

# Create two bootstrap samples when running the function
set.seed(1)
boot.fn(Auto,sample(392,392,replace=T))
boot.fn(Auto,sample(392,392,replace=T))

# Bootstrap using 1000 bootstraps (i.e. random samples, with replacement)
boot(Auto,boot.fn,1000)

# Obtain same standard errors from the regression process
summary(lm(mpg~horsepower,data=Auto))$coef


# Same process for horsepower raised to the 2 power
boot.fn=function(data,index)
    coefficients(lm(mpg~horsepower+I(horsepower^2),data=data,subset=index))

set.seed(1)
boot(Auto,boot.fn,1000)

summary(lm(mpg~horsepower+I(horsepower^2),data=Auto))$coef




# ---------
# Chapter 6
# ---------

# JRH stepwise variable selection from R for Everyone book
# Looks like the step function can do forward, backward, and stepwise selection
# step iterates through models, scope specifies lower and upper boundaries on possible models
# Object, I guess, is a full linear regression model

s1 <- step(object, scope, scale=0, direction = c("both", "backward", "forward"), steps = 100)

library(ISLR)
a <- lm(Salary~., data = Hitters)
s1 <- step(a, scale=0, direction = "both")


# --------

# -----------------------------------------
# Chapter 6 Lab 1: Subset Selection Methods
# -----------------------------------------

# ---------------------
# Best Subset Selection
# ---------------------

# is.na() can be used to id missing values... returns a vector of TRUE and FALSEs
# sum() can be used to count the missing values

library(ISLR)

fix(Hitters)  # allows you to edit an object, pops up an edit window
names(Hitters)
dim(Hitters)

sum(is.na(Hitters$Salary))  # count number of missing values

Hitters=na.omit(Hitters)  # removes observations with any missing values
dim(Hitters)
sum(is.na(Hitters))

# Run best subset selection
# by default, goes up to an eight variable model
library(leaps)
regfit.full=regsubsets(Salary~.,Hitters)
summary(regfit.full)

# Run best subset selection up to a 19 variable model
regfit.full=regsubsets(Salary~.,data=Hitters,nvmax=19)
reg.summary=summary(regfit.full)

# Get model fit statistics for all models
names(reg.summary)
reg.summary$rsq

# Create two plots for RSS and adj R squared
par(mfrow=c(2,2))
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")

# Find highest adj r squared and add a red dot (returns the index number)
which.max(reg.summary$adjr2)
points(11,reg.summary$adjr2[11], col="red",cex=2,pch=20)

# Plot the Cp and BIC in similar fashion and find the minimum
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
which.min(reg.summary$cp)
points(10,reg.summary$cp[10],col="red",cex=2,pch=20)
which.min(reg.summary$bic)
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
points(6,reg.summary$bic[6],col="red",cex=2,pch=20)

# regsubsets() built in plot functions... not that great here all at once
plot(regfit.full,scale="r2")
plot(regfit.full,scale="adjr2")
plot(regfit.full,scale="Cp")
plot(regfit.full,scale="bic")

# See coefficients related to a specific model
coef(regfit.full,6)

# ---------------------------------------
# Forward and Backward Stepwise Selection
# ---------------------------------------

# Forward selection
regfit.fwd=regsubsets(Salary~.,data=Hitters,nvmax=19,method="forward")
summary(regfit.fwd)

# Backward selection
regfit.bwd=regsubsets(Salary~.,data=Hitters,nvmax=19,method="backward")
summary(regfit.bwd)

coef(regfit.full,7)

coef(regfit.fwd,7)

coef(regfit.bwd,7)


# ---------------------
# Choosing Among Models
# ---------------------

# Split into train / test
set.seed(1)
train=sample(c(TRUE,FALSE), nrow(Hitters),rep=TRUE)
# Test is the reverse of train (change FALSE to TRUEs)
test=(!train)

# Run this on only training rows
regfit.best=regsubsets(Salary~.,data=Hitters[train,],nvmax=19)

# Run this only on test rows
test.mat=model.matrix(Salary~.,data=Hitters[test,])

val.errors=rep(NA,19)
for(i in 1:19){
    coefi=coef(regfit.best,id=i)
    pred=test.mat[,names(coefi)]%*%coefi
    val.errors[i]=mean((Hitters$Salary[test]-pred)^2)
}
val.errors

which.min(val.errors)
coef(regfit.best,10)

# Write a function to do those prediction steps for regsubsets()
# There is no predict function for regsubsets()
predict.regsubsets=function(object,newdata,id,...){
    form=as.formula(object$call[[2]])
    mat=model.matrix(form,newdata)
    coefi=coef(object,id=id)
    xvars=names(coefi)
    mat[,xvars]%*%coefi
}

# View coefficients of the 10-variable model, because it was the best
# performer
regfit.best=regsubsets(Salary~.,data=Hitters,nvmax=19)
coef(regfit.best,10)

# Create a vector that assigns each observation to one of 10 folds
k=10
set.seed(1)
folds=sample(1:k,nrow(Hitters),replace=TRUE)
cv.errors=matrix(NA,k,19, dimnames=list(NULL, paste(1:19)))

# Perform cross validation
for(j in 1:k){
    best.fit=regsubsets(Salary~.,data=Hitters[folds!=j,],nvmax=19)
    for(i in 1:19){
        pred=predict(best.fit,Hitters[folds==j,],id=i)
        cv.errors[j,i]=mean( (Hitters$Salary[folds==j]-pred)^2)
    }
}


mean.cv.errors=apply(cv.errors,2,mean)
mean.cv.errors
par(mfrow=c(1,1))
plot(mean.cv.errors,type='b')



reg.best=regsubsets(Salary~.,data=Hitters, nvmax=19)
coef(reg.best,11)


# -----------------------------------------------
# Chapter 6 Lab 2: Ridge Regression and the Lasso
# -----------------------------------------------

# Need to pass an x matrix and a y vector to perform
# Ridge and Lasso
x=model.matrix(Salary~.,Hitters)[,-1]
y=Hitters$Salary

library(glmnet)

# Ridge Regression
# glmnet automatically standardizes the variables
# to turn this off, use standardize=FALSE
grid=10^seq(10,-2,length=100)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)

dim(coef(ridge.mod))

# Find the lambda value and the coefficients for the 50th item
# and the l2 norm
ridge.mod$lambda[50]
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1,50]^2))

# Find the lambda value and the coefficients for the 60th item
# and the l2 norm
ridge.mod$lambda[60]
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1,60]^2))

# Use predict function to obtain ridge regression coefficients
# for a new value of lambda=50
predict(ridge.mod,s=50,type="coefficients")[1:20,]

# Create random set of n numbers to split into train / test
set.seed(1)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]

# Fit ridge regression model on training set
# evaluate MSE on test set.
ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid, thresh=1e-12)
ridge.pred=predict(ridge.mod,s=4,newx=x[test,])
mean((ridge.pred-y.test)^2)

mean((mean(y[train])-y.test)^2)

ridge.pred=predict(ridge.mod,s=1e10,newx=x[test,])
mean((ridge.pred-y.test)^2)

# Ridge regression with lambda=0.  Same as OLS regression.
ridge.pred=predict(ridge.mod,s=0,newx=x[test,],exact=T)
mean((ridge.pred-y.test)^2)

# OLS regression.  Coefficients are same as above.
lm(y~x, subset=train)
predict(ridge.mod,s=0,exact=T,type="coefficients")[1:20,]

# Use cross validation to select the value of lambda.
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam

# Test MSE associated with this lambda.
ridge.pred=predict(ridge.mod,s=bestlam,newx=x[test,])
mean((ridge.pred-y.test)^2)

# Fit the Ridge regression on the full dataset
# Use value of lambda chosen by cross validaion.
out=glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlam)[1:20,]


# ---------
# The Lasso
# ---------

# Use glmnet() function, but use alpha=1 for Lasso.
lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)

# Perform cross validation and compute test MSE
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2)

# Examine Lasso model coefficients... some are = 0
out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:20,]
lasso.coef

lasso.coef[lasso.coef!=0]  #View coefficients that are not=0.


# ---------------------------------------
# Chapter 6 Lab 3: PCR and PLS Regression
# ---------------------------------------

# -------------------------------
# Principal Components Regression
# -------------------------------

library(pls)
set.seed(2)
pcr.fit=pcr(Salary~., data=Hitters,scale=TRUE,validation="CV")
# Shows the % variance explained for the predictors and response
# using different components
summary(pcr.fit)

# Plot the cross validation MSE to determine number of principal components
validationplot(pcr.fit,val.type="MSEP")

# Perform PCR on the training data and evaluate test set performance
set.seed(1)
pcr.fit=pcr(Salary~., data=Hitters,subset=train,scale=TRUE, validation="CV")
validationplot(pcr.fit,val.type="MSEP")

# Compute test MSE
pcr.pred=predict(pcr.fit,x[test,],ncomp=7)
mean((pcr.pred-y.test)^2)

# Fit model on the full dataset using ideal number of components
pcr.fit=pcr(y~x,scale=TRUE,ncomp=7)
summary(pcr.fit)


# ---------------------
# Partial Least Squares
# ---------------------

set.seed(1)
pls.fit=plsr(Salary~., data=Hitters,subset=train,scale=TRUE, validation="CV")
summary(pls.fit)

# Determine number of components
validationplot(pls.fit,val.type="MSEP")

# Determine test set MSE
pls.pred=predict(pls.fit,x[test,],ncomp=2)
mean((pls.pred-y.test)^2)

# Perform PLS using full dataset
pls.fit=plsr(Salary~., data=Hitters,scale=TRUE,ncomp=2)
summary(pls.fit)


# ---------
# CHAPTER 7
# ---------

# Chapter 7 Lab: Non-linear Modeling

library(ISLR)
attach(Wage)


# Polynomial Regression and Step Functions
# ----------------------------------------

# Returns a matrix whose columns are a basis of orthogonal polynomials
# (each column is a linear combination of the variables age, age2, age3, age4)
fit=lm(wage~poly(age,4),data=Wage)
coef(summary(fit))

# Least squares polynomial to 4th degree (i.e. x, x2, x3, x4)
# This approach changes the coefficient estimates, but does not
# affect predictions.
fit2=lm(wage~poly(age,4,raw=T),data=Wage)
coef(summary(fit2))

# Alternative ways to fit the same model
fit2a=lm(wage~age+I(age^2)+I(age^3)+I(age^4),data=Wage)
coef(fit2a)

# Another alternative way to fit the same model
fit2b=lm(wage~cbind(age,age^2,age^3,age^4),data=Wage)
coef(fit2b)

# Predict new values using first model
agelims=range(age)
age.grid=seq(from=agelims[1],to=agelims[2])
preds=predict(fit,newdata=list(age=age.grid),se=TRUE)
se.bands=cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se.fit)

# Plot the data and add the fit line
par(mfrow=c(1,2),mar=c(4.5,4.5,1,1),oma=c(0,0,4,0))
plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
title("Degree-4 Polynomial",outer=T)
lines(age.grid,preds$fit,lwd=2,col="blue")
matlines(age.grid,se.bands,lwd=1,col="blue",lty=3)

# Predict using second model and compare the fits... 
# should be basically identical.. when predicting...
preds2=predict(fit2,newdata=list(age=age.grid),se=TRUE)
max(abs(preds$fit-preds2$fit))

# Use ANOVA to compare models of different degrees (i.e. x, x2, etc.)
# used to determine the level / degree of polynomial to use
# When comparing using ANOVA, the models have to be nested 
# (i.e. one model a subset of the other)
fit.1=lm(wage~age,data=Wage)
fit.2=lm(wage~poly(age,2),data=Wage)
fit.3=lm(wage~poly(age,3),data=Wage)
fit.4=lm(wage~poly(age,4),data=Wage)
fit.5=lm(wage~poly(age,5),data=Wage)

# Compare models of different degrees with ANOVA
# Ho: model1 is sufficient to explain the data
# H1: alternative hypothesis is that a more complex model is necessary
anova(fit.1,fit.2,fit.3,fit.4,fit.5)

# Do same test with p values as the ANOVA
coef(summary(fit.5))

# square the t statistic and compare to f statistic of ANOVA
(-11.983)^2

# Use ANOVA again to compare these three models
fit.1=lm(wage~education+age,data=Wage)
fit.2=lm(wage~education+poly(age,2),data=Wage)
fit.3=lm(wage~education+poly(age,3),data=Wage)
anova(fit.1,fit.2,fit.3)

# predict if someone earns > $250,000 using logistic regression
fit=glm(I(wage>250)~poly(age,4),data=Wage,family=binomial)

# Make predictions using the logistic regression model
preds=predict(fit,newdata=list(age=age.grid),se=T)

# Reverse the link function, to get values in right scale, etc...
pfit=exp(preds$fit)/(1+exp(preds$fit))
se.bands.logit = cbind(preds$fit+2*preds$se.fit, preds$fit-2*preds$se.fit)
se.bands = exp(se.bands.logit)/(1+exp(se.bands.logit))

# Predict probabilities directly by using the type="response" argument
# this undoes the link function automatically
# be careful, confidence intervals may not be correct...
preds=predict(fit,newdata=list(age=age.grid),type="response",se=T)

# Plot the logistic regression and the confidence intervals
plot(age,I(wage>250),xlim=agelims,type="n",ylim=c(0,.2))
points(jitter(age), I((wage>250)/5),cex=.5,pch="|",col="darkgrey")
lines(age.grid,pfit,lwd=2, col="blue")
matlines(age.grid,se.bands,lwd=1,col="blue",lty=3)


# Fit a step function
# -------------------
# Seems to 'cut' a continuous variable into a binned categorical variable
# Use breaks option to specify your own break points
table(cut(age,4))
fit=lm(wage~cut(age,4),data=Wage)
coef(summary(fit))


# Splines
# -------
library(splines)

# Regression Spline
# -----------------
# Fit a regression spline to the Wage data
# By default, cubic splines are produced
fit=lm(wage~bs(age,knots=c(25,40,60)),data=Wage)

pred=predict(fit,newdata=list(age=age.grid),se=T)

plot(age,wage,col="gray")
lines(age.grid,pred$fit,lwd=2)
lines(age.grid,pred$fit+2*pred$se,lty="dashed")
lines(age.grid,pred$fit-2*pred$se,lty="dashed")

# Use df option to produce a spline with knots at uniform quantiles of the data
dim(bs(age,knots=c(25,40,60)))
dim(bs(age,df=6))
attr(bs(age,df=6),"knots")


# Natural Spline
# --------------
# Use degree argument to create splines other than cubic
# ns() fits a natural spline
# This produces a natural spline with 4 degrees of freedom
fit2=lm(wage~ns(age,df=4),data=Wage)
pred2=predict(fit2,newdata=list(age=age.grid),se=T)
lines(age.grid, pred2$fit,col="red",lwd=2)


# Smoothing Spline
# ----------------
# use smooth.spline() to fit a smoothing spline
# This is figure 7.8 in the book
plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
title("Smoothing Spline")
fit=smooth.spline(age,wage,df=16)
fit2=smooth.spline(age,wage,cv=TRUE)
fit2$df
lines(fit,col="red",lwd=2)
lines(fit2,col="blue",lwd=2)
legend("topright",legend=c("16 DF","6.8 DF"),col=c("red","blue"),lty=1,lwd=2,cex=.8)


# Local Regression
# ----------------
# Use loess() to perfrom local regression.
# Local regression using spans of 0.2 and 0.5
# The larger the span, the smoother the fit.
# locfit library can be used for local regression as well
plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
title("Local Regression")
fit=loess(wage~age,span=.2,data=Wage)
fit2=loess(wage~age,span=.5,data=Wage)
lines(age.grid,predict(fit,data.frame(age=age.grid)),col="red",lwd=2)
lines(age.grid,predict(fit2,data.frame(age=age.grid)),col="blue",lwd=2)
legend("topright",legend=c("Span=0.2","Span=0.5"),col=c("red","blue"),lty=1,lwd=2,cex=.8)


# GAMs
# ----
library(gam)

# Fit a GAM to predict wage using natural spline functions of year and age
# treating education as a qualitative predictor.
gam1=lm(wage~ns(year,4)+ns(age,5)+education,data=Wage)

# Fit model 7.16 using smoothing splines instead of natural splines
# s() function indicates that we would like to use a smoothing spline.
# Specify that the function of year should have 4 degrees of freedom
# and age should have 5
gam.m3=gam(wage~s(year,4)+s(age,5)+education,data=Wage)
par(mfrow=c(1,3))
plot(gam.m3, se=TRUE,col="blue")

# Not a GAM, but can still use plot.gam
# Figure 7.1
plot.gam(gam1, se=TRUE, col="red")

# Use ANOVA to determine which of 3 models is best
# What is the null hypothesis here?  That models are equal?
gam.m1=gam(wage~s(age,5)+education,data=Wage)
gam.m2=gam(wage~year+s(age,5)+education,data=Wage)
anova(gam.m1,gam.m2,gam.m3,test="F")

# Summary of GAM fit
summary(gam.m3)

# Make predictions from GAM models
preds=predict(gam.m2,newdata=Wage)

# Use local regression fits as building blocks in a GAM 
# us lo() function
# use local regression for the age term with a span of 0.7
gam.lo=gam(wage~s(year,df=4)+lo(age,span=0.7)+education,data=Wage)
plot.gam(gam.lo, se=TRUE, col="green")

# Can us lo() function to create interaction term on year and age
gam.lo.i=gam(wage~lo(year,age,span=0.5)+education,data=Wage)

# Plot the resulting 2D surface using akima package
library(akima)
plot(gam.lo.i)

# Use I() function again to create a binary dependent variable for 
# use with logistic regression
gam.lr=gam(I(wage>250)~year+s(age,df=5)+education,family=binomial,data=Wage)
par(mfrow=c(1,3))
plot(gam.lr,se=T,col="green")

# Id the counts of high earners in the different categories
# >$250,000 wage
table(education,I(wage>250))

# Fit GAM model excluding the HS category
gam.lr.s=gam(I(wage>250)~year+s(age,df=5)+education,family=binomial,data=Wage,subset=(education!="1. < HS Grad"))
plot(gam.lr.s,se=T,col="green")


# ---------
# CHAPTER 4
# ---------

# Chapter 4 Lab: Logistic Regression, LDA, QDA, and KNN

# The Stock Market Data
library(ISLR)

# Exploratory Data Analysis
names(Smarket)
dim(Smarket)
summary(Smarket)
pairs(Smarket)  # creates scatterplot matrix
cor(Smarket)
cor(Smarket[,-9])

attach(Smarket)
plot(Volume)


# -------------------
# Logistic Regression
# -------------------

# fit a logistic regression model to the data
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial)
summary(glm.fit)

# Look at the logistic regression model coefficients
coef(glm.fit)
summary(glm.fit)$coef

# Look at the p values for the coefficients
summary(glm.fit)$coef[,4]

# Make predictions... type="response" ensures probabilities are returned.
glm.probs=predict(glm.fit,type="response")

# Examine first 10 probabilities
glm.probs[1:10]
contrasts(Direction)  # lets you see how dummy variables are coded in R.

# Convert probabilities to an Up or Down... > .5 = Up
# rep command repeats Down 1250 times....
glm.pred=rep("Down",1250)
glm.pred[glm.probs>.5]="Up"

# Examine frequencies
table(glm.pred,Direction)  # confusion matrix
(507+145)/1250  # Calculate overall prediction accuracy

mean(glm.pred==Direction)  # fraction of correct predictions

# Train / test split
train=(Year<2005) # Train set = years 2001 - 2004
Smarket.2005=Smarket[!train,]  # items that are not train... i.e. test
dim(Smarket.2005)
Direction.2005=Direction[!train]


# Fit logistic regression model on only the training set
# using the subset command
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial,subset=train)

# Make predictions on the test set
glm.probs=predict(glm.fit,Smarket.2005,type="response")

# Change predictions to Ups and Downs
glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]="Up"

# Confusion matrix and percentages of accurate predictions
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
mean(glm.pred!=Direction.2005)

# Refit the model using only lag1 and lag2
glm.fit=glm(Direction~Lag1+Lag2,data=Smarket,family=binomial,subset=train)

# Make predictions on test set
glm.probs=predict(glm.fit,Smarket.2005,type="response")

# Change probabilities to Ups and Downs
glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]="Up"

# Confusion Matrix and prediction accuracies...
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
106/(106+76)

# Predict up or down for specific values of lag1 and lag2
predict(glm.fit,newdata=data.frame(Lag1=c(1.2,1.5),Lag2=c(1.1,-0.8)),type="response")


# ---------------------------------------------------
# Linear Discriminant Analysis (LDA)  (>2 categories)
# ---------------------------------------------------

library(MASS)

# Fit LDA model
lda.fit=lda(Direction~Lag1+Lag2,data=Smarket,subset=train)
lda.fit
plot(lda.fit)

# Predict with LDA model
# lda.pred$class = the predicted categories
# lda.pred$posterior = the probabilities of being in each category
lda.pred=predict(lda.fit, Smarket.2005)
names(lda.pred)

# Stores the predicted categories
lda.class=lda.pred$class
table(lda.class,Direction.2005)  # confusion matrix
mean(lda.class==Direction.2005)

# Apply the 50% threshold to the posterior probabilities 
# Allows us to recreate the values in lda.pred$class
sum(lda.pred$posterior[,1]>=.5)  # count how many items predicted as Up
sum(lda.pred$posterior[,1]<.5)

# Check first 20 rows of the probabilities and ups and downs
lda.pred$posterior[1:20,1]
lda.class[1:20]
sum(lda.pred$posterior[,1]>.9)  # check to see how many probabilities are > .9


# -------------------------------------
# Quadratic Discriminant Analysis (QDA)
# -------------------------------------

# Fit a QDA model
qda.fit=qda(Direction~Lag1+Lag2,data=Smarket,subset=train)
qda.fit

# Make QDA predictions
qda.class=predict(qda.fit,Smarket.2005)$class

# Confusion matrix
table(qda.class,Direction.2005)
mean(qda.class==Direction.2005)


# -------------------
# K-Nearest Neighbors
# -------------------

library(class)

# Create matrices of predictor variables 
# for train and test
# Create vector with the response values for the training set.
train.X=cbind(Lag1,Lag2)[train,]
test.X=cbind(Lag1,Lag2)[!train,]
train.Direction=Direction[train]

# Set seed to be able to reproduce results
# Apply KNN to the datasets
set.seed(1)
knn.pred=knn(train.X,test.X,train.Direction,k=1)  # k=1
table(knn.pred,Direction.2005)
(83+43)/252

knn.pred=knn(train.X,test.X,train.Direction,k=3)  # k=3
table(knn.pred,Direction.2005)
mean(knn.pred==Direction.2005)


# -----------------------------------------------
# An Application of KNN to Caravan Insurance Data
# -----------------------------------------------

dim(Caravan)
attach(Caravan)
summary(Purchase)
348/5822

# The scale of the predictors matters for KNN
# Standardize the variables.
standardized.X=scale(Caravan[,-86])  # exclude column 86, the response variable

# Check the variances of the data before and after
# standardization
var(Caravan[,1])
var(Caravan[,2])
var(standardized.X[,1])
var(standardized.X[,2])

# Train / test split
test=1:1000
train.X=standardized.X[-test,]
test.X=standardized.X[test,]
train.Y=Purchase[-test]
test.Y=Purchase[test]

# Fit KNN model with K=1
set.seed(1)
knn.pred=knn(train.X,test.X,train.Y,k=1)
mean(test.Y!=knn.pred)
mean(test.Y!="No")
table(knn.pred,test.Y)
9/(68+9)  

# K=3
knn.pred=knn(train.X,test.X,train.Y,k=3)
table(knn.pred,test.Y)
5/26  # focus on percentage of correctly identified yeses

# K=5
knn.pred=knn(train.X,test.X,train.Y,k=5)
table(knn.pred,test.Y)
4/15   # focus on percentage of correctly identified yeses

# Fit a logistic regression model to compare
# use .25 as the probability cutoff (after standard .5 is examined)
glm.fit=glm(Purchase~.,data=Caravan,family=binomial,subset=-test)
glm.probs=predict(glm.fit,Caravan[test,],type="response")  # outputs probabilities
glm.pred=rep("No",1000)
glm.pred[glm.probs>.5]="Yes"
table(glm.pred,test.Y)

# Change cutoff to .25
glm.pred=rep("No",1000)
glm.pred[glm.probs>.25]="Yes"
table(glm.pred,test.Y)
11/(22+11)




# --------------------------
# Chapter 8 - Decision Trees
# --------------------------



# Chapter 8 Lab: Decision Trees

# ----------------------------
# Fitting Classification Trees
# ----------------------------

library(tree)
library(ISLR)
attach(Carseats)

# Create a binary response variable, attach to carseats dataframe
High=ifelse(Sales<=8,"No","Yes")
Carseats=data.frame(Carseats,High)

# Fit a classification tree 
tree.carseats=tree(High~.-Sales,Carseats)
summary(tree.carseats)

# Display the tree graphically
plot(tree.carseats)
text(tree.carseats,pretty=0)
tree.carseats

# split data into train / test
set.seed(2)
train=sample(1:nrow(Carseats), 200)
Carseats.test=Carseats[-train,]
High.test=High[-train]

# Test model on test dataset
tree.carseats=tree(High~.-Sales,Carseats,subset=train)
tree.pred=predict(tree.carseats,Carseats.test,type="class")  # class means return the actual classification prediction
table(tree.pred,High.test)
(86+57)/200


# Prune the tree
set.seed(3)
cv.carseats=cv.tree(tree.carseats,FUN=prune.misclass) # FUN=prune.misclass here states to use the classification error rate  to guide pruning
names(cv.carseats)                                    # default is for deviance to guide....    
cv.carseats

# plot the cross validation error rate by tree size and k value
par(mfrow=c(1,2))
plot(cv.carseats$size,cv.carseats$dev,type="b")
plot(cv.carseats$k,cv.carseats$dev,type="b")

# Prune to the best model based on graphs
# The 9-node tree was the best.
prune.carseats=prune.misclass(tree.carseats,best=9)
plot(prune.carseats)
text(prune.carseats,pretty=0)

# Check test set performance of the 9-node tree
tree.pred=predict(prune.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
(94+60)/200

# Test a larger tree (15 nodes)... bigger tree, worse performance.
prune.carseats=prune.misclass(tree.carseats,best=15)
plot(prune.carseats)
text(prune.carseats,pretty=0)
tree.pred=predict(prune.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
(86+62)/200


# ------------------------
# Fitting Regression Trees
# ------------------------

library(MASS)
set.seed(1)

# Create training set, create regression tree
train = sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston=tree(medv~.,Boston,subset=train)
summary(tree.boston)

# Plot the tree
plot(tree.boston)
text(tree.boston,pretty=0)

# See if pruning will help performance
cv.boston=cv.tree(tree.boston)
plot(cv.boston$size,cv.boston$dev,type='b')

# Alternate way to prune the tree
prune.boston=prune.tree(tree.boston,best=5)
plot(prune.boston)
text(prune.boston,pretty=0)

# Make predictions on the test set with the unpruned tree
yhat=predict(tree.boston,newdata=Boston[-train,])
boston.test=Boston[-train,"medv"]
plot(yhat,boston.test)
abline(0,1)
mean((yhat-boston.test)^2)


# -------
# Bagging
# -------

library(randomForest)
set.seed(1)

# Bagging - randomforest when m=p (the number of parameters)
bag.boston=randomForest(medv~.,data=Boston,subset=train,mtry=13,importance=TRUE)
bag.boston

# Predict on test set
yhat.bag = predict(bag.boston,newdata=Boston[-train,])
plot(yhat.bag, boston.test)
abline(0,1)
mean((yhat.bag-boston.test)^2)

# Change the number of trees grown using the ntree argument.
bag.boston=randomForest(medv~.,data=Boston,subset=train,mtry=13,ntree=25)
yhat.bag = predict(bag.boston,newdata=Boston[-train,])
mean((yhat.bag-boston.test)^2)

# -------------
# Random Forest
# -------------
# Random forest using 6 random variables
set.seed(1)
rf.boston=randomForest(medv~.,data=Boston,subset=train,mtry=6,importance=TRUE)
yhat.rf = predict(rf.boston,newdata=Boston[-train,])
mean((yhat.rf-boston.test)^2)

# View the importance of each variable
importance(rf.boston)
varImpPlot(rf.boston)  # Plot the importance measures


# --------
# Boosting
# --------

library(gbm)
set.seed(1)

# n.trees defines number of trees, depth limits the depth of each tree
boost.boston=gbm(medv~.,data=Boston[train,],distribution="gaussian",n.trees=5000,interaction.depth=4)
summary(boost.boston)

# partial dependence plots
# shows the marginal effect of the selected variables on the response
# after integrating out other variables.
par(mfrow=c(1,2))
plot(boost.boston,i="rm")
plot(boost.boston,i="lstat")

# Make predictions on the test set
yhat.boost=predict(boost.boston,newdata=Boston[-train,],n.trees=5000)
mean((yhat.boost-boston.test)^2)

# Boosting with a different value of the lambda parameter
# default is 0.001  (change the shrinkage parameter)
boost.boston=gbm(medv~.,data=Boston[train,],distribution="gaussian",n.trees=5000,interaction.depth=4,shrinkage=0.2,verbose=F)
yhat.boost=predict(boost.boston,newdata=Boston[-train,],n.trees=5000)
mean((yhat.boost-boston.test)^2)




# ------------------------------------
# CHAPTER 9 - SUPPORT VECTOR MACHINES
# ------------------------------------


# Support Vector Classifier


# Create the data
set.seed(1)
x=matrix(rnorm(20*2), ncol=2)
y=c(rep(-1,10), rep(1,10))
x[y==1,]=x[y==1,] + 1

# Plot to see if linearly seperable.  They are not.
plot(x, col=(3-y))

# Dependent variable needs to be coded as a factor
dat=data.frame(x=x, y=as.factor(y))

# Run support vector machine
library(e1071)
svmfit=svm(y~., data=dat, kernel="linear", cost=10,scale=FALSE)  # scale FALSE does not scale the independent variables

# Plot the support vector classifier
plot(svmfit, dat)

# Identify the support vectors
svmfit$index

# Get basic information about the support classifier
summary(svmfit)

# Use smaller cost parameter
svmfit=svm(y~., data=dat, kernel="linear", cost=0.1,scale=FALSE)

plot(svmfit, dat)

svmfit$index

# Perform cross validation (10 fold cross validation)
# compare SVMs with a linear kernel, using a range of values of the cost parameter.
set.seed(1)
tune.out=tune(svm,y~.,data=dat,kernel="linear",ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune.out)  # view the error from cross validation

# The tune command stores the best model... this is how to access.
bestmod=tune.out$best.model
summary(bestmod)

# Generate test data
xtest=matrix(rnorm(20*2), ncol=2)
ytest=sample(c(-1,1), 20, rep=TRUE)
xtest[ytest==1,]=xtest[ytest==1,] + 1
testdat=data.frame(x=xtest, y=as.factor(ytest))



ypred=predict(bestmod,testdat)
table(predict=ypred, truth=testdat$y)
svmfit=svm(y~., data=dat, kernel="linear", cost=.01,scale=FALSE)
ypred=predict(svmfit,testdat)
table(predict=ypred, truth=testdat$y)
x[y==1,]=x[y==1,]+0.5
plot(x, col=(y+5)/2, pch=19)
dat=data.frame(x=x,y=as.factor(y))
svmfit=svm(y~., data=dat, kernel="linear", cost=1e5)
summary(svmfit)
plot(svmfit, dat)
svmfit=svm(y~., data=dat, kernel="linear", cost=1)
summary(svmfit)
plot(svmfit,dat)

# Support Vector Machine

set.seed(1)
x=matrix(rnorm(200*2), ncol=2)
x[1:100,]=x[1:100,]+2
x[101:150,]=x[101:150,]-2
y=c(rep(1,150),rep(2,50))
dat=data.frame(x=x,y=as.factor(y))
plot(x, col=y)
train=sample(200,100)
svmfit=svm(y~., data=dat[train,], kernel="radial",  gamma=1, cost=1)
plot(svmfit, dat[train,])
summary(svmfit)
svmfit=svm(y~., data=dat[train,], kernel="radial",gamma=1,cost=1e5)
plot(svmfit,dat[train,])
set.seed(1)
tune.out=tune(svm, y~., data=dat[train,], kernel="radial", ranges=list(cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4)))
summary(tune.out)
table(true=dat[-train,"y"], pred=predict(tune.out$best.model,newx=dat[-train,]))

# ROC Curves

library(ROCR)
rocplot=function(pred, truth, ...){
    predob = prediction(pred, truth)
    perf = performance(predob, "tpr", "fpr")
    plot(perf,...)}
svmfit.opt=svm(y~., data=dat[train,], kernel="radial",gamma=2, cost=1,decision.values=T)
fitted=attributes(predict(svmfit.opt,dat[train,],decision.values=TRUE))$decision.values
par(mfrow=c(1,2))
rocplot(fitted,dat[train,"y"],main="Training Data")
svmfit.flex=svm(y~., data=dat[train,], kernel="radial",gamma=50, cost=1, decision.values=T)
fitted=attributes(predict(svmfit.flex,dat[train,],decision.values=T))$decision.values
rocplot(fitted,dat[train,"y"],add=T,col="red")
fitted=attributes(predict(svmfit.opt,dat[-train,],decision.values=T))$decision.values
rocplot(fitted,dat[-train,"y"],main="Test Data")
fitted=attributes(predict(svmfit.flex,dat[-train,],decision.values=T))$decision.values
rocplot(fitted,dat[-train,"y"],add=T,col="red")

# SVM with Multiple Classes

set.seed(1)
x=rbind(x, matrix(rnorm(50*2), ncol=2))
y=c(y, rep(0,50))
x[y==0,2]=x[y==0,2]+2
dat=data.frame(x=x, y=as.factor(y))
par(mfrow=c(1,1))
plot(x,col=(y+1))
svmfit=svm(y~., data=dat, kernel="radial", cost=10, gamma=1)
plot(svmfit, dat)

# Application to Gene Expression Data

library(ISLR)
names(Khan)
dim(Khan$xtrain)
dim(Khan$xtest)
length(Khan$ytrain)
length(Khan$ytest)
table(Khan$ytrain)
table(Khan$ytest)
dat=data.frame(x=Khan$xtrain, y=as.factor(Khan$ytrain))
out=svm(y~., data=dat, kernel="linear",cost=10)
summary(out)
table(out$fitted, dat$y)
dat.te=data.frame(x=Khan$xtest, y=as.factor(Khan$ytest))
pred.te=predict(out, newdata=dat.te)
table(pred.te, dat.te$y)





