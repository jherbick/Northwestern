# --------------------------------------
# PREDICT 422 Practical Machine Learning
# Final project, James R. Herbick
# --------------------------------------



# -------
# REUSLTS
# -------

#               c.valid
#chat.valid.log1   0   1
#              0 709  18
#              1 310 981
# check n.mail.valid = 310+981 = 1291
# check profit = 14.5*981-2*1291 = 11642.5

# ------------------------------

# n.mail Profit  Model
# 1329   11624.5 LDA1 - jumpstart code
# 1291   11642.5 Log1 - jumpstart code
# 1271.0 11653.5 JRH LOGISTIC model 2 
# 1334.0 11643.5 JRH LDA model
# 1439   11274   JRH QDA
# 1160   11267   JRH KNN k=20  ** no probabilities to use
# 1280   11737   JRH Logistic GAM ****
# 1271.0 11653.5 JRH LOGISTIC model 2  with KFOLD validation
# 1165   11140.5 JRH Decision Tree
# 1067   11264   JRH Random Forest
# -------------------------------


# select model.log1 since it has maximum profit in the validation sample




# Course Project - Example R Script File

# OBJECTIVE: A charitable organization wishes to develop a machine learning
# model to improve the cost-effectiveness of their direct marketing campaigns
# to previous donors.

# 1) Develop a classification model using data from the most recent campaign that
# can effectively capture likely donors so that the expected net profit is maximized.

# 2) Develop a prediction model to predict donation amounts for donors - the data
# for this will consist of the records for donors only.



# -------------
# Load the data
# -------------

# charity <- read.csv(file.choose()) # load the "charity.csv" file
fileLocation <- "C:/Users/james/Documents/Northwestern/PREDICT 422_Machine Learning/Project/charity.csv"
charity <- read.table (file = fileLocation, header = TRUE, sep = ",", stringsAsFactors=TRUE)



# -------------------------
# predictor transformations
# -------------------------

# Preserve original dataset
charity.t <- charity


# Logorithms
charity.t$avhv <- log(charity.t$avhv)
# add further transformations if desired
# for example, some statistical methods can struggle when predictors are highly skewed


# Box-Cox
# These need to be reversed
library(forecast)
lambda <- BoxCox.lambda(charity.t$avhv)  # -0.473644
charity.t$avhv <- BoxCox(charity.t$avhv, lambda)


# -----------------------------
# set up data for analysis
# Train, validation, test split
# -----------------------------

data.train <- charity.t[charity$part=="train",]
x.train <- data.train[,2:21] # removes dependent vars, obs, and partition columns
c.train <- data.train[,22] # donr
n.train.c <- length(c.train) # 3984
y.train <- data.train[c.train==1,23] # damt for observations with donr=1
n.train.y <- length(y.train) # 1995

data.valid <- charity.t[charity$part=="valid",]
x.valid <- data.valid[,2:21]
c.valid <- data.valid[,22] # donr
n.valid.c <- length(c.valid) # 2018
y.valid <- data.valid[c.valid==1,23] # damt for observations with donr=1
n.valid.y <- length(y.valid) # 999

data.test <- charity.t[charity$part=="test",]
n.test <- dim(data.test)[1] # 2007
x.test <- data.test[,2:21]

# Standardize data
x.train.mean <- apply(x.train, 2, mean)
x.train.sd <- apply(x.train, 2, sd)
x.train.std <- t((t(x.train)-x.train.mean)/x.train.sd) # standardize to have zero mean and unit sd
apply(x.train.std, 2, mean) # check zero mean
apply(x.train.std, 2, sd) # check unit sd

# Datasets to build models off of....
data.train.std.c <- data.frame(x.train.std, donr=c.train) # to classify donr
data.train.std.y <- data.frame(x.train.std[c.train==1,], damt=y.train) # to predict damt when donr=1

x.valid.std <- t((t(x.valid)-x.train.mean)/x.train.sd) # standardize using training mean and sd
data.valid.std.c <- data.frame(x.valid.std, donr=c.valid) # to classify donr
data.valid.std.y <- data.frame(x.valid.std[c.valid==1,], damt=y.valid) # to predict damt when donr=1

x.test.std <- t((t(x.test)-x.train.mean)/x.train.sd) # standardize using training mean and sd
data.test.std <- data.frame(x.test.std)



# -----------
# Initial EDA
# -----------

# Check overall structure (variables, observations, etc.)
str(charity)

# Get summary statistics of data
summary(charity)

# Check variable selection from Angoss decision tree
# Check for missing values and outliers
# Don't seem to be missing values (other than response vars in test set)
# Check for non linearity
pairs(donr ~ wrat + incm + npro + tdon + tlag, charity)


hist(charity$chld)


# --------------------------------------------------------------------
# run forward and backward stepwise selections to get an idea of which 
# variables to use # for logistic regression
# --------------------------------------------------------------------

library(leaps)
model.ols.fwd <- regsubsets(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + wrat + 
                                avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
                            data.train.std.c, nvmax=20, method="forward")
summary(model.ols.fwd)


model.ols.bwd <- regsubsets(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + wrat + 
                                avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
                            data.train.std.c, nvmax=20, method="backward")
summary(model.ols.bwd)




post.valid.log1 <- predict(model.log1, data.valid.std.c, type="response") # n.valid post probs


# ----------
# Do OLS EDA
# ----------

pairs(damt~avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, data.train.std.y)  




# --------------------------------------------------------------------
# run quick OLS linear regressions to get an idea of variables to use
# for OLS regression
# --------------------------------------------------------------------
library(leaps)
model.ols.fwd2 <- regsubsets(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + wrat + 
                                avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
                             data.train.std.y, nvmax=20, method="forward")
summary(model.ols.fwd2)


model.ols.bwd2 <- regsubsets(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + wrat + 
                                avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
                             data.train.std.y, nvmax=20, method="backward")
summary(model.ols.bwd2)




# ------------
# Build Models
# ------------

# -------------------------------------------------------
# 1. CLASSIFICATION MODELING - Predict Donor vs non donor
# -------------------------------------------------------


# -------------------
# logistic regression
# -------------------

# ----------------------------------
# Model 1 - supplied by starter code
# ----------------------------------
model.log1 <- glm(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
                      avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
                  data.train.std.c, family=binomial("logit"))

summary(model.log1)

post.valid.log1 <- predict(model.log1, data.valid.std.c, type="response") # n.valid post probs


# Convert probabilities to an 1 or 0... > .5 = 1 = donor
# rep command repeats Down 1250 times....
glm.pred1=rep(0,2018)
glm.pred1[post.valid.log1>.5]=1

# Examine frequencies
table(glm.pred1,data.valid.std.c$donr)  # confusion matrix

mean(glm.pred1==data.valid.std.c$donr)  # fraction of correct predictions = 87%



# ----------------
# Calculate Profit
# calculate ordered profit function using average donation = $14.50 and mailing cost = $2
# ---------------- 


# -------
# Model 1
# -------

profit.log1 <- cumsum(14.5*c.valid[order(post.valid.log1, decreasing=T)]-2)
plot(profit.log1) # see how profits change as more mailings are made

n.mail.valid <- which.max(profit.log1) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.log1)) # report number of mailings and maximum profit
# 1291.0 11642.5

cutoff.log1 <- sort(post.valid.log1, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.log1 <- ifelse(post.valid.log1>cutoff.log1, 1, 0) # mail to everyone above the cutoff
table(chat.valid.log1, c.valid) # classification table



# ------------------------------------------------------------
# Model 2 - JRH based off decision tree for variable selection
# and off of forward and backward var selection
# ------------------------------------------------------------

model.log2 <- glm(donr ~ chld + home + reg2 + wrat + reg1 + incm + npro + tdon + tlag + plow + I(hinc^2), data.train.std.c, family=binomial("logit"))

summary(model.log2)

post.valid.log2 <- predict(model.log2, data.valid.std.c, type="response") # n.valid post probs

# Convert probabilities to an 1 or 0... > .5 = 1 = donor
# rep command repeats Down 1250 times....
glm.pred2=rep(0,2018)
glm.pred2[post.valid.log2>.5]=1

# Examine frequencies
table(glm.pred2,data.valid.std.c$donr)  # confusion matrix

mean(glm.pred2==data.valid.std.c$donr)  # fraction of correct predictions = 83%


# ----------------
# Calculate Profit
# calculate ordered profit function using average donation = $14.50 and mailing cost = $2
# ---------------- 

profit.log2 <- cumsum(14.5*c.valid[order(post.valid.log2, decreasing=T)]-2)
plot(profit.log2) # see how profits change as more mailings are made

n.mail.valid2 <- which.max(profit.log2) # number of mailings that maximizes profits
c(n.mail.valid2, max(profit.log2)) # report number of mailings and maximum profit
# 1271.0 11653.5

# set cutoff based on n.mail.valid (cutoff is a probability)
cutoff.log2 <- sort(post.valid.log2, decreasing=T)[n.mail.valid2+1] 
# mail to everyone above the cutoff (a probability greater than the cutoff)
chat.valid.log2 <- ifelse(post.valid.log2>cutoff.log2, 1, 0) 
table(chat.valid.log2, c.valid) # classification table


# Check profit here
14.5*979-2*1271   # 11653.5


# -------------------------
# Logistic Regression - GAM
# -------------------------


# Test variables independently against the donr variable

# Simple logistic regression
model.chld <- glm(donr ~ chld, data.train.std.c, family=binomial("logit"))
summary(model.chld)


valid.chld <- predict(model.chld, data.valid.std.c, type="response") # n.valid post probs

# Convert probabilities to an 1 or 0... > .5 = 1 = donor
# rep command repeats Down 1250 times....
chld=rep(0,2018)
chld[valid.chld>.5]=1

# Examine frequencies
table(chld,data.valid.std.c$donr)  # confusion matrix

mean(chld==data.valid.std.c$donr)  # fraction of correct predictions = 74%


# Test polynomial regression on chld


# Simple logistic regression
model.chld.poly <- glm(donr ~ poly(chld, 4, raw = TRUE), data.train.std.c, family=binomial("logit"))
summary(model.chld.poly)

# Use ANOVA to determine best order of polynomial to use... 
chld.poly1 <- glm(donr ~ chld, data.train.std.c, family=binomial("logit"))
chld.poly2 <- glm(donr ~ poly(chld, 2), data.train.std.c, family=binomial("logit"))
chld.poly3 <- glm(donr ~ poly(chld, 3), data.train.std.c, family=binomial("logit"))
chld.poly4 <- glm(donr ~ poly(chld, 4), data.train.std.c, family=binomial("logit"))
chld.poly5 <- glm(donr ~ poly(chld, 5), data.train.std.c, family=binomial("logit"))

# make predictions
valid.poly1 <- predict(chld.poly1, data.valid.std.c, type="response") # n.valid post probs
valid.poly2 <- predict(chld.poly2, data.valid.std.c, type="response") # n.valid post probs
valid.poly3 <- predict(chld.poly3, data.valid.std.c, type="response") # n.valid post probs
valid.poly4 <- predict(chld.poly4, data.valid.std.c, type="response") # n.valid post probs
valid.poly5 <- predict(chld.poly5, data.valid.std.c, type="response") # n.valid post probs


# Convert probabilities to an 1 or 0... > .5 = 1 = donor
# rep command repeats Down 1250 times....
chld1=rep(0,2018)
chld1[valid.poly1>.5]=1

chld2=rep(0,2018)
chld2[valid.poly2>.5]=1

chld3=rep(0,2018)
chld3[valid.poly3>.5]=1

chld4=rep(0,2018)
chld4[valid.poly4>.5]=1

chld5=rep(0,2018)
chld5[valid.poly5>.5]=1

# Examine frequencies
table(chld,data.valid.std.c$donr)  # confusion matrix


# Doesn't seem to make much of a difference
mean(chld1==data.valid.std.c$donr)  # fraction of correct predictions = 75%
mean(chld2==data.valid.std.c$donr)  # fraction of correct predictions = 75%
mean(chld3==data.valid.std.c$donr)  # fraction of correct predictions = 75%
mean(chld4==data.valid.std.c$donr)  # fraction of correct predictions = 75%
mean(chld5==data.valid.std.c$donr)  # fraction of correct predictions = 75%


anova(chld.poly1, chld.poly2, chld.poly3, chld.poly4, chld.poly5)  # not sure this is working
coef(summary(chld.poly5))


# Try a smoothing spline on chld
library(splines)
library(gam)
smooth.chld <- gam(donr ~ s(chld, 4), data.train.std.c, family=binomial("logit"))
summary(smooth.chld)

plot.gam(smooth.chld)

anova(model.chld, smooth.chld)  # not really working again (doesn't give p values)


valid.smooth.chld <- predict(smooth.chld, data.valid.std.c, type="response") # n.valid post probs

# Convert probabilities to an 1 or 0... > .5 = 1 = donor
# rep command repeats Down 1250 times....
chld.smooth=rep(0,2018)
chld.smooth[valid.chld>.5]=1

# Examine frequencies
table(chld.smooth,data.valid.std.c$donr)  # confusion matrix

mean(chld.smooth==data.valid.std.c$donr)  # fraction of correct predictions = 75%




# Examine wrat variable
# ---------------------


# Simple logistic regression
model.wrat <- glm(donr ~ wrat, data.train.std.c, family=binomial("logit"))
summary(model.wrat)


valid.wrat <- predict(model.wrat, data.valid.std.c, type="response") # n.valid post probs

# Convert probabilities to an 1 or 0... > .5 = 1 = donor
# rep command repeats Down 1250 times....
wrat=rep(0,2018)
wrat[valid.wrat>.5]=1

# Examine frequencies
table(wrat,data.valid.std.c$donr)  # confusion matrix

mean(wrat==data.valid.std.c$donr)  # fraction of correct predictions = 56%




# Try a smoothing spline on wrat
library(splines)
library(gam)
smooth.wrat <- gam(donr ~ s(wrat, 4), data.train.std.c, family=binomial("logit"))
summary(smooth.wrat)

plot.gam(smooth.wrat)

anova(model.wrat, smooth.wrat)  # not really working again (doesn't give p values)


valid.smooth.wrat <- predict(smooth.wrat, data.valid.std.c, type="response") # n.valid post probs

# Convert probabilities to an 1 or 0... > .5 = 1 = donor
# rep command repeats Down 1250 times....
wrat.smooth=rep(0,2018)
wrat.smooth[valid.smooth.wrat>.5]=1

# Examine frequencies
table(wrat.smooth,data.valid.std.c$donr)  # confusion matrix

mean(wrat.smooth==data.valid.std.c$donr)  # fraction of correct predictions = 59%


# -------------------------------------
# Assemble the final logistic GAM model
# -------------------------------------

library(gam)

model.gam1 <- gam(donr ~ s(chld, 4) + home + reg2 + s(wrat, 4) + reg1 + incm + npro + tdon + tlag + plow + I(hinc^2), data=data.train.std.c, family=binomial("logit"))
plot(model.gam1, se=TRUE,col="blue")
plot.gam(model.gam1, se=TRUE,col="blue")

# Summary of GAM fit
summary(model.gam1)

# Make predictions from GAM models
# is this probabilities???
preds=predict(model.gam1,newdata=data.valid.std.c, type="response")


# Convert probabilities to an 1 or 0... > .5 = 1 = donor
# rep command repeats Down 1250 times....
donate <- rep(0,2018)
donate[preds > .5] = 1


# Examine frequencies
table(donate,data.valid.std.c$donr)  # confusion matrix

mean(donate==data.valid.std.c$donr)  # fraction of correct predictions = 84%


# Check profit

profit.gam1 <- cumsum(14.5*c.valid[order(preds, decreasing=T)]-2)
plot(profit.gam1) # see how profits change as more mailings are made

n.mail.valid <- which.max(profit.gam1) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.gam1)) # report number of mailings and maximum profit
# 1276 11643.5

cutoff.gam1 <- sort(preds, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.gam1 <- ifelse(preds > cutoff.gam1, 1, 0) # mail to everyone above the cutoff
table(chat.valid.gam1, c.valid) # classification table


# Check profit here
14.5*986-2*1280   # 11737


# ----------------------------
# linear discriminant analysis
# ----------------------------

# -----------------------------
# Model 1 - from jumpstart code
# -----------------------------

library(MASS)

model.lda1 <- lda(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
                      avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
                  data.train.std.c) # include additional terms on the fly using I()

# Note: strictly speaking, LDA should not be used with qualitative predictors,
# but in practice it often is if the goal is simply to find a good predictive model

post.valid.lda1 <- predict(model.lda1, data.valid.std.c)$posterior[,2] # n.valid.c post probs

# calculate ordered profit function using average donation = $14.50 and mailing cost = $2

profit.lda1 <- cumsum(14.5*c.valid[order(post.valid.lda1, decreasing=T)]-2)
plot(profit.lda1) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.lda1) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.lda1)) # report number of mailings and maximum profit
# 1329.0 11624.5

cutoff.lda1 <- sort(post.valid.lda1, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.lda1 <- ifelse(post.valid.lda1>cutoff.lda1, 1, 0) # mail to everyone above the cutoff
table(chat.valid.lda1, c.valid) # classification table
#               c.valid
#chat.valid.lda1   0   1
#              0 675  14
#              1 344 985
# check n.mail.valid = 344+985 = 1329
# check profit = 14.5*985-2*1329 = 11624.5



# -------------------------------------------------
# Model 2 - JRH using variable selection from above
# -------------------------------------------------

library(MASS)

model.lda2 <- lda(donr ~ chld + home + reg2 + wrat + reg1 + incm + npro + tdon + tlag + plow + I(hinc^2), data.train.std.c) # include additional terms on the fly using I()

# Note: strictly speaking, LDA should not be used with qualitative predictors,
# but in practice it often is if the goal is simply to find a good predictive model

post.valid.lda2 <- predict(model.lda2, data.valid.std.c)$posterior[,2] # n.valid.c post probs

# calculate ordered profit function using average donation = $14.50 and mailing cost = $2

profit.lda2 <- cumsum(14.5*c.valid[order(post.valid.lda2, decreasing=T)]-2)
plot(profit.lda2) # see how profits change as more mailings are made

n.mail.valid <- which.max(profit.lda2) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.lda2)) # report number of mailings and maximum profit
# 1329.0 11624.5

cutoff.lda2 <- sort(post.valid.lda2, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.lda2 <- ifelse(post.valid.lda2>cutoff.lda2, 1, 0) # mail to everyone above the cutoff
table(chat.valid.lda2, c.valid) # classification table
#               c.valid
#chat.valid.lda1   0   1
#              0 675  14
#              1 344 985
# check n.mail.valid = 344+985 = 1329

14.5*987-2*1334


# -------------------------------------------------------
# Model 3 - QDA - JRH using variable selection from above
# -------------------------------------------------------

library(MASS)

# Fit a QDA model
qda.fit1=qda(donr ~ chld + home + reg2 + wrat + reg1 + incm + npro + tdon + tlag + plow + I(hinc^2), data.train.std.c) # include additional terms on the fly using I()
qda.fit1

# Make QDA predictions
qda.class1.preds=predict(qda.fit1,data.valid.std.c)$posterior
qda.class1=predict(qda.fit1,data.valid.std.c)$class

# Save probabilities of 1=donor in a vector
qda.donr.pred <- qda.class1.preds[,2]


# Confusion matrix
# table(qda.class1,data.valid.std.c$donr)
# mean(qda.class1==data.valid.std.c$donr)



profit.qda1 <- cumsum(14.5*c.valid[order(qda.donr.pred, decreasing=T)]-2)
plot(profit.qda1) # see how profits change as more mailings are made

n.mail.valid <- which.max(profit.qda1) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.qda1)) # report number of mailings and maximum profit
# 1411.0 11083.5

cutoff.qda1 <- sort(qda.donr.pred, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.qda1 <- ifelse(qda.donr.pred > cutoff.qda1, 1, 0) # mail to everyone above the cutoff
table(chat.valid.qda1, c.valid) # classification table


# Check the profit
14.5*976-2*1439



# -------------------
# Model 4 = KNN - JRH
# -------------------


# training set
# data.train.std.c 
# 
# donr for training set
# c.train <- data.train[,22] # donr
#
#
# validation set
# data.valid.std.c
#
# donr for validation set
# c.valid <- data.valid[,22] # donr



library(class)


# Remove donr from train / test sets
knn.train <- data.train.std.c[-21]
knn.test <- data.valid.std.c[-21]


# Fit KNN model with K=1
set.seed(123)
knn.pred1=knn(knn.train,knn.test,c.train,k=1)
# Overall error rate... not really of interest
mean(c.valid!=knn.pred1)
# Percent of donors in test set
mean(c.valid!=1)
# Confusion matrix
table(knn.pred1,c.valid)
848/(848+278)   # 75% correct


# K=3
set.seed(123)
knn.pred3=knn(knn.train,knn.test,c.train,k=3)
# Overall error rate... not really of interest
mean(c.valid!=knn.pred3)
# Percent of donors in test set
mean(c.valid!=1)
# Confusion matrix
table(knn.pred3,c.valid)
899/(899+276)   # 77% correct

# Check profit here
14.5*899-2*1175   # 10685.5


# K=5
set.seed(123)
knn.pred5=knn(knn.train,knn.test,c.train,k=5)
# Overall error rate... not really of interest
mean(c.valid!=knn.pred5)
# Percent of donors in test set
mean(c.valid!=1)
# Confusion matrix
table(knn.pred5,c.valid)
919/(919+282)   # 77% correct




# -----------------------------------------------
# KNN on a subset of predictors, more values of k
# -----------------------------------------------


# -------------------
# Model 4 = KNN - JRH
# -------------------


# training set
# data.train.std.c 
# 
# donr for training set
# c.train <- data.train[,22] # donr
#
#
# validation set
# data.valid.std.c
#
# donr for validation set
# c.valid <- data.valid[,22] # donr



library(class)


# Remove donr and others from train / test sets
knn.train <- data.train.std.c[c(-21,-20,-17,-16,-15,-12,-10,-8,-4,-3)]

# create hinc^2, remove hinc
knn.train$hinc2 <- knn.train$hinc^2
knn.train <- knn.train[-5]



# Remove donr and others from train / test sets
knn.test <- data.valid.std.c[c(-21,-20,-17,-16,-15,-12,-10,-8,-4,-3)]

# create hinc^2, remove hinc
knn.test$hinc2 <- knn.test$hinc^2
knn.test <- knn.test[-5]



# Fit KNN model with K=1
set.seed(123)
knn.pred1=knn(knn.train,knn.test,c.train,k=1)
# Overall error rate... not really of interest
mean(c.valid!=knn.pred1)
# Percent of donors in test set
mean(c.valid!=1)
# Confusion matrix
table(knn.pred1,c.valid)
884/(884+189)   # 82% correct


# Check profit here
14.5*884-2*1073   # 10672



# *** Use K=3 *****
# K=3
set.seed(123)
knn.pred3=knn(knn.train,knn.test,c.train,k=3)
# Overall error rate... not really of interest
mean(c.valid!=knn.pred3)
# Percent of donors in test set
mean(c.valid!=1)
# Confusion matrix
table(knn.pred3,c.valid)
914/(914+199)   # 82% correct

# Check profit here
14.5*914-2*1113   # 11027



# K=5
set.seed(123)
knn.pred5=knn(knn.train,knn.test,c.train,k=5)
# Overall error rate... not really of interest
mean(c.valid!=knn.pred5)
# Percent of donors in test set
mean(c.valid!=1)
# Confusion matrix
table(knn.pred5,c.valid)
928/(928+198)   # 82% correct

# Check profit here
14.5*928-2*1126   # 11204





# K=10
set.seed(123)
knn.pred5=knn(knn.train,knn.test,c.train,k=10)
# Overall error rate... not really of interest
mean(c.valid!=knn.pred5)
# Percent of donors in test set
mean(c.valid!=1)
# Confusion matrix
table(knn.pred5,c.valid)
936/(936+217)   # 81% correct

# Check profit here
14.5*936-2*1153   # 11266




# K=20
set.seed(123)
knn.pred5=knn(knn.train,knn.test,c.train,k=20)
# Overall error rate... not really of interest
mean(c.valid!=knn.pred5)
# Percent of donors in test set
mean(c.valid!=1)
# Confusion matrix
table(knn.pred5,c.valid)
937/(937+223)   # 81% correct

# Check profit here
14.5*937-2*1160   # 11267




# ------------------
# Resampling methods
# ------------------

# ----------------------------------------------
# 4. K FOLD VALIDATION FOR LOGISTIC REGRESSION  ?????
# ----------------------------------------------

# Why is this here?
# model.ls2 <- lm(damt ~ reg3 + reg4 + home + chld + hinc + incm + plow + npro + rgif + agif, data.train.std.y)
# summary(model.ls2)  # check adj R squared


# Must be doing k fold validation on each model. Uses 10 folds.
library(boot)

set.seed(17)
cv.error.10=rep(0,10)
for (i in 1:10){
    log.kfold <- glm(donr ~ chld + home + reg2 + wrat + reg1 + incm + npro + tdon + tlag + plow + I(hinc^i), data.train.std.c, family=binomial("logit"))
    cv.error.10[i]=cv.glm(data.train.std.c, log.kfold, K=10)$delta[1]
}
cv.error.10


# Take model with lowest error from above (i.e. hinc^2)
model.log2 <- glm(donr ~ chld + home + reg2 + wrat + reg1 + incm + npro + tdon + tlag + plow + I(hinc^2), data.train.std.c, family=binomial("logit"))
summary(model.log2)

post.valid.log2 <- predict(model.log2, data.valid.std.c, type="response") # n.valid post probs

# Convert probabilities to an 1 or 0... > .5 = 1 = donor
# rep command repeats Down 1250 times....
glm.pred2=rep(0,2018)
glm.pred2[post.valid.log2>.5]=1

# Examine frequencies
table(glm.pred2,data.valid.std.c$donr)  # confusion matrix

mean(glm.pred2==data.valid.std.c$donr)  # fraction of correct predictions = 83%


# ----------------
# Calculate Profit
# calculate ordered profit function using average donation = $14.50 and mailing cost = $2
# ---------------- 

profit.log2 <- cumsum(14.5*c.valid[order(post.valid.log2, decreasing=T)]-2)
plot(profit.log2) # see how profits change as more mailings are made

n.mail.valid2 <- which.max(profit.log2) # number of mailings that maximizes profits
c(n.mail.valid2, max(profit.log2)) # report number of mailings and maximum profit
# 1271.0 11653.5

# set cutoff based on n.mail.valid (cutoff is a probability)
cutoff.log2 <- sort(post.valid.log2, decreasing=T)[n.mail.valid2+1] 
# mail to everyone above the cutoff (a probability greater than the cutoff)
chat.valid.log2 <- ifelse(post.valid.log2>cutoff.log2, 1, 0) 
table(chat.valid.log2, c.valid) # classification table


# Check profit here
14.5*979-2*1271   # 11653.5




# --------------
# Decision Trees
# --------------

library(tree)

# have to make donr a factor for classification tree to work
data.tree.train <- data.train.std.c
data.tree.train$donr <- as.factor(data.tree.train$donr)

data.tree.valid <- data.valid.std.c
data.tree.valid$donr <- as.factor(data.tree.valid$donr)



# Fit a classification tree 
# Donor has to be a factor to fit a classification tree
tree.donor=tree(donr~., data.tree.train)
summary(tree.donor)  # misclassification rate of 14%

# Display the tree graphically
plot(tree.donor)
text(tree.donor,pretty=0)

# See split criteria
tree.donor


# Test tree error on validation dataset
tree.pred=predict(tree.donor, data.tree.valid, type="class") # class gives predictions in 0, 1
table(tree.pred, data.tree.valid$donr)
929/(929 + 236)   # 80 % accuracy



# Try to calculate profit
14.5*929-2*1165   # 11140.5   This is best so far....


# Prune the tree
set.seed(3)
cv.donor=cv.tree(tree.donor, FUN=prune.misclass)
names(cv.donor)
cv.donor   # k = the cost complexity parameter


# plot the cross validation error rate by tree size and k value
par(mfrow=c(1,2))
plot(cv.donor$size,cv.donor$dev,type="b")
plot(cv.donor$k,cv.donor$dev,type="b")


# Prune to the best model based on graphs
prune.donor <- prune.misclass(tree.donor,best=9)
plot(prune.donor)
text(prune.donor,pretty=0)

# Run predictions again
tree.pred=predict(prune.donor, data.tree.valid,type="class")
table(tree.pred, data.tree.valid$donr)
825/(825 + 137)   # 86 % accuracy



# Try to calculate profit
# Profit went down
14.5*825-2*962   # 10038.5


# Try to apply bagging

library(randomForest)

set.seed(1)

# Bagging - use all variables
bag.donor <- randomForest(donr~.,data=data.tree.train, mtry=20,importance=TRUE)
bag.donor 


# Calculate error rate on the validation set
yhat.bag = predict(bag.donor, newdata=data.tree.valid)
plot(yhat.bag, data.tree.valid$donr)
abline(0,1)
# mean((data.tree.valid$donr-yhat.bag)^2)  # This would be for a regression problem
table(yhat.bag, data.tree.valid$donr)
901/(901 + 130)   # 87 % accuracy



# Try to calculate profit
# Profit is better than prune tree, worse than first tree
14.5*901-2*1031   # 11002.5


# Try Random Forest
# -----------------

set.seed(3)

# Random Forest - use default of sqrt p variables
rf.donor <- randomForest(donr~.,data=data.tree.train, importance=TRUE)
rf.donor 

# Random Forest - use 5 random variables for each split
rf.donor2 <- randomForest(donr~.,data=data.tree.train, mtry=5, importance=TRUE)
rf.donor2 




# Calculate error rate on the validation set
yhat.rf = predict(rf.donor2, newdata=data.tree.valid)
# plot(yhat.bag, data.tree.valid$donr)
# abline(0,1)
# mean((data.tree.valid$donr-yhat.bag)^2)  # This would be for a regression problem
table(yhat.rf, data.tree.valid$donr)
924/(924 + 143)   # 87 % accuracy... no change


importance(rf.donor)
varImpPlot(rf.donor)





# Try to calculate profit
# Increase over bagging
14.5*924-2*1067   # 11264


# Try boosting
# ------------

library(gbm)

set.seed(1)

par(mfrow=c(1,1))

# specify 5000 trees, depth of each tree is 8
boost.donor <- gbm(donr~.,data=data.tree.train, distribution="bernoulli", n.trees=5000, interaction.depth=8)
summary(boost.donor)  # not working for binary dependent var


# par(mfrow=c(1,2))
# plot(boost.boston,i="rm")
# plot(boost.boston,i="lstat")

# Make predictions .... not working
yhat.boost=predict(boost.donor, newdata=data.tree.valid, n.trees=5000)

table(yhat.boost, data.tree.valid$donr)
901/(901 + 130)   # 87 % accuracy... no change




mean((yhat.boost-boston.test)^2)
boost.boston=gbm(medv~.,data=Boston[train,],distribution="gaussian",n.trees=5000,interaction.depth=4,shrinkage=0.2,verbose=F)
yhat.boost=predict(boost.boston,newdata=Boston[-train,],n.trees=5000)
mean((yhat.boost-boston.test)^2)






# ------------------------------
# Posting classification results
# ------------------------------

post.test <- predict(model.gam1,newdata=data.test.std, type="response") # post probs for test data


# Oversampling adjustment for calculating number of mailings for test set

n.mail.valid <- which.max(profit.gam1)
tr.rate <- .1 # typical response rate is .1
vr.rate <- .5 # whereas validation response rate is .5
adj.test.1 <- (n.mail.valid/n.valid.c)/(vr.rate/tr.rate) # adjustment for mail yes
adj.test.0 <- ((n.valid.c-n.mail.valid)/n.valid.c)/((1-vr.rate)/(1-tr.rate)) # adjustment for mail no
adj.test <- adj.test.1/(adj.test.1+adj.test.0) # scale into a proportion
n.mail.test <- round(n.test*adj.test, 0) # calculate number of mailings for test set

cutoff.test <- sort(post.test, decreasing=T)[n.mail.test+1] # set cutoff based on n.mail.test
chat.test <- ifelse(post.test>cutoff.test, 1, 0) # mail to everyone above the cutoff
table(chat.test)
#    0    1 
# 1676  331
# based on this model we'll mail to the 331 highest posterior probabilities

# See below for saving chat.test into a file for submission









# --------------------------------------------
# 2. PREDICTION MODELING - for donation amount
# --------------------------------------------


# Results

# MPE  Model
# 1.867523 LS1 - given
# 1.867433 LS2 - given
# 1.857947 JRH OLS Model 
# 1.86851  JRH Best Subset Selection
# 1.85981  JRH LASSO
# 1.896256 JRH Principal Components
# 1.845264 JRH K fold OLS model ***


# select model.ls2 since it has minimum mean prediction error in the validation sample



# ---------------------------------------
# Least squares regression - starter code
# ---------------------------------------

model.ls1 <- lm(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + wrat + 
                    avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
                data.train.std.y)

summary(model.ls1)  # check adj R squared

pred.valid.ls1 <- predict(model.ls1, newdata = data.valid.std.y) # validation predictions

# mean prediction error / mean squared error
# 1.867523
mean((y.valid - pred.valid.ls1)^2) 

# std error
# 0.1696615
sd((y.valid - pred.valid.ls1)^2)/sqrt(n.valid.y) 



# drop wrat for illustrative purposes
model.ls2 <- lm(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + 
                    avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
                data.train.std.y)

pred.valid.ls2 <- predict(model.ls2, newdata = data.valid.std.y) # validation predictions
mean((y.valid - pred.valid.ls2)^2) # mean prediction error
# 1.867433
sd((y.valid - pred.valid.ls2)^2)/sqrt(n.valid.y) # std error
# 0.1696498

yhat.test <- predict(model.ls2, newdata = data.test.std) # test predictions




# ----------
# JRH models
# ----------


# ---------------------
# 1. STD OLS regression
# ---------------------

model.ls2 <- lm(damt ~ reg3 + reg4 + home + chld + hinc + incm + plow + npro + rgif + agif, data.train.std.y)

summary(model.ls2)  # check adj R squared

pred.valid.ls2 <- predict(model.ls2, newdata = data.valid.std.y) # validation predictions

# mean prediction error / mean squared error
# 1.857947
mean((y.valid - pred.valid.ls2)^2) 

# std error
# 0.1693538
sd((y.valid - pred.valid.ls2)^2)/sqrt(n.valid.y) 





# ------------------------
# 2. Best Subset Selection
# ------------------------

# is.na() can be used to id missing values... returns a vector of TRUE and FALSEs
# sum() can be used to count the missing values


# Run best subset selection
# by default, goes up to an eight variable model
library(leaps)
regfit.full=regsubsets(damt~.,data.train.std.y)
summary(regfit.full)

# Run best subset selection up to a 20 variable model
regfit.full=regsubsets(damt~.,data=data.train.std.y,nvmax=20)
reg.summary=summary(regfit.full)

# Get model fit statistics for all models
names(reg.summary)
reg.summary$adjr2

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
coef(regfit.full,11)


# Fit OLS model with 11 variables from best subset selection
# same as other OLS model + genf variable
# -----------------------------------------------------------

model.ls3 <- lm(damt ~ reg3 + reg4 + home + chld + hinc + incm + plow + npro + rgif + agif + genf, data.train.std.y)

summary(model.ls3)  # check adj R squared

pred.valid.ls3 <- predict(model.ls3, newdata = data.valid.std.y) # validation predictions

# mean prediction error / mean squared error
# 1.857947
mean((y.valid - pred.valid.ls3)^2) 

# std error
# 0.1693538
sd((y.valid - pred.valid.ls3)^2)/sqrt(n.valid.y) 


# --------
# 3. LASSO
# --------

library(glmnet)

# Train
x <- model.matrix(damt~., data.train.std.y)[,-1]  # are these the model coefficients?
y <- data.train.std.y[,21]

# Validation
x.val <- model.matrix(damt~., data.valid.std.y)[,-1]

grid=10^seq(10,-2,length=100)


# Use glmnet() function, but use alpha=1 for Lasso.
lasso.mod=glmnet(x, y, alpha=1, lambda = grid)
plot(lasso.mod)

# Perform cross validation and compute test MSE
set.seed(1)
cv.out=cv.glmnet(x,y,alpha=1)
plot(cv.out)

bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam,newx=x.val)
mean((y.valid - lasso.pred)^2)

# Examine Lasso model coefficients... some are = 0
out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:20,]
lasso.coef

lasso.coef[lasso.coef!=0]  #View coefficients that are not=0.


# --------------------------------
# 3. Principal Components Analysis
# --------------------------------

library(pls)

set.seed(2)
pcr.fit=pcr(damt~., data=data.train.std.y , scale=FALSE , validation="CV")
# Shows the % variance explained for the predictors and response
# using different components
summary(pcr.fit)

# Plot the cross validation MSE to determine number of principal components
validationplot(pcr.fit,val.type="MSEP")

# Perform PCR on the training data and evaluate test set performance
set.seed(1)
pcr.fit=pcr(damt~., data=data.train.std.y , scale=FALSE , validation="CV")
validationplot(pcr.fit,val.type="MSEP")

# Compute test MSE
pcr.pred=predict(pcr.fit, data.valid.std.y,ncomp=15)
mean((y.valid-pcr.pred)^2)

# Fit model on the full dataset using ideal number of components
pcr.fit=pcr(y~x,scale=TRUE,ncomp=7)
summary(pcr.fit)


# ------------------
# RESAMPLING METHODS
# ------------------

# ---------------------------------------
# 4. K FOLD VALIDATION FOR OLS REGRESSION
# ---------------------------------------

model.ls2 <- lm(damt ~ reg3 + reg4 + home + chld + hinc + incm + plow + npro + rgif + agif, data.train.std.y)
summary(model.ls2)  # check adj R squared


# Must be doing k fold validation on each model. Uses 10 folds.
library(boot)

set.seed(17)
cv.error.10=rep(0,10)
for (i in 1:10){
    model.ls4 <- glm(damt ~ reg3 + reg4 + home + chld + poly(hinc, i) + incm + plow + npro + rgif + agif, data.train.std.y, family = gaussian)
    cv.error.10[i]=cv.glm(data.train.std.y, model.ls4, K=10)$delta[1]
}
cv.error.10

# take model with lowest error from test above (i.e. hinc^3)
model.ls4 <- glm(damt ~ reg3 + reg4 + home + chld + poly(hinc, 3) + incm + plow + npro + rgif + agif, data.train.std.y, family = gaussian)
summary(model.ls4)


pred.valid.ls2 <- predict(model.ls4, newdata = data.valid.std.y) # validation predictions


# mean prediction error / mean squared error
# 1.845264
mean((y.valid - pred.valid.ls2)^2) 


# std error
# 0.1685115
sd((y.valid - pred.valid.ls2)^2)/sqrt(n.valid.y) 



# ------------------------------------
# Try an artificial neural network for
# regression prediction
# ------------------------------------

# Example 2: Create a ANN for prediction

# We give a brief example of regression with neural networks and comparison with
# multivariate linear regression. The data set is housing data for 506 census tracts of
# Boston from the 1970 census. The goal is to predict median value of owner-occupied homes.

# Load the data and inspect the range (which is 1 - 50)
# library(mlbench)
# data(BostonHousing)
# summary(BostonHousing$medv)

# Build the multiple linear regression model
# lm.fit <- lm(medv ~ ., data = BostonHousing)
# lm.predict <- predict(lm.fit)

# Calculate the MSE and plot
# mean((lm.predict - BostonHousing$medv)^2) # MSE = 21.89483
# par(mfrow = c(2,1))
# plot(BostonHousing$medv, lm.predict, main = "Linear Regression Predictions vs Actual (MSE = 21.9)", 
#      xlab = "Actual", ylab = "Predictions", pch = 19, col = "brown")



# Build the feed-forward ANN (w/ one hidden layer)
library(nnet)        # For Neural Network
nnet.fit <- nnet(damt/27 ~ ., data = data.train.std.y, size = 4) # size = the number of units in the hidden layer, divide by 27 to keep range 0-1
nnet.predict <- predict(nnet.fit, newdata = data.valid.std.y)*27   # restore original data scale.

# Calculate the MSE and plot 
mean((y.valid - nnet.predict)^2) # MSE = 1.51725
plot(y.valid, nnet.predict, main = "Artificial Neural Network Predictions vs Actual (MSE = 1.52)",
     xlab = "Actual", ylab = "Predictions", pch = 19, col = "blue")


# -------------------------------------
# ANN model for regression is the best
# make predictions on the test data
# -------------------------------------

yhat.test <- predict(nnet.fit, newdata = data.test.std)*27   # restore original data scale.




# -------------
# FINAL RESULTS
# -------------

# Save final results for both classification and regression

length(chat.test) # check length = 2007
length(yhat.test) # check length = 2007
chat.test[1:10] # check this consists of 0s and 1s
yhat.test[1:10] # check this consists of plausible predictions of damt

ip <- data.frame(chat=chat.test, yhat=yhat.test) # data frame with two variables: chat and yhat
write.csv(ip, file="JRH.csv", row.names=FALSE) # use your initials for the file name

# submit the csv file in Angel for evaluation based on actual test donr and damt values
