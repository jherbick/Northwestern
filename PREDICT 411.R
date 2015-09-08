#-------------------------------
# PREDICT 411 MONEYBALL, UNIT 01
#-------------------------------

# Read in a .csv file
fileLocation <- "C:/Users/James R. Herbick/Documents/Northwestern/PREDICT 411_GLM/UNIT 01/moneyball/moneyball.csv"
fullDataSet <- read.table (file = fileLocation, header = TRUE, sep = ",")

# Validate full dataset per Prof. Martin instructions
# Similar to a proc contents in SAS
str(fullDataSet)

# Create a data frame
moneyballDF <- data.frame(fullDataSet)

# Check the number of rows and columns
nrow(moneyballDF)
ncol(moneyballDF)
dim(moneyballDF)  # number of rows and columns of the data.frame

# Check the column names of the data.frame
names(moneyballDF)

# Print first few rows, this is similar to proc print (obs=10) in SAS
head(moneyballDF)

class(moneyballDF$INDEX)

# CHECK SUMMARY STATISTICS FOR COLUMNS (MEAN, MISSING VALUES)
summary(moneyballDF$TEAM_BATTING_SO)
summary(moneyballDF$TEAM_BASERUN_SB)
summary(moneyballDF$TEAM_BASERUN_CS)
summary(moneyballDF$TEAM_BATTING_HBP)
summary(moneyballDF$TEAM_PITCHING_SO)
summary(moneyballDF$TEAM_FIELDING_DP)

# SET NA'S TO THE MEAN
# need plyr package??  SEE p. 3167 of ebook for R for Everyone.
moneyballDF$TEAM_BATTING_SO[is.na(moneyballDF$TEAM_BATTING_SO)] <- 735.6
moneyballDF$TEAM_BASERUN_SB[is.na(moneyballDF$TEAM_BASERUN_SB)] <- 124.8
moneyballDF$TEAM_BASERUN_CS[is.na(moneyballDF$TEAM_BASERUN_CS)] <- 52.8
moneyballDF$TEAM_BATTING_HBP[is.na(moneyballDF$TEAM_BATTING_HBP)] <- 59.36
moneyballDF$TEAM_PITCHING_SO[is.na(moneyballDF$TEAM_PITCHING_SO)] <- 817.7
moneyballDF$TEAM_FIELDING_DP[is.na(moneyballDF$TEAM_FIELDING_DP)] <- 146.4

# Check to see if any NAs remain
any(is.na(moneyballDF$TEAM_BATTING_SO))
any(is.na(moneyballDF$TEAM_BASERUN_SB))
any(is.na(moneyballDF$TEAM_BASERUN_CS))
any(is.na(moneyballDF$TEAM_BATTING_HBP))
any(is.na(moneyballDF$TEAM_PITCHING_SO))
any(is.na(moneyballDF$TEAM_FIELDING_DP))

# create new variables (### CHECK THIS CODE ###) see p. 3167,
moneyballDF$WALK_RATIO <- with(moneyballDF, TEAM_BATTING_BB / TEAM_BATTING_SO)
moneyballDF$EXTRA_BASE_HITS <- with(moneyballDF, TEAM_BATTING_2B + TEAM_BATTING_3B + TEAM_BATTING_HR)
moneyballDF$TIMES_ON_BASE <- with(moneyballDF, TEAM_BATTING_H + TEAM_BATTING_BB + TEAM_BATTING_HBP)
moneyballDF$STRIKEOUT_RATIO <- with(moneyballDF, TEAM_PITCHING_SO / TEAM_PITCHING_BB)
# can't get the log function to work.
moneyballDF$LOG_TARGET_WINS <- with (moneyballDF, log(TARGET_WINS)

# Drop outliers
moneyballDF <- moneyballDF[moneyballDF$TEAM_BATTING_SO > 0, ]
moneyballDF <- moneyballDF[moneyballDF$TEAM_BATTING_SO < 1303, ]
moneyballDF <- moneyballDF[moneyballDF$TEAM_BATTING_BB < 765, ]
moneyballDF <- moneyballDF[moneyballDF$TEAM_BATTING_BB > 275, ]
moneyballDF <- moneyballDF[moneyballDF$TEAM_BATTING_H <= 1742, ]
moneyballDF <- moneyballDF[moneyballDF$TEAM_BATTING_H >= 1116, ]
moneyballDF <- moneyballDF[moneyballDF$TEAM_PITCHING_SO > 0, ]
moneyballDF <- moneyballDF[moneyballDF$TEAM_PITCHING_SO < 1490, ]
moneyballDF <- moneyballDF[moneyballDF$TEAM_PITCHING_BB < 810, ]
moneyballDF <- moneyballDF[moneyballDF$TEAM_PITCHING_BB > 284, ]


# no variable selection methods in R?  yes there are.. see p. 7141
# attempt variable selection using stepwise (add and subtract variables)
# specifies a low end model, will use my final model as upper bound.
null_model <- lm(TARGET_WINS ~ 1, data=moneyballDF)

# perform stepwise variable selection
stepwise_model <- step(null_model, scope=list(lower=null_model, upper=final_model), direction="both")

# view model and coefficients
stepwise_model


# Build final model
# requires coefplot package
final_model <- lm(TARGET_WINS ~ TEAM_BATTING_3B + TEAM_BASERUN_SB + TEAM_BASERUN_CS + TEAM_BATTING_HBP + TEAM_PITCHING_H + TEAM_PITCHING_HR + TEAM_PITCHING_BB + TEAM_FIELDING_E + TEAM_FIELDING_DP + WALK_RATIO + TIMES_ON_BASE + EXTRA_BASE_HITS, data=moneyballDF)
summary(final_model)
coefplot(final_model)
AIC(final_model)
BIC(final_model)




#-------------------------------
# PREDICT 411 INSURANCE, UNIT 02
#-------------------------------

# Read in a .csv file
fileLocation <- "C:/Users/James R. Herbick/Documents/Northwestern/PREDICT 411_GLM/UNIT 02/Insurance/logit_insurance.csv"
fullDataSet <- read.table (file = fileLocation, header = TRUE, sep = ",")
# fullDataSet <- read.csv(fileLocation, head=T)


# Validate full dataset per Prof. Martin instructions
# Similar to a proc contents in SAS
str(fullDataSet)

# Create a data frame, stringsAsFactors limits the variables turned into factors
insuranceDF <- data.frame(fullDataSet, stringsAsFactors=FALSE)

# Check the number of rows and columns
nrow(insuranceDF)
ncol(insuranceDF)
dim(insuranceDF)  # number of rows and columns of the data.frame

# Check the column names of the data.frame
names(insuranceDF)

# Print first few rows, this is similar to proc print (obs=10) in SAS
head(insuranceDF)


# CHECK SUMMARY STATISTICS FOR COLUMNS (MEAN, MISSING VALUES)
summary(insuranceDF[c("AGE", "CLM_FREQ", "YOJ")])


# I think I need to remove the $ from HOME_VAL and then turn numeric
# need stringr package
# homeValList <- str_split(string = insuranceDF$HOME_VAL, pattern = "$")
# head(homeValList)

# ***************
class(insuranceDF$INCOME)
#this may not be working....
insuranceDF$HOME_VAL <- as.character(insuranceDF$HOME_VAL)
insuranceDF$INCOME <- as.character(insuranceDF$INCOME)

# need stringr package
insuranceDF$HOME_VAL_FIX <- str_sub(insuranceDF$HOME_VAL,2,)
# str_extract_all(insuranceDF$HOME_VAL_FIX, ",")
insuranceDF$HOME_VAL_FIX <- ifelse(insuranceDF$HOME_VAL_FIX == "", "161,160", as.character(insuranceDF$HOME_VAL_FIX))
# need to remove the commas before doing this step!!!
split <- str_split(string=insuranceDF$HOME_VAL_FIX, pattern=",")
split2 <- data.frame(Reduce(rbind,split))

split2a <- str_trim(as.character(split2$X1),side="both")
split2b <- str_trim(as.character(split2$X2),side="both")

split3 <- paste(split2a, split2b, sep = "")
split4 <- as.numeric(split3)

insuranceDF$HOME_VAL_FIX <- split4

# **************

insuranceDF$INCOME[is.na(insuranceDF$INCOME)] <- "$54028.17"



# SET NA'S TO THE MEDIAN
# need plyr package??  SEE p. 3167 of ebook for R for Everyone.
# should try to create new variables and set flags here
# this seems to use filtering to assign a value to a subset of the vector
insuranceDF$AGE[is.na(insuranceDF$AGE)] <- 45.0
insuranceDF$JOB[is.na(insuranceDF$JOB)] <- "z_Blue Collar"
insuranceDF$YOJ[is.na(insuranceDF$YOJ)] <- 11.0
# insuranceDF$HOME_VAL[is.na(insuranceDF$HOME_VAL)] <- 161159.53
# insuranceDF$INCOME[is.na(insuranceDF$INCOME)] <- 54028.17
insuranceDF$CAR_AGE[is.na(insuranceDF$CAR_AGE)] <- 8.0

# Another way to do this with new variable, from Raymond.
# First thing to do is remove the NA's aka Missing variables. You can do this however 
# you choose. I want to impute with the mean.
# Get mean of Home value to impute for missing
# imp_homeVal <- mean(data$HOME_VAL, na.rm=T)

# Create 'fix' varaible to impute mean for missing
# data$home_val_fix <- ifelse(is.na(data$HOME_VAL),imp_homeVal,data$HOME_VAL)


# Check to see if any NAs remain
any(is.na(insuranceDF$AGE))
any(is.na(insuranceDF$JOB))
any(is.na(insuranceDF$YOJ))
any(is.na(insuranceDF$HOME_VAL_FIX))
# any(is.na(insuranceDF$INCOME))
any(is.na(insuranceDF$CAR_AGE))


# Re bin variables
#insuranceDF$HOME_VAL_BIN=0

#a <- which(insuranceDF$HOME_VAL == 0) 
#a
#insuranceDF[a, "HOME_VAL_BIN"] <- 0
#insuranceDF$HOME_VAL_BIN

#IF 0 < HOME_VAL <= 125000 THEN HOME_VAL_BIN=1;
#IF 125000 < HOME_VAL <= 162000 THEN HOME_VAL_BIN=2; 
#IF 162000 < HOME_VAL <= 234000 THEN HOME_VAL_BIN=3;
#IF 234000 < HOME_VAL <= 370000 THEN HOME_VAL_BIN=4;
#IF HOME_VAL > 370000 THEN HOME_VAL_BIN=5;


## Bin HOME_VAL_FIX varaible
insuranceDF$home_val_bin <- ifelse(insuranceDF$HOME_VAL_FIX == 0, 0, 
                     ifelse(insuranceDF$HOME_VAL_FIX <= 125000, 1, 
                     ifelse(insuranceDF$HOME_VAL_FIX <= 162000, 2,
                     ifelse(insuranceDF$HOME_VAL_FIX <= 234000, 3,
                     ifelse(insuranceDF$HOME_VAL_FIX <= 370000, 4,5)))))
summary(insuranceDF$home_val_bin)

# Check the Mix/Max of each group to confirm everything is Binned Correctly

#install.packages('psych') # run if you haven't installed this package Before
#library(psych) # Load the package

describeBy(insuranceDF$HOME_VAL_FIX, insuranceDF$home_val_bin)

# need to add income and oldclaim back in, if I fix it above.
crashCar <- glm(TARGET_FLAG ~ URBANICITY + JOB + home_val_bin + KIDSDRIV + CAR_TYPE + EDUCATION + MSTATUS + YOJ, data=insuranceDF, family=binomial(link="logit"))
summary(crashCar)







