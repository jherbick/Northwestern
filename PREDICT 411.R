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

# create new variables (### CHECK THIS COEE ###) see p. 3167,
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
fileLocation <- "C:/Users/James R. Herbick/Documents/Northwestern/PREDICT 411_GLM/UNIT 02/logit_insurance.csv"
fullDataSet <- read.table (file = fileLocation, header = TRUE, sep = ",")

# Validate full dataset per Prof. Martin instructions
# Similar to a proc contents in SAS
str(fullDataSet)

# Create a data frame
insuranceDF <- data.frame(fullDataSet)

# Check the number of rows and columns
nrow(insuranceDF)
ncol(insuranceDF)
dim(insuranceDF)  # number of rows and columns of the data.frame

# Check the column names of the data.frame
names(insuranceDF)

# Print first few rows, this is similar to proc print (obs=10) in SAS
head(insuranceDF)

# class(insuranceDF$INDEX)

# CHECK SUMMARY STATISTICS FOR COLUMNS (MEAN, MISSING VALUES)
summary(moneyballDF$TEAM_BATTING_SO)
summary(moneyballDF$TEAM_BASERUN_SB)
summary(moneyballDF$TEAM_BASERUN_CS)
summary(moneyballDF$TEAM_BATTING_HBP)
summary(moneyballDF$TEAM_PITCHING_SO)
summary(moneyballDF$TEAM_FIELDING_DP)








