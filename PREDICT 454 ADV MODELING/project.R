
# ------------------------------------------------------
#
# PREDICT 454 - Adv Modeling Project - Forest Cover Type
#
# ------------------------------------------------------


# ----------------
# DATA ACQUISITION
# ----------------

# Establish a vector of column names for the data, based off of the data dictionary
names <- c("Elevation", "Aspect", "Slope", "Horizontal_Distance_To_Hydrology", "Vertical_Distance_To_Hydrology", "Horizontal_Distance_To_Roadways", "Hillshade_9am", "Hillshade_Noon", "Hillshade_3pm", "Horizontal_Distance_To_Fire_Points", "WA1", "WA2", "WA3", "WA4", "ST1", "ST2", "ST3", "ST4", "ST5", "ST6", "ST7", "ST8", "ST9", "ST10", "ST11", "ST12", "ST13", "ST14", "ST15", "ST16", "ST17", "ST18", "ST19", "ST20", "ST21", "ST22", "ST23", "ST24", "ST25", "ST26", "ST27", "ST28", "ST29", "ST30", "ST31", "ST32", "ST33", "ST34", "ST35", "ST36", "ST37", "ST38", "ST39", "ST40", "Cover_Type")


# Read in the forest cover data
fileLocation <- "C:/Users/james/Documents/Northwestern/PREDICT 454_Adv Modeling/Project/covtype.data"
fullDataSet <- read.table (file = fileLocation, header = FALSE, sep = ",", stringsAsFactors=FALSE, col.names = names)

# Set a new variable to Cover_Type as a factor
fullDataSet$Cover_Type_Factor <- as.factor(fullDataSet$Cover_Type)


# Validate full dataset per Prof. Martin instructions
# Gives metadata of an object
str(fullDataSet)


# --------------
# SPLIT THE DATA
# --------------

# Create a sample group column
# generates random set of numbers between 0 and 1

# is this repeatable???, I think so.
set.seed(123)
fullDataSet$gp <- runif(dim(fullDataSet)[1])

# Set a train flag = 1 for those records in the training set.  0 for items in the test set.
fullDataSet$train <- as.integer(fullDataSet$gp <= .7)

# output this for use in Angoss
write.csv(fullDataSet, file = "forest_cover.csv")


# Split into train / test dataframes
train <- subset(fullDataSet, fullDataSet$gp <= .7)
test <- subset(fullDataSet, fullDataSet$gp > .7)


# -----
# NOTES
# -----

# looks like ST15 is all zeroes.
# don't want to standardize the last two columns... that were used to determine the training / test split
# probably don't want to standardize the cover type variable.
# maybe only standardize variables 1-10?

# do we need the Cover_Type variable to be a factor??  I think yes.
# do we need the dummy variables to be factors????



# ----------------------
# STANDARDIZE THE DATA??
# ----------------------

# see notes above.

# Standardize data
train.mean <- apply(train, 2, mean)
train.sd <- apply(train, 2, sd)

train.std <- t((t(train)-train.mean)/train.sd) # standardize to have zero mean and unit sd
apply(train.std, 2, mean) # check zero mean
apply(train.std, 2, sd) # check unit sd

test.std <- t((t(test)-train.mean)/train.sd) # standardize to have zero mean and unit sd
apply(test.std, 2, mean) # check zero mean
apply(test.std, 2, sd) # check unit sd




# ---
# EDA
# ---

library(ggplot2)
library(lattice)
library(latticeExtra)

# Examine the frequencies of the dependent variable
# -------------------------------------------------
table(train$Cover_Type)

hist.ct <- ggplot(train, aes(x=Cover_Type)) + geom_histogram(binwidth = .5, fill="darkblue") + ggtitle("Frequency Count of Cover_Type")
hist.ct + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())


# numerical summary of the data
# -----------------------------
summary(train)


# check for outliers
# ------------------
bwplot(~ Horizontal_Distance_To_Roadways | Cover_Type_Factor, data=train, main="Horizontal Distance to Roadway Boxplots", layout= c(3,3) )
bwplot(~ Horizontal_Distance_To_Hydrology | Cover_Type_Factor, data=train, main="Horizontal Distance to Hydrology Boxplots", layout= c(3,3) )
bwplot(~ Elevation | Cover_Type_Factor, data=train, main="Elevation Boxplots", layout= c(3,3) )


# check for correlations
# ----------------------
pairs(train[, 1:10])  # this runs a long time...break this up...




# run a classification tree
# -------------------------


library(tree)
attach(train)


# Fit a classification tree 
tree.forest.cover=tree(Cover_Type_Factor~.-train, gp, Cover_Type, fullDataSet)
summary(tree.wine)

# Display the tree graphically
plot(tree.forest.cover)
text(tree.forest.cover,pretty=0)
tree.forest.cover










# --------------
# Data Cleansing
# --------------

# need to clean horiz dist to Roadways, hydrology, and elevation at least

