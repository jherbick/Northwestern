

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


# Read in a .csv file
# Read in the full abalone dataset
fileLocation <- "C:/Users/james/Documents/Northwestern/PREDICT 454_Adv Modeling/Project/covtype.data"
fullDataSet <- read.table (file = fileLocation, header = FALSE, sep = ",", stringsAsFactors=FALSE, col.names = names)

# Validate full dataset per Prof. Martin instructions
# Gives metadata of an object
str(fullDataSet)















