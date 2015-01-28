#Question 9
#----------

# Read in the Coke.csv dataset
fileLocation <- "C:/Users/James R. Herbick/Documents/Northwestern/PREDICT 401/Exam 1/Coke.csv"
fullDataSet <- read.table (file = fileLocation, header = TRUE, sep = " ")

# Validate full dataset per Prof. Martin instructions
str(fullDataSet)

# Create a histogram for the Fill variable
ggplot(data=fullDataSet) + geom_histogram(aes(x=Fill), origin=339.0, binwidth=0.4, col="black", fill = "blue") + ggtitle("Coke Fill Frequencies")

# Create the 5-number summary
summary(fullDataSet)

# Create the stem and leaf plot
x <- fullDataSet[,1]
stem(x)


# Question 10
#------------

# Read in the Soap.csv dataset
fileLocation <- "C:/Users/James R. Herbick/Documents/Northwestern/PREDICT 401/Exam 1/Soap.csv"
SoapDataSet <- read.table (file = fileLocation, header = TRUE, sep = ",")

# Validate full dataset per Prof. Martin instructions
str(SoapDataSet)

# Create a histogram for the Sales variable
ggplot(data=SoapDataSet) + geom_histogram(aes(x=sales), col="black", fill = "blue") + ggtitle("Weekly Soap Sales")

# Create the 5-number summary
summary(SoapDataSet)

# Create box and whisker plot
ggplot(data=SoapDataSet) + geom_boxplot(aes(y=sales, x=week), outlier.colour="blue") 

# Create a scatterplot for sales and week
ggplot(data=SoapDataSet) + geom_point(aes(y=sales, x=week)) + ggtitle("Weekly Soap Sales")











