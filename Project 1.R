
# Read in the full abalone dataset
fileLocation <- "C:/Users/James R. Herbick/Documents/Northwestern/PREDICT 401/Report 1/abalone.csv"
fullDataSet <- read.table (file = fileLocation, header = TRUE, sep = " ")

# Validate full dataset per Prof. Martin instructions
str(fullDataSet)


# Question 1 - take a random sample
#----------------------------------

# Create index variable for selecting rows from abalone.csv
set.seed(123)
index <- sample(1:nrow(fullDataSet), 500)
# View the indexes created
str(index)
# Pull out the random sample from abalone.csv
abaloneSample <- fullDataSet[index,]

# Save sample to a file (not working right now)
# save(abaloneSample,file="C:/Users/James R. Herbick/Documents/Northwestern/PREDICT 401/Report 1/mydata.csv")


# Question 8 - Quantiles
#------------------------

female <- abaloneSample$SEX=="F"
male <- abaloneSample$SEX=="M"

femaleAbalone <- abaloneSample[female,]  #Isolate the females from the data sample
femaleWhole <- abaloneSample[female,5]   #Isolate only the WHOLE values for females
maleAbalone <- abaloneSample[male,]      #Isolate the males from the data sample
maleWhole <- abaloneSample[male,5]       #Isolate only the WHOLE values for males

quantile(femaleWhole, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))
quantile(maleWhole, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))


# Question 9 - histograms
#------------------------

hist(femaleWhole)   #Histogram of female whole values
hist(maleWhole)     #Histogram of male whole values


















