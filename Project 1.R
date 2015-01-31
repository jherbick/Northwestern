
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

# Save sample to a file
write.csv(abaloneSample, "C:/Users/James R. Herbick/Documents/Northwestern/PREDICT 401/Report 1/mydata.csv", row.names=T)


# Question 5 - Volume
#--------------------

# Read in the full abalone dataset
fileLocation <- "C:/Users/James R. Herbick/Documents/Northwestern/PREDICT 401/Report 1/mydata.csv"
sampleDataSet <- read.table (file = fileLocation, header = TRUE, sep = ",")

# Create vectors from sample data for each variable needed
length <- c(sampleDataSet[,3])
diam <- c(sampleDataSet[,4])
height <- c(sampleDataSet[,5])

# Create a new vector with the volume as defined in the problem.
volume <- length*diam*height

d <- data.frame(sampleDataSet, VOLUME=volume)

# Create new dataset with the VOLUME attribute
write.csv(d, "C:/Users/James R. Herbick/Documents/Northwestern/PREDICT 401/Report 1/volume.csv", row.names=F)

# Create 3 histograms plotting the WHOLE value by sex
plot1 <- ggplot(data=d) + geom_histogram(aes(x=VOLUME), binwidth=0.02, col="black", fill = "blue") + ggtitle("Volume Histogram, All Abalone")
plot2 <- ggplot(data=d) + geom_histogram(aes(x=WHOLE), binwidth=0.2, col="black", fill = "blue") + ggtitle("Whole Histogram, All Abalone")

# Put 2 histograms onto a single panel
grid.arrange(plot1, plot2, ncol=2)


# Question 8 - Quantiles
#------------------------

female <- abaloneSample$SEX=="F"
male <- abaloneSample$SEX=="M"
infant <- abaloneSample$SEX=="I"

femaleAbalone <- abaloneSample[female,]  #Isolate the females from the data sample
femaleWhole <- abaloneSample[female,5]   #Isolate only the WHOLE values for females
# Save whole values by sex to a file
write.csv(femaleWhole, "C:/Users/James R. Herbick/Documents/Northwestern/PREDICT 401/Report 1/femalewhole.csv", row.names=F)

maleAbalone <- abaloneSample[male,]      #Isolate the males from the data sample
maleWhole <- abaloneSample[male,5]       #Isolate only the WHOLE values for males
write.csv(maleWhole, "C:/Users/James R. Herbick/Documents/Northwestern/PREDICT 401/Report 1/malewhole.csv", row.names=F)

infantAbalone <- abaloneSample[infant,]  #Isolate the infants from the data sample
infantWhole <- abaloneSample[infant,5]   #Isolate only the WHOLE values for infants
write.csv(infantWhole, "C:/Users/James R. Herbick/Documents/Northwestern/PREDICT 401/Report 1/infantwhole.csv", row.names=F)

quantile(femaleWhole, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))
quantile(maleWhole, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))
quantile(infantWhole, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))

# Create normal quantile plots
# Create the normal probability plots
qqnorm(femaleWhole, main = "Female Abalone Whole Weight")
qqline(femaleWhole, datax = FALSE, distribution = qnorm, probs = c(0.25, 0.75), qtype=7)

qqnorm(maleWhole, main = "Male Abalone Whole Weight")
qqline(maleWhole, datax = FALSE, distribution = qnorm, probs = c(0.25, 0.75), qtype=7)

qqnorm(infantWhole, main = "Infant Abalone Whole Weight")
qqline(infantWhole, datax = FALSE, distribution = qnorm, probs = c(0.25, 0.75), qtype=7)


# Question 9 - histograms
#------------------------

# Create 3 histograms plotting the WHOLE value by sex
plot1 <- ggplot(data=femaleAbalone) + geom_histogram(aes(x=WHOLE), binwidth=0.2, col="black", fill = "blue") + ggtitle("Female Abalone Whole Weight")
plot2 <- ggplot(data=maleAbalone) + geom_histogram(aes(x=WHOLE), binwidth=0.2, col="black", fill = "blue") + ggtitle("Male Abalone Whole Weight")
plot3 <- ggplot(data=infantAbalone) + geom_histogram(aes(x=WHOLE), binwidth=0.1, col="black", fill = "blue") + ggtitle("Infant Abalone Whole Weight")
# Put 3 histograms onto a single panel
# Need grid and gridExtra packages here.
grid.arrange(plot1, plot2, plot3, ncol=3)



# Old way of generating histograms.  Works, but does not use ggplot2
#par(mfrow=c(1,3))  # Keep graphs together

#Female Histogram, whole values
#hist(femaleWhole, main="Female Abalone Whole Weight", col="blue", 
#     xlab="Female Abalone Whole Weight", ylim=c(0,35))

#Male Histogram, whole values
#hist(maleWhole, main="Male Abalone Whole Weight", col="blue", 
#     xlab="Male Abalone Whole Weight",  ylim=c(0,35))

#Infant Histogram, whole values
#hist(infantWhole, main="Infant Abalone Whole Weight", col="blue", 
#     xlab="Infant Abalone Whole Weight",  ylim=c(0,35))


# Absolute most basic histograms
#hist(femaleWhole)   #Histogram of female whole values
#hist(maleWhole)     #Histogram of male whole values
#hist(infantWhole)   #Histogram of infants whole values














