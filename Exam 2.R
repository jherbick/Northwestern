
#--------------------------------------------------
# DISHES Questions

# Read in a .csv file
fileLocation <- "C:/Users/James R. Herbick/Documents/Northwestern/PREDICT 401/Exam 2/dishes.csv"
fullDataSet <- read.table (file = fileLocation, header = TRUE, sep = ",")

# Validate full dataset per Prof. Martin instructions
str(fullDataSet)

# Saves the basis of a ggplot to a variable.  Then can add layers on top
# Breaks RATE up by the TYPE attribute
dishRate <- ggplot(fullDataSet, aes(x = TYPE, y = RATE))
dishRate + geom_boxplot(outlier.colour="blue") + ggtitle("Dish Rates by Dish Type")

dishPrice <- ggplot(fullDataSet, aes(x = TYPE, y = PRICE))
dishPrice + geom_boxplot(outlier.colour="blue") + ggtitle("Dish Prices by Dish Type")


# One Way ANOVA
dishRateANOVA <- aov(RATE~TYPE, data=fullDataSet)
summary(dishRateANOVA)

dishPriceANOVA <- aov(PRICE~TYPE, data=fullDataSet)
summary(dishPriceANOVA)


#------------------------------------------------------
# KEYBOARD Questions

# Read in a .csv file
fileLocation2 <- "C:/Users/James R. Herbick/Documents/Northwestern/PREDICT 401/Exam 2/keyboard.csv"
fullDataSet2 <- read.table (file = fileLocation2, header = TRUE, sep = ",")

# Validate full dataset per Prof. Martin instructions
str(fullDataSet2)

# Saves the basis of a ggplot to a variable.  Then can add layers on top
keyboard <- ggplot(fullDataSet2, aes(x = STYLE_A, y = STYLE_B))
keyboard + geom_point()

# Generate the 5-number summary to determine the IQR
summary(fullDataSet2)

# Side-by-side histograms for STYLE_A, STYLE_B
# Create 3 histograms plotting the WHOLE value by sex
plot1 <- ggplot(data=fullDataSet2) + geom_histogram(aes(x=STYLE_A), col="black", fill = "blue") + ggtitle("Style A Keyboards")
plot2 <- ggplot(data=fullDataSet2) + geom_histogram(aes(x=STYLE_B), col="black", fill = "blue") + ggtitle("Sytle B Keyboards")

# Put 2 histograms onto a single panel
grid.arrange(plot1, plot2, ncol=2)


# T tests
styleA <- fullDataSet2$STYLE_A
styleB <- fullDataSet2$STYLE_B
t.test(styleA, y=styleB, data=fullDataSet2, var.equal=TRUE, conf.level = 0.95)

# Paired T test
t.test(styleA, y=styleB, data=fullDataSet2, var.equal=TRUE, , paired=TRUE, conf.level = 0.95)

# Calculate the differences per student
keyboardDiff <- styleA - styleB

# Add DIFFERENCE column in the original dataset
newFullDataSet2 <- data.frame(fullDataSet2, DIFFERENCE=keyboardDiff)

# Plot a histogram of the differences in typing speed per student
ggplot(data=newFullDataSet2) + geom_histogram(aes(x=DIFFERENCE), col="black", fill = "blue") + ggtitle("Differences in Typing Speeds per Student, Keyboard A - Keyboard B")

# Generate quantile plot of the student typing differences
qqnorm(newFullDataSet2$DIFFERENCE, main = "Per Student Differences Keyboard A - Keyboard B")
qqline(newFullDataSet2$DIFFERENCE, datax = FALSE, distribution = qnorm, probs = c(0.25, 0.75), qtype=7)



