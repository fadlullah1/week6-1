cars <- mtcars
str(cars)
View(cars)
# Question1:  Relationship between Car Weight and Fuel Efficiency

# Objective: Examine if there's a link between the weight of cars and their fuel efficiency, measured by miles per gallon (MPG).

# Hypothesis Test:
# Null Hypothesis (H0): There is no correlation between car weight and MPG.
# Alternative Hypothesis (H1): There is a correlation between car weight and MPG.

# Variables:
# mpg: Continuous variable representing miles per gallon.
# wt: Continuous variable representing weight in 1000 lbs.

# Data Conversion: No data conversion is necessary as both variables are already in suitable formats for correlation analysis.


# Question2: Distribution Check and Statistical Test Selection

# Objective: Verify the normal distribution of MPG and weight variables.

# Load necessary library
library(ggplot2)

windows(20,10)
# Plot histogram for mpg
ggplot(cars, aes(x = mpg)) + 
  geom_histogram(binwidth = 1, fill = "blue", color = "black") + 
  ggtitle("Histogram of Miles Per Gallon (MPG)") +
  xlab("Miles Per Gallon") + 
  ylab("Frequency")

windows(20,10)
# Plot histogram for wt
ggplot(cars, aes(x = wt)) + 
  geom_histogram(binwidth = 0.25, fill = "red", color = "black") + 
  ggtitle("Histogram of Car Weight (1000 lbs)") +
  xlab("Weight (1000 lbs)") + 
  ylab("Frequency")


# Q-Q plot for the 'mpg' variable
attach(cars)

windows(20,10)
qqnorm(mtcars$mpg, main = "Q-Q Plot for Miles Per Gallon (MPG)")
qqline(mtcars$mpg, col = "red")  # Adding a reference line

# Q-Q plot for the 'wt' variable
windows(20,10)
qqnorm(mtcars$wt, main = "Q-Q Plot for Car Weight")
qqline(mtcars$wt, col = "red")  # Adding a reference line

# Methods: Use visual methods (like histograms and Q-Q plots) and 
# statistical tests (like Shapiro-Wilk) to check for normal distribution.


normality_test <- shapiro.test(cars$mpg)
normality_test
normality_test$p.value

library(psych)

windows(16,10)
pairs(cars, labels = colnames(cars),main = "cars dataset correlation plot")

windows(20,10)
pairs.panels(cars,
             smooth = FALSE,
             scale = FALSE,
             density = TRUE,
             ellipses = FALSE,
             method = "spearman",
             pch = 21,
             lm =FALSE,
             cor = TRUE,
             jiggle = FALSE,
             factor = 2,
             hist.col = 4,
             stars = TRUE,
             ci = TRUE)

cars1 <- na.omit(cars)
cars1
sapply(cars,function(x) sum(is.na(x)))

cor.test(cars$wt,cars$mpg, method ="pearson")

summary(cars)



