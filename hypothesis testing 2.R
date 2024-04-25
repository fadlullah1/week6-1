newcars <- mtcars
#Objective: Examine the relationship between horsepower and the type of gearbox (automatic vs. manual).
#Hypothesis Test:
 # Null Hypothesis (H0): There is no correlation between horsepower and gearbox type.
#Alternative Hypothesis (H1): There is a correlation between horsepower and gearbox type.
#Variables:
 # hp: Continuous variable representing gross horsepower.
#am: Categorical variable representing transmission type (0 for automatic, 1 for manual).
#Data Conversion: Ensure am is treated as a categorical factor in R.

newcars$am <- factor(newcars$am, labels = c("automatic","manual"))
str(newcars)
View(newcars)

newcars$vs <- factor(newcars$vs, labels = c("v_shaped","straight"))
str(newcars)
View(newcars)

library(ggplot2)

windows(20,10)
# Plot histogram for mpg
ggplot(newcars, aes(x = hp)) + 
  geom_histogram(binwidth = 1, fill = "blue", color = "black") + 
  ggtitle("Histogram of hp") +
  xlab("Miles Per Gallon") + 
  ylab("Frequency")


# Assuming the 'mtcars' dataset is loaded
# Load the ggplot2 library
library(ggplot2)

# Create a bar plot for the 'am' variable
ggplot(newcars, aes(x = factor(am), fill = factor(am))) +
  geom_bar() +
  scale_fill_manual(values = c("blue", "red"), labels = c("Automatic", "Manual")) +
  labs(x = "Transmission Type", y = "Count", title = "Distribution of Transmission Types") +
  theme_minimal()


windows(20,10)
par(mfrow = c(1,2))
with(newcars,{
  qqnorm(hp[am == "automatic"],
         main = "automatic transmission")
  qqline(hp[am=="automatic"])
})

with(newcars,{
  qqnorm(hp[am == "manual"],
         main = "manual transmision")
  qqline(hp[am=="manual"])
})

tapply(newcars$hp,newcars$am, shapiro.test)

install.packages("rstatix")
library(rstatix)
attach(newcars)
wilcox.test(hp~am)

windows(20,10)
pairs.panels(newcars,
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