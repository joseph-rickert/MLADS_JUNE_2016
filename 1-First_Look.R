# First Look at R
# MLADS June 2016
# Joseph Rickert
# May 5, 2106
# R is an interpretive language. At its most basic, the console can bu used as a simple calculator.
# Notice that typing "#" is the way to introduce a comment
#
2 + 2 
log(10); sqrt(99); sin(pi)   # ";" lets you put simple statements on the same line
# R was designed for statistical computing. 
# Here we draw 1000 numbers from a Normal (Gaussian)
# distribution with mean 0 and variance 1 and assign them to the vector x all in one
# line of code
x <- rnorm(1000)
# Notice that nothing happened. R normally does not give you any output
# until you ask for it.
head(x, 10)   # Look at the first 10 numbers in the vector x
tail(x,5)     # Look at the last 5 numbers in the vector xlength(x)     
length(x)     # To check on the length of x
summary(x)    # Get a summary of x
plot(x)       # Produce a scatter plot of x the index (the numbers 1 to 1000)
hist(x)       # Look at the histogram
# Produce a fancier histogram with some color, a superimposed 
# standard normal distribution curve and  a "rug" underneath showing
# where the points are.
hist(x, freq = FALSE, col = "light grey")         
curve(dnorm,                                    # plot normal density
      col = "dark blue",                        # set coor of curve
      lwd=2,                                    # fill in the area under the curve
      add = TRUE)                               # add curve to existing plot
rug(x,col="red") 
# R is rich in built-in functions.
# In addition to the mathematical functions sin(), log() etc rnorm(), head(), tail(), 
# length(), summary(), plot(), hist(), curve() are all R functions
# Type ?foo to get help with the function foo
?rnorm
## R is vectorized. 
# There is not so much of a need to write loops in R

y <- 1:10         # Assign the numbers 1 to 10 to a vector
y                 # Print the vector
2 + y; 3 * y      # Perform vector addition and multiplication

# There are lots of built in data sets in R
# Type "data()" to see what data sets are available. This depends on what 
# packages you have loaded
# type "data(ds_name)" to load the data set. Then type "ds_name" to look at it
data()
data(mtcars)
# The data was extracted from the 1974 Motor Trend US magazine, and comprises fuel 
# consumption and 10 aspects of automobile design and performance for 32 automobiles 
# (1973–74 models).

help(mtcars)      #  or type ?mtcars

# mtcars is a data frame. This is the fundamental data structure for 
# doing statistical analysis
class(mtcars)     # To check on the class of an R object

head(mtcars)      #  to look at the first 6 rows


# Another way R is vectorized
# The sapply() function applies another function  to every column
# in the data frame simultaneously
sapply(mtcars,class)    # Gives class of each variable
sapply(mtcars,typeof)   # Gives type of each variable

# Some Simple Data Preparation
# The note on meaning of V/S in stackoverflow shows that vs is really a categorical variable
# Categorical variables are called "factors" in R
# http://stackoverflow.com/questions/18617174/r-mtcars-dataset-meaning-of-vs-variable
# Convert the variables vm and as to factprs
mtcars$vs <- factor(mtcars$vs,labels=c("V","S"))
mtcars$am <- factor(mtcars$am,labels=c("automatic","manual"))
head(mtcars)
sapply(mtcars,class)    # Gives class of each variable
sapply(mtcars,typeof)   # Gives type of each variable


# R shines at exploratory data analysis
summary(mtcars)     # summarize each variable in the data frame

plot(mtcars)        # Producer pairwise plots
cor(mtcars[,-c(8,9)])         # look at the correlations
df <-sapply(mtcars[,-c(8,9)],scale)         # center and scale all of the variables
boxplot(df, col="light grey")


# Fit a Simple Regression Model
reg1 <- lm(mpg ~ wt, data = mtcars)
summary(reg1)        # Notice this is the same summary function we used above on the vector x
# Plot the data with the regression line added
plot(mtcars$wt,mtcars$mpg, main="Regression Model")
abline(reg1,col="red")
par(mfrow=c(2,2))    # Some code to put the 4 plots together
plot(reg1) 

reg1$coefficients    # Pull out the coefficients

# Fit Multiple Regression Model
reg2 <- lm(mpg ~ wt + qsec + gear + vs + am, data = mtcars)
summary(reg2)
plot(reg2)
reg2$coefficients
str(reg2)            # Look at the model object

par(mfrow=c(1,1))
  
# Clean up
rm(mtcars)           # Remove mtcars
rm(list=ls())        # Remove everything: USE WITH GREAT CARE!!

