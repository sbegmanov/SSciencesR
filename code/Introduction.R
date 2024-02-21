### When to use R in a research project
# 1. Identify a research problem
# 2. Survey the literature (Find out what is known about the problem)
# 3. Formulate a theoretical argument and some testable hypothesis
# 4. Measure concepts
# 5. Collect data
## 6. Prepare data
## 7. Analyze data
## 8. Report findings and implications

# efficiency of an analysis - a range of tasks (as importing data into software, 
# merging different datasets together, verifying data, creating new variables, 
# recoding and renaming variables, visualizing data, running statistical 
# estimation procedures, carrying out diagnostic tests, and so on)

# reproducibility and integrity of the analysis - an analyst needs to be able 
# to reproduce his or her own analysis, including dataset construction and 
# estimation results, even years later.

### Calculate descriptive statistics for a vector

v1 <- c(1, 2, 0, 2, 4, 5, 10, 1)

# find the number of observations in variable v1
length(v1)

# find v1's sample mean (two ways: mean function or formula)

mean(v1)
sum(v1)/length(v1)

# find v1's sample variance (variance function or formula)
var(v1)
sum((v1-mean(v1))^2)/(length(v1)-1)

# find v1's sample standard deviation (function
# or square root of sample variance)
sd(v1)
sqrt(var(v1))

# find v1's sample minimum and maximum using functions
max(v1)
min(v1)

### Handle missing values in descriptive statistics
v2 <- c(1, 2, 0, 2, NA, 5, 10, NA)

# compute mean of v2 without removing missing values
mean(v2)

# compute mean of v2 after removing missing values
mean(v2, na.rm = TRUE)

# display output from is.na() function
is.na(v2)

# display output from !is.na() function
!is.na(v2)

# find total number of observations in v2
length(v2)

# find the number of missing values in v2
sum(is.na(v2))

# find the number of non-missing values in v2
sum(!is.na(v2))

## convert data vector v1 into a data frame vd
vd <- data.frame(v1)

# combine vectors v1 and v2 into data frame vd of two variables v1 and v2
vd <- data.frame(v1, v2)


### Produce descriptive statistics table
library(stargazer)

# produce formatted descriptive statistics of variables in data frame vd
stargazer(vd, type = "text")

# display dataset in a table format
stargazer(vd, type = "text", summary = FALSE, rownames = FALSE)

# add additional statistics to be reported median, interquartile 
# range (25th and 75th percentile)
stargazer(vd, type = "text", median = TRUE, iqr = TRUE)

# use c() function to choose statistics to be reported
stargazer(vd, type = "text", summary.stat = c("n", "mean", "median", "sd"))

# produce formatted select descriptive statistics of variables in vd
stargazer(vd, type = "text", summary.stat = c("n", "mean", "sd", "min",
                                              "p25", "median", "max"))
## display the frequency count of v1 
table(vd$v1)

# graph distribution of discrete variable vd$v1: bar chart
barplot(table(vd$v1))

# graph distribution of continuous variable vd$v1: box plot and histogram
boxplot(vd$v1)
hist(vd$v1)

# create a figure with two plots
# set graphical parameters for a figure of one row, two columns
par(mfrow = c(1, 2))

# graph distribution of continuous variable vd$v1: box plot and histogram
boxplot(vd$v1)
hist(vd$v1)

### data from Iversen and Sockie
# assign c() function output to vector object country
country <- c("Australia","Austria","Belgium","Canada",
             "Denmark","Finland","France","Germany","Ireland", "Italy",
             "Japan","Netherlands","New Zealand","Norway","Sweden","U.K.",
             "US")



