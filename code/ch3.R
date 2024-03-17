# 1. Conceptual preparation for statistical inference.
# 2. Learn to separate data preparation and analysis into different program files.
# 3. Learn to prepare data for analysis using R.
# 4. Learn to conduct null hypothesis testing using R.
# 5. Learn to construct confidence interval using R.
# 6. Learn to conduct statistical inference using one-sample t-test and the
# difference-of-means test.

# install.packages(c("DataCombine", "ggplot2", "Rmisc", "stargazer"), 
#                  dependencies=TRUE)

# rm(list=ls(all=TRUE))

library(tidyverse)

# comma-delimited data file
pwt7 <- read.csv("data/pwt70_w_country_names.csv", header = TRUE, strip.white = TRUE,
                 stringsAsFactors = FALSE, na.strings = c("NA",""))

# sort in ascending order
pwt7 <- pwt7[order(pwt7$country, pwt7$year),]

# remove second set of China observation isocodes
pwt7 <- pwt7[pwt7$isocode != "CH2",]

# annual economic growth rate in a panel dataset
library(DataCombine)

# sort data in ascending order by country and by year
pwt7 <- pwt7[order(pwt7$country, pwt7$year),]

# one-year leading variable for rgdpl
pwt7 <- slide(pwt7, Var = "rgdpl", NewVar = "rgdplead", 
              GroupVar = "country", slideBy = 1)

# one-year lagged variable for rgdpl
pwt7 <- slide(pwt7, Var = "rgdpl", NewVar="rgdplag", 
              GroupVar = "country", slideBy = -1)

# annual growth rate based on rgdpl
pwt7$growth <- (pwt7$rgdpl - pwt7$rgdplag) / pwt7$rgdplag

# a subset of pwt7 with three variables
pwt7g <- pwt7[, c("country", "year", "rgdpl", "growth")]

# save R dataset
save(pwt7g, file = "data/pwt7g.RData")


# before running program, remove all objects in workspace
rm(list = ls(all = TRUE))

# load R data
load("data/pwt7g.RData")

# sample mean of growth
# one estimate of the population mean based on one sample
mean(pwt7g$growth, na.rm = TRUE)

# sample variance of growth
# an estimate of the population variance
var(pwt7g$growth, na.rm = TRUE)

# compute sample SD
sd(pwt7g$growth, na.rm = TRUE)

# one sample mean t-test
# assuming average growth rate is 3%
# our average growth rate is 2.3%
t.test(pwt7g$growth, mu = 0.03)


library(ggplot2)

ggplot(pwt7g, aes(growth * 100)) +
  geom_histogram()

# summary statistics
library(Rmisc)

# mean and CI
growm <- summarySE(pwt7g, measurevar = "growth", na.rm = TRUE)
class(growm)

# mean and 95% CI
ggplot(growm, aes(x = factor(""), y = growth)) +
  geom_errorbar(aes(ymin = growth - ci, ymax = growth + ci), width = .1) +
  geom_point() +
  scale_x_discrete("") +
  scale_y_continuous(
    name = "Economic Growth",
    limits = c(0.021, 0.025),
    breaks = scales::pretty_breaks(n = 8)
  )


# sample average growth rate in 1960
mean(pwt7g$growth[pwt7g$year == 1960], na.rm = TRUE)

# sample average growth rate in 1990
mean(pwt7g$growth[pwt7g$year == 1990], na.rm = TRUE)

# difference between sample means of 1960 and 1990
mean(pwt7g$growth[pwt7g$year == 1960], na.rm = TRUE) -
  mean(pwt7g$growth[pwt7g$year == 1990], na.rm = TRUE)


# Scenario 1: The populations are independent, normally distributed with a common variance.
# Scenario 2: The populations are independent, normally distributed wit unequal variances.
# Scenario 3: The populations are dependent, normally distributed.
# Scenario 4: The populations are dependent and not normally distributed.


# Difference-of-means test under scenario 1
t.test(pwt7g$growth[pwt7g$year == 1960],
       pwt7g$growth[pwt7g$year == 1990],
       var.equal = TRUE)

# Difference-of-means test under scenario 2
# Welch’s two-sample t-test
t.test(pwt7g$growth[pwt7g$year == 1960],
       pwt7g$growth[pwt7g$year == 1990],
       var.equal = FALSE)

# Difference-of-means test under scenario 3
t.test(pwt7g$growth[pwt7g$year == 1960],
       pwt7g$growth[pwt7g$year == 1990],
       var.equal  = FALSE, paired = TRUE)


# generate two separate datasets
g1960 <- pwt7g[pwt7g$year == 1960, c("growth", "country")]
g1990 <- pwt7g[pwt7g$year == 1990, c("growth", "country")]

# merge two datasets by country
g <- merge(g1960, g1990, by = "country")

# show variable names
names(g)

# Difference-of-means test under scenario 3
t.test(g$growth.x, g$growth.y, var.equal = FALSE, paired = TRUE)


# Wilcoxon Signed-Rank TestUnder Scenario 4
# non-parametric Wilcoxon signed-rank test

# compute median growth rates and their difference
median60 <- median(pwt7g$growth[pwt7g$year == 1960], na.rm = TRUE)
median90 <- median(pwt7g$growth[pwt7g$year == 1990], na.rm = TRUE)
dif <- median60 - median90

# Non-parametric Wilcoxon signed-rank test under scenario 4
wilcox.test(pwt7g$growth[pwt7g$year == 1960],
            pwt7g$growth[pwt7g$year == 1990],
            paired = TRUE)

wilcox.test(g$growth.x, g$growth.y, paired = TRUE)


library(Rmisc)

# create dataset of mean and confidence interval
growm2 <- summarySE(pwt7g, measurevar = "growth",
                    groupvars = "year", na.rm = TRUE)

# keep only data from 1960 and 1990
growm2 <- growm2[growm2$year == 1960|growm2$year == 1990,]

library(ggplot2)

# convert year variable into a factor variable
growm2$year <- as.factor(growm2$year)



# plot mean and 95% CI
ggplot(growm2, aes(x = year, y = growth)) +
  geom_errorbar(aes(ymin = growth - ci, ymax = growth + ci), width = .1)+
  geom_point()+
  scale_x_discrete(name = "Year")+
  scale_y_continuous(name = "Economic Growth",
                     limits = c(-0.01, 0.06),
                     breaks = scales::pretty_breaks(n = 7))


### Miscellaneous
# Calculation of sample mean, variance, SD

# number of non-missing values of growth: n
n <- sum(!is.na(pwt7g$growth))


# find sample mean of growth: m1
m1 <- mean(pwt7g$growth, na.rm = TRUE)

# squared deviation of growth for each observation: d2
d2 <- (pwt7g$growth - m1)^2

# sum of squared deviations of growth: s3
s3 <- sum(d2, na.rm = TRUE)

# sample variance of growth: var2
var2 <- s3 / (n - 1)

# sample SD of growth: std2
std2 <- sqrt(var2)

# results
cbind(n, s3, m1,var2, std2)

# mean of growth
sum(pwt7g$growth, na.rm = TRUE) / n

# sample variance
sum((pwt7g$growth - mean(pwt7g$growth, na.rm = TRUE))^2, na.rm = TRUE) / (n - 1)

# sample SD
sqrt(sum((pwt7g$growth - mean(pwt7g$growth, na.rm = TRUE))^2, na.rm = TRUE) / (n - 1))


## One-sample t-test
# t statistic for null hypothesis (population mean=0.03)
t <- (m1 - 0.03)/(std2 / sqrt(n))

# results 
cbind(n, m1, std2, t)

# confirm the results
t.test(pwt7g$growth, mu = 0.03)

## 95% CI calculation
# quantile t distribution critical value for 95% CI
# for 99% CI = 0.995
t.c <- qt(0.975, n - 1)

# lower and upper bounds
m1 - t.c * (std2 / sqrt(n))
m1 + t.c * (std2 / sqrt(n))

# confirm the results
t.test(pwt7g$growth, mu = 0.03, conf.level = 0.95)

## Difference-of-means test and CI
mean70 <- mean(pwt7g$growth[pwt7g$year == 1970], na.rm = TRUE)
mean80 <- mean(pwt7g$growth[pwt7g$year == 1980], na.rm = TRUE)

n70 <- sum(!is.na(pwt7g$growth[pwt7g$year == 1970]))
n80 <- sum(!is.na(pwt7g$growth[pwt7g$year == 1980]))

se70 <- sd(pwt7g$growth[pwt7g$year == 1970], na.rm = TRUE) / sqrt(n70)
se80 <- sd(pwt7g$growth[pwt7g$year == 1980], na.rm = TRUE) / sqrt(n80)

mean_dif <- (mean70 - mean80)
se_dif <- sqrt(se70^2 + se80^2)
t_dif <- mean_dif / se_dif

revised_df <- ((se70^2 / n70 + se80^2 / n80)^2) / (((1/(n70 - 1)) * (se70^2 / n70)^2) 
                                                   + ((1 / (n80 - 1)) * (se80^2 / n80)^2))

# results
round(cbind(mean70, mean80, se70, se80, mean_dif, se_dif, t_dif, revised_df), 4)


# 95% CI of difference of means
# difference of means plus or minus
# (95% t-critical*standard error of difference of means)

mean_dif + qt(c(0.025, 0.975), revised_df) * se_dif

round(c(mean_dif + qt(c(0.025, 0.975), revised_df) * se_dif), 4)

# confirm the  results
t.test(pwt7g$growth[pwt7g$year == 1970], pwt7g$growth[pwt7g$year == 1980], var.equal  = FALSE)

# F test of whether two populations have equal variances
var.test(pwt7g$growth[pwt7g$year == 1960], pwt7g$growth[pwt7g$year == 1990])

# step-by-step calculation of F test
var(pwt7g$growth[pwt7g$year == 1960], na.rm = TRUE)
var(pwt7g$growth[pwt7g$year == 1990], na.rm = TRUE)

F.ratio <- var(pwt7g$growth[pwt7g$year == 1960], na.rm = TRUE) / var(pwt7g$growth[pwt7g$year == 1990], na.rm = TRUE)
















